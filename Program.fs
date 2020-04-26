module CodeComplexityTrendAnalyzer.Program

open Argu
open Fake.IO
open Fake.IO.FileSystemOperators
open System

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CliArguments>(programName = "CodeComplexityTrendAnalyzer.exe")
    if argv.Length = 0 then 
        printfn "%s" <| parser.PrintUsage ()
        -1
    else 
        let argResults = parser.Parse(argv)
    
        let cmd = argResults.GetResult Command
        let repo = argResults.PostProcessResult (<@ Repository_Path @>, DirectoryInfo.ofPath)
        let file = argResults.GetResult Source_File
        let output = argResults.Contains Output_File
                

        let outputName n = 
            if output then
                let fileInfo = FileInfo.ofPath file
                let newFileInfo =  FileInfo.ofPath (fileInfo.Directory.FullName </> (sprintf "%s-%s.csv" n fileInfo.Name))
                Directory.ensure newFileInfo.Directory.FullName
                File.create newFileInfo.FullName
                newFileInfo.FullName
            else 
                n

        let writer name ss =
            let writeLine name s = 
                match output with
                | true -> appendToFile name s
                | false -> Console.Out.WriteLine(sprintf "%s: %s" name s)

            ss |> Seq.iter (writeLine (outputName name))

        let printDone n =
            let out = outputName n
            if output then Console.Out.WriteLine("Done. Check for output file {0}", out)
            else Console.Out.WriteLine("Done with {0}.", out)


        let git = Git.gitResult repo.FullName
        let revs = file |> (Git.revs git >> List.map (ROP.tee Database.saveCommitInfo))

        if cmd = All || cmd = Members then
            let getDiffsBetweenCommits = MemberAnalysis.getDiffsBetweenCommits git file
            let getFilesForCommits = MemberAnalysis.getFilesForCommits git file 

            revs
            |> List.pairwise // NOTE, unless there is some caching, this will do double the work unnecessarily
            |> List.map (CommitPair.ofTuple 
                         >> getDiffsBetweenCommits 
                         >> getFilesForCommits 
                         >> MemberAnalysis.getMemberChangeData 
                         >> List.map (ROP.tee Database.toTable))
            |> List.collect id
            |> (MemberAnalysis.asCsv >> writer (nameof MemberAnalysis))

            printDone "MethodAnalysis"
        
        if cmd = All || cmd = File then
            let getFileAtRev = Git.getFileAtRevMemoized git file

            revs
            |> List.map (getFileAtRev
                         >> FileComplexityAnalysis.toFileComplexity
                         >> ROP.tee Database.toTable)
            |> (FileComplexityAnalysis.asCsv >> writer (nameof FileComplexityAnalysis))

            printDone "FileComplexity"


        0 // return an integer exit code
