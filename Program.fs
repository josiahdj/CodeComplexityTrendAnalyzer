﻿open CodeComplexityTrendAnalyzer
open Argu
open Fake.IO
open Fake.IO.FileSystemOperators
open System

type CliCommand =
    | Members
    | File
    | All
type CliArguments = 
    | [<MainCommand; ExactlyOnce; First>] Command of command : CliCommand
    | [<Mandatory; AltCommandLine("-r")>] Repository_Path of repo : string
    | [<Mandatory; AltCommandLine("-s")>] Source_File of file : string
    | [<AltCommandLine("-o")>] Output_File
with 
    interface IArgParserTemplate with
        member x.Usage =
            match x with
            | Command _ -> "Choose 'members' to return the member-level (constructors, methods, properties) file analysis; choose 'file' to return the file-level complexity growth analysis; choose 'all' to run both."
            | Repository_Path _ -> "Specify a repository path, e.g. C:\repo_root_dir"
            | Source_File _ -> "Specify a source file to be analyzed. The path is relalive to the repository directory, e.g. path/to/file.ext"
            | Output_File _ -> "Output results to a file. The analysis files will be named after the source file. The binary's directory will be used."

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

        let git = Git.gitResult repo.FullName

        let outputName n = 
            if output then
                let fileInfo = FileInfo.ofPath file
                let newFileInfo =  FileInfo.ofPath (fileInfo.Directory.FullName </> (sprintf "%s-%s.csv" n fileInfo.Name))
                Directory.ensure newFileInfo.Directory.FullName
                File.create newFileInfo.FullName
                newFileInfo.FullName
            else 
                n

        let writeLine name s = 
            match output with
            | true -> appendToFile name s
            | false -> Console.Out.WriteLine(sprintf "%s: %s" name s)

        let writer name ss =
            ss |> Seq.iter (writeLine (outputName name))

        let printDone n =
            let out = outputName n
            if output then Console.Out.WriteLine("Done. Check for output file {0}", out)
            else Console.Out.WriteLine("Done with {0}.", out)

        if cmd = All || cmd = Members then
            let getFileChangesAtRev = MemberAnalysis.getFileChangesAtRev git file
            let getFileAtRev = MemberAnalysis.getFileAtRev git file 
            let getMemberRevisions = CommitPair.ofTuple >> getFileChangesAtRev >> getFileAtRev >> MemberAnalysis.getRawData

            file 
            |> (Git.revs git >> ROP.tee Database.toTable)  // get from DB, if available?
            |> List.pairwise // NOTE, unless there is some caching, this will do double the work unnecessarily
            |> List.map (getMemberRevisions >> ROP.tee Database.toTable)
            |> List.collect id
            |> (MemberAnalysis.asCsv >> writer (nameof MemberAnalysis))

            printDone "MethodAnalysis"
        
        if cmd = All || cmd = File then
            file
            |> (Git.revs git >> ROP.tee Database.toTable)  // get from DB, if available?
            |> (FileComplexityAnalysis.getRawData git file
                >> ROP.tee Database.toTable
                >> FileComplexityAnalysis.asCsv 
                >> writer (nameof FileComplexityAnalysis))

            printDone "FileComplexity"


        0 // return an integer exit code
