namespace CodeComplexityTrendAnalyzer

module Program =

    open Fake.IO
    open Fake.IO.FileSystemOperators
    open System
    open System.IO
    open CodeComplexityTrendAnalyzer
    open CommandLineParameters

    type ApplicationArguments =
        {
            Cmd: CliCommand
            RepoPath:DirectoryInfo
            File: string
            Output: bool
            StartDate: DateTime
        }

    type OutputTarget =
    | FileInfo of FileInfo
    | Console of StepName:string

    let getOutputTarget shouldWriteToFile filePath stepName = 
        if shouldWriteToFile then
            let fileInfo = FileInfo.ofPath filePath
            let newFileInfo =  FileInfo.ofPath (fileInfo.Directory.FullName </> (sprintf "%s-%s.csv" stepName fileInfo.Name))
            FileInfo newFileInfo
        else 
            Console stepName
        
    let ensureOutputTargetExists = function
        | FileInfo fileInfo -> 
            Directory.ensure fileInfo.Directory.FullName
            File.create fileInfo.FullName
        | Console _ -> ()

    let run (arguments:ApplicationArguments) =
        use database = new Database(Database.InMemoryConnectionString)

        let outputWriter outputTarget resultRows =

            ensureOutputTargetExists outputTarget

            let writeLine outputTarget resultRow =
                match outputTarget with
                | FileInfo fileInfo -> appendToFile fileInfo.FullName resultRow
                | Console stepName -> Console.Out.WriteLine(sprintf "%s: %s" stepName resultRow)
        
            resultRows |> Seq.iter (writeLine outputTarget)

        let printDone = function
            | FileInfo fileInfo -> Console.Out.WriteLine("Done. Check for output file {0}", fileInfo.FullName)
            | Console stepName -> Console.Out.WriteLine("Done with {0}.", stepName)

        let git = Git.gitResult arguments.RepoPath.FullName
        let revs = 
            arguments.File
            |> Git.revs git
            |> List.filter (fun c -> c.Date |> Option.map (fun dt -> dt.Date >= arguments.StartDate.Date) |> Option.defaultValue false)
            |> List.map (ROP.tee database.saveCommitInfo)
            
        if arguments.Cmd = All || arguments.Cmd = Members then
            let getDiffsBetweenCommits = MemberAnalysis.getDiffsBetweenCommits git arguments.File
            let getFilesForCommits = MemberAnalysis.getFilesForCommits git arguments.File
            let outputTarget = getOutputTarget arguments.Output arguments.File (nameof MemberAnalysis)
        
            revs
            |> List.pairwise // NOTE, unless there is some caching, this will do ~double the work unnecessarily
            |> List.map (CommitPair.ofTuple 
                            >> getDiffsBetweenCommits
                            >> getFilesForCommits
                            >> MemberAnalysis.getMemberChangeData
                            >> List.map (ROP.tee database.toTable))
            |> List.collect id
            |> (MemberAnalysis.asCsv >> outputWriter outputTarget)

            printDone outputTarget
        
        if arguments.Cmd = All || arguments.Cmd = File then
            let getFileAtRev = Git.getFileAtRevMemoized git arguments.File
            let outputTarget = getOutputTarget arguments.Output arguments.File (nameof FileComplexityAnalysis)
        
            revs
            |> List.map (getFileAtRev
                            >> FileComplexityAnalysis.toFileComplexity
                            >> ROP.tee database.toTable)
            |> (FileComplexityAnalysis.asCsv >> outputWriter outputTarget)

            printDone outputTarget
