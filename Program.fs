open CodeComplexityTrendAnalyzer
open Argu
open Fake.IO
open System.IO
open System

type CliCommand =
    | Methods
    | Complexity
    | All
type CliArguments = 
    | [<MainCommand; ExactlyOnce; First>] Command of command : CliCommand
    | [<Mandatory; AltCommandLine("-r")>] Repository_Path of repo : string
    | [<Mandatory; AltCommandLine("-s")>] Source_File of file : string
    | [<AltCommandLine("-o")>] Output_Path of output : string
with 
    interface IArgParserTemplate with
        member x.Usage =
            match x with
            | Command _ -> "Choose 'methods' to analyze the methods; choose 'complexity' to return the complexity trend; choose 'all' to run both."
            | Repository_Path _ -> "Specify a repository path, e.g. C:\repo_root_dir"
            | Source_File _ -> "Specify a source file to be analyzed. The path is relalive to the repository directory, e.g. path/to/file.ext"
            | Output_Path _ -> "The output directory. The analysis files will be named after the source file. If not given, the current directory will be used."

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
        let output = argResults.TryGetResult Output_Path 
                        |> Option.defaultValue (Directory.GetCurrentDirectory()) 
                        |> DirectoryInfo.ofPath

        let git = Git.gitResult repo.FullName

        // let outFileName = 
        //     let fullFilePath = FileInfo.ofPath filePath
        //     fullFilePath.Name
        // let complexityOutFile = out </> sprintf "%s-FileComplexity.csv" outFileName
        let writer ss =
            ss |> Seq.iter (fun (s : string) -> Console.Out.WriteLine(s))

        if cmd = All || cmd = Methods then
            MethodAnalysis.getMethodInfo git file |> writer
        
        if cmd = All || cmd = Complexity then
            FileComplexity.getStats git file |> writer

        0 // return an integer exit code
