module CommandLineParameters

open Argu
open System
open System.IO

type CliCommand =
    | Members
    | File
    | All

type CliArguments =
    | [<MainCommand; ExactlyOnce; First>] Command of command: CliCommand
    | [<Mandatory; AltCommandLine("-r")>] RepositoryPath of repo: string
    | [<Mandatory; AltCommandLine("-s")>] SourceFile of file: string
    | [<AltCommandLine("-o")>] OutputFile
    | [<AltCommandLine("-d")>] StartDate of string
    interface IArgParserTemplate with
        member x.Usage =
            match x with
            | Command _ ->
                "Choose 'members' to return the member-level (constructors, methods, properties) file analysis; choose 'file' to return the file-level complexity growth analysis; choose 'all' to run both."
            | RepositoryPath _ -> "Specify a repository path, e.g. C:\\code"
            | SourceFile _ ->
                "Specify a source file to be analyzed. The path is relalive to the repository directory, e.g. path/to/file.ext"
            | OutputFile _ ->
                "Output results to a file. The analysis files will be named after the source file. The binary's directory will be used."
            | StartDate _ -> "Date after which to analyze"

type ApplicationArguments =
    {
        Cmd: CliCommand
        RepoPath:DirectoryInfo
        File: string
        Output: bool
        StartDate: DateTime
    }