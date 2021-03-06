open Argu
open System
open Fake.IO
open CodeComplexityTrendAnalyzer
open CommandLineParameters
open Program

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CliArguments>(programName = "CodeComplexityTrendAnalyzer.exe")
    if argv.Length = 0 then 
        printfn "%s" <| parser.PrintUsage ()
        -1
    else 
        let argResults = parser.Parse(argv)
   
        run { 
              Cmd = argResults.GetResult Command
              RepoPath = argResults.PostProcessResult (<@ RepositoryPath @>, DirectoryInfo.ofPath)
              File = argResults.GetResult SourceFile
              Output = argResults.Contains OutputFile
              StartDate = argResults.TryPostProcessResult (<@ StartDate @>, DateTime.tryParseOrDefault DateTime.MinValue) |> Option.defaultValue DateTime.MinValue 
            }
        0