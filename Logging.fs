namespace CodeComplexityTrendAnalyzer

open Serilog
open System
open Fake.IO

[<AutoOpen>]
module Logging =
    let logger = LoggerConfiguration()
                    .MinimumLevel.Debug()
                    .WriteTo.LiterateConsole()
                    .WriteTo.RollingFile("CodeComplexityTrendAnalyzer-{Date}.log", retainedFileCountLimit = Nullable 3)
                    .CreateLogger();
    let tee f x =
        f x
        x

    let dumpToFile fileName text = 
        let file = FileInfo.ofPath fileName
        Directory.ensure file.Directory.FullName
        File.create file.FullName
        File.write false file.FullName text
