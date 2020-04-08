namespace CodeComplexityTrendAnalyzer

open Serilog
open System

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

