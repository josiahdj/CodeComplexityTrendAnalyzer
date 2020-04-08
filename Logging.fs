namespace CodeComplexityTrendAnalyzer

open Serilog
open Serilog.Formatting.Json
open System

module Logging =
    let logger = LoggerConfiguration()
                    .MinimumLevel.Debug()
                    .WriteTo.LiterateConsole()
                    .WriteTo.RollingFile(JsonFormatter(), "CodeComplexityTrendAnalyzer-{Date}.log", retainedFileCountLimit = Nullable 3)
                    .CreateLogger();

