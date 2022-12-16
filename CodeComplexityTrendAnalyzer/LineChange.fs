namespace CodeComplexityTrendAnalyzer

open CodeComplexityTrendAnalyzer.DomainTypes

[<RequireQualifiedAccess>]
module LineChange =
    let toAbsolutePosition diff lineChange =
        let startLineForOp op =
            match op with
            | AddLine -> diff.AfterLine
            | RemoveLine -> diff.BeforeLine
            | LeaveLine -> diff.AfterLine
            |> Option.defaultValue 0

        let startLine = startLineForOp lineChange.Operation
        startLine + lineChange.LineNumber - 1

