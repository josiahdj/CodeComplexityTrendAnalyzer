namespace CodeComplexityTrendAnalyzer

[<RequireQualifiedAccess>]
module Strings =
    open System
    open System.Text.RegularExpressions
    open System.Linq
    open Fake.Core

    let splitLines s = String.splitStr "\r\n" s

    let countLeadingWhitespace (s : string) = s.TakeWhile(Char.IsWhiteSpace).Count()

    let emptyLineRegex = Regex("^\s*$", RegexOptions.Compiled)
    let containsCode line =
        not <| emptyLineRegex.IsMatch line

    let countLeadingSpaces line =
        String.replace "\t" "" line
        |> countLeadingWhitespace

    let countLeadingTabs line =
        String.replace " " "" line
        |> countLeadingWhitespace


