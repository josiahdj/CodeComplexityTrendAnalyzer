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

[<RequireQualifiedAccess>]
module DateTime = 
    open System 

    let tryParse (s: string) =
        match DateTime.TryParse(s) with 
        | true, date -> Some date
        | false, _ -> None

    let tryParseOrDefault ``default`` (s: string) =
        match tryParse s with 
        | Some date -> date
        | None -> ``default``


    let inline stringf format (x : ^a) = 
        (^a : (member ToString : string -> string) (x, format))

    let optionToString (dt: DateTime option) =
        dt 
        |> Option.map (stringf "yyyy/MM/dd") 
        |> Option.defaultValue "0000/00/00"