module Regex

open System.Text.RegularExpressions

let (|FromRegex|_|) (regex:Regex) input = 
    let m = regex.Match(input)
    if m.Success then Some(m.Groups)
    else None
