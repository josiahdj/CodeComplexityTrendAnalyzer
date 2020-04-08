namespace CodeComplexityTrendAnalyzer

type RevisionInfo = { Hash: string; Date: string; Author: string }
type LineChangeOperation = | LineUnchanged | AddLine | RemoveLine
type LineChange = { Operation: LineChangeOperation; LineNumber: int; Text: string }
type LineInfo = { StartLine: int option; StartLineCount: int option; EndLine: int option; EndLineCount: int option; MemberName: string; LineChanges: LineChange list }
type DiffChange = { File: string; Revision: string; LineInfo: LineInfo }

module Git = 
    open System
    open System.Text.RegularExpressions
    open Fake.Tools.Git
    open Fake.Core

    let gitResult repoPath = 
        CommandHelper.getGitResult repoPath

    let revs git filePath : string list = 
        let gitCmd = sprintf "log --date=short --pretty=format:%%h--%%ad--%%an %s" filePath
        git gitCmd 
        |> List.rev

    let parseRev commit =
        let parts = String.splitStr "--" commit
        { Hash = parts.[0]; Date = parts.[1]; Author = parts.[2] }

    let getFileAtRev git file rev : string list =
        let theFile = String.replace "\\" "/" file
        let gitCmd = sprintf "show %s:%s" rev theFile
        git gitCmd

    let lineInfoRegex = Regex("@@ -(?<start_line>[0-9]+)(,(?<start_line_count>[0-9]+))? \+(?<end_line>[0-9]+)(,(?<end_line_count>[0-9]+))?( @@) ?(?<member>.*?)$", RegexOptions.Compiled)
    let isLineInfo s =
        let matches = lineInfoRegex.Match(s)
        let isMatch = matches.Success && matches.Groups.Count > 0
        (isMatch, matches.Groups)

    let groupValue (name : string) (grps : GroupCollection) =
        if grps.[name].Success then
            Some grps.[name].Value
        else 
            None

    let toLineInfo grps =
        let memberName = grps |> groupValue "member" |> Option.defaultValue "???"
        let startLine = grps |> groupValue "start_line" |> Option.map int
        let startLineCount = grps |> groupValue "start_line_count" |> Option.map int
        let endLine = grps |> groupValue "end_line" |> Option.map int
        let endLineCount = grps |> groupValue "end_line_count" |> Option.map int
        
        { StartLine = startLine
          StartLineCount = startLineCount
          EndLine = endLine
          EndLineCount = endLineCount
          MemberName = memberName
          LineChanges = []}

    let lineChangeRegex = Regex("^(?<op>\+|\-)(?<line>[^+-].*)$", RegexOptions.Compiled)
    let isLineChange s =
        let matches = lineChangeRegex.Match(s)
        let isMatch = matches.Success && matches.Groups.Count > 0
        (isMatch, matches.Groups)

    let toLineChange grps =
        let op = grps |> groupValue "op" |> Option.map (fun s -> match s with | "+" -> AddLine | "-" -> RemoveLine | _ -> LineUnchanged) |> Option.get
        let line = grps |> groupValue "line" |> Option.defaultValue String.Empty
        { Operation = op; LineNumber = 0; Text = line }

    let diffHeaderRegex = Regex("^(diff|index|---|\+\+\+) ", RegexOptions.Compiled)
    let isDiffHeader s = diffHeaderRegex.IsMatch s
        
    let (|LineInfo|LineChange|DiffHeader|UnchangedLine|) s = 
        let isInfo, grps = isLineInfo s
        if isInfo then
            LineInfo (toLineInfo grps)
        else
            let isChange, grps' = isLineChange s
            if isChange then
                LineChange (toLineChange grps')
            else 
                if isDiffHeader s then
                    DiffHeader s
                else
                    UnchangedLine s

    let getFileChangesAtRev git file rev =

        let toHunks lines =
            let rec toHunks' hunks lineNum lines' = 
                let parseLine line rest =
                    match line with
                    | LineInfo li ->  toHunks' (li::hunks) 0 rest
                    | LineChange cl -> 
                        match hunks with
                        | [] -> failwith "this shouldn't really happen!"
                        | [curr] ->
                            let l = lineNum + 1
                            let lineChanges = { cl with LineNumber = l }::curr.LineChanges
                            let newCurr = [{ curr with LineChanges = lineChanges }]
                            toHunks' newCurr l rest
                        | curr::tail ->
                            let l = lineNum + 1
                            let lineChanges = { cl with LineNumber = l }::curr.LineChanges
                            let newCurr = { curr with LineChanges = lineChanges }::tail
                            toHunks' newCurr l rest
                    | DiffHeader dh -> toHunks' hunks 0 rest
                    | UnchangedLine ul -> toHunks' hunks (lineNum + 1) rest

                match lines' with
                | [] -> hunks // The End
                | [line] -> parseLine line [] // Last Line -> The End
                | line::rest -> parseLine line rest

            toHunks' List.empty<LineInfo> 0 lines

        let toFileChanges l =
            { File = file; 
              Revision = rev; 
              LineInfo = l }

        let theFile = String.replace "\\" "/" file
        // let gitCmd = sprintf "diff %s --unified=0 -- %s" rev theFile
        let gitCmd = sprintf "diff %s -- %s" rev theFile

        git gitCmd 
        |> toHunks
        |> List.map toFileChanges
