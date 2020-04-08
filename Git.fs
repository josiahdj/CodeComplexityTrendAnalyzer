namespace CodeComplexityTrendAnalyzer

type RevisionInfo = { Hash: string; Date: string; Author: string }
type LineChangeOperation = | LineUnchanged | AddLine | RemoveLine
type LineChange = { Operation: LineChangeOperation; LineNumber: int; Text: string }
type DiffHunk = { BeforeLine: int option; BeforeLineCount: int option; AfterLine: int option; AfterLineCount: int option; MemberName: string option; LineChanges: LineChange list }
type DiffChange = { File: string; Revision: string; DiffHunk: DiffHunk }

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

    let lineInfoRegex = Regex("@@ -(?<before_line>[0-9]+)(,(?<before_line_count>[0-9]+))? \+(?<after_line>[0-9]+)(,(?<after_line_count>[0-9]+))?( @@) ?(?<member>.*?)$", RegexOptions.Compiled)
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
        let memberName = grps |> groupValue "member"
        let beforeLine = grps |> groupValue "before_line" |> Option.map int
        let beforeLineCount = grps |> groupValue "before_line_count" |> Option.map int
        let afterLine = grps |> groupValue "after_line" |> Option.map int
        let afterLineCount = grps |> groupValue "after_line_count" |> Option.map int
        
        { BeforeLine = beforeLine
          BeforeLineCount = beforeLineCount
          AfterLine = afterLine
          AfterLineCount = afterLineCount
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
        //printfn "Parsing Hunks from %s, Rev %s =========================================" file rev
        let toHunks lines =
            let rec toHunks' hunks lineNum lines' = 
                let parseLine line' =
                    match line' with
                    | LineInfo li -> 
                            //printfn "LineInfo: %A" li
                            toHunks' (li::hunks) 0
                    | LineChange lc -> 
                        match hunks with
                        | [] -> 
                            failwith "this shouldn't really happen!"
                        | [curr] ->
                            let l = lineNum + 1
                            //printfn "Hunk #%i, LineChange %i: %s" hunks.Length l lc.Text
                            let lineChanges = { lc with LineNumber = l }::curr.LineChanges // I know, List.rev is faster, but these should be small, and this is easier than looping through all of the parents and reversing al these lists
                            let newCurr = [{ curr with LineChanges = lineChanges }]
                            toHunks' newCurr l
                        | curr::tail ->
                            let l = lineNum + 1
                            //printfn "Hunk #%i, LineChange %i: %s" hunks.Length l lc.Text
                            let lineChanges = { lc with LineNumber = l }::curr.LineChanges
                            let newCurr = { curr with LineChanges = lineChanges }::tail
                            toHunks' newCurr l
                    | DiffHeader dh -> 
                            //printfn "DiffHeader %s" dh
                            toHunks' hunks 0
                    | UnchangedLine ul -> 
                            let l = lineNum + 1
                            //printfn "Hunk #%i, UnchangedLine %i: %s" hunks.Length l ul
                            toHunks' hunks l

                match lines' with
                | [] -> hunks // The End
                | [line] -> parseLine line [] // Last Line -> The End
                | line::rest -> parseLine line rest

            toHunks' List.empty<DiffHunk> 0 lines

        let toFileChanges l =
            { File = file; 
              Revision = rev; 
              DiffHunk = l }

        let theFile = String.replace "\\" "/" file
        let gitCmd = sprintf "diff %s --unified=0 -- %s" rev theFile
        // let gitCmd = sprintf "diff %s -- %s" rev theFile

        git gitCmd 
        |> toHunks
        |> List.map toFileChanges
        |> List.rev
