namespace CodeComplexityTrendAnalyzer

type RevisionInfo = { Hash: string; Date: string; Author: string }
type LineChangeOperation = | LeaveLine | AddLine | RemoveLine
type LineChange = { Operation: LineChangeOperation; LineNumber: int; Text: string }
type DiffHunk = { 
    BeforeLine: int option
    BeforeLineCount: int option
    AfterLine: int option
    AfterLineCount: int option
    MemberName: string option
    LineChanges: LineChange list
    LinesAdded: int
    LinesRemoved: int }
type DiffChange = { File: string; Revision: string; DiffHunk: DiffHunk }

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

[<RequireQualifiedAccess>]
module Git = 
    open System
    open System.Text.RegularExpressions
    open Fake.Tools.Git
    open Fake.Core
    open Logging

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
          LineChanges = []
          LinesAdded = 0
          LinesRemoved = 0}

    let lineChangeRegex = Regex("^(?<op>\+|\-)(?<line>[^+-].*?)$", RegexOptions.Compiled) // unfortunately (or fortunately) eliminates add/remove of empty lines
    let isLineChange s =
        let matches = lineChangeRegex.Match(s)
        let isMatch = matches.Success && matches.Groups.Count > 0
        (isMatch, matches.Groups)

    let toLineChange grps =
        let op = grps |> groupValue "op" |> Option.map (fun s -> match s with | "+" -> AddLine | "-" -> RemoveLine | _ -> LeaveLine) |> Option.get
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

    let getFileChangesAtRev git file revBefore revAfter =
        logger.Information("Parsing Hunks from {File}, Rev {RevBefore} to Rev {RevAfter} =========================================", file, revBefore, revAfter)
        let toHunks lines =
            //dumpToFile (sprintf "%s-%s--%s.diff" file revBefore revAfter) lines
            let rec toHunks' hunks lineNum lines' = 
                let updateDiffHunk dh chg dhs ln =
                    let added, removed = 
                        match chg.Operation with
                        | AddLine -> dh.LinesAdded + 1, dh.LinesRemoved
                        | RemoveLine -> dh.LinesAdded, dh.LinesRemoved + 1
                        | LeaveLine -> dh.LinesAdded, dh.LinesRemoved

                    let lineChanges = { chg with LineNumber = ln }::dh.LineChanges // NOTE: line changes will be in reverse order!
                    { dh with LineChanges = lineChanges; LinesAdded = added; LinesRemoved = removed }::dhs

                let parseLine line' =
                    match line' with
                    | LineInfo li -> 
                            logger.Debug("LineInfo: @{LineInfo}", li)
                            toHunks' (li::hunks) 0
                    | LineChange lc -> 
                        match hunks with
                        | [] -> 
                            failwith "this shouldn't really happen!"
                        | [curr] ->
                            let l = lineNum + 1
                            logger.Debug("Hunk #{HunkNumber}, LineChange {LineNumber}: {Line}", hunks.Length, l, lc.Text)
                            let newCurr = updateDiffHunk curr lc [] l
                            toHunks' newCurr l
                        | curr::tail ->
                            let l = lineNum + 1
                            logger.Debug("Hunk #{HunkNumber}, LineChange {LineNumber}: {Line}", hunks.Length, l, lc.Text)
                            let newCurr = updateDiffHunk curr lc tail l
                            toHunks' newCurr l
                    | DiffHeader dh -> 
                            logger.Debug("DiffHeader {DiffHeader}", dh)
                            toHunks' hunks 0
                    | UnchangedLine ul -> 
                            let l = lineNum + 1
                            logger.Debug("Hunk #{HunkNumber}, UnchangedLine {LineNumber}: {Line}", hunks.Length, l, ul)
                            toHunks' hunks l

                match lines' with
                | [] -> hunks // The End
                | [line] -> parseLine line [] // Last Line -> The End
                | line::rest -> parseLine line rest

            toHunks' List.empty<DiffHunk> 0 lines

        let toFileChanges l =
            { File = file; 
              Revision = revBefore; 
              DiffHunk = l }

        let theFile = String.replace "\\" "/" file
        let gitCmd = sprintf "diff %s..%s --unified=0 -- %s" revBefore revAfter theFile

        git gitCmd 
        |> toHunks
        |> List.map toFileChanges
        |> List.rev
