namespace CodeComplexityTrendAnalyzer

type RevisionInfo = { Hash: string; Date: string; Author: string }
type DiffChange = { File: string; Revision: string; StartLine: int; LineCount: int; MemberName: string }

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

    let parseRevHashes commit =
        let parts = String.splitStr "--" commit
        { Hash = parts.[0]; Date = parts.[1]; Author = parts.[2] }

    let getFileAtRev git file rev : string list =
        let theFile = String.replace "\\" "/" file
        let gitCmd = sprintf "show %s:%s" rev theFile
        git gitCmd

    let hunkRegex = Regex("@@ -(?<start_line>[0-9]+)(,(?<line_count>[0-9]+))? \+([0-9]+)(,([0-9]+))?( @@) ?(?<member>.*?)$", RegexOptions.Compiled)
    let getFileChangesAtRev git file rev =
        let theFile = String.replace "\\" "/" file
        let gitCmd = sprintf "diff %s --unified=0 -- %s" rev theFile
        let hunkHeaderStartLine s =
            let matches = hunkRegex.Match(s)
            if matches.Success && matches.Captures.Count > 0 then
                let startLine = matches.Groups.["start_line"].Value |> Int32.Parse
                let lineCount =
                    if matches.Groups.["line_count"].Success then
                        matches.Groups.["line_count"].Value |> Int32.Parse
                    else 
                        0
                let memberName = matches.Groups.["member"].Value
                Some { File = file; Revision = rev; StartLine = startLine; LineCount = lineCount; MemberName = memberName }
            else
                None
        git gitCmd 
        |> List.map hunkHeaderStartLine
        |> List.choose id
