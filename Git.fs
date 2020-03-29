namespace CodeComplexityTrendAnalyzer

type RevisionDate = { Hash: string; Date: string; Author: string }

module Git = 

    open Fake.Tools.Git

    let gitResult repoPath = 
        CommandHelper.getGitResult repoPath

    let revs git filePath = 
        let gitCmd = sprintf "log --date=short --pretty=format:%%h--%%ad--%%an %s" filePath
        git gitCmd 
        |> List.rev

    let parseRevHashes (commit : string) =
        let parts = commit.Split("--")
        { Hash = parts.[0]; Date = parts.[1]; Author = parts.[2] }

    let getFileAtRev git (file : string) rev =
        let theFile = file.Replace("\\", "/")
        let gitCmd = sprintf "show %s:%s" rev theFile
        let fileLines = git gitCmd
        fileLines
