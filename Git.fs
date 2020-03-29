namespace CodeComplexityTrendAnalyzer

type RevisionDate = { Hash: string; Date: string }

module Git = 

    open Fake.Tools.Git

    let gitResult repoPath = 
        CommandHelper.getGitResult repoPath

    let revs git filePath = 
        let gitCmd = sprintf "log --date=short --pretty=format:%%h--%%ad %s" filePath
        git gitCmd 
        |> List.rev

    let parseRevHashes (commit : string) =
        let revAndDate = commit.Split("--")
        { Hash = revAndDate.[0]; Date = revAndDate.[1] }

    let getFileAtRev git (file : string) rev =
        let theFile = file.Replace("\\", "/")
        let gitCmd = sprintf "show %s:%s" rev theFile
        let fileLines = git gitCmd
        fileLines
