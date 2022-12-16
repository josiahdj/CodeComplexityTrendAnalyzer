namespace CodeComplexityTrendAnalyzer.DomainTypes

open System

// Git
[<CLIMutable>]
type CommitInfo =
    { Hash: string
      Date: DateTime option
      Author: string }

type LineChangeOperation =
    | LeaveLine
    | AddLine
    | RemoveLine

type LineChange =
    { Operation: LineChangeOperation
      LineNumber: int
      Text: string }

type DiffHunk =
    { BeforeLine: int option
      BeforeLineCount: int option
      AfterLine: int option
      AfterLineCount: int option
      MemberName: string option
      LineChanges: LineChange list
      LinesAdded: int
      LinesRemoved: int }

type FileRevision =
    { File: string
      Hash: string
      DiffHunk: DiffHunk }

type CommitPair =
    { Previous: CommitInfo
      Current: CommitInfo }

module CommitPair =
    let ofTuple (prev, curr) = { Previous = prev; Current = curr }

type CommitDiffs = CommitPair * FileRevision list

type CodeToDiff =
    { Commit: CommitInfo
      FileRevisions: FileRevision list
      PreviousCode: string
      CurrentCode: string }

