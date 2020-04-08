namespace CodeComplexityTrendAnalyzer

type MethodRevision = { Revision: RevisionInfo; Member: string }

module MethodAnalysis = 
    open System
    open Microsoft.CodeAnalysis.Text
    open Microsoft.CodeAnalysis.CSharp
    open System.Linq
    open Microsoft.CodeAnalysis.CSharp.Syntax
    open Microsoft.CodeAnalysis
    open Fake.Core
    
    let getMethodInfo git file =
        let parseRevPair (prev, curr) =
            Git.parseRev prev, Git.parseRev curr

        let memoize f =
            let cache = ref Map.empty
            fun x ->
                match (!cache).TryFind(x) with
                | Some res -> res
                | None ->
                    let res = f x
                    cache := (!cache).Add(x,res)
                    res

        let getFileChangesAtRevMemoized = memoize (Git.getFileChangesAtRev git file)
        let getFileChangesAtRev' ((revBefore, refAfter) : RevisionInfo * RevisionInfo) =
            let diffs = getFileChangesAtRevMemoized revBefore.Hash
            revBefore, refAfter, diffs

        let getFileAtRevMemoized = memoize (Git.getFileAtRev git file)
        let getFileAtRev' ((revBefore, revAfter, diffs) : RevisionInfo * RevisionInfo * DiffChange list) =
            let codeBefore = getFileAtRevMemoized revBefore.Hash |> String.toLines
            let codeAfter = getFileAtRevMemoized revAfter.Hash |> String.toLines
            revBefore, diffs, codeBefore, codeAfter

        let getMemberName (stBefore : SyntaxTree) (stAfter : SyntaxTree)  (diff : DiffChange) =
            let getMemberName' (lines : TextLineCollection) lineNumber =
                if lines.Count > lineNumber then
                    let span = lines.[lineNumber].Span;
                    let members = stBefore.GetRoot().DescendantNodes().OfType<MemberDeclarationSyntax>().Where(fun x -> x.Span.IntersectsWith(span))

                    let printMember (n : MemberDeclarationSyntax) =
                        match n with 
                        | :? PropertyDeclarationSyntax as p -> sprintf "Property: %O" p.Identifier
                        | :? MethodDeclarationSyntax as m -> sprintf "Method: %O" m.Identifier
                        | :? ConstructorDeclarationSyntax as c -> sprintf "Constructor: %O" c.Identifier
                        | _ -> String.Empty
        
                    let notEmpty s =
                        s <> String.Empty

                    if members.Any() then
                        members 
                        |> Seq.map printMember 
                        |> Seq.filter notEmpty
                        |> Seq.tryHead
                    else None
                else None

            let beforeLines = stBefore.GetText().Lines
            let afterLines = stAfter.GetText().Lines
            let getLines op = 
                match op with 
                | AddLine -> afterLines
                | RemoveLine -> beforeLines
                | UnchangedLine -> afterLines

            match diff.DiffHunk.LineChanges with
                | [] -> None
                | [lineChange] -> 
                        let lineNumber = LineChange.toAbsolutePosition diff.DiffHunk lineChange
                        let lines = getLines lineChange.Operation
                        getMemberName' lines lineNumber
                | lineChange::rest -> 
                        let lineNumber = LineChange.toAbsolutePosition diff.DiffHunk lineChange
                        let lines = getLines lineChange.Operation
                        getMemberName' lines lineNumber

        let getMembers ((rev, diffs, codeBefore, codeAfter) : RevisionInfo * DiffChange list * string * string) =
            //let expandLines (diff : DiffChange) : int list =
            //    [diff.StartLine..(diff.StartLine + diff.LineCount)]
            let parseCode (code : string) =
                let st = SourceText.From(code);
                SyntaxFactory.ParseSyntaxTree(st);

            let sf = parseCode codeBefore
            let sf' = parseCode codeAfter

            diffs 
            |> List.map (getMemberName sf sf')
            |> List.choose id
            |> List.distinct
            |> List.map (fun m -> { Revision = rev; Member = m })

        let asCsv (methodRev : MethodRevision) =
            sprintf "%s,%s,%s,%s" methodRev.Revision.Hash methodRev.Revision.Date methodRev.Revision.Author methodRev.Member            
             
        seq {
            yield sprintf "hash,date,author,member"
            yield! Git.revs git file
                    |> List.pairwise // TODO: pulling each diff and file twice this way? Cache them (limit size to 1?), pull from cache second time.
                    |> List.map (parseRevPair >> getFileChangesAtRev' >> getFileAtRev' >> getMembers)
                    |> List.collect id
                    |> List.map asCsv
        }
