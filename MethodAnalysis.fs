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
    open FSharp.Collections.ParallelSeq
    
    let getMethodInfo git file =
        let parseRevPair (prev, curr) =
            Git.parseRev prev, Git.parseRev curr

        let getFileChangesAtRev' ((rev, rev') : RevisionInfo * RevisionInfo) =
            let diffs = Git.getFileChangesAtRev git file rev.Hash
            rev, rev', diffs

        let getFileAtRev' ((rev, rev', diffs) : RevisionInfo * RevisionInfo * DiffChange list) =
            let code = Git.getFileAtRev git file rev.Hash |> String.toLines
            let code' = Git.getFileAtRev git file rev'.Hash |> String.toLines
            rev, diffs, code, code'

        let getMemberName (st : SyntaxTree) (st' : SyntaxTree)  (diff : DiffChange) =
            let lines = st.GetText().Lines
            let lineNum = diff.LineInfo.StartLine |> Option.defaultValue 0
            if lines.Count > lineNum then
                let span = lines.[lineNum].Span;
                let members = st.GetRoot().DescendantNodes().OfType<MemberDeclarationSyntax>().Where(fun x -> x.Span.IntersectsWith(span))

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

        let getMembers ((rev, diffs, code, code') : RevisionInfo * DiffChange list * string * string) =
            //let expandLines (diff : DiffChange) : int list =
            //    [diff.StartLine..(diff.StartLine + diff.LineCount)]
            let parseCode (code : string) =
                let st = SourceText.From(code);
                SyntaxFactory.ParseSyntaxTree(st);

            let sf = parseCode code
            let sf' = parseCode code'

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
                    |> List.pairwise
                    |> List.map (parseRevPair >> getFileChangesAtRev' >> getFileAtRev' >> getMembers)
                    |> List.collect id
                    |> List.map asCsv
                    // |> PSeq.map (parseRevPair >> getFileChangesAtRev' >> getFileAtRev' >> getMembers)
                    // |> PSeq.collect id
                    // |> PSeq.map asCsv
        }
