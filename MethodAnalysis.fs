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
        let getFileChangesAtRev' (rev : RevisionInfo) =
            let diffs = Git.getFileChangesAtRev git file rev.Hash
            rev, diffs

        let getFileAtRev' ((rev, diffs) : RevisionInfo * DiffChange list) =
            let code = Git.getFileAtRev git file rev.Hash |> String.toLines
            rev, diffs, code

        let getMemberName (st : SyntaxTree) lineNum  =
            let lines = st.GetText().Lines
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

        let getMemberName' (code : SyntaxTree) (diff : DiffChange) =
            getMemberName code diff.StartLine

        let getMembers ((rev, diffs, code) : RevisionInfo * DiffChange list * string) =
            //let expandLines (diff : DiffChange) : int list =
            //    [diff.StartLine..(diff.StartLine + diff.LineCount)]
            let st = SourceText.From(code);
            let sf = SyntaxFactory.ParseSyntaxTree(st);

            diffs 
            |> List.map (getMemberName' sf)
            |> List.choose id
            |> List.distinct
            |> List.map (fun m -> { Revision = rev; Member = m })

        let asCsv (methodRev : MethodRevision) =
            sprintf "%s,%s,%s,%s" methodRev.Revision.Hash methodRev.Revision.Date methodRev.Revision.Author methodRev.Member            
             
        seq {
            yield sprintf "hash,date,author,member"
            yield! Git.revs git file
                    |> PSeq.map (Git.parseRevHashes >> getFileChangesAtRev' >> getFileAtRev' >> getMembers)
                    |> PSeq.collect id
                    |> PSeq.map asCsv
        }
