namespace CodeComplexityTrendAnalyzer

open CodeComplexityTrendAnalyzer.DomainTypes
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp.Syntax
open System.Linq
open System

type InitialMemberInfo =
    { Name: string
      Type: MemberType
      Parameters: string option }
    member private __.ParametersToCompare(pl: ParameterListSyntax) =
        pl.Parameters.ToList()
        |> Seq.map (fun p -> p.ToString())
        |> Seq.sort
        |> String.concat ","

    member this.Params
        with private get () =
            this.Parameters
            |> Option.defaultValue String.Empty

    member this.FindPropertyDeclaration(ast: SyntaxTree): Result<PropertyDeclarationSyntax, FindDeclarationError> =
        let ms =
            ast
                .GetRoot()
                .DescendantNodes()
                .OfType<PropertyDeclarationSyntax>()
                .Where(fun x -> x.Identifier.Text = this.Name)

        ms
        |> Seq.tryExactlyOne
        |> Result.ofOption PropertyNotFoundByName

    member this.FindMethodDeclaration(ast: SyntaxTree): Result<MethodDeclarationSyntax, FindDeclarationError> =
        let ms =
            ast
                .GetRoot()
                .DescendantNodes()
                .OfType<MethodDeclarationSyntax>()
                .Where(fun x -> x.Identifier.Text = this.Name)

        if ms.Any() then
            ms
            |> Seq.tryFind (fun x -> (this.ParametersToCompare x.ParameterList) = this.Params)
            |> Option.orElseWith (fun () -> ms |> Seq.tryExactlyOne) // assume that the signature was changed and that there's only one instance (if there are overrides, all bets are off)
            |> Result.ofOption (MethodNotFoundWithParams ms)
        else
            Error(MethodNotFoundByName ms)

    member this.FindConstructorDeclaration
        (ast: SyntaxTree)
        : Result<ConstructorDeclarationSyntax, FindDeclarationError> =
        let ms =
            ast
                .GetRoot()
                .DescendantNodes()
                .OfType<ConstructorDeclarationSyntax>()
                .Where(fun x -> x.Identifier.Text = this.Name)

        if ms.Any() then
            ms
            |> Seq.tryFind (fun x -> (this.ParametersToCompare x.ParameterList) = this.Params)
            |> Option.orElseWith (fun () -> ms |> Seq.tryExactlyOne) // assume that the signature was changed and that there's only one instance (if there are overrides, all bets are off)
            |> Result.ofOption (ConstructorNotFoundWithParams ms)
        else
            Error(ConstructorNotFoundByName ms)

    member this.ToLinesOfCode(ast: SyntaxTree): Result<string list, ComplexityError> =
        try
            match this.Type with
            | Property ->
                result {
                    let! p =
                        this.FindPropertyDeclaration(ast)
                        |> Result.mapError (fun err -> MemberNotFound(this.Name, this.Type, String.Empty, List.empty))

                    return
                        p
                        |> ((fun s -> s.ToString()) >> Strings.splitLines)
                }
            | Method ->
                result {
                    let! m =
                        this.FindMethodDeclaration(ast)
                        |> Result.mapError
                            (fun err ->
                                let ms =
                                    FindDeclarationError.toMethodDeclarations err

                                let meths =
                                    ms
                                    |> Seq.map (fun m -> m.ParameterList.Parameters.ToString())
                                    |> List.ofSeq

                                MemberNotFound(this.Name, this.Type, this.Params, meths))

                    return
                        m
                        |> ((fun s -> s.ToString()) >> Strings.splitLines)
                //    logger.Debug("Couldn't find Method {Identifier} {Parameters}. Hoping for single method (no overloads)... Options:\r\n{Constructors}", mem.Name, params', meths)
                }
            | Constructor ->
                result {
                    let! c =
                        this.FindConstructorDeclaration(ast)
                        |> Result.mapError
                            (fun err ->
                                let ms =
                                    FindDeclarationError.toConstructorDeclarations err

                                let meths =
                                    ms
                                    |> Seq.map (fun m -> m.ParameterList.Parameters.ToString())
                                    |> List.ofSeq

                                MemberNotFound(this.Name, this.Type, this.Params, meths))

                    return
                        c
                        |> ((fun s -> s.ToString()) >> Strings.splitLines)
                //    logger.Debug("Couldn't find Constructor {Identifier} ({Parameters}). Hoping for for single constructor (no overloads)... Options:\r\n{Constructors}", mem.Name, params', ctors)
                }
        with ex ->
            // logger.Error(ex, "Couldn't find the {MemberKind} {Identifier} {Parameters} in the AST", mem.Type, mem.Name, params')
            Error(Unexpected(ex.ToString()))