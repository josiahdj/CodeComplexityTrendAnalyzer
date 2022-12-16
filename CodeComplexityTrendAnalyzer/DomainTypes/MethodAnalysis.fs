namespace CodeComplexityTrendAnalyzer.DomainTypes

open Microsoft.CodeAnalysis.CSharp.Syntax

// MethodAnalysis
type FindDeclarationError =
    | PropertyNotFoundByName
    | MethodNotFoundByName of MethodDeclarationSyntax seq
    | MethodNotFoundWithParams of MethodDeclarationSyntax seq
    | ConstructorNotFoundByName of ConstructorDeclarationSyntax seq
    | ConstructorNotFoundWithParams of ConstructorDeclarationSyntax seq

module FindDeclarationError =
    let toMethodDeclarations err =
        match err with
        | PropertyNotFoundByName -> Seq.empty
        | MethodNotFoundByName ms
        | MethodNotFoundWithParams ms -> ms
        | ConstructorNotFoundByName _
        | ConstructorNotFoundWithParams _ -> Seq.empty

    let toConstructorDeclarations err =
        match err with
        | PropertyNotFoundByName -> Seq.empty
        | MethodNotFoundByName _
        | MethodNotFoundWithParams _ -> Seq.empty
        | ConstructorNotFoundByName cs
        | ConstructorNotFoundWithParams cs -> cs

type MemberType =
    | Constructor
    | Method
    | Property

[<StructuralComparison>]
[<StructuralEquality>]
type ComplexityError =
    | Unexpected of string
    | MemberNotFound of memberName: string * memberType: MemberType * wantedParams: string * hadParams: string list

[<StructuralComparison>]
[<StructuralEquality>]
type CompleteMemberInfo =
    { Name: string
      Type: MemberType
      Parameters: string option
      LineCount: int option
      Complexity: Result<ComplexityStats, ComplexityError> }
