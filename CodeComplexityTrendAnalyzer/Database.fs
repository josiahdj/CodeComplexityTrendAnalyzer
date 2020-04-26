namespace CodeComplexityTrendAnalyzer

open FSharp.Data.Sql
open System.Data.SqlClient
open System

module Database = 
    let [<Literal>] dbVendor = Common.DatabaseProviderTypes.MSSQLSERVER
    let [<Literal>] connString = "Server=localhost;Database=CodeAnalysis;Integrated Security=True;"
    let [<Literal>] indivAmount = 1000
    let [<Literal>] useOptTypes  = true
    let [<Literal>] contextSchemaPath = __SOURCE_DIRECTORY__ + @".\Schema.json" // ,ContextSchemaPath = contextSchemaPath
    
    type sql =
        SqlDataProvider<
            dbVendor,
            connString,
            IndividualsAmount = indivAmount,
            UseOptionTypes = useOptTypes>
    
    let ctx = sql.GetDataContext()
    // ctx.SaveContextSchema() |> ignore
    let commitInfo = ctx.Dbo.CommitInfo


    let toTable data =
        ()

    let saveCommitInfo (ci : CommitInfo) =
        let (success, date) = DateTime.TryParse(ci.Date)
        if success then
            try 
                let newCi = commitInfo.``Create(Author, Date, Hash)``(ci.Author, date, ci.Hash)
                ctx.SubmitUpdates()
                logger.Debug("Created commit record with hash {Hash} and Id {Id}", newCi.Hash, newCi.Id)
            with 
            | :? SqlException as ex when ex.Message.Contains("unique") -> 
                logger.Debug("The commit with hash {Hash} already exists.", ci.Hash)
        ()