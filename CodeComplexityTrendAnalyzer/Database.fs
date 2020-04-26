namespace CodeComplexityTrendAnalyzer

open FSharp.Data.Sql

module Database = 
    let [<Literal>] dbVendor = Common.DatabaseProviderTypes.MSSQLSERVER
    let [<Literal>] connString = "Data Source=.;Integrated Security=True;Persist Security Info=False;Pooling=False;MultipleActiveResultSets=False;Connect Timeout=60;Encrypt=False;TrustServerCertificate=False"
    let [<Literal>] indivAmount = 1000
    let [<Literal>] useOptTypes  = true
    
    type sql =
        SqlDataProvider<
            dbVendor,
            connString,
            IndividualsAmount = indivAmount,
            UseOptionTypes = useOptTypes>
    
    let ctx = sql.GetDataContext()

    let toTable data =
        ()

    let saveCommitInfo (ci : CommitInfo) =
        //let commitInfo = ctx.Dbo.CommitInfo
        ()