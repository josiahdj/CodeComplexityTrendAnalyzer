namespace CodeComplexityTrendAnalyzer

type Stats(name : string, date: string, allValues : int list) as this =
    let count = List.length allValues
    member _.Count with get () = count
    member _.Total with get () = List.sum allValues
    member _.Min with get () = List.min allValues
    member _.Max with get () = List.max allValues
    member _.StdDev with get () = 
        let avg = this.Mean
        let variance (v : int) =
            (float v - avg) ** 2.0
        let avgVariance sum =
            sum / float this._SafeDenomenator
        allValues |> List.sumBy variance |> avgVariance |> sqrt
    
    member _.Mean with get () = float this.Total / float this._SafeDenomenator
    member _.Name with get () = name
    member _.Date with get () = date
    member private _._SafeDenomenator with get () = if count = 0 then 1 else count


module FileComplexity = 
    open Fake
    open Fake.IO
    open Fake.IO.FileSystemOperators

    let calculateComplexity lines =
        let lineComplexity (line : string) =
            // count indentations as a proxy for complexity; see: http://softwareprocess.es/static/WhiteSpace.html
            (Strings.countLeadingSpaces line)/4 + (Strings.countLeadingTabs line)
        
        let lineComplexities = 
            lines
            |> List.filter Strings.containsCode 
            |> List.map lineComplexity

        lineComplexities

    let printStats repo file out =
        let repoPath = DirectoryInfo.ofPath repo
        let filePath = repoPath.FullName </> file
        let git = Git.gitResult repoPath.FullName

        let calculateComplexity' revLines =
            let rev, date, lines = revLines
            rev, date, calculateComplexity lines

        let getFileAtRev' revDate =
            let { Hash = rev; Date = date } = revDate
            rev, date, Git.getFileAtRev git file rev

        let asCsv (stat : Stats) =
            sprintf "%s,%s,%i,%i,%.2f,%.2f" stat.Name stat.Date stat.Count stat.Total stat.Mean stat.StdDev

        let fileComplexityTrendAsCsv = 
            Git.parseRevHashes 
            >> getFileAtRev'
            >> calculateComplexity'
            >> Stats 
            >> asCsv 

        let filename = Fake.Core.String.splitStr "\/" file |> List.last
        let outFile = out </> sprintf "%s-FileComplexity.csv" filename
        let header = sprintf "hash,date,num_lines,total_complex,avg_complex,sd"
        File.writeString false outFile header
        Git.revs git filePath
        |> List.map fileComplexityTrendAsCsv
        |> List.iter (File.writeString true outFile)
