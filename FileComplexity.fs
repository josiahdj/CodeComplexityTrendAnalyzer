namespace CodeComplexityTrendAnalyzer

type Stats(hash : string, date: string, author: string, values : int list) as this =
    let count = List.length values
    member _.Count with get () = count
    member _.Total with get () = List.sum values
    member _.Min with get () = List.min values
    member _.Max with get () = List.max values
    member _.StdDev with get () = 
        let avg = this.Mean
        let variance (v : int) =
            (float v - avg) ** 2.0
        let avgVariance sum =
            sum / float this._SafeDenomenator
        values |> List.sumBy variance |> avgVariance |> sqrt
    
    member _.Mean with get () = float this.Total / float this._SafeDenomenator
    member _.Hash with get () = hash
    member _.Author with get () = author
    member _.Date with get () = date
    member private _._SafeDenomenator with get () = if count = 0 then 1 else count


module FileComplexity = 
    let calculateComplexity lines =
        let lineComplexity (line : string) =
            // count indentations as a proxy for complexity; see: http://softwareprocess.es/static/WhiteSpace.html
            (Strings.countLeadingSpaces line)/4 + (Strings.countLeadingTabs line)
        
        let lineComplexities = 
            lines
            |> List.filter Strings.containsCode 
            |> List.map lineComplexity

        lineComplexities

    let getStats git file =
        let calculateComplexity' revLines =
            let rev, date, author, lines = revLines
            rev, date, author, calculateComplexity lines

        let getFileAtRev' revDate =
            let { Hash = rev; Date = date; Author = author } = revDate
            rev, date, author, Git.getFileAtRev git file rev

        let asCsv (stat : Stats) =
            sprintf "%s,%s,%s,%i,%i,%.2f,%.2f" stat.Hash stat.Date stat.Author stat.Count stat.Total stat.Mean stat.StdDev

        let fileComplexityTrendAsCsv = 
            Git.parseRevHashes 
            >> getFileAtRev'
            >> calculateComplexity'
            >> Stats 
            >> asCsv 

        seq { 
            yield sprintf "hash,date,author,num_lines,total_complex,avg_complex,sd"
            yield! Git.revs git file |> List.map fileComplexityTrendAsCsv
        }
