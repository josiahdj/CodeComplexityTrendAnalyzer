namespace CodeComplexityTrendAnalyzer

type ComplexityStats(lines : string list) as this =
    let values =
        let lineComplexity (line : string) =
            // count indentations as a proxy for complexity; see: http://softwareprocess.es/static/WhiteSpace.html
            (Strings.countLeadingSpaces line)/4 + (Strings.countLeadingTabs line)
        
        let lineComplexities = 
            lines
            |> List.filter Strings.containsCode 
            |> List.map lineComplexity

        lineComplexities

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
    member private _._SafeDenomenator with get () = if count = 0 then 1 else count


module FileAnalysis = 
    open FSharp.Collections.ParallelSeq
    

    let analyze git file =
        let getFileAtRev' revInfo =
            let { Hash = rev; Date = date; Author = author } = revInfo
            rev, date, author, Git.getFileAtRev git file rev

        let asCsv (rev, date, author, stat : ComplexityStats) =
            sprintf "%s,%s,%s,%i,%i,%.2f,%.2f" rev date author stat.Count stat.Total stat.Mean stat.StdDev

        let fileComplexityTrendAsCsv = 
            Git.parseRev 
            >> getFileAtRev'
            >> (fun (rev, date, author, cs) -> rev, date, author, ComplexityStats(cs)) 
            >> asCsv 

        seq { 
            yield sprintf "hash,date,author,num_lines,total_complex,avg_complex,sd"
            yield! Git.revs git file |> PSeq.map fileComplexityTrendAsCsv
        }
