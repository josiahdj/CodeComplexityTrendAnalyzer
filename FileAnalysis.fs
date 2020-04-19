namespace CodeComplexityTrendAnalyzer

type ComplexityStats = private {
    Count : int
    Total : int
    Min : int
    Max : int
    StdDev : float
    Mean : float
}

module ComplexityStats =
    let create (lines : string list) =
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
        let total = List.sum values
        let max = List.max values
        let min = List.min values
        let safeDen = if count = 0 then 1 else count
        let mean = float total / float safeDen
        let stdDev = 
           let variance (v : int) =
               (float v - mean) ** 2.0
           let avgVariance sum =
               sum / float safeDen
           values |> List.sumBy variance |> avgVariance |> sqrt

        { Count = count; Total = total; Min = min; Max = max; Mean = mean; StdDev = stdDev }

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
            >> (fun (rev, date, author, cs) -> rev, date, author, ComplexityStats.create cs) 
            >> asCsv 

        seq { 
            yield sprintf "hash,date,author,num_lines,total_complex,avg_complex,sd"
            yield! Git.revs git file |> PSeq.map fileComplexityTrendAsCsv
        }
