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
                // count indentations as a proxy for complexity; see: 
                // http://softwareprocess.es/static/WhiteSpace.html
                // https://empear.com/blog/bumpy-road-code-complexity-in-context/
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

type FileComplexity = {
        Hash : string
        Date : string
        Author : string
        Complexity : ComplexityStats
    }
module FileComplexityAnalysis = 
    let getRawData git file revs =
        let getFileAtRev' commit =
            let { CommitInfo.Hash = hash; Date = date; Author = author } = commit
            hash, date, author, Git.getFileAtRev git file hash

        let asComplexityStat (rev, date, author, code) = 
            let complexity = ComplexityStats.create code
            { Hash = rev
              Date = date
              Author = author
              Complexity = complexity }

        revs
        |> List.map (getFileAtRev' >> asComplexityStat)

    let asCsv fcs =
        let asCsv' fc =
            let complexity = fc.Complexity
            sprintf "%s,%s,%s,%i,%i,%.2f,%.2f" 
                fc.Hash 
                fc.Date 
                fc.Author 
                complexity.Count 
                complexity.Total 
                complexity.Mean 
                complexity.StdDev

        seq { 
            yield sprintf "hash,date,author,num_lines,total_complex,avg_complex,sd"
            yield! fcs |> Seq.map asCsv'
        }
        
