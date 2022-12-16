[<RequireQualifiedAccess>]
module CodeComplexityTrendAnalyzer.ComplexityStats

open CodeComplexityTrendAnalyzer.DomainTypes

let private zero =
    { Count = 0
      Total = 0
      Min = 0
      Max = 0
      Mean = 0.0
      StdDev = 0.0 }

let create (lines: string list) =
    match lines with
    | [] -> zero
    | _ ->
        let lineComplexity (line: string) =
            // count indentations as a proxy for complexity; see:
            // http://softwareprocess.es/static/WhiteSpace.html
            // https://empear.com/blog/bumpy-road-code-complexity-in-context/
            (Strings.countLeadingSpaces line) / 4
            + (Strings.countLeadingTabs line)

        let values =
            lines
            |> List.filter Strings.containsCode
            |> List.map lineComplexity

        let count = List.length values
        let total = List.sum values
        let max = List.max values
        let min = List.min values
        let safeDen = if count = 0 then 1 else count
        let mean = float total / float safeDen

        let stdDev =
            let variance (v: int) = (float v - mean) ** 2.0
            let avgVariance sum = sum / float safeDen

            values
            |> List.sumBy variance
            |> avgVariance
            |> sqrt

        { Count = count
          Total = total
          Min = min
          Max = max
          Mean = mean
          StdDev = stdDev }