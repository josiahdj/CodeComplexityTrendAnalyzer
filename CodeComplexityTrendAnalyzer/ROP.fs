namespace CodeComplexityTrendAnalyzer

module ROP =
    let tee f x =
        f x |> ignore
        x

