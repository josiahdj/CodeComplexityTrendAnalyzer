namespace CodeComplexityTrendAnalyzer

module Caching =
    let memoize f =
        let mutable cache = Map.empty

        fun x ->
            match cache.TryFind(x) with
            | Some res -> res
            | None ->
                let res = f x
                cache <- cache.Add(x, res)
                res
