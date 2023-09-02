open ParserModule
open System

let strParser = str "hll"

let testParser = choice [
    digits
    letters
]

let result = run testParser "hllhllldsad113lee123ea1aaaaa"

match result with
| Ok(result) -> printfn $"{result}"
| Error e -> printfn $"{e}"
