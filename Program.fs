open ParserModule
open System

let strParser = str "hll"

let testParser = choice [
    // strParser
    digits
    letters
    // strParser
    // letter
    // digit
    // digit
]

let result = run testParser "hllhllldsad113l"

match result with
| Ok(result) -> printfn $"{result}"
| Error e -> printfn $"{e}"
