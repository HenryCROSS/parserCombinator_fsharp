open Parser.ParserModule

let strParser = str "hll"

let testParser = sequenceOf [
    strParser
    strParser
    letters
    // digit
]

let result = run testParser "hllhllldsad113l"

match result with
| Ok(result) -> printfn $"{result}"
| Error e -> printfn $"{e}"