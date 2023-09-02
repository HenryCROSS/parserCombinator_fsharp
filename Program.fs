open Parser.ParserModule

let strParser = str "hll"

let testParser = sequenceOf [
    strParser
    strParser
]

let result = run testParser "hlllhllll"

match result with
| Ok(result) -> printfn $"{result}"
| Error e -> printfn $"{e}"