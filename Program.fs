open Parser.ParserModule

let strParser = str "hll"

let testParser = sequenceOf [
    strParser
    strParser
    anyLetter
    digit
]

let result = run testParser "hllhlll13l"

match result with
| Ok(result) -> printfn $"{result}"
| Error e -> printfn $"{e}"