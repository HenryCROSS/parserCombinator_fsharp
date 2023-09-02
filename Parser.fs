// namespace Parser
module ParserModule

open System


type ParserState = {
    Matched: string list
    Rest: string
    Index: int32
}

let private getSlice (fn: char -> bool) (str: string) : string =
    let rec innerFn (fn: char -> bool) (str: string) (result: string) : string =
        if str.Length > 0 && fn str[0] then
            innerFn fn str[1..] (result + str[0..0])
        else
            result

    innerFn fn str ""

let run
    (parser: Result<ParserState, string> -> Result<ParserState, string>)
    (str: string)
    : Result<ParserState, string> =
    let initStr = { Matched = []; Rest = str; Index = -1 }
    parser (Ok initStr)

let rec sequenceOf
    (parsers: (Result<ParserState, string> -> Result<ParserState, string>) list)
    (state: Result<ParserState, string>)
    : Result<ParserState, string> =
    match parsers with
    | [] -> Error "No parser"
    | lastOne :: [] -> lastOne state
    | head :: tail -> sequenceOf tail (head state)

let rec choice
    (parsers: (Result<ParserState, string> -> Result<ParserState, string>) list)
    (state: Result<ParserState, string>)
    : Result<ParserState, string> =
    let rec tryEachParser parsers =
        match parsers with
        | [] -> Error "No parser" // never exec
        | lastOne :: [] -> lastOne state
        | head :: tail ->
            let re = head (state)
            if Result.isOk re then re else tryEachParser tail

    match parsers with
    | [] -> Error "No parser"
    | _ ->
        let result = tryEachParser parsers

        match result with
        | Ok re -> if re.Rest.Length > 0 then choice parsers result else result
        | Error _ -> result


let letter (state: Result<ParserState, string>) : Result<ParserState, string> =
    state
    |> Result.mapError (fun e -> e)
    |> Result.bind (fun state ->
        if state.Rest.Length > 0 && Char.IsLetter state.Rest[0] then
            Ok(
                {
                    Matched = state.Matched @ [ state.Rest[0..0] ]
                    Rest = state.Rest[1..]
                    Index = state.Index + 1
                }
            )
        else
            Error $"Err: suppose to match letter (index: {state.Index + 1})")

let letters (state: Result<ParserState, string>) : Result<ParserState, string> =
    state
    |> Result.mapError (fun e -> e)
    |> Result.bind (fun state ->
        let matched = getSlice (fun x -> Char.IsLetter x) state.Rest

        if matched.Length > 0 then
            Ok(
                {
                    Matched = state.Matched @ [ matched ]
                    Rest = state.Rest[matched.Length ..]
                    Index = state.Index + matched.Length
                }
            )
        else
            Error $"Err: suppose to match letters (index: {state.Index + 1})")

let digit (state: Result<ParserState, string>) : Result<ParserState, string> =
    state
    |> Result.mapError (fun e -> e)
    |> Result.bind (fun state ->
        if state.Rest.Length > 0 && Char.IsNumber state.Rest[0] then
            Ok(
                {
                    Matched = state.Matched @ [ state.Rest[0..0] ]
                    Rest = state.Rest[1..]
                    Index = state.Index + 1
                }
            )
        else
            Error $"Err: suppose to match digit (index: {state.Index + 1})")

let digits (state: Result<ParserState, string>) : Result<ParserState, string> =
    state
    |> Result.mapError (fun e -> e)
    |> Result.bind (fun state ->
        let matched = getSlice (fun x -> Char.IsDigit x) state.Rest

        if matched.Length > 0 then
            Ok(
                {
                    Matched = state.Matched @ [ matched ]
                    Rest = state.Rest[matched.Length ..]
                    Index = state.Index + matched.Length
                }
            )
        else
            Error $"Err: suppose to match digits (index: {state.Index + 1})")

let str (pattern: string) (state: Result<ParserState, string>) : Result<ParserState, string> =
    state
    |> Result.mapError (fun e -> e)
    |> Result.bind (fun state ->
        if state.Rest.StartsWith pattern then
            Ok(
                {
                    Matched = state.Matched @ [ pattern ]
                    Rest = state.Rest[pattern.Length ..]
                    Index = state.Index + pattern.Length
                }
            )
        else
            Error $"Err: suppose to match {pattern} (index: {state.Index + 1})")