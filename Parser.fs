namespace Parser

open System


type ParserState = {
    Matched: string list
    Rest: string
    Index: int32
}

module ParserModule =

    let run
        (parser: Result<ParserState, string> -> Result<ParserState, string>)
        (str: string)
        : Result<ParserState, string> =
        let initStr = { Matched = []; Rest = str; Index = 0 }
        parser (Ok initStr)

    let rec sequenceOf
        (parsers: (Result<ParserState, string> -> Result<ParserState, string>) list)
        (state: Result<ParserState, string>)
        : Result<ParserState, string> =
        match parsers with
        | [] -> Error "No parser"
        | lastOne :: [] -> lastOne state
        | head :: tail -> sequenceOf tail (head state)
    
    let choice
        (parsers: (Result<ParserState, string> -> Result<ParserState, string>) list)
        (state: Result<ParserState, string>)
        : Result<ParserState, string> =
        Error "Not implement"

    let anyLetter (state: Result<ParserState, string>) : Result<ParserState, string> =
        state
        |> Result.mapError (fun e -> e)
        |> Result.bind (fun state ->
            if Char.IsLetter state.Rest[0] then
                Ok(
                    {
                        Matched = state.Matched @ [ string state.Rest[0] ]
                        Rest = state.Rest[1..]
                        Index = state.Index + 1
                    }
                )
            else
                Error $"Err: suppose to match letter (index: {state.Index + 1})")

    let digit (state: Result<ParserState, string>) : Result<ParserState, string> =
        state
        |> Result.mapError (fun e -> e)
        |> Result.bind (fun state ->
            if Char.IsNumber state.Rest[0] then
                Ok(
                    {
                        Matched = state.Matched @ [ string state.Rest[0] ]
                        Rest = state.Rest[1..]
                        Index = state.Index + 1
                    }
                )
            else
                Error $"Err: suppose to match digit (index: {state.Index + 1})")

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