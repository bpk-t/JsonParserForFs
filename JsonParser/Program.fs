open System
open FParsec

type Json =
    | JInteger of int
    | JFloat of float
    | JString of string
    | JBool of bool
    | JArray of Json list
    | JObject of Map<string, Json>
    | JNull

let ws = spaces

let parseBy p str = 
    match run (ws >>. p .>> eof) str with 
    | Success(res, _, _) -> res
    | Failure(msg, _, _) -> failwithf "parse error: %s" msg

let minusSign = opt (pchar '-') |>> Option.isSome
let digit1to9 = anyOf ['1'..'9']
let integer = (many1Chars2 digit1to9 digit <|> pstring "0") |>> int
let jinteger =
    minusSign .>>. integer
    |>> (fun (hasMinus, x) -> JInteger (if hasMinus then -x else x))
let jfloat = 
    tuple3 minusSign integer (pchar '.' >>. integer)
    |>> (fun (hasMinus, i, flac) -> 
        let f = float i + float ("0." + string flac)
        JFloat (if hasMinus then -f else f))

let jnumber = (attempt jfloat <|> jinteger) .>> ws

let convEsc = function
    | 'b' -> '\b'
    | 'f' -> '\f'
    | 'n' -> '\n'
    | 'r' -> '\r'
    | 't' -> '\t'
    | t   -> t

let nonEscapedChar = noneOf ['\\'; '"']
let escapedChar = pchar '\\' >>. anyOf @"\""/bfnrt" |>> convEsc
let jstring =
    manyChars (nonEscapedChar <|> escapedChar)
    |> between (pchar '"') (pchar '"')
    .>> ws
    |>> JString

let jbool =
    choice [pstring "true"; pstring "false"]
    .>> ws
    |>> (fun x -> match x with
        | "true" -> JBool(true)
        | "false" -> JBool(false))
let json, jsonRef = createParserForwardedToRef()
let jarray = 
    sepBy json (pchar ',' >>. ws)
    |> between (pchar '[' >>. ws) (pchar ']' >>. ws)
    |>> (fun xs -> JArray xs)

let objKey =
    manyChars nonEscapedChar
    |> between (pchar '"') (pchar '"')
    .>> ws
let keyAndValue =
    objKey .>> (pchar ':' >>. ws) .>>. json

let jobject =
    sepBy keyAndValue (pchar ',' >>. ws)
    |> between (pchar '{' >>. ws) (pchar '}' >>. ws)
    |>> (fun xs -> JObject(Map(xs)))

let jnull = 
    pstring "null" .>> ws
    |>> (fun _ -> JNull)

jsonRef := choice [jnumber; jstring; jarray; jbool; jobject; jnull]

let rec print (json:Json):string =
    let encObjBranket (x:string):string = "{" + x + "}"
    let encArrayBranket (x:string):string = "{" + x + "}"
    let encKeyName (x:string):string = "\"" + x + "\""
    let kvToString (key:string, value:Json):string = (encKeyName key) + ":" + (print value)
    match json with 
    | JInteger(x)    -> x.ToString()
    | JFloat(x)      -> x.ToString()
    | JString(x)     -> x
    | JBool(x)       -> x.ToString()
    | JArray(list)   -> list |> List.map print |> String.concat "," |> encArrayBranket
    | JObject(kvMap) -> (Map.toList kvMap) |> List.map kvToString |> String.concat "," |> encObjBranket
    | JNull          -> "null"

[<EntryPoint>]
let main argv =
    let ans = "1.3" |> parseBy jnumber
    printfn "ans = %A" ans

    let a = "[ 1 , 2.5 , 3, true, false ]" |> parseBy jarray
    printfn "%A" a

    let b = "true" |> parseBy jbool
    printfn "%A" b

    let obj = """{
        "a" : 100,
        "b": 0.5,
        "c": [100, 200],
        "d": null}""" |> parseBy json
    printfn "%A" obj
    printfn "%s" (print obj)

    0