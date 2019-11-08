#r @"bin/Debug/netcoreapp3.0/FParsecCS.dll"
#r @"bin/Debug/netcoreapp3.0/FParsec.dll"

open FParsec

let p : Parser<string,unit> = pstring "test"

let test p str =
  match run p str with
  | Success(result,_,_) -> printfn "Success: %A" result
  | Failure(errMsg,_,_) -> printfn "Failure: %s" errMsg

type Json = 
    | JString of string
    | JNumber of float
    | JBool of bool
    | JNull
    | JList of Json list
    | JObject of Map<string, Json>

// start by covering the simple cases
let jnull:Parser<Json, unit>   = stringReturn "null" JNull
let jtrue:Parser<Json, unit>   = stringReturn "true" (JBool true)
let jfalse:Parser<Json, unit>  = stringReturn "false" (JBool false)
let jnumber:Parser<Json, unit> = pfloat |>> JNumber 

(*
    val (|>>): Parser<'a,'u> -> ('a -> 'b) -> Parser<'b,'u>
    The parser p |>> f applies the parser p and returns the 
    result of the function application f x, where x is the 
    result returned by p.

    p |>> f is an optimized implementation of 
    p >>= fun x -> preturn (f x)
*)

// JString

let str s = pstring s
let stringLiteral:Parser<string,unit> =
    let escape = 
        anyOf "\"\\/bfnrt"
        |>> function
            | 'b' -> "\b"
            | 'f' -> "\u000C"
            | 'n' -> "\n"
            | 'r' -> "\r"
            | 't' -> "\t"
            | c   -> string c
    let unicodeEscape =
        /// converts a hex char to it's integer number
        let hex2int c = (int c &&& 15) + (int c >>> 6) * 9
        str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
            (hex2int h3) * 4096 +
            (hex2int h2) * 256  +
            (hex2int h1) * 16   +
            hex2int h0
            |> char |> string)
    let escapedCharSnippet = str "\\" >>. (escape <|> unicodeEscape)
    let normalCharSnippet  = manySatisfy (fun c -> c <> '"' && c <> '\\')

    between (str "\"") (str "\"")
        (stringsSepBy normalCharSnippet escapedCharSnippet)

let jstring = stringLiteral |>> JString 

let jvalue, jvalueRef = createParserForwardedToRef<Json,unit>()

(*
    To handle recursive nature of lists and maps:
    createParserForwardedToRef creates a parser (jvalue) that 
    forwards all invocations to the parser in a reference cell 
    (jvalueRef). Initially, the reference cell holds a dummy parser, 
    but since the reference cell is mutable, we can later replace 
    the dummy parser with the actual value parser, once we have 
    finished constructing it.
*)

let ws = spaces

let listBetweenStrings sOpen sClose pElement f =
    between (str sOpen) (str sClose)
        (ws >>. sepBy (pElement .>> ws) (str "," >>. ws) |>> f)

let jlist = listBetweenStrings "[" "]" jvalue JList

let keyValue = stringLiteral .>>. (ws >>. str ":" >>. ws >>. jvalue)

let jobject = listBetweenStrings "{" "}" keyValue (Map.ofList >> JObject)

do jvalueRef := choice 
    [
        jobject
        jlist
        jstring
        jnumber
        jtrue
        jfalse
        jnumber
    ]

let json = ws >>. jvalue .>> ws .>> eof

test json """
{
    "name": "eric",
    "age": 56,
    "lotto_numbers": [32,544,33,343,565],
    "projects": [
        { "name": "vision", "active": false },
        { "name": "flight deck", "active": true }
    ]
}
"""