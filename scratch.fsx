#r @"bin/Debug/netcoreapp3.0/FParsecCS.dll"
#r @"bin/Debug/netcoreapp3.0/FParsec.dll"

open FParsec
open System

let p : Parser<string,unit> = pstring "test"

let test p str =
  match run p str with
  | Success(result,_,_) -> printfn "Success: %A" result
  | Failure(errMsg,_,_) -> printfn "Failure: %s" errMsg

test p "input" 

test pfloat "1.23"

test pfloat "1.25E 3"

// parsing a float between brackets

let str s = pstring s
let floatBetweenBrackets:Parser<'a,unit> = str "[" >>. pfloat .>> str "]"

test floatBetweenBrackets "[1.0]"

test floatBetweenBrackets "[]"

let between pBegin pEnd p = pBegin >>. p >>. pEnd

let betweenStrings s1 s2 p = str s1 >>. p .>> str s2

let floatBetweenBrackets:Parser<float,unit> = betweenStrings "[" "]" pfloat

let floatBetweenDoubleBrackets:Parser<float,unit> = pfloat |> betweenStrings "[[" "]]"

test (many floatBetweenBrackets) ""

test (many floatBetweenBrackets) "[1.0]"

test (many floatBetweenBrackets) "[1][2][23][43.12]"

test (many floatBetweenBrackets) "[2][3][34E]"

test (many1 floatBetweenBrackets) ""

let floatList:Parser<'a list,unit> = str "[" >>. sepBy pfloat (str ",") .>> str "]"

test floatList "[]"

test floatList "[2,3,4,23.33,3.4E3]"

test floatList "[2,434,422"

test floatList "[23,11, 4343"

let ws = spaces

let str_ws s = pstring s .>> ws
let float_ws:Parser<float,unit> = pfloat .>> ws

let numberList = str_ws "[" >>. sepBy float_ws (str_ws ",") .>> str_ws "]"

test numberList "[1,2,3]"

test numberList 9

// NEXT: 4.7 Parsing String Data