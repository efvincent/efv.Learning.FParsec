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
