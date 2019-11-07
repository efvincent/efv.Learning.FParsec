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

let between pBegin pEnd p = pBegin >>. p .>> pEnd

let betweenStrings s1 s2 p = between (str s1) (str s2) p

let floatBetweenBrackets:Parser<float,unit> = betweenStrings "[" "]" pfloat

test floatBetweenBrackets "[1.0]"

test floatBetweenBrackets "[]"

let floatBetweenDoubleBrackets:Parser<float,unit> = pfloat |> betweenStrings "[[" "]]"

test (many floatBetweenBrackets) ""

test (many floatBetweenBrackets) "[1.0]"

test (many floatBetweenBrackets) "[1][2][23][43.12]"

test (many floatBetweenBrackets) "[2][3][34E]"

test (many1 floatBetweenBrackets) ""

let floatList:Parser<float list,unit> = str "[" >>. sepBy pfloat (str ",") .>> str "]"

test floatList "[]"

test floatList "[2,3,4,23.33,3.4E3]"

test floatList "[2,434,422"

test floatList "[23,11, 4343"

let ws = spaces
let ws1 = spaces1

let str_ws s = pstring s .>> ws
let float_ws:Parser<float,unit> = pfloat .>> ws

let numberList = str_ws "[" >>. sepBy float_ws (str_ws ",") .>> str_ws "]"

test numberList "[1,2,3]"

test numberList "   [    3,   434    ,343 ,  345e9 ] "

// 4.7 Parsing String Data

test (many (str "a" <|> str "b")) "abbaaaaaba"

test (skipStringCI "<float>" >>. pfloat) "<FloAT>1.0"

let ident:Parser<string,unit> = 
  let isIdentifierFirstChar c = isLetter c || c = '_'
  let isIdentifierChar c = isLetter c || isDigit c || c = '_'
  many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
  .>> ws

///
/// many1Satisfy2L - parses one or more characters (many1) where the first
/// character satisfies the first predicate, and each of the rest satisfy
/// the second predicate. The "L" at the end means it expects a label for
/// the parser, useful in errors.
/// 

test ident "eric"
test ident "__private21"
test ident "33skadoo"
  

(*
  given the grammar:
  stringLiteral: '"' (normalChar|escapedChar)* '"'
  normalChar:    any char except '\' and '"'
  escapedChar:   '\\' ('\\'|'"'|'n'|'r'|'t')
*)

let stringLiteral:Parser<string,unit> =
  let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
  let unescape = function 
  | 'n' -> '\n'
  | 'r' -> '\r'
  | 't' -> '\t'
  | c -> c

  let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
  between (pstring "\"") (pstring "\"")
    (manyChars (normalChar <|> escapedChar))

let stringLiteral2:Parser<string,unit> =
    let normalCharSnippet = many1Satisfy (fun c -> c <> '\\' && c <> '"')
    let unescape c = match c with
                     | 'n' -> "\n"
                     | 'r' -> "\r"
                     | 't' -> "\t"
                     | c   -> string c
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    between (pstring "\"") (pstring "\"")
            (manyStrings (normalCharSnippet <|> escapedChar))

test stringLiteral2 "\"abc\""

let normalChar:Parser<string,unit> = 
  many1Satisfy (fun c -> "\\\"[]{}()" |> Seq.contains c |> not)

let escapedChar:Parser<string,unit> = 
  pstring "\\" >>. 
  (anyOf "\\nrt\"" 
    |>> function
        | 'n' -> "\n"
        | 'r' -> "\r"
        | 't' -> "\t"
        | c   -> string c)

let stringLiteral3:Parser<string,unit> =
  between (pstring "\"") (pstring "\"")
          (manyStrings (normalChar <|> escapedChar))

test normalChar "abce"

test normalChar "ab\ncd\te"

test escapedChar "\\n"

test (normalChar <|> escapedChar) "abc\nde"

test (manyStrings (normalChar <|> escapedChar)) "abc\nde"

test (betweenStrings "[" "]" (manyStrings (normalChar <|> escapedChar))) "[ab\ncde]"

test ((pstring "[") >>. (manyStrings (normalChar <|> escapedChar)) .>> (pstring "]")) "[ab\ncde]"

test stringLiteral2 "\"a\\\"bc\""

test (ws >>. stringLiteral2 .>> ws) """

"This is a string literal
that has some carrage returns in it. Along with
\tsome 
\ttabs. It's also built to ignore whitespace coming before and after the string literal"

"""

test stringLiteral2 "abc"

test stringLiteral3 "\"abc\""

test stringLiteral3 "abc"

// 4.8 Sequentially applying parsers
// the pipe2-5 combinators apply parsers in order and take a function that
// can combine the results into a final result.

let product = pipe2 float_ws (str_ws "*" >>. float_ws) (fun x y -> x * y)

test product "309 * 339"

test product "3430*3300"

test product "232E10 * 332E-8"

type StringConstant = StringConstant of string * string

let stringConstant = 
  pipe3 ident (str_ws "=") stringLiteral3 (fun id _ s -> StringConstant(id,s))

test stringConstant "myConstant = \"LEFT-RIGHT-LEFT\""

// 4.9 Parsing Alternatives
let boolean:Parser<bool,unit> = 
  let sret s v = pstring s .>> (eof <|> ws1) >>% v
  ((sret "true" true) <|> (sret "false" false))

test boolean "false"

test boolean "true"

test boolean "trueish"

// for choice <|>, the right side is only attempted if the left side
// did not consume input. For example:

test ((ws >>. str "a") <|> (ws >>. str "b")) " b"

// fails because the ws on the left always succeeds and consumes the space
// and when it gets to the `str "a"` that fails and moves on. But with the
// space consumed, the right will never be attempted and the final result is
// the failure of `str "a"`
// For more info see 
// http://www.quanttec.com/fparsec/users-guide/parsing-alternatives.html
// and
// http://www.quanttec.com/fparsec/users-guide/looking-ahead-and-backtracking.html

// 4.11 Parsing JSON

