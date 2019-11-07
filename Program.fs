// Learn more about F# at http://fsharp.org

open System
open FParsec

let test p s =
  match run p s with 
  | Success(result,_,_) -> printfn "Success: %A" result 
  | Failure(errMsg,_,_) -> printfn "Failure: %s" errMsg

[<EntryPoint>]
let main argv =
  let p = pstring "test"
  test p "this is a string"
  printfn "\nbye!\n"
  0 // return an integer exit code
