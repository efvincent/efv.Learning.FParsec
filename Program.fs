// Learn more about F# at http://fsharp.org

open System
open FParsec

[<EntryPoint>]
let main argv =
  let p = pstring "test"
  run p "input" |> printfn "%A"
  printfn "\nbye!\n"
  0 // return an integer exit code
