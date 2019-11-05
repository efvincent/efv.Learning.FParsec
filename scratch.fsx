#r @"bin/Debug/netcoreapp3.0/FParsec.dll"
#r @"bin/Debug/netcoreapp3.0/FParsecCS.dll"

open FParsec
open System

let p = pstring "test"

(*
results in:

/Users/.../code/scratch/fparsec-001/scratch.fsx(6,9): error FS0074: 
The type referenced through 'FParsec.CharStream`1' is defined in an assembly 
that is not referenced. You must add a reference to assembly 'FParsecCS'.

*)

let test p str =
  match run p str with
  | Success(result,_,_) -> printfn "Success: %A" result
  | Failure(errMsg,_,_) -> printfn "Failure: %s" errMsg


