# FParsec-001
Experimentation in FParsec.
A learning exercise.

## Setup
1. Setup the project
```bash
$ dotnet new console -lang F# -o efv.fparsec.practice
$ cd efv.fparsec.practice
$ dotnet new tool-manifest
$ dotnet tool install paket
```
2. Add an fsi file, content:
```fsharp
#r @"bin/Debug/netcoreapp3.0/FParsecCS.dll"
#r @"bin/Debug/netcoreapp3.0/FParsec.dll"

open FParsec
open System
```
Note the order of the references is important, the `*CS.dll` comes first or you'll get the error
```
error FS0074: The type referenced through’FParsec.CharStream`1’ is defined in an assembly that is not referenced.
```
