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

### FSI Setup
FSI Code completion & intellisense doesn't work when FSAC Runtime is set to .NET Core in vscode. It needs to be set to net:
```
"FSharp.fsacRuntime": "net"
```

## References
[Tutorial](http://www.quanttec.com/fparsec/tutorial.html#preliminaries)
[Small Example in Blog](https://geekeh.com/post/starting-small-with-fparsec/)
[User's Guide](https://www.quanttec.com/fparsec/users-guide)
[Trelford's Small Basic Parser Tutorial](http://trelford.com/blog/post/FParsec.aspx)
Interesting post from perspective of a Haskeller [refactoring to FParsec](https://blog.leifbattermann.de/2015/12/15/refactoring-to-fparsec)
