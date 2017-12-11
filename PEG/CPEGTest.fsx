#load "ParserCombinator.fs"
#load "CPEG.fs"

open PEG.ParserCombinator
open PEG.CPEG

let parseBy p str =
    match run p str with
    | Success (x,y)   -> printfn "Success %A %A" x y
    | Failure (a,b,c) -> printfn "Failure %A %A %A" a b c

"_a23" |> parseBy Primary
"\"aaa\"" |> parseBy Primary
"[a-zA-Z]" |> parseBy Primary
"." |> parseBy Primary

"!A" |> parseBy Prefix
"A*" |> parseBy Suffix
"!A*"|> parseBy Prefix

"A B* !C" |> parseBy Sequence

"A / B / C" |> parseBy Expression
"A B" |> parseBy Expression

"A = A B;" |> parseBy Definition
"A = A/B;" |> parseBy Definition

"A = A B; B = C/D;" |> parseBy Grammar





