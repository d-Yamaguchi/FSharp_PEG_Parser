
#load "ParserCombinator.fs"

open System
open PEG.ParserCombinator

(*
    bind: (T -> Parser<S>) -> (Parser<T>) -> Parser<S>
    let ( >>= ) p f = bindP f p

    map: (T -> S) -> (Parser<T> -> Parser<S>)
    let ( <!> ) = mapP
    let ( |>> ) x f = mapP f x

    apply: Parser<T -> S> -> (Parser<T> -> Parser<S>)
    let ( <*> ) = applyP

    let ( .>>. ) = andThen
    let ( <|> ) = orElse
    let ( <?> ) = setLabel
*)

type Json =
    | JNumber of float
    | JArray of Json list

let parseBy p str =
    match run p str with
    | Success (s,_)   -> printfn "Success %A" s
    | Failure (_,e,_) -> printfn "Failure %s" e

let jnumber = pfloat |>> (fun x -> JNumber x)
let jarray =
    let plist = sepBy jnumber (pchar ',')
    between (pchar '[') plist (pchar ']')


"1.5" |> parseBy jnumber
"[1.5,2.0,3.0]" |> parseBy jarray
"ab" |> parseBy ((pchar 'a') .>>. (pchar 'b'))

"ab" |> parseBy ((not <| pchar 'b') .>>. pchar 'a')
"ba" |> parseBy ((not <| pchar 'b') .>>. pchar 'a')

"ab" |> parseBy ((pchar 'a') .>>. loc .>>. (pchar 'b'))