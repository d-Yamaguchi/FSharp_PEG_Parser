namespace PEG

open System
open System.IO
open System.Collections


module JSONParser =
    let parseTest parser src =
        use sr = new StringReader(src)
        try
            printfn "%A" (parser sr)
        with e ->
            printfn "%s" e.Message

    let anyChar (tr:TextReader) =
        let ch = tr.Read()
        if ch >= 0 then char ch else
            failwith "anyChar: unexpected end of input"

    let plist list tr = [for p in list -> p tr]

    let peek (tr:TextReader) = 
        let ch = tr.Peek()
        if ch >= 0 then char ch else
            failwith "peek: unexpected end of input"

    let isOneOf (s:string) (tr:TextReader) =
        let ch = tr.Peek()
        if ch = -1 || s.IndexOf(char ch) < 0 then false else
            tr.Read() |> ignore
            true

    let oneOf (s:string) (tr:TextReader) =
        let ch = tr.Peek()
        if isOneOf s tr then char ch else
            failwith <| sprintf "oneOf: '%c' is not in \"%s\"" (char ch) s

    let many f (tr:TextReader) =
        use sw = new StringWriter()
        let rec g() =
            let ch = tr.Peek()
            if ch >= 0 && f (char ch) then
                sw.Write(char ch)
                tr.Read() |> ignore
                g()
        g()
        sw.ToString()

    let isSpace (ch:char) = " \r\n\t".IndexOf ch >= 0

    let rec spaces (tr:TextReader) =
        let ch = tr.Peek()
        if ch >= 0 && isSpace (char ch) then
            tr.Read() |> ignore
            spaces tr
    
    let (@>>) a b = fun tr -> a tr |> ignore; b tr
    let (@<<) a b = fun tr -> let r = a tr in b tr |> ignore; r


    let jsonHex tr =
        match anyChar tr with
        | ch when '0' <= ch && ch <= '9' -> int ch - int '0'
        | ch when 'A' <= ch && ch <= 'F' -> int ch - int 'A' + 10
        | ch when 'a' <= ch && ch <= 'f' -> int ch - int 'f' + 10
        | ch -> failwith <| sprintf "hexChar: '%c' is not hex char" ch

    let jsonUnescape tr =
        match anyChar tr with
        | 'b' -> '\b'
        | 't' -> '\t'
        | 'n' -> '\n'
        | 'v' -> char 11
        | 'f' -> char 12
        | 'r' -> '\r'
        | 'x' -> (jsonHex tr <<<  4) ||| (jsonHex tr) |> char
        | 'u' -> (jsonHex tr <<< 12) ||| (jsonHex tr <<< 8) |||
                 (jsonHex tr <<<  4) ||| (jsonHex tr) |> char
        | ch  -> ch
   
    let jsonString tr =
        let start = oneOf "'\"" tr
        use sw = new StringWriter()
        let rec f() =
            match anyChar tr with
            | ch when ch = start -> ()
            | '\\' -> sw.Write (jsonUnescape tr); f()
            | ch -> sw.Write ch; f()
        f()
        sw.ToString()

    let rec jsonNumber tr =
        if isOneOf "-" tr then "-" + jsonNumber tr else
            let n1 = many Char.IsDigit tr
            if not <| isOneOf "." tr then n1 else
                n1 + "." + many Char.IsDigit tr

    let jsonValue tr =
        match peek tr with
        | '\'' | '"' -> jsonString tr
        | '-'        -> jsonNumber tr
        | ch when Char.IsDigit ch -> jsonNumber tr
        | ch when Char.IsLetter ch -> many Char.IsLetterOrDigit tr
        | ch -> failwith <| sprintf "jsonValue: unknown '%c'" ch


    let jsonParser (tr:TextReader) =
        let rec value stack = seq {
            match  (spaces @>> peek) tr with
            | '{' ->
                while isOneOf "{," tr && (spaces @>> peek) tr <> '}' do
                    match peek tr with
                    | '\'' | '"' ->
                        let name = (jsonString @<< spaces @<< oneOf ":") tr
                        let ch = (spaces @>> peek) tr
                        match ch with
                        | '{' | '[' ->
                            yield name, ch, "", stack
                            yield! value (name::stack)
                            yield name, (if ch = '{' then '}' else ']'), "", stack
                        | _ ->
                            yield name, ':', jsonValue tr, stack
                    | ch ->
                        failwith <| sprintf "jsonParser: unknown '%c'" ch
                (spaces @<< oneOf "}") tr
            | '[' ->
                while isOneOf "[," tr && (spaces @>> peek) tr <> ']' do
                    let ch = peek tr
                    match ch with
                    | '{' | '[' ->
                        yield "", ch, "", stack
                        yield! value (""::stack)
                        yield "", (if ch = '{' then '}' else ']'), "", stack
                    | _ ->
                        yield "", ':', jsonValue tr, stack
                (spaces @<< oneOf "]") tr
            | ch ->
                failwith <| sprintf "jsonParser: unknown '%c'" ch }
        value []


    type JSONParser(tr:TextReader) =
        let en = (jsonParser tr).GetEnumerator()
        member x.Dispose() = tr.Dispose()
        interface IDisposable with
            member x.Dispose() = x.Dispose()
        member x.Current = en.Current
        member x.Name  = let (n,_,_,_) = en.Current in n
        member x.Type  = let (_,t,_,_) = en.Current in t
        member x.Value = let (_,_,v,_) = en.Current in v
        member x.Stack = let (_,_,_,s) = en.Current in s
        member x.Read() = en.MoveNext()
        member x.Find(name:string) =
            let rec f() =
                if not <| x.Read() then false
                elif x.Name = name then true else f()
            f()
        member x.Each() =
           let len = x.Stack.Length
           let num = ref 0
           seq {
                while x.Read() && x.Stack.Length <> len do
                    yield !num
                    num := !num + 1 }



