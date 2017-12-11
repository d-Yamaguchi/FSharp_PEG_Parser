namespace PEG

module TextInput =
    open System

    type Position = {
        line: int
        column: int
    }
    let initPos = {line=0; column=0}
    let incrCol pos = {pos with column=pos.column + 1}
    let incrLine pos = {line=pos.line+1; column=0}

    type InputState = {
        lines: string[]
        position: Position
    }
    let currentLine inputState = 
        let linePos = inputState.position.line
        if linePos < inputState.lines.Length then
            inputState.lines.[linePos]
        else
            "end of file"

    let fromStr str = 
        if String.IsNullOrEmpty(str) then
            {lines=[||]; position=initPos}
        else
            let separators = [| "\r\n"; "\n" |]
            let lines = str.Split(separators, StringSplitOptions.None)
            {lines=lines; position=initPos}

    let nextChar input =
        let linePos = input.position.line
        let colPos = input.position.column

        if linePos >= input.lines.Length then
            input, None
        else
            let currentLine = currentLine input
            if colPos < currentLine.Length then
                let char = currentLine.[colPos]
                let newPos = incrCol input.position 
                let newState = {input with position=newPos}
                newState, Some char
            else 
                let char = '\n'
                let newPos = incrLine input.position 
                let newState = {input with position=newPos}
                newState, Some char
    
    let getPos (state:InputState) =
        state.position

    let isEof input =
        (*
        let linePos = input.position.line
        let lineLen = input.lines.Length
        let colPos = input.position.column
        let colLen = (currentLine input).Length
        printfn "isEof Check (%d,%d) -> (%d,%d)" linePos colPos lineLen colLen
        *)
        (input.position.line >= input.lines.Length - 1) 
            && (input.position.column >= (currentLine input).Length)

module ParserCombinator =
    open System
    open Microsoft.FSharp.Collections

    // Define
    type Input = TextInput.InputState
    type ParserLabel = string
    type ParserError = string

    type ParserPosition = {
        current: string
        line: int
        column: int
    }

    type Result<'a> =
        | Success of 'a
        | Failure of ParserLabel * ParserError * ParserPosition 

    type Parser<'a> = {
        parseFn : (Input -> Result<'a * Input>)
        label:  ParserLabel 
        }

    // run
    let runOnInput parser input = 
        parser.parseFn input

    let run parser inputStr = 
        runOnInput parser (TextInput.fromStr inputStr)

    //error
    let parserPositionFromInputState (state:Input) = {
        current = TextInput.currentLine state
        line = state.position.line
        column = state.position.column
    }

    let printResult result =
        match result with
        | Success (value,input) -> 
            printfn "%A" value
        | Failure (label,error,parserPos) -> 
            let errorLine = parserPos.current
            let colPos = parserPos.column
            let linePos = parserPos.line
            let failureCaret = sprintf "%*s^%s" colPos "" error
            printfn "Line:%i Col:%i Error parsing %s\n%s\n%s" linePos colPos label errorLine failureCaret 

    //label
    let getLabel parser = 
        parser.label
    let setLabel parser newLabel = 
        let newInner input = 
            let result = parser.parseFn input
            match result with
            | Success s ->
                Success s 
            | Failure (oldLabel,err,pos) -> 
                Failure (newLabel,err,pos) 
        {parseFn=newInner; label=newLabel}

    let ( <?> ) = setLabel

    //base combinator

    //isEof
    let eof =
        let label = "eof"
        let inner (input:TextInput.InputState) =
            if TextInput.isEof input then
                Success ("",input)
            else
                let err = "Not EOF"
                let pos = parserPositionFromInputState input
                Failure (label, err, pos)
        {parseFn=inner;label=label}

    //notPredicate
    let not p =
        let label = "not"
        let inner input =
            match runOnInput p input with
            | Success (value,rest) ->
                let err = sprintf "Unexpected \"%A\"" value
                let pos = parserPositionFromInputState input
                Failure (label,err,pos)
            | Failure (label,err,pos) -> 
                Success ("",input)
        {parseFn=inner;label=label} 
    let ( ! ) = not

    //getPosition
    let loc =
        let label = "loc"
        let inner (input: TextInput.InputState) =
            let pos = TextInput.getPos input
            Success (pos, input)
        {parseFn=inner;label=label}


    let satisfy pred label =
        let inner input =
            let rest,char = TextInput.nextChar input 
            match char with
            | None -> 
                let err = "No more input"
                let pos = parserPositionFromInputState input
                Failure (label,err,pos)
            | Some c -> 
                if pred c then
                    Success (c,rest)
                else
                    let err = sprintf "Unexpected '%c'" c
                    let pos = parserPositionFromInputState input
                    Failure (label,err,pos)
        {parseFn=inner;label=label}

    // (T -> Parser<S>) -> (Parser<T>) -> Parser<S> 
    let bindP f p =
        let label = "unknown"
        let inner input =
            let result1 = runOnInput p input 
            match result1 with
            | Failure (label,err,pos) -> 
                Failure (label,err,pos)  
            | Success (value1,rest) ->
                let p2 = f value1
                runOnInput p2 rest
        {parseFn=inner; label=label}

    let ( >>= ) p f = bindP f p

    // T -> Parser<T>
    let returnP x = 
        let label = sprintf "%A" x
        let inner input =
            Success (x,input)
        {parseFn=inner; label=label}

    // (T -> S) -> (Parser<T> -> Parser<S>)
    let mapP f = 
        bindP (f >> returnP)

    let ( <!> ) = mapP

    let ( |>> ) x f = mapP f x

    // Parser<T -> S> -> (Parser<T> -> Parser<S>)
    let applyP fP xP =         
        fP >>= (fun f -> 
        xP >>= (fun x -> 
            returnP (f x) ))

    let ( <*> ) = applyP

    let lift2 f xP yP =
        returnP f <*> xP <*> yP

    let andThen p1 p2 =         
        let label = sprintf "%s andThen %s" (getLabel p1) (getLabel p2)
        p1 >>= (fun p1Result -> 
        p2 >>= (fun p2Result -> 
            returnP (p1Result,p2Result) ))
        <?> label

    let ( .>>. ) = andThen

    let orElse p1 p2 =
        let label = sprintf "%s orElse %s" (getLabel p1) (getLabel p2)
        let inner input =
            match runOnInput p1 input with
            | Success result -> 
                Success result
            | Failure _ -> 
                runOnInput p2 input
        {parseFn=inner; label=label}

    let ( <|> ) = orElse

    let choice parsers = 
        List.reduce ( <|> ) parsers 
       
    let rec sequence parsers =
        let cons head tail = head::tail
        let consP = lift2 cons
        match parsers with
        | [] -> 
            returnP []
        | head::tail ->
            consP head (sequence tail)

    let rec parseZeroOrMore parser input =
        let firstResult = runOnInput parser input 
        match firstResult with
        | Failure (_,_,_) -> 
            ([],input)  
        | Success (firstMatch,firstRest) -> 
            let (secondMatchs,secondRest) = 
                parseZeroOrMore parser firstRest
            (firstMatch::secondMatchs, secondRest) 

    let any =
        let label = "any"
        let inner input =
            let rest,char = TextInput.nextChar input 
            match char with
            | None -> 
                let err = "No more input"
                let pos = parserPositionFromInputState input
                Failure (label,err,pos)
            | Some c -> 
                Success (c,rest)
        {parseFn=inner;label=label}

    let many parser = 
         let label = sprintf "many %s" (getLabel parser)
         let rec inner input =
             Success (parseZeroOrMore parser input)
         {parseFn=inner; label=label}
    
    let many1 p =         
        let label = sprintf "many1 %s" (getLabel p)
        p >>= (fun head -> 
            many p >>= (fun tail -> 
                returnP (head::tail) ))
        <?> label

    let opt p = 
        let label = sprintf "opt %s" (getLabel p)
        let some = p |>> Some
        let none = returnP None
        (some <|> none) <?> label

    let (.>>) p1 p2 = 
        p1 .>>. p2 
        |> mapP (fun (a,b) -> a) 

    let (>>.) p1 p2 = 
        p1 .>>. p2 
        |> mapP (fun (a,b) -> b) 

    let between p1 p2 p3 = 
        p1 >>. p2 .>> p3 
    
    let sepBy1 p sep =
        let sepThenP = sep >>. p            
        p .>>. many sepThenP 
        |>> fun (p,pList) -> p::pList

    let sepBy p sep =
        sepBy1 p sep <|> returnP []

    //standard parser
    let pchar char = 
        let label = sprintf "%c" char 
        let pred ch = (ch = char) 
        satisfy pred label 

    let anyOf chars = 
        let label = sprintf "anyOf %A" chars
        chars
        |> List.map pchar
        |> choice
        <?> label

    let charListToStr charList =
        String(List.toArray charList)

    let manyChars cp =
        many cp
        |>> charListToStr

    let manyChars1 cp =
        many1 cp
        |>> charListToStr

    let pstring str = 
        let label = str 

        str
        |> List.ofSeq
        |> List.map pchar 
        |> sequence
        |> mapP charListToStr 
        <?> label

    let range f t =
        let label = sprintf "range %A -> %A" f t
        let inner input =
            let rest,char = TextInput.nextChar input 
            match char with
            | None -> 
                let err = "No more input"
                let pos = parserPositionFromInputState input
                Failure (label,err,pos)
            | Some c -> 
                if f <= c && c <= t then
                    Success (c,rest)
                else
                    let err = sprintf "Unexpected '%c'" c
                    let pos = parserPositionFromInputState input
                    Failure (label,err,pos)
        {parseFn=inner;label=label}

    //whitespace
    let whitespaceChar = 
        let pred = Char.IsWhiteSpace 
        let label = "whitespace"
        satisfy pred label 

    let spaces = many whitespaceChar
    let spaces1 = many1 whitespaceChar

    //number
    let digitChar = 
        let pred = Char.IsDigit 
        let label = "digit"
        satisfy pred label
    
    let pint = 
        let label = "integer" 
        let toInt (sign,digits) = 
            let i = int digits
            match sign with
            | Some ch -> -i 
            | None -> i
                
        opt (pchar '-') .>>. manyChars1 digitChar 
        |> mapP toInt
        <?> label

    let pfloat = 
        let label = "float" 
        let toFloat (((sign,digits1),point),digits2) = 
            let fl = sprintf "%s.%s" digits1 digits2 |> float
            match sign with
            | Some ch -> -fl  // negate the float
            | None -> fl
        let digits = manyChars1 digitChar 

        opt (pchar '-') .>>. digits .>>. pchar '.' .>>. digits 
        |> mapP toFloat
        <?> label