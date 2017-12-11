namespace PEG

open System
open System.Collections
open PEG.ParserCombinator

module CPEG =
    //type Grammar = NonTermSymbols * TermSymbols * Rules * Exp * LabelSymbols
    type Grammar = Rules * Exp
    and  TermSymbol = char
    and  NonTermSymbol = string
    and  TermSymbols = Set<TermSymbol>
    and  NonTermSymbols = Set<NonTermSymbol>
    and  Rules = Map<NonTermSymbol, Exp>
    and  Label = string
    and  LabelSymbols = Set<Label>
    and  Exp =
         | Empty
         | Dot
         | Char of TermSymbol
         | CharRange of (TermSymbol*TermSymbol) list
         | NonTerm of NonTermSymbol
         | Seq of Exp * Exp
         | OrdChoice of Exp * Exp
         | Many of Exp
         | Not of Exp
         | Capture of Label * Exp
         | LFold of Label * Exp * Exp

    let inline stringToNonTerm (str:string):NonTermSymbol = str
    let inline charToTerm (c:char):TermSymbol = c

    //ここ修正(末尾が(Empty,Empty))
    let inline charListToTermSeq (cs: char list) = 
        List.foldBack (fun a b -> (Char <| charToTerm a), Seq b) cs (Empty,Empty)
    let inline expListToSequence (es: Exp list) =
        List.foldBack (fun a b -> Seq (a, b)) es (Seq (Empty,Empty))
    let inline expListToOrderedChoice (es: Exp list) =
        List.foldBack (fun a b -> OrdChoice (a, b)) es (OrdChoice (Empty,Empty))

    // parser symbol
    let Eol    = (pchar '\r' >>. pchar '\n') <|> (pchar '\n') <|> (pchar '\r')
    let NotEol = (not Eol) >>. any
    let NotEof = not eof

    let Space    = (pchar ' ') <|> (pchar '\t') <|> (pchar '\n')
    let Comment  = (pchar '#' .>>. (many NotEol |>> charListToStr) .>> Eol) 
                     |>> (fun (a,b) -> string a + b)
    let Spacing = many (Comment <|> (Space |>> fun c -> c.ToString()))

    // Symbol
    let SEMICOLON = (pchar ';') .>> Spacing
    let EQ = (pchar '=') .>> Spacing
    let LEFTARROW = (pstring "<-") .>> Spacing
    let SLASH = (pchar '/') .>> Spacing
    let AND = (pchar '&') .>> Spacing
    let NOT = (pchar '!') .>> Spacing
    let QUESTION = (pchar '?') .>> Spacing
    let STAR = (pchar '*') .>> Spacing
    let PLUS = (pchar '+') .>> Spacing
    let OPENP = (pchar '(') .>> Spacing
    let CLOSEP = (pchar ')') .>> Spacing
    let DOT = (pchar '.') .>> Spacing 

    let IdentStart = (range 'a' 'z') <|> (range 'A' 'Z') <|> (pchar '_')
    let IdentCont = IdentStart <|> (range '0' '9')
    let Identifier = (loc .>>. IdentStart .>>. (many IdentCont) .>> Spacing)
                        |>> (fun ((pos,pre), cs) ->
                                let ident = String(List.toArray (pre::cs))
                                stringToNonTerm <| stringToNonTerm ident)
    
    let Literal = 
        let sqt = between (pchar '\'') (many ( !(pchar '\'') >>. any)) (pchar '\'' .>> Spacing)
        let dqt = between (pchar '\"') (many ( !(pchar '\"') >>. any)) (pchar '\"' .>> Spacing)
        loc .>>. (sqt <|> dqt)
            |>> (fun (pos,cs) -> Seq <| charListToTermSeq cs)

    let Range = ((any .>> (pchar '-')) .>>. any)
               |>> (fun (a,b) -> (charToTerm a, charToTerm b))

    //単文字への対応
    let Class = 
        let range = many ( !(pchar ']') >>. Range )
        loc .>>. between (pchar '[') range (pchar ']')
            |>> (fun (pos,ranges) -> CharRange ranges)
               
    let Primary = 
        (Identifier |>> (fun ide -> NonTerm ide))
        //<|> (between OPENP Expression CLOSEP)
        <|> Literal
        <|> Class
        <|> ((loc .>> DOT) |>> (fun c -> Dot))

    let rec Prefix =
        //(loc .>> AND) .>>. Suffix
        ( ((loc .>> NOT) .>>. Suffix) |>> (fun (pos,exp) -> Not exp) )
        <|> Suffix
    and Suffix =
        //((loc .>>. Primary) .>> QUESTION) |>> 
        //((loc .>>. Primary) .>> PLUS) |>>
        ( ((loc .>>. Primary) .>> STAR) |>> (fun (pos,exp) -> Many exp) )
        <|> Primary
    and Sequence = (many1 Prefix)
                    |>> (fun exps -> expListToSequence exps)
    and Expression = Sequence .>>. (many (SLASH >>. Sequence))
                    |>> (fun (exp,exps) -> 
                        match (exp,exps) with
                        | (e,[]) -> e
                        | (e,es) -> OrdChoice (e,expListToOrderedChoice es))

    let Definition = ((Identifier .>> EQ) .>>. Expression .>> SEMICOLON)
                    |>> (fun (nt,exp) -> (nt,exp))
    
    let Grammar = (loc .>> Spacing) .>>. (many1 (Definition .>> Spacing))
                    |>> (fun (pos, rules) -> 
                            let ruleMap = Map.ofList rules
                            let startExp = rules.[0]
                            (ruleMap, startExp))