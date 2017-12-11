ype Nez =
    | NonTerminal of Nez
    | Empty
    | Byte of char
    | ByteSet of list<char>
    | Any
    | Pair of Nez * Nez
    | Choice of bool * list<Nez>
    | Option of Nez
    | Many of int * int * Nez //min, max
    | Predicate of Nez
    | NotPredicate of Nez
    | Tree of bool * option<string> * list<Symbol>
    (*
    | Trap
    | Fail
    | Dispatch
    | Tree
    | Detree
    | LinkTree
    | Tag
    | Value
    | SymbolScope
    | SymbolAction
    | SymbolPredicate
    | If
    | On
    *)
and Symbol = option<string> * Nez
