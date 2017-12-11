namespace PEG

// A Generalized Foundation of Parsing ExpressionGrammars
type GPEG =
    | Empty
    | Character of char
    | Any
    | NonTerminal of GPEG //Non terminal
    | Sequence of GPEG * GPEG
    | OrderedChoice of GPEG * GPEG
    | UnorderedChoice of GPEG * GPEG
    | Many of GPEG
    | NotPredicate of GPEG