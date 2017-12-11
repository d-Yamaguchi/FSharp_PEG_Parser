// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "JSONParser.fs"
open PEG.JSONParser
open System
open System.IO

printfn "Hello World"

parseTest anyChar "abcd"
parseTest (plist [anyChar; anyChar]) "abc"
parseTest (plist [anyChar; peek; anyChar]) "abc"

parseTest (isOneOf "ab") "abc"
parseTest (isOneOf "ab") "def"

parseTest (oneOf "ab") "abc"
parseTest (oneOf "ab") "def"

parseTest (many Char.IsDigit) "123abc"

parseTest (spaces @>> anyChar) "   123"
parseTest (plist [anyChar @<< spaces; anyChar]) "1   23"

parseTest jsonString "\"abc\""
parseTest jsonString @"'a\\b\\c'"
parseTest jsonString @"'A\x42\u0043'"

parseTest jsonNumber "123"
parseTest jsonNumber "-3.14"

parseTest jsonValue "abc 456"
parseTest jsonValue "-1,2"

let test = """
{
  "log": {
    "version": "1.1",
    "creator": {
      "name": "Foo",
      "version": "1.0" },
    "pages": [
      { "id": "page_1", "title": "Test1" },
      { "id": "page_2", "title": "Test2" }
    ],
    "test": [-1.23, null, [1, 2, 3]]
  }
}
"""

try
    use jp = new JSONParser(new StringReader(test))
    if jp.Find "pages" then
        for i in jp.Each() do
        for j in jp.Each() do
        printfn "%d:%d. %s = %s" i j jp.Name jp.Value
with e ->
    printf "%A" e
