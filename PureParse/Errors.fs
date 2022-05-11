namespace PureParse

open System.Text

exception ParseError of message:string * index:int * line:int * column:int
exception CharParseError of c:char * index:int * line:int * column:int
exception RuneParseError of r:Rune * index:int * line:int * column:int
exception StringParseError of s:string * index:int * line:int * column:int
exception NamedParserError of name:string * description:string * error:exn

