namespace PureParse

open System.Text

exception ParseError of message:string * line:int * column:int
exception CharParseError of c:char * line:int * column:int
exception RuneParseError of r:Rune * line:int * column:int
exception StringParseError of s:string * line:int * column:int
exception NoWhiteSpaceParseError of line:int * column:int

