{
open Parser

let line_count = ref 1
exception Illegal_char of char
}

let id = ['a'-'z' 'A'-'Z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']*

rule tokenizer = parse    
  | id as word { ID word }
  | '('        { BEGIN }
  | ')'        { END }
  | '\\'       { LAMBDA }
  | "->"       { ARROW }
  | [' ' '\t'] { tokenizer lexbuf }
  | ['\n']     { incr line_count; tokenizer lexbuf }
  | _ as c     { raise (Illegal_char c) }
  | eof        { EOF }
