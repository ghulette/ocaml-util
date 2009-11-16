open Printf

let parse line = 
  try 
    let parser = Parser.main Lexer.tokenizer in
    match parser (Lexing.from_string line) with
      | Some expr -> 
        let result = Little.eval expr (Little.empty ()) in
        printf "Input: %s\n" (Little.string_of_expr expr);
        printf "Result: %s\n" (Little.string_of_expr result)
      | None -> printf "Nothing\n"
  with 
    | Lexer.Illegal_char t -> 
      printf "Illegal character %c\n" t
    | Parsing.Parse_error -> 
      printf "Parse error\n"
    | Failure x ->
      printf "Failure\n"

let main () =
  parse "(\\x -> \\y -> x y) (\\z -> z) (\\k -> k)"
  
;;
main ()
