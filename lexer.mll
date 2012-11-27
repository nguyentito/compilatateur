{
  open Parser

  let digit_of_char c =
    if '0' <= c && c <= '9'
    then int_of_char c - int_of_char '0'
    else int_of_char (Char.lowercase c) - int_of_char 'a' + 10
    
  let int32_digit_of_char c = Int32.of_int (digit_of_char c)

  let read_character s =
    let f = function
      | "\\\\" -> '\\' | "\\'" -> '\'' | "\\\"" -> '"'
      | "\\n"  -> '\n' | "\\t" -> '\t' | "\\r"  -> '\r'
      | s when String.length s = 4 && s.[0] = '\\' && s.[1] = 'x' ->
          char_of_int (16 * digit_of_char s.[2] + digit_of_char s.[3])
      | s -> s.[0]
    in IntV (Int32.of_int (int_of_char (f s)))
}

let digit = ['0' - '9']
let digit_hex = ['0' - '9']|['A' - 'F']|['a' - 'f']
let digit_oct = ['0' - '7']
let alpha = ['a' -'z']|['A' - 'Z']
let ident = (alpha | '_') (alpha | digit | '_')*
let character = ([' ' - '\127'] # [ '\\' '\'' '"']) 
               | "\\\\" | "\\'"| "\\\"" | "\\n" | "\\t" | "\\r"
               | "\\x" digit_hex digit_hex
let string = '"' character* '"'

rule get_token = parse
  (* This will allow correct handling of line numbers for error messages *)
  | '\n' { Lexing.new_line lexbuf; get_token lexbuf }
      
  (* Junk elimination *)
  | ['\005' - '\032'] { get_token lexbuf }
  | "//" { comment_oneliner lexbuf }
  | "/*" { comment_c89 lexbuf }

  | "char" {Char}  | "else" {Else}  | "for" {For}  | "if" {If} | "int" {Int}
  | "return" {Return}  | "sizeof" {Sizeof}  | "struct" {Struct}
  | "union" {Union}  | "void" {Void} | "while" {While}

  | ident as a {Ident a}
  | string as s {StringV s}

  | "0x" (digit_hex+ as s) { read_base 16 Int32.zero (Lexing.from_string s) }
  | ('0' digit_oct*) as s  { read_base 8  Int32.zero (Lexing.from_string s) }
  | digit+           as s  { read_base 10 Int32.zero (Lexing.from_string s) }
  | "'" character "'" as s { read_character s }

  | '(' {LParen}  | ')' {RParen}
  | '[' {LBracket}  | ']' {RBracket}
  | '{' {LCurly}  | '}' {RCurly}

  | ',' {Comma} | ';' {Semicolon} | ':' {Colon}

  | "->" {Arrow}  | '.' {Dot}  | '=' {Assign}  | '&' {Address}
  | "==" {Equal}  | "!=" {Different}  | "||" {Or}  | "&&" {And} | '!' {Not} 
  | '<' {Less}  | "<=" {LessEq}  | '>' {Greater}  | ">=" {GreaterEq}
  | '+' {Plus}   | '-' {Minus}   | '*' {Star}  | '/' {Divide} | '%' {Modulo}  
  | "++" {Increment} | "--" {Decrement}
  | eof {Eof}

and comment_oneliner = parse
  | '\n' { Lexing.new_line lexbuf; get_token lexbuf }
  | eof {Eof}
  | [^ '\n'] { comment_oneliner lexbuf }
    
and comment_c89 = parse
  | "*/" { get_token lexbuf }
  | '\n' { Lexing.new_line lexbuf; comment_c89 lexbuf }
  | _ { comment_c89 lexbuf }

and read_base b acc = parse
  | digit_hex as d { read_base b (Int32.add (int32_digit_of_char d)
                                            (Int32.mul (Int32.of_int b) acc))
                               lexbuf }
  | eof { IntV acc } (* Useless... *)
  | "" { IntV acc }

{

}
