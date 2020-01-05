type token =
  | CODE of (Coqpp_ast.code)
  | COMMENT of (string)
  | IDENT of (string)
  | QUALID of (string)
  | STRING of (string)
  | INT of (int)
  | VERNAC
  | TACTIC
  | GRAMMAR
  | EXTEND
  | END
  | DECLARE
  | PLUGIN
  | DEPRECATED
  | ARGUMENT
  | RAW_PRINTED
  | GLOB_PRINTED
  | COMMAND
  | CLASSIFIED
  | PRINTED
  | TYPED
  | INTERPRETED
  | GLOBALIZED
  | SUBSTITUTED
  | BY
  | AS
  | BANGBRACKET
  | HASHBRACKET
  | LBRACKET
  | RBRACKET
  | PIPE
  | ARROW
  | FUN
  | COMMA
  | EQUAL
  | STAR
  | LPAREN
  | RPAREN
  | COLON
  | SEMICOLON
  | GLOBAL
  | FIRST
  | LAST
  | BEFORE
  | AFTER
  | LEVEL
  | LEFTA
  | RIGHTA
  | NONA
  | EOF

val file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Coqpp_ast.t
