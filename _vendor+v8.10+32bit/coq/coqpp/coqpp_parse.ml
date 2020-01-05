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

open Parsing;;
let _ = parse_error;;
# 10 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"

open Coqpp_ast

let starts s pat =
  let len = String.length s in
  let patlen = String.length pat in
  if patlen <= len && String.sub s 0 patlen = pat then
    Some (String.sub s patlen (len - patlen))
  else None

let ends s pat =
  let len = String.length s in
  let patlen = String.length pat in
  if patlen <= len && String.sub s (len - patlen) patlen = pat then
    Some (String.sub s 0 (len - patlen))
  else None

let between s pat1 pat2 = match starts s pat1 with
| None -> None
| Some s -> ends s pat2  

let without_sep k sep r =
  if sep <> "" then raise Parsing.Parse_error else k r

let parse_user_entry s sep =
  let table = [
    "ne_", "_list", without_sep (fun r -> Ulist1 r);
    "ne_", "_list_sep", (fun sep r -> Ulist1sep (r, sep));
    "", "_list", without_sep (fun r -> Ulist0 r);
    "", "_list_sep", (fun sep r -> Ulist0sep (r, sep));
    "", "_opt", without_sep (fun r -> Uopt r);
  ] in
  let rec parse s sep = function
  | [] ->
    let () = without_sep ignore sep () in
    begin match starts s "tactic" with
    | Some ("0"|"1"|"2"|"3"|"4"|"5" as s) -> Uentryl ("tactic", int_of_string s)
    | Some _ | None -> Uentry s
    end
  | (pat1, pat2, k) :: rem ->
    match between s pat1 pat2 with
    | None -> parse s sep rem
    | Some s ->
      let r = parse s "" table in
      k sep r      
  in
  parse s sep table

# 105 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
let yytransl_const = [|
  263 (* VERNAC *);
  264 (* TACTIC *);
  265 (* GRAMMAR *);
  266 (* EXTEND *);
  267 (* END *);
  268 (* DECLARE *);
  269 (* PLUGIN *);
  270 (* DEPRECATED *);
  271 (* ARGUMENT *);
  272 (* RAW_PRINTED *);
  273 (* GLOB_PRINTED *);
  274 (* COMMAND *);
  275 (* CLASSIFIED *);
  276 (* PRINTED *);
  277 (* TYPED *);
  278 (* INTERPRETED *);
  279 (* GLOBALIZED *);
  280 (* SUBSTITUTED *);
  281 (* BY *);
  282 (* AS *);
  283 (* BANGBRACKET *);
  284 (* HASHBRACKET *);
  285 (* LBRACKET *);
  286 (* RBRACKET *);
  287 (* PIPE *);
  288 (* ARROW *);
  289 (* FUN *);
  290 (* COMMA *);
  291 (* EQUAL *);
  292 (* STAR *);
  293 (* LPAREN *);
  294 (* RPAREN *);
  295 (* COLON *);
  296 (* SEMICOLON *);
  297 (* GLOBAL *);
  298 (* FIRST *);
  299 (* LAST *);
  300 (* BEFORE *);
  301 (* AFTER *);
  302 (* LEVEL *);
  303 (* LEFTA *);
  304 (* RIGHTA *);
  305 (* NONA *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* CODE *);
  258 (* COMMENT *);
  259 (* IDENT *);
  260 (* QUALID *);
  261 (* STRING *);
  262 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\004\000\005\000\008\000\008\000\013\000\013\000\
\017\000\017\000\018\000\018\000\014\000\014\000\015\000\015\000\
\016\000\016\000\012\000\012\000\020\000\020\000\020\000\006\000\
\021\000\021\000\022\000\022\000\022\000\023\000\023\000\024\000\
\025\000\025\000\030\000\030\000\030\000\031\000\031\000\026\000\
\026\000\032\000\028\000\028\000\029\000\029\000\007\000\033\000\
\033\000\034\000\034\000\019\000\019\000\035\000\027\000\027\000\
\036\000\036\000\036\000\036\000\009\000\009\000\010\000\010\000\
\037\000\037\000\011\000\011\000\038\000\039\000\039\000\041\000\
\041\000\041\000\041\000\041\000\042\000\042\000\043\000\043\000\
\044\000\044\000\044\000\040\000\040\000\045\000\046\000\046\000\
\047\000\047\000\048\000\049\000\049\000\050\000\050\000\051\000\
\051\000\053\000\053\000\053\000\053\000\053\000\052\000\052\000\
\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\003\000\006\000\012\000\007\000\000\000\003\000\
\000\000\003\000\000\000\003\000\000\000\003\000\000\000\003\000\
\000\000\003\000\000\000\003\000\001\000\002\000\005\000\007\000\
\001\000\001\000\000\000\003\000\003\000\001\000\002\000\010\000\
\000\000\003\000\001\000\002\000\003\000\003\000\001\000\000\000\
\003\000\001\000\000\000\001\000\000\000\002\000\007\000\000\000\
\002\000\000\000\002\000\001\000\002\000\006\000\000\000\002\000\
\001\000\001\000\004\000\006\000\001\000\001\000\000\000\004\000\
\000\000\002\000\000\000\002\000\007\000\000\000\001\000\001\000\
\001\000\002\000\002\000\002\000\000\000\001\000\000\000\001\000\
\001\000\001\000\001\000\001\000\003\000\005\000\000\000\001\000\
\001\000\003\000\003\000\000\000\001\000\001\000\003\000\003\000\
\001\000\001\000\003\000\003\000\003\000\001\000\001\000\002\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\004\000\005\000\000\000\000\000\000\000\000\000\
\000\000\113\000\000\000\000\000\006\000\007\000\008\000\009\000\
\010\000\034\000\000\000\033\000\000\000\000\000\000\000\000\000\
\000\000\001\000\003\000\000\000\000\000\000\000\070\000\069\000\
\000\000\011\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\057\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\059\000\000\000\000\000\000\000\000\000\012\000\076\000\
\029\000\000\000\000\000\000\000\000\000\016\000\000\000\014\000\
\061\000\036\000\037\000\000\000\000\000\032\000\039\000\055\000\
\074\000\072\000\080\000\081\000\000\000\000\000\000\000\000\000\
\079\000\000\000\030\000\000\000\000\000\000\000\000\000\065\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\082\000\
\083\000\084\000\000\000\000\000\022\000\000\000\000\000\000\000\
\000\000\000\000\064\000\000\000\042\000\000\000\050\000\000\000\
\000\000\086\000\000\000\000\000\000\000\000\000\024\000\000\000\
\000\000\000\000\000\000\000\000\046\000\045\000\049\000\000\000\
\000\000\089\000\090\000\091\000\000\000\088\000\000\000\031\000\
\026\000\000\000\000\000\000\000\000\000\067\000\062\000\000\000\
\077\000\000\000\093\000\018\000\000\000\000\000\000\000\052\000\
\000\000\000\000\110\000\000\000\000\000\000\000\000\000\096\000\
\000\000\000\000\101\000\000\000\105\000\000\000\020\000\013\000\
\068\000\000\000\000\000\000\000\000\000\000\000\000\000\094\000\
\000\000\000\000\000\000\112\000\054\000\000\000\104\000\109\000\
\108\000\107\000\098\000\099\000\103\000\040\000"

let yydgoto = "\002\000\
\010\000\011\000\012\000\013\000\014\000\015\000\016\000\017\000\
\174\000\041\000\053\000\043\000\045\000\077\000\102\000\120\000\
\138\000\156\000\059\000\075\000\021\000\047\000\064\000\065\000\
\085\000\111\000\105\000\169\000\187\000\108\000\109\000\128\000\
\039\000\050\000\060\000\106\000\069\000\054\000\096\000\131\000\
\097\000\132\000\149\000\150\000\133\000\175\000\176\000\177\000\
\178\000\179\000\180\000\181\000\182\000"

let yysindex = "\008\000\
\120\255\000\000\000\000\000\000\059\255\055\255\076\255\079\255\
\110\255\000\000\018\000\120\255\000\000\000\000\000\000\000\000\
\000\000\000\000\113\255\000\000\115\255\018\255\092\255\052\255\
\104\255\000\000\000\000\128\255\130\255\123\255\000\000\000\000\
\109\255\000\000\131\255\114\255\134\255\153\255\111\255\116\255\
\092\255\132\255\114\255\135\255\125\255\088\255\133\255\000\000\
\155\255\125\255\092\255\124\255\148\255\092\255\005\255\140\255\
\164\255\137\255\156\255\125\255\167\255\166\255\142\255\160\255\
\133\255\000\000\161\255\092\255\136\255\097\255\000\000\000\000\
\000\000\005\255\170\255\149\255\152\255\000\000\100\255\000\000\
\000\000\000\000\000\000\092\255\150\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\173\255\174\255\175\255\154\255\
\000\000\008\255\000\000\180\255\159\255\158\255\151\255\000\000\
\162\255\100\255\163\255\165\255\145\255\092\255\168\255\000\000\
\000\000\000\000\181\255\005\255\000\000\186\255\169\255\177\255\
\188\255\171\255\000\000\092\255\000\000\092\255\000\000\172\255\
\100\255\000\000\176\255\099\255\178\255\003\255\000\000\189\255\
\179\255\182\255\047\255\195\255\000\000\000\000\000\000\183\255\
\184\255\000\000\000\000\000\000\185\255\000\000\181\255\000\000\
\000\000\199\255\190\255\125\255\196\255\000\000\000\000\191\255\
\000\000\021\255\000\000\000\000\207\255\200\255\192\255\000\000\
\193\255\194\255\000\000\021\255\030\255\143\255\197\255\000\000\
\201\255\202\255\000\000\198\255\000\000\030\255\000\000\000\000\
\000\000\209\255\203\255\030\255\210\255\204\255\211\255\000\000\
\021\255\216\255\021\255\000\000\000\000\217\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\207\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\207\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\032\255\000\000\000\000\
\065\255\000\000\067\255\205\255\208\255\000\000\125\255\000\000\
\212\255\000\000\093\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\213\255\000\000\000\000\212\255\000\000\095\255\
\000\000\000\000\000\000\214\255\000\000\000\000\053\255\000\000\
\220\255\000\000\000\000\213\255\000\000\215\255\000\000\000\000\
\000\000\000\000\077\255\000\000\252\254\000\000\218\255\000\000\
\000\000\000\000\000\000\000\000\221\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\006\255\049\255\000\000\
\000\000\218\255\026\255\000\000\219\255\000\000\000\000\000\000\
\000\000\000\000\024\255\000\000\000\000\000\000\000\000\044\255\
\000\000\000\000\000\000\000\000\000\000\222\255\000\000\000\000\
\218\255\000\000\000\000\225\255\226\255\000\000\000\000\000\000\
\000\000\224\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\024\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\112\255\
\000\000\119\255\000\000\000\000\000\000\000\000\000\000\000\000\
\227\255\255\254\000\000\228\255\000\000\011\255\000\000\000\000\
\231\255\000\000\000\000\230\255\000\000\098\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\228\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\200\000\000\000\000\000\000\000\000\000\000\000\000\000\
\234\255\000\000\165\000\000\000\177\000\000\000\000\000\000\000\
\000\000\000\000\206\255\187\255\000\000\000\000\156\000\000\000\
\000\000\000\000\157\255\000\000\000\000\096\000\000\000\000\000\
\000\000\000\000\000\000\000\000\160\000\000\000\000\000\082\000\
\000\000\000\000\000\000\000\000\000\000\000\000\101\255\000\000\
\000\000\042\000\000\000\138\255\000\000"

let yytablesize = 262
let yytable = "\067\000\
\033\000\070\000\070\000\070\000\098\000\099\000\123\000\073\000\
\001\000\081\000\099\000\023\000\023\000\106\000\106\000\106\000\
\189\000\026\000\052\000\023\000\030\000\025\000\025\000\170\000\
\032\000\171\000\023\000\070\000\068\000\144\000\070\000\052\000\
\031\000\032\000\171\000\070\000\025\000\203\000\070\000\106\000\
\152\000\074\000\106\000\116\000\070\000\068\000\134\000\106\000\
\106\000\172\000\106\000\066\000\085\000\066\000\190\000\047\000\
\034\000\173\000\172\000\018\000\017\000\107\000\056\000\196\000\
\022\000\047\000\173\000\071\000\071\000\199\000\085\000\085\000\
\085\000\019\000\017\000\071\000\020\000\056\000\066\000\041\000\
\157\000\041\000\027\000\027\000\158\000\023\000\027\000\127\000\
\027\000\027\000\027\000\024\000\028\000\028\000\031\000\032\000\
\028\000\027\000\028\000\028\000\028\000\141\000\103\000\107\000\
\104\000\166\000\035\000\028\000\015\000\015\000\021\000\021\000\
\061\000\062\000\015\000\015\000\015\000\021\000\021\000\025\000\
\003\000\004\000\028\000\015\000\029\000\021\000\005\000\006\000\
\007\000\111\000\036\000\008\000\037\000\044\000\009\000\111\000\
\038\000\111\000\091\000\092\000\093\000\094\000\095\000\051\000\
\051\000\146\000\147\000\148\000\095\000\040\000\100\000\042\000\
\046\000\048\000\051\000\058\000\049\000\055\000\071\000\057\000\
\066\000\076\000\070\000\063\000\078\000\079\000\080\000\082\000\
\083\000\084\000\086\000\088\000\099\000\100\000\101\000\090\000\
\110\000\112\000\113\000\114\000\117\000\119\000\115\000\118\000\
\126\000\130\000\135\000\121\000\191\000\153\000\139\000\122\000\
\137\000\136\000\125\000\159\000\129\000\124\000\155\000\164\000\
\167\000\143\000\140\000\154\000\168\000\145\000\002\000\183\000\
\151\000\197\000\184\000\027\000\160\000\162\000\165\000\202\000\
\204\000\206\000\072\000\056\000\087\000\142\000\075\000\161\000\
\060\000\186\000\192\000\089\000\188\000\185\000\038\000\193\000\
\163\000\194\000\198\000\015\000\205\000\195\000\035\000\200\000\
\000\000\201\000\000\000\078\000\000\000\000\000\000\000\063\000\
\043\000\048\000\000\000\044\000\073\000\087\000\019\000\092\000\
\000\000\000\000\053\000\100\000\097\000\102\000"

let yycheck = "\050\000\
\023\000\003\001\004\001\005\001\074\000\003\001\106\000\003\001\
\001\000\060\000\003\001\016\001\017\001\003\001\004\001\005\001\
\172\000\000\000\041\000\024\001\003\001\016\001\017\001\003\001\
\004\001\005\001\031\001\029\001\051\000\129\000\032\001\054\000\
\003\001\004\001\005\001\037\001\031\001\193\000\040\001\029\001\
\038\001\037\001\032\001\036\001\046\001\068\000\116\000\037\001\
\038\001\029\001\040\001\003\001\029\001\005\001\173\000\030\001\
\005\001\037\001\029\001\001\001\017\001\084\000\031\001\182\000\
\010\001\040\001\037\001\003\001\004\001\188\000\047\001\048\001\
\049\001\015\001\031\001\011\001\018\001\046\001\030\001\027\001\
\034\001\029\001\016\001\017\001\038\001\010\001\020\001\110\000\
\022\001\023\001\024\001\013\001\016\001\017\001\003\001\004\001\
\020\001\031\001\022\001\023\001\024\001\124\000\003\001\126\000\
\005\001\156\000\003\001\031\001\016\001\017\001\016\001\017\001\
\025\001\026\001\022\001\023\001\024\001\023\001\024\001\010\001\
\001\001\002\001\010\001\031\001\010\001\031\001\007\001\008\001\
\009\001\032\001\003\001\012\001\003\001\020\001\015\001\038\001\
\014\001\040\001\042\001\043\001\044\001\045\001\046\001\032\001\
\033\001\047\001\048\001\049\001\030\001\041\001\032\001\021\001\
\019\001\001\001\039\001\031\001\046\001\026\001\011\001\025\001\
\006\001\022\001\039\001\031\001\001\001\029\001\011\001\001\001\
\003\001\028\001\011\001\011\001\003\001\025\001\023\001\040\001\
\027\001\005\001\005\001\005\001\001\001\024\001\029\001\025\001\
\040\001\005\001\001\001\037\001\046\001\001\001\003\001\030\001\
\016\001\025\001\030\001\001\001\029\001\035\001\017\001\001\001\
\005\001\030\001\032\001\025\001\014\001\030\001\000\000\001\001\
\031\001\001\001\011\001\012\000\030\001\029\001\025\001\005\001\
\001\001\001\001\054\000\043\000\065\000\126\000\011\001\040\001\
\011\001\033\001\030\001\068\000\035\001\038\001\011\001\031\001\
\151\000\032\001\032\001\031\001\195\000\040\001\031\001\030\001\
\255\255\038\001\255\255\029\001\255\255\255\255\255\255\030\001\
\030\001\029\001\255\255\030\001\040\001\029\001\031\001\030\001\
\255\255\255\255\032\001\032\001\030\001\032\001"

let yynames_const = "\
  VERNAC\000\
  TACTIC\000\
  GRAMMAR\000\
  EXTEND\000\
  END\000\
  DECLARE\000\
  PLUGIN\000\
  DEPRECATED\000\
  ARGUMENT\000\
  RAW_PRINTED\000\
  GLOB_PRINTED\000\
  COMMAND\000\
  CLASSIFIED\000\
  PRINTED\000\
  TYPED\000\
  INTERPRETED\000\
  GLOBALIZED\000\
  SUBSTITUTED\000\
  BY\000\
  AS\000\
  BANGBRACKET\000\
  HASHBRACKET\000\
  LBRACKET\000\
  RBRACKET\000\
  PIPE\000\
  ARROW\000\
  FUN\000\
  COMMA\000\
  EQUAL\000\
  STAR\000\
  LPAREN\000\
  RPAREN\000\
  COLON\000\
  SEMICOLON\000\
  GLOBAL\000\
  FIRST\000\
  LAST\000\
  BEFORE\000\
  AFTER\000\
  LEVEL\000\
  LEFTA\000\
  RIGHTA\000\
  NONA\000\
  EOF\000\
  "

let yynames_block = "\
  CODE\000\
  COMMENT\000\
  IDENT\000\
  QUALID\000\
  STRING\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'nodes) in
    Obj.repr(
# 80 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( _1 )
# 432 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : Coqpp_ast.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( [] )
# 438 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'nodes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'node) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'nodes) in
    Obj.repr(
# 87 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( _1 :: _2 )
# 446 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'nodes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Coqpp_ast.code) in
    Obj.repr(
# 91 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
       ( Code _1 )
# 453 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 92 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
          ( Comment _1 )
# 460 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'declare_plugin) in
    Obj.repr(
# 93 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                 ( _1 )
# 467 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'grammar_extend) in
    Obj.repr(
# 94 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                 ( _1 )
# 474 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vernac_extend) in
    Obj.repr(
# 95 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                ( _1 )
# 481 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tactic_extend) in
    Obj.repr(
# 96 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                ( _1 )
# 488 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'argument_extend) in
    Obj.repr(
# 97 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                  ( _1 )
# 495 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 101 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                        ( DeclarePlugin _3 )
# 502 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'declare_plugin))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'qualid_or_ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'globals) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'gram_entries) in
    Obj.repr(
# 106 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( GramExt { gramext_name = _3; gramext_globals = _4; gramext_entries = _5 } )
# 511 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'grammar_extend))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 8 : 'typed_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 7 : 'printed_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 6 : 'interpreted_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 5 : 'globalized_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 4 : 'substituted_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 3 : 'raw_printed_opt) in
    let _10 = (Parsing.peek_val __caml_parser_env 2 : 'glob_printed_opt) in
    let _11 = (Parsing.peek_val __caml_parser_env 1 : 'tactic_rules) in
    Obj.repr(
# 120 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( ArgumentExt {
    argext_name = _3;
    argext_rules = _11;
    argext_rprinter = _9;
    argext_gprinter = _10;
    argext_tprinter = _5;
    argext_interp = _6;
    argext_glob = _7;
    argext_subst = _8;
    argext_type = _4;
  } )
# 536 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'argument_extend))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'printed_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'tactic_rules) in
    Obj.repr(
# 132 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( VernacArgumentExt {
    vernacargext_name = _4;
    vernacargext_printer = _5;
    vernacargext_rules = _6;
  } )
# 549 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'argument_extend))
; (fun __caml_parser_env ->
    Obj.repr(
# 140 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( None )
# 555 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'printed_opt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Coqpp_ast.code) in
    Obj.repr(
# 141 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                  ( Some _3 )
# 562 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'printed_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 145 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( None )
# 568 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'raw_printed_opt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Coqpp_ast.code) in
    Obj.repr(
# 146 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                      ( Some _3 )
# 575 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'raw_printed_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 150 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( None )
# 581 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'glob_printed_opt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Coqpp_ast.code) in
    Obj.repr(
# 151 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                       ( Some _3 )
# 588 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'glob_printed_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 155 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( None )
# 594 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'interpreted_opt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Coqpp_ast.code) in
    Obj.repr(
# 156 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                      ( Some _3 )
# 601 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'interpreted_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 160 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( None )
# 607 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'globalized_opt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Coqpp_ast.code) in
    Obj.repr(
# 161 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                     ( Some _3 )
# 614 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'globalized_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 165 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( None )
# 620 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'substituted_opt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Coqpp_ast.code) in
    Obj.repr(
# 166 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                      ( Some _3 )
# 627 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'substituted_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 170 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( None )
# 633 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'typed_opt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'argtype) in
    Obj.repr(
# 171 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                   ( Some _3 )
# 640 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'typed_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 175 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
        ( ExtraArgType _1 )
# 647 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'argtype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'argtype) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 176 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                (
    match _2 with
    | "list" -> ListArgType _1
    | "option" ->  OptArgType _1
    | _ -> raise Parsing.Parse_error
  )
# 660 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'argtype))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'argtype) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'argtype) in
    Obj.repr(
# 182 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                                     ( PairArgType (_2, _4) )
# 668 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'argtype))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'vernac_entry) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'vernac_classifier) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'vernac_rules) in
    Obj.repr(
# 187 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( VernacExt {
    vernacext_name = _4;
    vernacext_entry = _2;
    vernacext_class = _5;
    vernacext_rules = _6;
  } )
# 683 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'vernac_extend))
; (fun __caml_parser_env ->
    Obj.repr(
# 196 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
          ( None )
# 689 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'vernac_entry))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Coqpp_ast.code) in
    Obj.repr(
# 197 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
       ( Some _1 )
# 696 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'vernac_entry))
; (fun __caml_parser_env ->
    Obj.repr(
# 201 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( ClassifDefault )
# 702 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'vernac_classifier))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Coqpp_ast.code) in
    Obj.repr(
# 202 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                     ( ClassifCode _3 )
# 709 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'vernac_classifier))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 203 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                      ( ClassifName _3 )
# 716 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'vernac_classifier))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vernac_rule) in
    Obj.repr(
# 207 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
              ( [_1] )
# 723 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'vernac_rules))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vernac_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vernac_rules) in
    Obj.repr(
# 208 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                           ( _1 :: _2 )
# 731 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'vernac_rules))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : 'vernac_attributes_opt) in
    let _3 = (Parsing.peek_val __caml_parser_env 7 : 'vernac_state_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'ext_tokens) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : 'rule_deprecation) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'rule_classifier) in
    let _10 = (Parsing.peek_val __caml_parser_env 0 : Coqpp_ast.code) in
    Obj.repr(
# 213 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( {
      vernac_atts = _2;
      vernac_state= _3;
      vernac_toks = _5;
      vernac_depr = _7;
      vernac_class= _8;
      vernac_body = _10;
  } )
# 750 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'vernac_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 224 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( None )
# 756 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'vernac_attributes_opt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vernac_attributes) in
    Obj.repr(
# 225 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                                         ( Some _2 )
# 763 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'vernac_attributes_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vernac_attribute) in
    Obj.repr(
# 229 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                   ( [_1] )
# 770 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'vernac_attributes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vernac_attribute) in
    Obj.repr(
# 230 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                             ( [_1] )
# 777 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'vernac_attributes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vernac_attribute) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vernac_attributes) in
    Obj.repr(
# 231 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                                               ( _1 :: _3 )
# 785 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'vernac_attributes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'qualid_or_ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'qualid_or_ident) in
    Obj.repr(
# 235 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                                        ( (_1, _3) )
# 793 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'vernac_attribute))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'qualid_or_ident) in
    Obj.repr(
# 236 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                  ( (_1, _1) )
# 800 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'vernac_attribute))
; (fun __caml_parser_env ->
    Obj.repr(
# 240 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( None )
# 806 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'vernac_state_opt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'vernac_state) in
    Obj.repr(
# 241 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                                    ( Some _2 )
# 813 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'vernac_state_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'qualid_or_ident) in
    Obj.repr(
# 245 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                  ( _1 )
# 820 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'vernac_state))
; (fun __caml_parser_env ->
    Obj.repr(
# 248 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( false )
# 826 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'rule_deprecation))
; (fun __caml_parser_env ->
    Obj.repr(
# 249 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
             ( true )
# 832 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'rule_deprecation))
; (fun __caml_parser_env ->
    Obj.repr(
# 253 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( None )
# 838 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'rule_classifier))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Coqpp_ast.code) in
    Obj.repr(
# 254 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
           ( Some _2 )
# 845 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'rule_classifier))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'tactic_deprecated) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'tactic_level) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'tactic_rules) in
    Obj.repr(
# 259 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( TacticExt { tacext_name = _3; tacext_deprecated = _4; tacext_level = _5; tacext_rules = _6 } )
# 855 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'tactic_extend))
; (fun __caml_parser_env ->
    Obj.repr(
# 263 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( None )
# 861 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'tactic_deprecated))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Coqpp_ast.code) in
    Obj.repr(
# 264 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                  ( Some _2 )
# 868 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'tactic_deprecated))
; (fun __caml_parser_env ->
    Obj.repr(
# 268 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( None )
# 874 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'tactic_level))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 269 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
            ( Some _2 )
# 881 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'tactic_level))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tactic_rule) in
    Obj.repr(
# 273 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
              ( [_1] )
# 888 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'tactic_rules))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'tactic_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'tactic_rules) in
    Obj.repr(
# 274 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                           ( _1 :: _2 )
# 896 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'tactic_rules))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'ext_tokens) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Coqpp_ast.code) in
    Obj.repr(
# 279 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( { tac_toks = _3; tac_body = _6 } )
# 904 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'tactic_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 283 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( [] )
# 910 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'ext_tokens))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ext_token) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ext_tokens) in
    Obj.repr(
# 284 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                       ( _1 :: _2 )
# 918 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'ext_tokens))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 288 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
         ( ExtTerminal _1 )
# 925 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'ext_token))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 289 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
        (
  let e = parse_user_entry _1 "" in
  ExtNonTerminal (e, TokNone) 
  )
# 935 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'ext_token))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 293 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                            (
  let e = parse_user_entry _1 "" in
  ExtNonTerminal (e, TokName _3)
  )
# 946 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'ext_token))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 297 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                                         (
  let e = parse_user_entry _1 _5 in
  ExtNonTerminal (e, TokName _3)
)
# 958 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'ext_token))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 304 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
         ( _1 )
# 965 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'qualid_or_ident))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 305 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
        ( _1 )
# 972 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'qualid_or_ident))
; (fun __caml_parser_env ->
    Obj.repr(
# 309 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( [] )
# 978 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'globals))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'idents) in
    Obj.repr(
# 310 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                                ( _3 )
# 985 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'globals))
; (fun __caml_parser_env ->
    Obj.repr(
# 314 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( [] )
# 991 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'idents))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'qualid_or_ident) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'idents) in
    Obj.repr(
# 315 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                         ( _1 :: _2 )
# 999 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'idents))
; (fun __caml_parser_env ->
    Obj.repr(
# 319 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( [] )
# 1005 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'gram_entries))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'gram_entry) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'gram_entries) in
    Obj.repr(
# 320 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                          ( _1 :: _2 )
# 1013 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'gram_entries))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'qualid_or_ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'position_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'levels) in
    Obj.repr(
# 325 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( { gentry_name = _1; gentry_pos = _3; gentry_rules = _5; } )
# 1022 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'gram_entry))
; (fun __caml_parser_env ->
    Obj.repr(
# 329 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( None )
# 1028 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'position_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'position) in
    Obj.repr(
# 330 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
           ( Some _1 )
# 1035 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'position_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 334 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
        ( First )
# 1041 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'position))
; (fun __caml_parser_env ->
    Obj.repr(
# 335 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
       ( Last )
# 1047 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'position))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 336 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                ( Before _2 )
# 1054 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'position))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 337 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
               ( After _2 )
# 1061 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'position))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 338 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
               ( Level _2 )
# 1068 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'position))
; (fun __caml_parser_env ->
    Obj.repr(
# 342 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( None )
# 1074 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'string_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 343 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
         ( Some _1 )
# 1081 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'string_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 347 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( None )
# 1087 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'assoc_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assoc) in
    Obj.repr(
# 348 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
        ( Some _1 )
# 1094 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'assoc_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 352 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
        ( LeftA )
# 1100 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'assoc))
; (fun __caml_parser_env ->
    Obj.repr(
# 353 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
         ( RightA )
# 1106 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'assoc))
; (fun __caml_parser_env ->
    Obj.repr(
# 354 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
       ( NonA )
# 1112 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'assoc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'level) in
    Obj.repr(
# 358 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
        ( [_1] )
# 1119 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'levels))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'level) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'levels) in
    Obj.repr(
# 359 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                    ( _1 :: _3 )
# 1127 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'levels))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'string_opt) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'assoc_opt) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'rules_opt) in
    Obj.repr(
# 364 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( { grule_label = _1; grule_assoc = _2; grule_prods = _4; } )
# 1136 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'level))
; (fun __caml_parser_env ->
    Obj.repr(
# 368 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( [] )
# 1142 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'rules_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rules) in
    Obj.repr(
# 369 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
        ( _1 )
# 1149 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'rules_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rule) in
    Obj.repr(
# 373 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
       ( [_1] )
# 1156 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'rules))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'rules) in
    Obj.repr(
# 374 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                  ( _1 :: _3 )
# 1164 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'rules))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'symbols_opt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Coqpp_ast.code) in
    Obj.repr(
# 379 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( { gprod_symbs = _1; gprod_body = _3; } )
# 1172 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 383 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
  ( [] )
# 1178 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'symbols_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'symbols) in
    Obj.repr(
# 384 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
          ( _1 )
# 1185 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'symbols_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'symbol) in
    Obj.repr(
# 388 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
         ( [_1] )
# 1192 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'symbols))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'symbol) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'symbols) in
    Obj.repr(
# 389 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                           ( _1 :: _3 )
# 1200 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'symbols))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'gram_tokens) in
    Obj.repr(
# 393 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                          ( (Some _1, _3) )
# 1208 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'gram_tokens) in
    Obj.repr(
# 394 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
              ( (None, _1) )
# 1215 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'qualid_or_ident) in
    Obj.repr(
# 398 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                  ( GSymbQualid (_1, None) )
# 1222 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'gram_token))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'qualid_or_ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 399 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                               ( GSymbQualid (_1, Some _3) )
# 1230 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'gram_token))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'gram_tokens) in
    Obj.repr(
# 400 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                            ( GSymbParen _2 )
# 1237 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'gram_token))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'rules) in
    Obj.repr(
# 401 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                          ( GSymbProd _2 )
# 1244 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'gram_token))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 402 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
         ( GSymbString _1 )
# 1251 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'gram_token))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'gram_token) in
    Obj.repr(
# 406 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
             ( [_1] )
# 1258 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'gram_tokens))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'gram_token) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'gram_tokens) in
    Obj.repr(
# 407 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.mly"
                         ( _1 :: _2 )
# 1266 "_vendor+v8.10+32bit/coq/coqpp/coqpp_parse.ml"
               : 'gram_tokens))
(* Entry file *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let file (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Coqpp_ast.t)
