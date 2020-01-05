# 17 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
      
open Printf
open Lexing
(*i*)

(*s Command-line options. *)

let spec_only = ref false
let proof_only = ref false
let percentage = ref false
let skip_header = ref true

(*s Counters. [clines] counts the number of code lines of the current
    file, and [dlines] the number of comment lines. [tclines] and [tdlines]
    are the corresponding totals. *)

let slines = ref 0
let plines = ref 0
let dlines = ref 0

let tslines = ref 0
let tplines = ref 0
let tdlines = ref 0

let update_totals () =
  tslines := !tslines + !slines;
  tplines := !tplines + !plines;
  tdlines := !tdlines + !dlines

(*s The following booleans indicate whether we have seen spec, proof or
    comment so far on the current line; when a newline is reached, [newline]
    is called and updates the counters accordingly. *)

let seen_spec = ref false
let seen_proof = ref false
let seen_comment = ref false

let newline () =
  if !seen_spec then incr slines;
  if !seen_proof then incr plines;
  if !seen_comment then incr dlines;
  seen_spec := false; seen_proof := false; seen_comment := false

let reset_counters () =
  seen_spec := false; seen_proof := false; seen_comment := false;
  slines := 0; plines := 0; dlines := 0

(*s Print results. *)

let print_line sl pl dl fo =
  if not !proof_only then printf " %8d" sl;
  if not !spec_only then printf " %8d" pl;
  if not (!proof_only || !spec_only) then printf " %8d" dl;
  (match fo with Some f -> printf " %s" f | _ -> ());
  if !percentage then begin
    let s = sl + pl + dl in
    let p = if s > 0 then 100 * dl / s else 0 in
    printf " (%d%%)" p
  end;
  print_newline ()

let print_file fo = print_line !slines !plines !dlines fo

let print_totals () = print_line !tslines !tplines !tdlines (Some "total")

(*i*)
# 69 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\248\255\249\255\002\000\000\000\000\000\001\000\000\000\
    \000\000\001\000\002\000\000\000\003\000\002\000\003\000\253\255\
    \254\255\000\000\001\000\003\000\252\255\006\000\002\000\000\000\
    \007\000\000\000\251\255\006\000\007\000\002\000\000\000\001\000\
    \001\000\006\000\008\000\016\000\012\000\007\000\250\255\015\000\
    \028\000\012\000\020\000\031\000\021\000\016\000\017\000\031\000\
    \034\000\018\000\025\000\035\000\022\000\023\000\031\000\035\000\
    \038\000\045\000\027\000\039\000\034\000\036\000\005\000\006\000\
    \099\000\037\000\042\000\014\000\016\000\061\000\052\000\056\000\
    \059\000\066\000\048\000\060\000\055\000\057\000\066\000\064\000\
    \060\000\066\000\056\000\068\000\063\000\065\000\061\000\061\000\
    \081\000\069\000\081\000\080\000\143\000\008\000\153\000\163\000\
    \215\000\249\255\250\255\144\000\017\000\056\000\253\255\254\255\
    \008\000\009\000\011\000\251\255\252\255\221\000\025\000\231\000\
    \241\000\034\001\248\255\249\255\145\000\213\000\220\000\011\000\
    \253\255\254\255\032\000\033\000\039\000\250\255\252\255\251\255\
    \045\001\038\000\055\001\065\001\114\001\247\255\248\255\147\000\
    \087\000\087\000\088\000\093\000\107\000\252\255\214\000\254\255\
    \041\000\043\000\045\000\253\255\120\000\121\000\132\000\037\001\
    \251\255\039\001\123\000\135\000\045\001\126\000\139\000\139\000\
    \138\000\147\000\133\000\161\000\045\000\249\255\163\000\162\000\
    \160\000\156\000\166\000\168\000\196\000\196\000\195\000\195\000\
    \210\000\200\000\201\000\217\000\220\000\118\001\050\000\135\001\
    \150\001\199\001\249\255\250\255\033\001\119\001\252\255\117\001\
    \254\255\048\000\050\000\045\001\253\255\251\255\201\001\054\000\
    \211\001\221\001\014\002\249\255\250\255\039\001\204\001\252\255\
    \253\255\055\000\023\001\025\001\047\001\251\255\254\255\018\002\
    \031\001\028\002\038\002\126\001\251\255\252\255\253\255\042\001\
    \255\255\254\255\089\002\251\255\252\255\210\001\254\255\040\001\
    \255\255\100\001\252\255\253\255\048\001\092\001\255\255\254\255\
    \125\001\253\255\254\255\255\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\003\000\255\255\
    \255\255\006\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \004\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\005\000\004\000\005\000\255\255\255\255\
    \005\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\006\000\005\000\006\000\006\000\
    \255\255\255\255\006\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\007\000\
    \007\000\007\000\007\000\007\000\007\000\255\255\002\000\255\255\
    \007\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\005\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\005\000\005\000\255\255\002\000\
    \255\255\005\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\005\000\004\000\255\255\
    \255\255\005\000\005\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\003\000\
    \255\255\255\255\255\255\255\255\255\255\002\000\255\255\003\000\
    \255\255\255\255\255\255\255\255\002\000\002\000\255\255\255\255\
    \255\255\255\255\255\255\255\255";
  Lexing.lex_default =
   "\002\000\000\000\000\000\093\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \000\000\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \098\000\000\000\000\000\110\000\255\255\255\255\000\000\000\000\
    \255\255\255\255\255\255\000\000\000\000\255\255\255\255\255\255\
    \255\255\115\000\000\000\000\000\129\000\255\255\255\255\255\255\
    \000\000\000\000\255\255\255\255\255\255\000\000\000\000\000\000\
    \255\255\255\255\255\255\255\255\134\000\000\000\000\000\182\000\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\000\000\
    \255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\164\000\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\187\000\000\000\000\000\199\000\255\255\000\000\255\255\
    \000\000\255\255\255\255\255\255\000\000\000\000\255\255\255\255\
    \255\255\255\255\204\000\000\000\000\000\216\000\255\255\000\000\
    \000\000\255\255\255\255\255\255\255\255\000\000\000\000\255\255\
    \255\255\255\255\255\255\221\000\000\000\000\000\000\000\255\255\
    \000\000\000\000\228\000\000\000\000\000\255\255\000\000\255\255\
    \000\000\235\000\000\000\000\000\255\255\255\255\000\000\000\000\
    \242\000\000\000\000\000\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\014\000\015\000\000\000\014\000\014\000\063\000\063\000\
    \014\000\000\000\063\000\063\000\000\000\000\000\000\000\068\000\
    \000\000\068\000\100\000\068\000\000\000\068\000\100\000\000\000\
    \014\000\000\000\016\000\014\000\000\000\063\000\063\000\003\000\
    \017\000\255\255\018\000\019\000\020\000\019\000\068\000\002\000\
    \068\000\100\000\105\000\106\000\107\000\106\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \098\000\108\000\108\000\008\000\005\000\108\000\011\000\009\000\
    \126\000\004\000\123\000\124\000\012\000\115\000\006\000\007\000\
    \125\000\124\000\010\000\145\000\013\000\146\000\147\000\146\000\
    \108\000\134\000\194\000\165\000\195\000\187\000\092\000\069\000\
    \214\000\031\000\054\000\026\000\032\000\078\000\065\000\039\000\
    \027\000\030\000\021\000\022\000\025\000\026\000\086\000\045\000\
    \043\000\023\000\024\000\028\000\029\000\026\000\034\000\035\000\
    \033\000\036\000\037\000\038\000\040\000\041\000\042\000\026\000\
    \044\000\026\000\046\000\047\000\048\000\049\000\050\000\051\000\
    \052\000\053\000\026\000\055\000\056\000\057\000\058\000\059\000\
    \060\000\061\000\062\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\066\000\067\000\070\000\
    \071\000\072\000\073\000\074\000\075\000\076\000\077\000\026\000\
    \079\000\080\000\081\000\082\000\083\000\084\000\085\000\038\000\
    \087\000\088\000\089\000\090\000\091\000\038\000\093\000\255\255\
    \255\255\173\000\255\255\172\000\167\000\166\000\162\000\094\000\
    \094\000\094\000\094\000\094\000\094\000\094\000\094\000\094\000\
    \094\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\093\000\093\000\093\000\093\000\093\000\
    \093\000\093\000\093\000\093\000\093\000\148\000\117\000\142\000\
    \100\000\102\000\117\000\142\000\100\000\127\000\127\000\149\000\
    \150\000\127\000\151\000\093\000\109\000\128\000\159\000\181\000\
    \157\000\093\000\158\000\152\000\160\000\117\000\142\000\100\000\
    \161\000\103\000\152\000\163\000\127\000\093\000\099\000\104\000\
    \001\000\093\000\255\255\093\000\110\000\101\000\164\000\164\000\
    \168\000\169\000\170\000\171\000\164\000\111\000\111\000\111\000\
    \111\000\111\000\111\000\111\000\111\000\111\000\111\000\112\000\
    \112\000\112\000\112\000\112\000\112\000\112\000\112\000\112\000\
    \112\000\110\000\110\000\110\000\110\000\110\000\110\000\110\000\
    \110\000\110\000\110\000\117\000\120\000\255\255\153\000\117\000\
    \156\000\176\000\153\000\174\000\156\000\175\000\156\000\164\000\
    \108\000\110\000\156\000\177\000\178\000\179\000\180\000\110\000\
    \164\000\211\000\117\000\212\000\121\000\153\000\204\000\156\000\
    \255\255\116\000\122\000\110\000\225\000\156\000\255\255\110\000\
    \118\000\110\000\232\000\152\000\129\000\152\000\196\000\195\000\
    \213\000\212\000\239\000\152\000\119\000\130\000\130\000\130\000\
    \130\000\130\000\130\000\130\000\130\000\130\000\130\000\131\000\
    \131\000\131\000\131\000\131\000\131\000\131\000\131\000\131\000\
    \131\000\129\000\129\000\129\000\129\000\129\000\129\000\129\000\
    \129\000\129\000\129\000\142\000\141\000\198\000\191\000\142\000\
    \197\000\197\000\191\000\215\000\197\000\238\000\225\000\243\000\
    \222\000\129\000\000\000\000\000\236\000\000\000\237\000\129\000\
    \255\255\255\255\142\000\255\255\143\000\191\000\000\000\197\000\
    \225\000\135\000\144\000\129\000\154\000\182\000\155\000\129\000\
    \224\000\129\000\154\000\000\000\155\000\000\000\183\000\183\000\
    \183\000\183\000\183\000\183\000\183\000\183\000\183\000\183\000\
    \000\000\000\000\000\000\136\000\000\000\000\000\137\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \184\000\000\000\140\000\138\000\000\000\139\000\182\000\182\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\182\000\
    \191\000\190\000\182\000\000\000\191\000\206\000\000\000\097\000\
    \182\000\206\000\223\000\229\000\127\000\000\000\000\000\229\000\
    \000\000\000\000\000\000\000\000\182\000\000\000\000\000\191\000\
    \182\000\192\000\182\000\000\000\206\000\000\000\188\000\193\000\
    \199\000\000\000\229\000\000\000\000\000\189\000\000\000\000\000\
    \000\000\200\000\200\000\200\000\200\000\200\000\200\000\200\000\
    \200\000\200\000\200\000\201\000\201\000\201\000\201\000\201\000\
    \201\000\201\000\201\000\201\000\201\000\199\000\199\000\199\000\
    \199\000\199\000\199\000\199\000\199\000\199\000\199\000\206\000\
    \207\000\000\000\000\000\206\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\114\000\000\000\000\000\199\000\000\000\255\255\
    \000\000\000\000\000\000\199\000\000\000\000\000\206\000\000\000\
    \208\000\000\000\000\000\000\000\000\000\205\000\210\000\199\000\
    \209\000\216\000\000\000\199\000\000\000\199\000\000\000\000\000\
    \000\000\000\000\217\000\217\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\218\000\218\000\218\000\218\000\
    \218\000\218\000\218\000\218\000\218\000\218\000\216\000\216\000\
    \216\000\216\000\216\000\216\000\216\000\216\000\216\000\216\000\
    \000\000\000\000\229\000\230\000\234\000\000\000\229\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\216\000\000\000\
    \000\000\000\000\133\000\000\000\216\000\000\000\000\000\197\000\
    \000\000\229\000\000\000\000\000\000\000\241\000\220\000\000\000\
    \216\000\231\000\000\000\000\000\216\000\000\000\216\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\186\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\203\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\227\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\014\000\000\000\062\000\063\000\
    \014\000\255\255\062\000\063\000\255\255\255\255\255\255\067\000\
    \255\255\068\000\100\000\067\000\255\255\068\000\100\000\255\255\
    \000\000\255\255\000\000\014\000\255\255\062\000\063\000\000\000\
    \000\000\003\000\017\000\018\000\019\000\019\000\067\000\093\000\
    \068\000\100\000\104\000\105\000\106\000\106\000\063\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \110\000\101\000\101\000\000\000\000\000\101\000\000\000\000\000\
    \119\000\000\000\122\000\123\000\000\000\129\000\000\000\000\000\
    \124\000\124\000\000\000\144\000\000\000\145\000\146\000\146\000\
    \101\000\182\000\193\000\164\000\194\000\199\000\003\000\068\000\
    \209\000\011\000\007\000\029\000\031\000\005\000\006\000\010\000\
    \012\000\011\000\013\000\021\000\024\000\025\000\004\000\008\000\
    \009\000\022\000\023\000\027\000\028\000\032\000\033\000\034\000\
    \030\000\035\000\036\000\037\000\039\000\040\000\041\000\042\000\
    \043\000\044\000\045\000\046\000\047\000\048\000\049\000\050\000\
    \051\000\052\000\053\000\054\000\055\000\056\000\057\000\058\000\
    \059\000\060\000\061\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\065\000\066\000\069\000\
    \070\000\071\000\072\000\073\000\074\000\075\000\076\000\077\000\
    \078\000\079\000\080\000\081\000\082\000\083\000\084\000\085\000\
    \086\000\087\000\088\000\089\000\090\000\091\000\092\000\099\000\
    \116\000\136\000\135\000\136\000\137\000\138\000\139\000\092\000\
    \092\000\092\000\092\000\092\000\092\000\092\000\092\000\092\000\
    \092\000\094\000\094\000\094\000\094\000\094\000\094\000\094\000\
    \094\000\094\000\094\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\140\000\117\000\142\000\
    \096\000\096\000\117\000\142\000\096\000\118\000\118\000\148\000\
    \149\000\118\000\150\000\092\000\099\000\116\000\154\000\135\000\
    \155\000\092\000\157\000\158\000\159\000\117\000\142\000\096\000\
    \160\000\096\000\161\000\162\000\118\000\092\000\096\000\096\000\
    \000\000\092\000\003\000\092\000\109\000\096\000\163\000\166\000\
    \167\000\168\000\169\000\170\000\171\000\109\000\109\000\109\000\
    \109\000\109\000\109\000\109\000\109\000\109\000\109\000\111\000\
    \111\000\111\000\111\000\111\000\111\000\111\000\111\000\111\000\
    \111\000\112\000\112\000\112\000\112\000\112\000\112\000\112\000\
    \112\000\112\000\112\000\113\000\113\000\164\000\151\000\113\000\
    \153\000\172\000\151\000\173\000\153\000\174\000\156\000\175\000\
    \101\000\109\000\156\000\176\000\177\000\178\000\179\000\109\000\
    \180\000\210\000\113\000\211\000\113\000\151\000\216\000\153\000\
    \188\000\113\000\113\000\109\000\223\000\156\000\205\000\109\000\
    \113\000\109\000\231\000\151\000\128\000\153\000\195\000\195\000\
    \212\000\212\000\236\000\156\000\113\000\128\000\128\000\128\000\
    \128\000\128\000\128\000\128\000\128\000\128\000\128\000\130\000\
    \130\000\130\000\130\000\130\000\130\000\130\000\130\000\130\000\
    \130\000\131\000\131\000\131\000\131\000\131\000\131\000\131\000\
    \131\000\131\000\131\000\132\000\132\000\188\000\191\000\132\000\
    \189\000\189\000\191\000\205\000\189\000\237\000\223\000\240\000\
    \219\000\128\000\255\255\255\255\233\000\255\255\233\000\128\000\
    \099\000\116\000\132\000\135\000\132\000\191\000\255\255\189\000\
    \223\000\132\000\132\000\128\000\153\000\181\000\153\000\128\000\
    \219\000\128\000\156\000\255\255\156\000\255\255\181\000\181\000\
    \181\000\181\000\181\000\181\000\181\000\181\000\181\000\181\000\
    \255\255\255\255\255\255\132\000\255\255\255\255\132\000\183\000\
    \183\000\183\000\183\000\183\000\183\000\183\000\183\000\183\000\
    \183\000\255\255\132\000\132\000\255\255\132\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \185\000\185\000\181\000\255\255\185\000\206\000\255\255\096\000\
    \181\000\206\000\219\000\229\000\118\000\255\255\255\255\229\000\
    \255\255\255\255\255\255\255\255\181\000\255\255\255\255\185\000\
    \181\000\185\000\181\000\255\255\206\000\255\255\185\000\185\000\
    \198\000\255\255\229\000\255\255\255\255\185\000\255\255\255\255\
    \255\255\198\000\198\000\198\000\198\000\198\000\198\000\198\000\
    \198\000\198\000\198\000\200\000\200\000\200\000\200\000\200\000\
    \200\000\200\000\200\000\200\000\200\000\201\000\201\000\201\000\
    \201\000\201\000\201\000\201\000\201\000\201\000\201\000\202\000\
    \202\000\255\255\255\255\202\000\255\255\255\255\255\255\255\255\
    \255\255\188\000\113\000\255\255\255\255\198\000\255\255\205\000\
    \255\255\255\255\255\255\198\000\255\255\255\255\202\000\255\255\
    \202\000\255\255\255\255\255\255\255\255\202\000\202\000\198\000\
    \202\000\215\000\255\255\198\000\255\255\198\000\255\255\255\255\
    \255\255\255\255\215\000\215\000\215\000\215\000\215\000\215\000\
    \215\000\215\000\215\000\215\000\217\000\217\000\217\000\217\000\
    \217\000\217\000\217\000\217\000\217\000\217\000\218\000\218\000\
    \218\000\218\000\218\000\218\000\218\000\218\000\218\000\218\000\
    \255\255\255\255\226\000\226\000\233\000\255\255\226\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\215\000\255\255\
    \255\255\255\255\132\000\255\255\215\000\255\255\255\255\189\000\
    \255\255\226\000\255\255\255\255\255\255\240\000\219\000\255\255\
    \215\000\226\000\255\255\255\255\215\000\255\255\215\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\185\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\202\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\226\000";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec spec lexbuf =
   __ocaml_lex_spec_rec lexbuf 0
and __ocaml_lex_spec_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 108 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( comment lexbuf; spec lexbuf )
# 406 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 1 ->
# 109 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( let n = string lexbuf in slines := !slines + n;
	     seen_spec := true; spec lexbuf )
# 412 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 2 ->
# 111 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( newline (); spec lexbuf )
# 417 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 3 ->
# 113 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( spec lexbuf )
# 422 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 4 ->
# 115 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( seen_spec := true; spec_to_dot lexbuf; proof lexbuf )
# 427 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 5 ->
# 117 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( seen_spec := true; definition lexbuf )
# 432 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 6 ->
# 119 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
    ( seen_spec := true; spec lexbuf )
# 437 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 7 ->
# 120 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( () )
# 442 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_spec_rec lexbuf __ocaml_lex_state

and spec_to_dot lexbuf =
   __ocaml_lex_spec_to_dot_rec lexbuf 96
and __ocaml_lex_spec_to_dot_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 125 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( comment lexbuf; spec_to_dot lexbuf )
# 454 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 1 ->
# 126 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( let n = string lexbuf in slines := !slines + n;
	     seen_spec := true; spec_to_dot lexbuf )
# 460 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 2 ->
# 128 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( newline (); spec_to_dot lexbuf )
# 465 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 3 ->
# 129 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( () )
# 470 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 4 ->
# 131 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( spec_to_dot lexbuf )
# 475 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 5 ->
# 133 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
    ( seen_spec := true; spec_to_dot lexbuf )
# 480 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 6 ->
# 134 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( () )
# 485 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_spec_to_dot_rec lexbuf __ocaml_lex_state

and definition lexbuf =
   __ocaml_lex_definition_rec lexbuf 113
and __ocaml_lex_definition_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 140 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( comment lexbuf; definition lexbuf )
# 497 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 1 ->
# 141 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( let n = string lexbuf in slines := !slines + n;
	     seen_spec := true; definition lexbuf )
# 503 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 2 ->
# 143 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( newline (); definition lexbuf )
# 508 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 3 ->
# 144 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( seen_spec := true; spec lexbuf )
# 513 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 4 ->
# 145 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( proof lexbuf )
# 518 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 5 ->
# 147 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( definition lexbuf )
# 523 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 6 ->
# 149 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
    ( seen_spec := true; definition lexbuf )
# 528 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 7 ->
# 150 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( () )
# 533 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_definition_rec lexbuf __ocaml_lex_state

and proof lexbuf =
   __ocaml_lex_proof_rec lexbuf 132
and __ocaml_lex_proof_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 155 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( comment lexbuf; proof lexbuf )
# 545 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 1 ->
# 156 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( let n = string lexbuf in plines := !plines + n;
	     seen_proof := true; proof lexbuf )
# 551 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 2 ->
# 159 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( proof lexbuf )
# 556 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 3 ->
# 160 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( newline (); proof lexbuf )
# 561 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 4 ->
# 164 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( seen_proof := true; proof lexbuf )
# 566 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 5 ->
# 166 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( proof_term lexbuf )
# 571 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 6 ->
# 168 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( seen_proof := true; spec lexbuf )
# 576 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 7 ->
# 170 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
    ( seen_proof := true; proof lexbuf )
# 581 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 8 ->
# 171 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( () )
# 586 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_proof_rec lexbuf __ocaml_lex_state

and proof_term lexbuf =
   __ocaml_lex_proof_term_rec lexbuf 185
and __ocaml_lex_proof_term_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 174 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( comment lexbuf; proof_term lexbuf )
# 598 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 1 ->
# 175 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( let n = string lexbuf in plines := !plines + n;
	     seen_proof := true; proof_term lexbuf )
# 604 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 2 ->
# 178 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( proof_term lexbuf )
# 609 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 3 ->
# 179 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( newline (); proof_term lexbuf )
# 614 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 4 ->
# 180 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( spec lexbuf )
# 619 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 5 ->
# 182 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
    ( seen_proof := true; proof_term lexbuf )
# 624 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 6 ->
# 183 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( () )
# 629 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_proof_term_rec lexbuf __ocaml_lex_state

and comment lexbuf =
   __ocaml_lex_comment_rec lexbuf 202
and __ocaml_lex_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 188 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( comment lexbuf; comment lexbuf )
# 641 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 1 ->
# 189 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( () )
# 646 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 2 ->
# 190 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( let n = string lexbuf in dlines := !dlines + n;
	     seen_comment := true; comment lexbuf )
# 652 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 3 ->
# 192 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( newline (); comment lexbuf )
# 657 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 4 ->
# 194 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
    ( comment lexbuf )
# 662 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 5 ->
# 196 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
    ( seen_comment := true; comment lexbuf )
# 667 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 6 ->
# 197 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( () )
# 672 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_comment_rec lexbuf __ocaml_lex_state

and string lexbuf =
   __ocaml_lex_string_rec lexbuf 219
and __ocaml_lex_string_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 203 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
         ( 0 )
# 684 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 1 ->
# 204 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
                            ( string lexbuf )
# 689 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 2 ->
# 205 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
         ( succ (string lexbuf) )
# 694 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 3 ->
# 206 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
         ( string lexbuf )
# 699 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 4 ->
# 207 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
         ( 0 )
# 704 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_string_rec lexbuf __ocaml_lex_state

and read_header lexbuf =
   __ocaml_lex_read_header_rec lexbuf 226
and __ocaml_lex_read_header_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 216 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( skip_comment lexbuf; skip_until_nl lexbuf;
	     read_header lexbuf )
# 717 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 1 ->
# 218 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( () )
# 722 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 2 ->
# 219 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( read_header lexbuf )
# 727 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 3 ->
# 220 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - 1 )
# 732 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 4 ->
# 221 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
           ( () )
# 737 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_header_rec lexbuf __ocaml_lex_state

and skip_comment lexbuf =
   __ocaml_lex_skip_comment_rec lexbuf 233
and __ocaml_lex_skip_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 224 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
         ( () )
# 749 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 1 ->
# 225 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
         ( skip_comment lexbuf; skip_comment lexbuf )
# 754 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 2 ->
# 226 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
         ( skip_comment lexbuf )
# 759 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 3 ->
# 227 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
         ( () )
# 764 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_skip_comment_rec lexbuf __ocaml_lex_state

and skip_until_nl lexbuf =
   __ocaml_lex_skip_until_nl_rec lexbuf 240
and __ocaml_lex_skip_until_nl_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 230 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
         ( () )
# 776 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 1 ->
# 231 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
         ( skip_until_nl lexbuf )
# 781 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | 2 ->
# 232 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
         ( () )
# 786 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_skip_until_nl_rec lexbuf __ocaml_lex_state

;;

# 234 "_vendor+v8.10+32bit/coq/tools/coqwc.mll"
      (*i*)

(*s Processing files and channels. *)

let process_channel ch =
  let lb = Lexing.from_channel ch in
  reset_counters ();
  if !skip_header then read_header lb;
  spec lb

[@@@ocaml.warning "-52"]
let process_file f =
  try
    let ch = open_in f in
    process_channel ch;
    close_in ch;
    print_file (Some f);
    update_totals ()
  with
    | Sys_error "Is a directory" ->
	flush stdout; eprintf "coqwc: %s: Is a directory\n" f; flush stderr
    | Sys_error s ->
	flush stdout; eprintf "coqwc: %s\n" s; flush stderr
[@@@ocaml.warning "+52"]

(*s Parsing of the command line. *)

let usage () =
  prerr_endline "usage: coqwc [options] [files]";
  prerr_endline "Options are:";
  prerr_endline "  -p   print percentage of comments";
  prerr_endline "  -s   print only the spec size";
  prerr_endline "  -r   print only the proof size";
  prerr_endline "  -e   (everything) do not skip headers";
  exit 1

let rec parse = function
  | [] -> []
  | ("-h" | "-?" | "-help" | "--help") :: _ -> usage ()
  | ("-s" | "--spec-only") :: args ->
      proof_only := false; spec_only := true; parse args
  | ("-r" | "--proof-only") :: args ->
      spec_only := false; proof_only := true; parse args
  | ("-p" | "--percentage") :: args -> percentage := true; parse args
  | ("-e" | "--header") :: args -> skip_header := false; parse args
  | f :: args -> f :: (parse args)

(*s Main program. *)

let _ =
  let files = parse (List.tl (Array.to_list Sys.argv)) in
  if not (!spec_only || !proof_only) then
    printf "     spec    proof comments\n";
  match files with
    | [] -> process_channel stdin; print_file None
    | [f] -> process_file f
    | _ -> List.iter process_file files; print_totals ()

(*i*)
# 853 "_vendor+v8.10+32bit/coq/tools/coqwc.ml"
