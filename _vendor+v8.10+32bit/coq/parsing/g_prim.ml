
# 11 "g_prim.mlg"
 

open Names
open Libnames

open Pcoq.Prim

let prim_kw = ["{"; "}"; "["; "]"; "("; ")"; "'"]
let _ = List.iter CLexer.add_keyword prim_kw


let local_make_qualid loc l id = make_qualid ~loc (DirPath.make l) id

let check_int loc = function
  | { NumTok.int = i; frac = ""; exp = "" } -> i
  | _ -> CErrors.user_err ~loc (Pp.str "This number is not an integer.")

let my_int_of_string loc s =
  try
    int_of_string s
  with Failure _ ->
    CErrors.user_err ~loc (Pp.str "This number is too large.")



let _ = let field = Pcoq.Entry.create "field"
        and fields = Pcoq.Entry.create "fields"
        and basequalid = Pcoq.Entry.create "basequalid"
        in
        let () =
        Pcoq.grammar_extend preident None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Atoken (Tok.PIDENT (None)))),
                 (fun s loc -> 
# 42 "g_prim.mlg"
                       s 
                               ))])])
        in let () =
        Pcoq.grammar_extend ident None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Atoken (Tok.PIDENT (None)))),
                 (fun s loc -> 
# 45 "g_prim.mlg"
                       Id.of_string s 
                               ))])])
        in let () =
        Pcoq.grammar_extend pattern_ident None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PLEFTQMARK))),
                              (Extend.Aentry ident)),
                 (fun id _ loc -> 
# 48 "g_prim.mlg"
                                   id 
                                  ))])])
        in let () =
        Pcoq.grammar_extend pattern_identref None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry pattern_ident)),
                 (fun id loc -> 
# 51 "g_prim.mlg"
                                CAst.make ~loc id 
                                ))])])
        in let () =
        Pcoq.grammar_extend var None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry ident)),
                 (fun id loc -> 
# 54 "g_prim.mlg"
                        CAst.make ~loc id 
                                ))])])
        in let () =
        Pcoq.grammar_extend identref None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry ident)),
                 (fun id loc -> 
# 57 "g_prim.mlg"
                        CAst.make ~loc id 
                                ))])])
        in let () =
        Pcoq.grammar_extend field None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Atoken (Tok.PFIELD (None)))),
                 (fun s loc -> 
# 60 "g_prim.mlg"
                       Id.of_string s 
                               ))])])
        in let () =
        Pcoq.grammar_extend fields None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry field)),
                 (fun id loc -> 
# 64 "g_prim.mlg"
                        ([],id) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry field)),
                             (Extend.Aentry fields)),
                (fun f id loc -> 
# 63 "g_prim.mlg"
                                    let (l,id') = f in (l@[id],id') 
                                 ))])])
        in let () =
        Pcoq.grammar_extend fullyqualid None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry ident)),
                 (fun id loc -> 
# 69 "g_prim.mlg"
                        CAst.make ~loc [id] 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry ident)),
                             (Extend.Aentry fields)),
                (fun f id loc -> 
# 68 "g_prim.mlg"
                                  let (l,id') = f in CAst.make ~loc @@ id::List.rev (id'::l) 
                                 ))])])
        in let () =
        Pcoq.grammar_extend basequalid None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry ident)),
                 (fun id loc -> 
# 74 "g_prim.mlg"
                        qualid_of_ident ~loc id 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry ident)),
                             (Extend.Aentry fields)),
                (fun f id loc -> 
# 73 "g_prim.mlg"
                                  let (l,id') = f in  local_make_qualid loc (l@[id]) id' 
                                 ))])])
        in let () =
        Pcoq.grammar_extend name None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry ident)),
                 (fun id loc -> 
# 79 "g_prim.mlg"
                        CAst.make ~loc @@ Name id 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("_"))))),
                (fun _ loc -> 
# 78 "g_prim.mlg"
                        CAst.make ~loc Anonymous 
                              ))])])
        in let () =
        Pcoq.grammar_extend reference None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry ident)),
                 (fun id loc -> 
# 85 "g_prim.mlg"
                        local_make_qualid loc [] id 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry ident)),
                             (Extend.Aentry fields)),
                (fun f id loc -> 
# 82 "g_prim.mlg"
                                   
        let (l,id') = f in
        local_make_qualid loc (l@[id]) id' 
                                 ))])])
        in let () =
        Pcoq.grammar_extend by_notation None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry ne_string)),
                              (Extend.Aopt (Extend.Arules [Extend.Rules 
                                                          (Extend.NextNoRec 
                                                          (Extend.NextNoRec 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (Tok.PKEYWORD ("%")))),
                                                          (Extend.Atoken (Tok.PIDENT (None)))),
                                                          (fun key _ loc ->
                                                          
# 89 "g_prim.mlg"
                                                       key 
                                                          ))]))),
                 (fun sc s loc -> 
# 89 "g_prim.mlg"
                                                                    (s, sc) 
                                  ))])])
        in let () =
        Pcoq.grammar_extend smart_global None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry by_notation)),
                 (fun ntn loc -> 
# 93 "g_prim.mlg"
                               CAst.make ~loc @@ Constrexpr.ByNotation ntn 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry reference)),
                (fun c loc -> 
# 92 "g_prim.mlg"
                           CAst.make ~loc @@ Constrexpr.AN c 
                              ))])])
        in let () =
        Pcoq.grammar_extend qualid None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry basequalid)),
                 (fun qid loc -> 
# 96 "g_prim.mlg"
                              qid 
                                 ))])])
        in let () =
        Pcoq.grammar_extend ne_string None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Atoken (Tok.PSTRING (None)))),
                 (fun s loc -> 
# 100 "g_prim.mlg"
          if s="" then CErrors.user_err ~loc (Pp.str"Empty string."); s 
                               ))])])
        in let () =
        Pcoq.grammar_extend ne_lstring None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry ne_string)),
                 (fun s loc -> 
# 104 "g_prim.mlg"
                           CAst.make ~loc s 
                               ))])])
        in let () =
        Pcoq.grammar_extend dirpath None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry ident)),
                              (Extend.Alist0 (Extend.Aentry field))),
                 (fun l id loc -> 
# 108 "g_prim.mlg"
          DirPath.make (List.rev (id::l)) 
                                  ))])])
        in let () =
        Pcoq.grammar_extend string None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Atoken (Tok.PSTRING (None)))),
                 (fun s loc -> 
# 111 "g_prim.mlg"
                        s 
                               ))])])
        in let () =
        Pcoq.grammar_extend lstring None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry string)),
                 (fun s loc -> 
# 114 "g_prim.mlg"
                        CAst.make ~loc s 
                               ))])])
        in let () =
        Pcoq.grammar_extend integer None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PKEYWORD ("-")))),
                              (Extend.Atoken (Tok.PNUMERAL None))),
                 (fun i _ loc -> 
# 118 "g_prim.mlg"
                              - my_int_of_string loc (check_int loc i) 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PNUMERAL None))),
                (fun i loc -> 
# 117 "g_prim.mlg"
                              my_int_of_string loc (check_int loc i) 
                              ))])])
        in let () =
        Pcoq.grammar_extend natural None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Atoken (Tok.PNUMERAL None))),
                 (fun i loc -> 
# 121 "g_prim.mlg"
                         my_int_of_string loc (check_int loc i) 
                               ))])])
        in let () =
        Pcoq.grammar_extend bigint None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Atoken (Tok.PNUMERAL None))),
                 (fun i loc -> 
# 124 "g_prim.mlg"
                         check_int loc i 
                               ))])])
        in ()

