let __coq_plugin_name = "ltac_plugin"
let _ = Mltop.add_known_module __coq_plugin_name

# 13 "g_ltac.mlg"
 

open Util
open Pp
open Glob_term
open Constrexpr
open Tacexpr
open Namegen
open Genarg
open Genredexpr
open Tok (* necessary for camlp5 *)
open Names
open Attributes

open Pcoq
open Pcoq.Prim
open Pcoq.Constr
open Pvernac.Vernac_
open Pltac

let fail_default_value = Locus.ArgArg 0

let arg_of_expr = function
    TacArg { CAst.v } -> v
  | e -> Tacexp (e:raw_tactic_expr)

let genarg_of_unit () = in_gen (rawwit Stdarg.wit_unit) ()
let genarg_of_int n = in_gen (rawwit Stdarg.wit_int) n
let genarg_of_ipattern pat = in_gen (rawwit Tacarg.wit_simple_intropattern) pat
let genarg_of_uconstr c = in_gen (rawwit Stdarg.wit_uconstr) c
let in_tac tac = in_gen (rawwit Tacarg.wit_ltac) tac

let reference_to_id qid =
  if Libnames.qualid_is_ident qid then
    CAst.make ?loc:qid.CAst.loc @@ Libnames.qualid_basename qid
  else
    CErrors.user_err ?loc:qid.CAst.loc
      (str "This expression should be a simple identifier.")

let tactic_mode = Entry.create "vernac:tactic_command"

let new_entry name =
  let e = Entry.create name in
  e

let toplevel_selector = new_entry "vernac:toplevel_selector"
let tacdef_body = new_entry "tactic:tacdef_body"

(* Registers [tactic_mode] as a parser for proof editing *)
let classic_proof_mode = Pvernac.register_proof_mode "Classic" tactic_mode

(* Hack to parse "[ id" without dropping [ *)
let test_bracket_ident =
  Pcoq.Entry.of_parser "test_bracket_ident"
    (fun strm ->
      match stream_nth 0 strm with
        | KEYWORD "[" ->
            (match stream_nth 1 strm with
              | IDENT _ -> ()
              | _ -> raise Stream.Failure)
        | _ -> raise Stream.Failure)

(* Tactics grammar rules *)

let hint = G_proofs.hint



let _ = let tactic_then_last = Pcoq.Entry.create "tactic_then_last"
        and tactic_then_gen = Pcoq.Entry.create "tactic_then_gen"
        and tactic_then_locality = Pcoq.Entry.create "tactic_then_locality"
        and failkw = Pcoq.Entry.create "failkw"
        and tactic_arg_compat = Pcoq.Entry.create "tactic_arg_compat"
        and fresh_id = Pcoq.Entry.create "fresh_id"
        and tactic_atom = Pcoq.Entry.create "tactic_atom"
        and match_key = Pcoq.Entry.create "match_key"
        and input_fun = Pcoq.Entry.create "input_fun"
        and let_clause = Pcoq.Entry.create "let_clause"
        and match_pattern = Pcoq.Entry.create "match_pattern"
        and match_hyps = Pcoq.Entry.create "match_hyps"
        and match_context_rule = Pcoq.Entry.create "match_context_rule"
        and match_context_list = Pcoq.Entry.create "match_context_list"
        and match_rule = Pcoq.Entry.create "match_rule"
        and match_list = Pcoq.Entry.create "match_list"
        and message_token = Pcoq.Entry.create "message_token"
        and ltac_def_kind = Pcoq.Entry.create "ltac_def_kind"
        and range_selector = Pcoq.Entry.create "range_selector"
        and range_selector_or_nth = Pcoq.Entry.create "range_selector_or_nth"
        and selector_body = Pcoq.Entry.create "selector_body"
        and selector = Pcoq.Entry.create "selector"
        in
        let () =
        Pcoq.grammar_extend tactic_then_last None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 89 "g_ltac.mlg"
             [||] 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("|")))),
                             (Extend.Alist0sep ((Extend.Aopt (Extend.Aentry tactic_expr)), (Extend.Atoken (Tok.PKEYWORD ("|")))))),
                (fun lta _ loc -> 
# 88 "g_ltac.mlg"
          Array.map (function None -> TacId [] | Some t -> t) (Array.of_list lta) 
                                  ))])])
        in let () =
        Pcoq.grammar_extend tactic_then_gen None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 98 "g_ltac.mlg"
             ([TacId []], None) 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("|")))),
                             (Extend.Aentry tactic_then_gen)),
                (fun tg _ loc -> 
# 97 "g_ltac.mlg"
                                       let (first,last) = tg in (TacId [] :: first, last) 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry tactic_expr)),
                (fun ta loc -> 
# 96 "g_ltac.mlg"
                              ([ta], None) 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("..")))),
                             (Extend.Aentry tactic_then_last)),
                (fun l _ loc -> 
# 95 "g_ltac.mlg"
                                        ([], Some (TacId [], l)) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry tactic_expr)),
                                          (Extend.Atoken (Tok.PKEYWORD ("..")))),
                             (Extend.Aentry tactic_then_last)),
                (fun l _ ta loc -> 
# 94 "g_ltac.mlg"
                                                          ([], Some (ta, l)) 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry tactic_expr)),
                                          (Extend.Atoken (Tok.PKEYWORD ("|")))),
                             (Extend.Aentry tactic_then_gen)),
                (fun tg _ ta loc -> 
# 93 "g_ltac.mlg"
                                                         let (first,last) = tg in (ta::first, last) 
                                    ))])])
        in let () =
        Pcoq.grammar_extend tactic_then_locality None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PKEYWORD ("[")))),
                              (Extend.Aopt (Extend.Atoken (Tok.PKEYWORD (">"))))),
                 (fun l _ loc -> 
# 103 "g_ltac.mlg"
                            if Option.is_empty l then true else false 
                                 ))])])
        in let () =
        Pcoq.grammar_extend tactic_expr None
        (None, [(Some ("5"), Some (Gramlib.Gramext.RightA),
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry binder_tactic)),
                 (fun te loc -> 
# 107 "g_ltac.mlg"
                                te 
                                ))]);
               (Some ("4"), Some (Gramlib.Gramext.LeftA),
               [Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Aentry tactic_expr)),
                                                                    (Extend.Atoken (Tok.PKEYWORD (";")))),
                                                       (Extend.Aentry tactic_then_locality)),
                                          (Extend.Aentry tactic_then_gen)),
                             (Extend.Atoken (Tok.PKEYWORD ("]")))),
                (fun _ tg l _ ta0 loc -> 
# 111 "g_ltac.mlg"
                                                                                        
          let (first,tail) = tg in
          match l , tail with
          | false , Some (t,last) -> TacThen (ta0,TacExtendTac (Array.of_list first, t, last))
          | true  , Some (t,last) -> TacThens3parts (ta0, Array.of_list first, t, last)
          | false , None -> TacThen (ta0,TacDispatch first)
          | true  , None -> TacThens (ta0,first) 
                                         ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                      (Extend.Aentry tactic_expr)),
                                         (Extend.Atoken (Tok.PKEYWORD (";")))),
                            (Extend.Aentry tactic_expr)),
               (fun ta1 _ ta0 loc -> 
# 110 "g_ltac.mlg"
                                                       TacThen (ta0,ta1) 
                                     ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                      (Extend.Aentry tactic_expr)),
                                         (Extend.Atoken (Tok.PKEYWORD (";")))),
                            (Extend.Aentry binder_tactic)),
               (fun ta1 _ ta0 loc -> 
# 109 "g_ltac.mlg"
                                                         TacThen (ta0, ta1) 
                                     ))]);
               (Some ("3"), Some (Gramlib.Gramext.RightA),
               [Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry selector)),
                             (Extend.Aentry tactic_expr)),
                (fun ta sel loc -> 
# 132 "g_ltac.mlg"
                                              TacSelect (sel, ta) 
                                   ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                   (Extend.Atoken (Tok.PIDENT (Some
                                                                   ("abstract"))))),
                                                      Extend.Anext),
                                         (Extend.Atoken (Tok.PKEYWORD ("using")))),
                            (Extend.Aentry ident)),
               (fun s _ tc _ loc -> 
# 131 "g_ltac.mlg"
          TacAbstract (tc,Some s) 
                                    ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Stop,
                                         (Extend.Atoken (Tok.PIDENT (Some
                                         ("abstract"))))),
                            Extend.Anext),
               (fun tc _ loc -> 
# 129 "g_ltac.mlg"
                                         TacAbstract (tc,None) 
                                ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Stop,
                                         (Extend.Atoken (Tok.PIDENT (Some
                                         ("infoH"))))),
                            (Extend.Aentry tactic_expr)),
               (fun ta _ loc -> 
# 127 "g_ltac.mlg"
                                             TacShowHyps ta 
                                ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Stop,
                                         (Extend.Atoken (Tok.PIDENT (Some
                                         ("exactly_once"))))),
                            (Extend.Aentry tactic_expr)),
               (fun ta _ loc -> 
# 126 "g_ltac.mlg"
                                                    TacExactlyOnce ta 
                                ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Stop,
                                         (Extend.Atoken (Tok.PIDENT (Some
                                         ("once"))))),
                            (Extend.Aentry tactic_expr)),
               (fun ta _ loc -> 
# 125 "g_ltac.mlg"
                                            TacOnce ta 
                                ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Stop,
                                         (Extend.Atoken (Tok.PIDENT (Some
                                         ("progress"))))),
                            (Extend.Aentry tactic_expr)),
               (fun ta _ loc -> 
# 124 "g_ltac.mlg"
                                                TacProgress ta 
                                ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Stop,
                                         (Extend.Atoken (Tok.PIDENT (Some
                                         ("repeat"))))),
                            (Extend.Aentry tactic_expr)),
               (fun ta _ loc -> 
# 123 "g_ltac.mlg"
                                              TacRepeat ta 
                                ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                      (Extend.Atoken (Tok.PIDENT (Some
                                                      ("time"))))),
                                         (Extend.Aopt (Extend.Aentry string))),
                            (Extend.Aentry tactic_expr)),
               (fun ta s _ loc -> 
# 122 "g_ltac.mlg"
                                                            TacTime (s,ta) 
                                  ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                      (Extend.Atoken (Tok.PIDENT (Some
                                                      ("timeout"))))),
                                         (Extend.Aentry int_or_var)),
                            (Extend.Aentry tactic_expr)),
               (fun ta n _ loc -> 
# 121 "g_ltac.mlg"
                                                               TacTimeout (n,ta) 
                                  ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                      (Extend.Atoken (Tok.PIDENT (Some
                                                      ("do"))))),
                                         (Extend.Aentry int_or_var)),
                            (Extend.Aentry tactic_expr)),
               (fun ta n _ loc -> 
# 120 "g_ltac.mlg"
                                                          TacDo (n,ta) 
                                  ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Stop,
                                         (Extend.Atoken (Tok.PIDENT (Some
                                         ("try"))))),
                            (Extend.Aentry tactic_expr)),
               (fun ta _ loc -> 
# 119 "g_ltac.mlg"
                                           TacTry ta 
                                ))]);
               (Some ("2"), Some (Gramlib.Gramext.RightA),
               [Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry tactic_expr)),
                                          (Extend.Atoken (Tok.PKEYWORD ("||")))),
                             (Extend.Aentry tactic_expr)),
                (fun ta1 _ ta0 loc -> 
# 141 "g_ltac.mlg"
                                                        TacOrelse (ta0,ta1) 
                                      ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                      (Extend.Aentry tactic_expr)),
                                         (Extend.Atoken (Tok.PKEYWORD ("||")))),
                            (Extend.Aentry binder_tactic)),
               (fun ta1 _ ta0 loc -> 
# 140 "g_ltac.mlg"
                                                          TacOrelse (ta0,ta1) 
                                     ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                   (Extend.Next 
                                                                   (Extend.Stop,
                                                                   (Extend.Atoken (Tok.PIDENT (Some
                                                                   ("tryif"))))),
                                                                   (Extend.Aentry tactic_expr)),
                                                                   (Extend.Atoken (Tok.PKEYWORD ("then")))),
                                                      (Extend.Aentry tactic_expr)),
                                         (Extend.Atoken (Tok.PKEYWORD ("else")))),
                            (Extend.Aentry tactic_expr)),
               (fun tae _ tat _ ta _ loc -> 
# 139 "g_ltac.mlg"
                                              TacIfThenCatch(ta,tat,tae) 
                                            ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                      (Extend.Aentry tactic_expr)),
                                         (Extend.Atoken (Tok.PKEYWORD ("+")))),
                            (Extend.Aentry tactic_expr)),
               (fun ta1 _ ta0 loc -> 
# 136 "g_ltac.mlg"
                                                       TacOr (ta0,ta1) 
                                     ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                      (Extend.Aentry tactic_expr)),
                                         (Extend.Atoken (Tok.PKEYWORD ("+")))),
                            (Extend.Aentry binder_tactic)),
               (fun ta1 _ ta0 loc -> 
# 135 "g_ltac.mlg"
                                                         TacOr (ta0,ta1) 
                                     ))]);
               (Some ("1"), Some (Gramlib.Gramext.RightA),
               [Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry reference)),
                             (Extend.Alist0 (Extend.Aentry tactic_arg_compat))),
                (fun la r loc -> 
# 160 "g_ltac.mlg"
          TacArg(CAst.make ~loc @@ TacCall (CAst.make ~loc (r,la))) 
                                 ));
               Extend.Rule
               (Extend.Next (Extend.Stop, (Extend.Aentry tactic_arg)),
               (fun a loc -> 
# 158 "g_ltac.mlg"
                            TacArg(CAst.make ~loc a) 
                             ));
               Extend.Rule
               (Extend.Next (Extend.Stop, (Extend.Aentry simple_tactic)),
               (fun st loc -> 
# 157 "g_ltac.mlg"
                                st 
                              ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                      (Extend.Aentry failkw)),
                                         (Extend.Arules [Extend.Rules 
                                                        (Extend.Stop, (fun
                                                        loc -> 
# 155 "g_ltac.mlg"
                                                       fail_default_value 
                                                               ));
                                                        Extend.Rules 
                                                        (Extend.NextNoRec 
                                                        (Extend.Stop,
                                                        (Extend.Aentry int_or_var)),
                                                        (fun n loc -> 
                                                        
# 155 "g_ltac.mlg"
                                            n 
                                                        ))])),
                            (Extend.Alist0 (Extend.Aentry message_token))),
               (fun l n g loc -> 
# 156 "g_ltac.mlg"
                                       TacFail (g,n,l) 
                                 ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Stop,
                                         (Extend.Atoken (Tok.PIDENT (Some
                                         ("idtac"))))),
                            (Extend.Alist0 (Extend.Aentry message_token))),
               (fun l _ loc -> 
# 154 "g_ltac.mlg"
                                                    TacId l 
                               ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                   (Extend.Atoken (Tok.PIDENT (Some
                                                                   ("solve"))))),
                                                      (Extend.Atoken (Tok.PKEYWORD ("[")))),
                                         (Extend.Alist0sep ((Extend.Aentry tactic_expr), (Extend.Atoken (Tok.PKEYWORD ("|")))))),
                            (Extend.Atoken (Tok.PKEYWORD ("]")))),
               (fun _ l _ _ loc -> 
# 153 "g_ltac.mlg"
            TacSolve l 
                                   ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                   (Extend.Atoken (Tok.PIDENT (Some
                                                                   ("first"))))),
                                                      (Extend.Atoken (Tok.PKEYWORD ("[")))),
                                         (Extend.Alist0sep ((Extend.Aentry tactic_expr), (Extend.Atoken (Tok.PKEYWORD ("|")))))),
                            (Extend.Atoken (Tok.PKEYWORD ("]")))),
               (fun _ l _ _ loc -> 
# 151 "g_ltac.mlg"
            TacFirst l 
                                   ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                   (Extend.Stop,
                                                                   (Extend.Aentry match_key)),
                                                                   (Extend.Aentry tactic_expr)),
                                                      (Extend.Atoken (Tok.PKEYWORD ("with")))),
                                         (Extend.Aentry match_list)),
                            (Extend.Atoken (Tok.PKEYWORD ("end")))),
               (fun _ mrl _ c b loc -> 
# 149 "g_ltac.mlg"
            TacMatch (b,c,mrl) 
                                       ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                   (Extend.Next 
                                                                   (Extend.Stop,
                                                                   (Extend.Aentry match_key)),
                                                                   (Extend.Atoken (Tok.PIDENT (Some
                                                                   ("reverse"))))),
                                                                   (Extend.Atoken (Tok.PIDENT (Some
                                                                   ("goal"))))),
                                                      (Extend.Atoken (Tok.PKEYWORD ("with")))),
                                         (Extend.Aentry match_context_list)),
                            (Extend.Atoken (Tok.PKEYWORD ("end")))),
               (fun _ mrl _ _ _ b loc -> 
# 147 "g_ltac.mlg"
            TacMatchGoal (b,true,mrl) 
                                         ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                   (Extend.Stop,
                                                                   (Extend.Aentry match_key)),
                                                                   (Extend.Atoken (Tok.PIDENT (Some
                                                                   ("goal"))))),
                                                      (Extend.Atoken (Tok.PKEYWORD ("with")))),
                                         (Extend.Aentry match_context_list)),
                            (Extend.Atoken (Tok.PKEYWORD ("end")))),
               (fun _ mrl _ _ b loc -> 
# 144 "g_ltac.mlg"
            TacMatchGoal (b,false,mrl) 
                                       ))]);
               (Some ("0"), None,
               [Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry tactic_atom)),
                (fun a loc -> 
# 169 "g_ltac.mlg"
                             TacArg (CAst.make ~loc a) 
                              ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                   (Extend.Atoken (Tok.PKEYWORD ("[")))),
                                                      (Extend.Atoken (Tok.PKEYWORD (">")))),
                                         (Extend.Aentry tactic_then_gen)),
                            (Extend.Atoken (Tok.PKEYWORD ("]")))),
               (fun _ tg _ _ loc -> 
# 163 "g_ltac.mlg"
                                                
          let (tf,tail) = tg in
          begin match tail with
          | Some (t,tl) -> TacExtendTac(Array.of_list tf,t,tl)
          | None -> TacDispatch tf
          end 
                                    ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                      (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                         (Extend.Aentry tactic_expr)),
                            (Extend.Atoken (Tok.PKEYWORD (")")))),
               (fun _ a _ loc -> 
# 162 "g_ltac.mlg"
                                       a 
                                 ))])])
        in let () =
        Pcoq.grammar_extend failkw None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                              ("gfail"))))),
                 (fun _ loc -> 
# 172 "g_ltac.mlg"
                                                        TacGlobal 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("fail"))))),
                (fun _ loc -> 
# 172 "g_ltac.mlg"
                        TacLocal 
                              ))])])
        in let () =
        Pcoq.grammar_extend binder_tactic None
        (None, [(None, Some (Gramlib.Gramext.RightA),
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PIDENT (Some
                                           ("info"))))),
                              (Extend.Aentryl (tactic_expr, "5"))),
                 (fun tc _ loc -> 
# 182 "g_ltac.mlg"
                                                      TacInfo tc 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("let")))),
                                                                    (Extend.Arules 
                                                                    [Extend.Rules 
                                                                    (Extend.Stop,
                                                                    (fun
                                                                    loc -> 
                                                                    
# 179 "g_ltac.mlg"
                                                       false 
                                                                    ));
                                                                    Extend.Rules 
                                                                    (Extend.NextNoRec 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("rec"))))),
                                                                    (fun _
                                                                    loc -> 
                                                                    
# 179 "g_ltac.mlg"
                                         true 
                                                                    ))])),
                                                       (Extend.Alist1sep ((Extend.Aentry let_clause), (Extend.Atoken (Tok.PKEYWORD ("with")))))),
                                          (Extend.Atoken (Tok.PKEYWORD ("in")))),
                             (Extend.Aentryl (tactic_expr, "5"))),
                (fun body _ llc isrec _ loc -> 
# 181 "g_ltac.mlg"
                                            TacLetIn (isrec,llc,body) 
                                               ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("fun")))),
                                                       (Extend.Alist1 (Extend.Aentry input_fun))),
                                          (Extend.Atoken (Tok.PKEYWORD ("=>")))),
                             (Extend.Aentryl (tactic_expr, "5"))),
                (fun body _ it _ loc -> 
# 178 "g_ltac.mlg"
            TacFun (it,body) 
                                        ))])])
        in let () =
        Pcoq.grammar_extend tactic_arg_compat None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Atoken (Tok.PKEYWORD ("()")))),
                 (fun _ loc -> 
# 189 "g_ltac.mlg"
                  TacGeneric (genarg_of_unit ()) 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry Constr.constr)),
                (fun c loc -> 
# 187 "g_ltac.mlg"
                               (match c with { CAst.v = CRef (r,None) } -> Reference r | c -> ConstrMayEval (ConstrTerm c)) 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry tactic_arg)),
                (fun a loc -> 
# 186 "g_ltac.mlg"
                            a 
                              ))])])
        in let () =
        Pcoq.grammar_extend tactic_arg None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                              ("numgoals"))))),
                 (fun _ loc -> 
# 196 "g_ltac.mlg"
                              TacNumgoals 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("type_term"))))),
                             (Extend.Aentry uconstr)),
                (fun c _ loc -> 
# 195 "g_ltac.mlg"
                                          TacPretype c 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("fresh"))))),
                             (Extend.Alist0 (Extend.Aentry fresh_id))),
                (fun l _ loc -> 
# 194 "g_ltac.mlg"
                                               TacFreshId l 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry constr_eval)),
                (fun c loc -> 
# 193 "g_ltac.mlg"
                             ConstrMayEval c 
                              ))])])
        in let () =
        Pcoq.grammar_extend fresh_id None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry qualid)),
                 (fun qid loc -> 
# 203 "g_ltac.mlg"
                            Locus.ArgVar (CAst.make ~loc @@ Libnames.qualid_basename qid) 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PSTRING (None)))),
                (fun s loc -> 
# 202 "g_ltac.mlg"
                        Locus.ArgArg s (*| id = ident -> Locus.ArgVar (!@loc,id)*) 
                              ))])])
        in let () =
        Pcoq.grammar_extend constr_eval None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Atoken (Tok.PIDENT (Some
                                                        ("type"))))),
                                           (Extend.Atoken (Tok.PIDENT (Some
                                           ("of"))))),
                              (Extend.Aentry Constr.constr)),
                 (fun c _ _ loc -> 
# 211 "g_ltac.mlg"
            ConstrTypeOf c 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("context"))))),
                                                                    (Extend.Aentry identref)),
                                                       (Extend.Atoken (Tok.PKEYWORD ("[")))),
                                          (Extend.Aentry Constr.lconstr)),
                             (Extend.Atoken (Tok.PKEYWORD ("]")))),
                (fun _ c _ id _ loc -> 
# 209 "g_ltac.mlg"
            ConstrContext (id,c) 
                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("eval"))))),
                                                       (Extend.Aentry red_expr)),
                                          (Extend.Atoken (Tok.PKEYWORD ("in")))),
                             (Extend.Aentry Constr.constr)),
                (fun c _ rtc _ loc -> 
# 207 "g_ltac.mlg"
            ConstrEval (rtc,c) 
                                      ))])])
        in let () =
        Pcoq.grammar_extend constr_may_eval None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry Constr.constr)),
                 (fun c loc -> 
# 215 "g_ltac.mlg"
                               ConstrTerm c 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry constr_eval)),
                (fun c loc -> 
# 214 "g_ltac.mlg"
                             c 
                              ))])])
        in let () =
        Pcoq.grammar_extend tactic_atom None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Atoken (Tok.PKEYWORD ("()")))),
                 (fun _ loc -> 
# 220 "g_ltac.mlg"
                  TacGeneric (genarg_of_unit ()) 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry reference)),
                (fun r loc -> 
# 219 "g_ltac.mlg"
                           TacCall (CAst.make ~loc (r,[])) 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry integer)),
                (fun n loc -> 
# 218 "g_ltac.mlg"
                         TacGeneric (genarg_of_int n) 
                              ))])])
        in let () =
        Pcoq.grammar_extend match_key None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Atoken (Tok.PKEYWORD ("multimatch")))),
                 (fun _ loc -> 
# 225 "g_ltac.mlg"
                          General 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("lazymatch")))),
                (fun _ loc -> 
# 224 "g_ltac.mlg"
                         Select 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("match")))),
                (fun _ loc -> 
# 223 "g_ltac.mlg"
                     Once 
                              ))])])
        in let () =
        Pcoq.grammar_extend input_fun None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry ident)),
                 (fun l loc -> 
# 229 "g_ltac.mlg"
                       Name.Name l 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("_")))),
                (fun _ loc -> 
# 228 "g_ltac.mlg"
                 Name.Anonymous 
                              ))])])
        in let () =
        Pcoq.grammar_extend let_clause None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Aentry identref)),
                                                        (Extend.Alist1 (Extend.Aentry input_fun))),
                                           (Extend.Atoken (Tok.PKEYWORD (":=")))),
                              (Extend.Aentry tactic_expr)),
                 (fun te _ args idr loc -> 
# 237 "g_ltac.mlg"
           (CAst.map (fun id -> Name id) idr, arg_of_expr (TacFun(args,te))) 
                                           ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Arules 
                                                       [Extend.Rules 
                                                       (Extend.NextNoRec 
                                                       (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD ("_")))),
                                                       (fun _ loc -> 
                                                       
# 234 "g_ltac.mlg"
                       CAst.make ~loc Anonymous 
                                                       ))])),
                                          (Extend.Atoken (Tok.PKEYWORD (":=")))),
                             (Extend.Aentry tactic_expr)),
                (fun te _ na loc -> 
# 235 "g_ltac.mlg"
           (na, arg_of_expr te) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry identref)),
                                          (Extend.Atoken (Tok.PKEYWORD (":=")))),
                             (Extend.Aentry tactic_expr)),
                (fun te _ idr loc -> 
# 233 "g_ltac.mlg"
           (CAst.map (fun id -> Name id) idr, arg_of_expr te) 
                                     ))])])
        in let () =
        Pcoq.grammar_extend match_pattern None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Aentry Constr.lconstr_pattern)),
                 (fun pc loc -> 
# 243 "g_ltac.mlg"
                                         Term pc 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("context"))))),
                                                                    (Extend.Aopt (Extend.Aentry Constr.ident))),
                                                       (Extend.Atoken (Tok.PKEYWORD ("[")))),
                                          (Extend.Aentry Constr.lconstr_pattern)),
                             (Extend.Atoken (Tok.PKEYWORD ("]")))),
                (fun _ pc _ oid _ loc -> 
# 242 "g_ltac.mlg"
          Subterm (oid, pc) 
                                         ))])])
        in let () =
        Pcoq.grammar_extend match_hyps None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Aentry name)),
                                           (Extend.Atoken (Tok.PKEYWORD (":=")))),
                              (Extend.Aentry match_pattern)),
                 (fun mpv _ na loc -> 
# 249 "g_ltac.mlg"
          let t, ty =
            match mpv with
            | Term t -> (match t with
              | { CAst.v = CCast (t, (CastConv ty | CastVM ty | CastNative ty)) } -> Term t, Some (Term ty)
              | _ -> mpv, None)
            | _ -> mpv, None
          in Def (na, t, Option.default (Term (CAst.make @@ CHole (None, IntroAnonymous, None))) ty) 
                                      ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Aentry name)),
                                                                    (Extend.Atoken (Tok.PKEYWORD (":=")))),
                                                                    (Extend.Atoken (Tok.PKEYWORD ("[")))),
                                                                    (Extend.Aentry match_pattern)),
                                                       (Extend.Atoken (Tok.PKEYWORD ("]")))),
                                          (Extend.Atoken (Tok.PKEYWORD (":")))),
                             (Extend.Aentry match_pattern)),
                (fun mpt _ _ mpv _ _ na loc -> 
# 247 "g_ltac.mlg"
                                                                                      Def (na, mpv, mpt) 
                                               ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry name)),
                                          (Extend.Atoken (Tok.PKEYWORD (":")))),
                             (Extend.Aentry match_pattern)),
                (fun mp _ na loc -> 
# 246 "g_ltac.mlg"
                                                 Hyp (na, mp) 
                                    ))])])
        in let () =
        Pcoq.grammar_extend match_context_rule None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("_")))),
                                           (Extend.Atoken (Tok.PKEYWORD ("=>")))),
                              (Extend.Aentry tactic_expr)),
                 (fun te _ _ loc -> 
# 263 "g_ltac.mlg"
                                         All te 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("[")))),
                                                                    (Extend.Alist0sep ((Extend.Aentry match_hyps), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                                                                    (Extend.Atoken (Tok.PKEYWORD ("|-")))),
                                                                    (Extend.Aentry match_pattern)),
                                                       (Extend.Atoken (Tok.PKEYWORD ("]")))),
                                          (Extend.Atoken (Tok.PKEYWORD ("=>")))),
                             (Extend.Aentry tactic_expr)),
                (fun te _ _ mp _ largs _ loc -> 
# 262 "g_ltac.mlg"
                                         Pat (largs, mp, te) 
                                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Alist0sep ((Extend.Aentry match_hyps), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                                                                    (Extend.Atoken (Tok.PKEYWORD ("|-")))),
                                                       (Extend.Aentry match_pattern)),
                                          (Extend.Atoken (Tok.PKEYWORD ("=>")))),
                             (Extend.Aentry tactic_expr)),
                (fun te _ mp _ largs loc -> 
# 260 "g_ltac.mlg"
                                    Pat (largs, mp, te) 
                                            ))])])
        in let () =
        Pcoq.grammar_extend match_context_list None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PKEYWORD ("|")))),
                              (Extend.Alist1sep ((Extend.Aentry match_context_rule), (Extend.Atoken (Tok.PKEYWORD ("|")))))),
                 (fun mrl _ loc -> 
# 267 "g_ltac.mlg"
                                                         mrl 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Alist1sep ((Extend.Aentry match_context_rule), (Extend.Atoken (Tok.PKEYWORD ("|")))))),
                (fun mrl loc -> 
# 266 "g_ltac.mlg"
                                                    mrl 
                                ))])])
        in let () =
        Pcoq.grammar_extend match_rule None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("_")))),
                                           (Extend.Atoken (Tok.PKEYWORD ("=>")))),
                              (Extend.Aentry tactic_expr)),
                 (fun te _ _ loc -> 
# 271 "g_ltac.mlg"
                                         All te 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry match_pattern)),
                                          (Extend.Atoken (Tok.PKEYWORD ("=>")))),
                             (Extend.Aentry tactic_expr)),
                (fun te _ mp loc -> 
# 270 "g_ltac.mlg"
                                                        Pat ([],mp,te) 
                                    ))])])
        in let () =
        Pcoq.grammar_extend match_list None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PKEYWORD ("|")))),
                              (Extend.Alist1sep ((Extend.Aentry match_rule), (Extend.Atoken (Tok.PKEYWORD ("|")))))),
                 (fun mrl _ loc -> 
# 275 "g_ltac.mlg"
                                                 mrl 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Alist1sep ((Extend.Aentry match_rule), (Extend.Atoken (Tok.PKEYWORD ("|")))))),
                (fun mrl loc -> 
# 274 "g_ltac.mlg"
                                            mrl 
                                ))])])
        in let () =
        Pcoq.grammar_extend message_token None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry integer)),
                 (fun n loc -> 
# 280 "g_ltac.mlg"
                         MsgInt n 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PSTRING (None)))),
                (fun s loc -> 
# 279 "g_ltac.mlg"
                        MsgString s 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry identref)),
                (fun id loc -> 
# 278 "g_ltac.mlg"
                           MsgIdent id 
                               ))])])
        in let () =
        Pcoq.grammar_extend ltac_def_kind None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Atoken (Tok.PKEYWORD ("::=")))),
                 (fun _ loc -> 
# 285 "g_ltac.mlg"
                   true 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD (":=")))),
                (fun _ loc -> 
# 284 "g_ltac.mlg"
                  false 
                              ))])])
        in let () =
        Pcoq.grammar_extend tacdef_body None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Aentry Constr.global)),
                                           (Extend.Aentry ltac_def_kind)),
                              (Extend.Aentry tactic_expr)),
                 (fun body redef name loc -> 
# 298 "g_ltac.mlg"
          if redef then Tacexpr.TacticRedefinition (name, body)
          else
            let id = reference_to_id name in
            Tacexpr.TacticDefinition (id, body) 
                                             ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Aentry Constr.global)),
                                                       (Extend.Alist1 (Extend.Aentry input_fun))),
                                          (Extend.Aentry ltac_def_kind)),
                             (Extend.Aentry tactic_expr)),
                (fun body redef it name loc -> 
# 292 "g_ltac.mlg"
          if redef then Tacexpr.TacticRedefinition (name, TacFun (it, body))
          else
            let id = reference_to_id name in
            Tacexpr.TacticDefinition (id, TacFun (it, body)) 
                                               ))])])
        in let () =
        Pcoq.grammar_extend tactic None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry tactic_expr)),
                 (fun tac loc -> 
# 305 "g_ltac.mlg"
                               tac 
                                 ))])])
        in let () =
        Pcoq.grammar_extend range_selector None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry natural)),
                 (fun n loc -> 
# 310 "g_ltac.mlg"
                         (n, n) 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry natural)),
                                          (Extend.Atoken (Tok.PKEYWORD ("-")))),
                             (Extend.Aentry natural)),
                (fun m _ n loc -> 
# 309 "g_ltac.mlg"
                                             (n, m) 
                                  ))])])
        in let () =
        Pcoq.grammar_extend range_selector_or_nth None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry natural)),
                              (Extend.Aopt (Extend.Arules [Extend.Rules 
                                                          (Extend.NextNoRec 
                                                          (Extend.NextNoRec 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (Tok.PKEYWORD (",")))),
                                                          (Extend.Alist1sep ((Extend.Aentry range_selector), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                                                          (fun l _ loc -> 
                                                          
# 319 "g_ltac.mlg"
                                                            l 
                                                          ))]))),
                 (fun l n loc -> 
# 320 "g_ltac.mlg"
          let open Goal_select in
          Option.cata (fun l -> SelectList ((n, n) :: l)) (SelectNth n) l 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Aentry natural)),
                                                       (Extend.Atoken (Tok.PKEYWORD ("-")))),
                                          (Extend.Aentry natural)),
                             (Extend.Aopt (Extend.Arules [Extend.Rules 
                                                         (Extend.NextNoRec 
                                                         (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (Tok.PKEYWORD (",")))),
                                                         (Extend.Alist1sep ((Extend.Aentry range_selector), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                                                         (fun l _ loc -> 
                                                         
# 316 "g_ltac.mlg"
                                                            l 
                                                         ))]))),
                (fun l m _ n loc -> 
# 317 "g_ltac.mlg"
          Goal_select.SelectList ((n, m) :: Option.default [] l) 
                                    ))])])
        in let () =
        Pcoq.grammar_extend selector_body None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Aentry test_bracket_ident)),
                                                        (Extend.Atoken (Tok.PKEYWORD ("[")))),
                                           (Extend.Aentry ident)),
                              (Extend.Atoken (Tok.PKEYWORD ("]")))),
                 (fun _ id _ _ loc -> 
# 325 "g_ltac.mlg"
                                                    Goal_select.SelectId id 
                                      ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Aentry range_selector_or_nth)),
                (fun l loc -> 
# 324 "g_ltac.mlg"
                                     l 
                              ))])])
        in let () =
        Pcoq.grammar_extend selector None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Atoken (Tok.PIDENT (Some
                                                        ("only"))))),
                                           (Extend.Aentry selector_body)),
                              (Extend.Atoken (Tok.PKEYWORD (":")))),
                 (fun _ sel _ loc -> 
# 328 "g_ltac.mlg"
                                                    sel 
                                     ))])])
        in let () =
        Pcoq.grammar_extend toplevel_selector None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PIDENT (Some
                                           ("all"))))),
                              (Extend.Atoken (Tok.PKEYWORD (":")))),
                 (fun _ _ loc -> 
# 333 "g_ltac.mlg"
                              Goal_select.SelectAll 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("!")))),
                             (Extend.Atoken (Tok.PKEYWORD (":")))),
                (fun _ _ loc -> 
# 332 "g_ltac.mlg"
                      Goal_select.SelectAlreadyFocused 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry selector_body)),
                             (Extend.Atoken (Tok.PKEYWORD (":")))),
                (fun _ sel loc -> 
# 331 "g_ltac.mlg"
                                      sel 
                                  ))])])
        in let () =
        Pcoq.grammar_extend tactic_mode None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aopt (Extend.Aentry toplevel_selector))),
                              (Extend.Atoken (Tok.PKEYWORD ("{")))),
                 (fun _ g loc -> 
# 337 "g_ltac.mlg"
                                            Vernacexpr.VernacSubproof g 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aopt (Extend.Aentry toplevel_selector))),
                             (Extend.Aentry G_vernac.query_command)),
                (fun tac g loc -> 
# 336 "g_ltac.mlg"
                                                                     tac g 
                                  ))])])
        in let () =
        Pcoq.grammar_extend command None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PIDENT (Some
                                                        ("Proof"))))),
                                                        (Extend.Atoken (Tok.PKEYWORD ("using")))),
                                           (Extend.Aentry G_vernac.section_subset_expr)),
                              (Extend.Aopt (Extend.Arules [Extend.Rules 
                                                          (Extend.NextNoRec 
                                                          (Extend.NextNoRec 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (Tok.PKEYWORD ("with")))),
                                                          (Extend.Aentry Pltac.tactic)),
                                                          (fun ta _ loc -> 
                                                          
# 344 "g_ltac.mlg"
                                                  in_tac ta 
                                                          ))]))),
                 (fun ta l _ _ loc -> 
# 345 "g_ltac.mlg"
            Vernacexpr.VernacProof (ta,Some l) 
                                      ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Proof"))))),
                                                       (Extend.Atoken (Tok.PKEYWORD ("with")))),
                                          (Extend.Aentry Pltac.tactic)),
                             (Extend.Aopt (Extend.Arules [Extend.Rules 
                                                         (Extend.NextNoRec 
                                                         (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (Tok.PKEYWORD ("using")))),
                                                         (Extend.Aentry G_vernac.section_subset_expr)),
                                                         (fun l _ loc -> 
                                                         
# 341 "g_ltac.mlg"
                                                                 l 
                                                         ))]))),
                (fun l ta _ _ loc -> 
# 342 "g_ltac.mlg"
            Vernacexpr.VernacProof (Some (in_tac ta), l) 
                                     ))])])
        in let () =
        Pcoq.grammar_extend hint None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PIDENT (Some
                                                        ("Extern"))))),
                                                        (Extend.Aentry natural)),
                                                        (Extend.Aopt (Extend.Aentry Constr.constr_pattern))),
                                           (Extend.Atoken (Tok.PKEYWORD ("=>")))),
                              (Extend.Aentry Pltac.tactic)),
                 (fun tac _ c n _ loc -> 
# 350 "g_ltac.mlg"
          Hints.HintsExtern (n,c, in_tac tac) 
                                         ))])])
        in let () =
        Pcoq.grammar_extend operconstr None
        (Some
        (Gramlib.Gramext.Level "0"), [(None, None,
                                      [Extend.Rule
                                       (Extend.Next (Extend.Next (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Stop,
                                                                 (Extend.Atoken (Tok.PIDENT (Some
                                                                 ("ltac"))))),
                                                                 (Extend.Atoken (Tok.PKEYWORD (":")))),
                                                                 (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                                 (Extend.Aentry Pltac.tactic_expr)),
                                                    (Extend.Atoken (Tok.PKEYWORD (")")))),
                                       (fun _ tac _ _ _ loc -> 
# 354 "g_ltac.mlg"
          let arg = Genarg.in_gen (Genarg.rawwit Tacarg.wit_tactic) tac in
          CAst.make ~loc @@ CHole (None, IntroAnonymous, Some arg) 
                                                               ))])])
        in ()


# 359 "g_ltac.mlg"
 

open Stdarg
open Tacarg
open Vernacextend
open Goptions
open Libnames

let print_info_trace = ref None

let () = declare_int_option {
  optdepr = false;
  optname = "print info trace";
  optkey = ["Info" ; "Level"];
  optread = (fun () -> !print_info_trace);
  optwrite = fun n -> print_info_trace := n;
}

let vernac_solve ~pstate n info tcom b =
  let open Goal_select in
  let pstate, status = Proof_global.with_current_proof (fun etac p ->
      let with_end_tac = if b then Some etac else None in
      let global = match n with SelectAll | SelectList _ -> true | _ -> false in
      let info = Option.append info !print_info_trace in
      let (p,status) =
        Pfedit.solve n info (Tacinterp.hide_interp global tcom None) ?with_end_tac p
      in
      (* in case a strict subtree was completed,
         go back to the top of the prooftree *)
      let p = Proof.maximal_unfocus Vernacentries.command_focus p in
      p,status) pstate in
  if not status then Feedback.feedback Feedback.AddedAxiom;
  Some pstate

let pr_ltac_selector s = Pptactic.pr_goal_selector ~toplevel:true s



let (wit_ltac_selector, ltac_selector) = Vernacextend.vernac_argument_extend ~name:"ltac_selector" 
                                         {
                                         Vernacextend.arg_parsing = Vernacextend.Arg_alias (toplevel_selector);
                                         Vernacextend.arg_printer = fun env sigma -> 
                                         
# 397 "g_ltac.mlg"
                                                  pr_ltac_selector 
                                         ;
                                         }
let _ = (wit_ltac_selector, ltac_selector)


# 401 "g_ltac.mlg"
 

let pr_ltac_info n = str "Info" ++ spc () ++ int n



let (wit_ltac_info, ltac_info) = Vernacextend.vernac_argument_extend ~name:"ltac_info" 
                                 {
                                 Vernacextend.arg_parsing = Vernacextend.Arg_rules (
                                                            [(Extend.Rule
                                                              (Extend.Next 
                                                               (Extend.Next 
                                                               (Extend.Stop,
                                                               (Extend.Atoken (CLexer.terminal "Info"))),
                                                               (Extend.Aentry natural)),
                                                              (fun n _ loc ->
                                                              
# 408 "g_ltac.mlg"
                             n 
                                                              )))]);
                                 Vernacextend.arg_printer = fun env sigma -> 
                                 
# 407 "g_ltac.mlg"
                                              pr_ltac_info 
                                 ;
                                 }
let _ = (wit_ltac_info, ltac_info)


# 411 "g_ltac.mlg"
 

let pr_ltac_use_default b =
  if b then (* Bug: a space is inserted before "..." *) str ".." else mt ()



let (wit_ltac_use_default, ltac_use_default) = Vernacextend.vernac_argument_extend ~name:"ltac_use_default" 
                                               {
                                               Vernacextend.arg_parsing = 
                                               Vernacextend.Arg_rules (
                                               [(Extend.Rule
                                                 (Extend.Next (Extend.Stop,
                                                              (Extend.Atoken (CLexer.terminal "..."))),
                                                 (fun _ loc -> 
# 420 "g_ltac.mlg"
                 true 
                                                               )));
                                               (Extend.Rule
                                                (Extend.Next (Extend.Stop,
                                                             (Extend.Atoken (CLexer.terminal "."))),
                                                (fun _ loc -> 
# 419 "g_ltac.mlg"
               false 
                                                              )))]);
                                               Vernacextend.arg_printer = fun env sigma -> 
                                               
# 418 "g_ltac.mlg"
                                                     pr_ltac_use_default 
                                               ;
                                               }
let _ = (wit_ltac_use_default, ltac_use_default)


# 423 "g_ltac.mlg"
 

let is_anonymous_abstract = function
  | TacAbstract (_,None) -> true
  | TacSolve [TacAbstract (_,None)] -> true
  | _ -> false
let rm_abstract = function
  | TacAbstract (t,_) -> t
  | TacSolve [TacAbstract (t,_)] -> TacSolve [t]
  | x -> x
let is_explicit_terminator = function TacSolve _ -> true | _ -> false



let () = Vernacextend.vernac_extend ~command:"VernacSolve"  ?entry:(Some ( tactic_mode )) 
         [(Vernacextend.TyML (false, Vernacextend.TyNonTerminal (Extend.TUopt (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_ltac_selector)), 
                                     Vernacextend.TyNonTerminal (Extend.TUopt (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_ltac_info)), 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_ltac_use_default), 
                                     Vernacextend.TyNil)))), (let coqpp_body g
                                                             n t def
                                                             () ~st = 
                                                             let proof = (
                                                             
# 439 "g_ltac.mlg"
                                  
    let g = Option.default (Goal_select.get_default_goal_selector ()) g in
    Vernacentries.vernac_require_open_proof vernac_solve g n t def
  
                                                             ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun g
                                                             n t def ~atts
                                                             ~st
                                                             -> coqpp_body g
                                                             n t def
                                                             (Attributes.unsupported_attributes atts) ~st), Some 
         (fun g n t def -> 
# 439 "g_ltac.mlg"
      classify_as_proofstep 
         )));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("par", Vernacextend.TyTerminal (":", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUopt (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_ltac_info)), 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_ltac_use_default), 
                                                                    Vernacextend.TyNil))))), 
         (let coqpp_body n t def
         () ~st = let proof = (
# 451 "g_ltac.mlg"
          
      let t = rm_abstract t in
      Vernacentries.vernac_require_open_proof vernac_solve Goal_select.SelectAll n t def
    
                  ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun n
         t def ~atts ~st -> coqpp_body n t def
         (Attributes.unsupported_attributes atts) ~st), Some (fun n t def
                                                             -> 
# 444 "g_ltac.mlg"
     
      let anon_abstracting_tac = is_anonymous_abstract t in
      let solving_tac = is_explicit_terminator t in
      let parallel = `Yes (solving_tac,anon_abstracting_tac) in
      let pbr = if solving_tac then Some "par" else None in
      VtProofStep{ parallel = parallel; proof_block_detection = pbr },
      VtLater
    
                                                             )))]


# 457 "g_ltac.mlg"
 

let pr_ltac_tactic_level n = str "(at level " ++ int n ++ str ")"



let (wit_ltac_tactic_level, ltac_tactic_level) = Vernacextend.vernac_argument_extend ~name:"ltac_tactic_level" 
                                                 {
                                                 Vernacextend.arg_parsing = 
                                                 Vernacextend.Arg_rules (
                                                 [(Extend.Rule
                                                   (Extend.Next (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "("))),
                                                                (Extend.Atoken (CLexer.terminal "at"))),
                                                                (Extend.Atoken (CLexer.terminal "level"))),
                                                                (Extend.Aentry natural)),
                                                                (Extend.Atoken (CLexer.terminal ")"))),
                                                   (fun _ n _ _ _ loc -> 
# 464 "g_ltac.mlg"
                                           n 
                                                                    )))]);
                                                 Vernacextend.arg_printer = fun env sigma -> 
                                                 
# 463 "g_ltac.mlg"
                                                      pr_ltac_tactic_level 
                                                 ;
                                                 }
let _ = (wit_ltac_tactic_level, ltac_tactic_level)

let (wit_ltac_production_sep, ltac_production_sep) = Vernacextend.vernac_argument_extend ~name:"ltac_production_sep" 
                                                     {
                                                     Vernacextend.arg_parsing = 
                                                     Vernacextend.Arg_rules (
                                                     [(Extend.Rule
                                                       (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal ","))),
                                                                    (Extend.Aentry string)),
                                                       (fun sep _ loc -> 
# 468 "g_ltac.mlg"
                           sep 
                                                                    )))]);
                                                     Vernacextend.arg_printer = fun env sigma -> 
                                                     fun _ -> Pp.str "missing printer";
                                                     }
let _ = (wit_ltac_production_sep, ltac_production_sep)


# 471 "g_ltac.mlg"
 

let pr_ltac_production_item = function
| Tacentries.TacTerm s -> quote (str s)
| Tacentries.TacNonTerm (_, ((arg, None), None)) -> str arg
| Tacentries.TacNonTerm (_, ((arg, Some _), None)) -> assert false
| Tacentries.TacNonTerm (_, ((arg, sep), Some id)) ->
  let sep = match sep with
  | None -> mt ()
  | Some sep -> str "," ++ spc () ++ quote (str sep)
  in
  str arg ++ str "(" ++ Id.print id ++ sep ++ str ")"



let (wit_ltac_production_item, ltac_production_item) = Vernacextend.vernac_argument_extend ~name:"ltac_production_item" 
                                                       {
                                                       Vernacextend.arg_parsing = 
                                                       Vernacextend.Arg_rules (
                                                       [(Extend.Rule
                                                         (Extend.Next 
                                                          (Extend.Stop,
                                                          (Extend.Aentry ident)),
                                                         (fun nt loc -> 
# 491 "g_ltac.mlg"
    Tacentries.TacNonTerm (Loc.tag ~loc ((Id.to_string nt, None), None)) 
                                                                    )));
                                                       (Extend.Rule
                                                        (Extend.Next 
                                                         (Extend.Next 
                                                         (Extend.Next 
                                                         (Extend.Next 
                                                         (Extend.Next 
                                                         (Extend.Stop,
                                                         (Extend.Aentry ident)),
                                                         (Extend.Atoken (CLexer.terminal "("))),
                                                         (Extend.Aentry ident)),
                                                         (Extend.Aopt (Extend.Aentry ltac_production_sep))),
                                                         (Extend.Atoken (CLexer.terminal ")"))),
                                                        (fun _ sep p _ nt
                                                        loc -> 
# 489 "g_ltac.mlg"
    Tacentries.TacNonTerm (Loc.tag ~loc ((Id.to_string nt, sep), Some p)) 
                                                               )));
                                                       (Extend.Rule
                                                        (Extend.Next 
                                                         (Extend.Stop,
                                                         (Extend.Aentry string)),
                                                        (fun s loc -> 
# 487 "g_ltac.mlg"
                     Tacentries.TacTerm s 
                                                                    )))]);
                                                       Vernacextend.arg_printer = fun env sigma -> 
                                                       
# 486 "g_ltac.mlg"
                                                         pr_ltac_production_item 
                                                       ;
                                                       }
let _ = (wit_ltac_production_item, ltac_production_item)

let () = Vernacextend.vernac_extend ~command:"VernacTacticNotation"  ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Tactic", 
                                     Vernacextend.TyTerminal ("Notation", 
                                     Vernacextend.TyNonTerminal (Extend.TUopt (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_ltac_tactic_level)), 
                                     Vernacextend.TyNonTerminal (Extend.TUlist1 (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_ltac_production_item)), 
                                     Vernacextend.TyTerminal (":=", Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                                                    Vernacextend.TyNil)))))), 
         (let coqpp_body n r e
         (deprecation, locality) ~st = let () = 
# 498 "g_ltac.mlg"
   
    let n = Option.default 0 n in
    Tacentries.add_tactic_notation (Locality.make_module_locality locality) n ?deprecation r e;
  
                                        in st in fun n
         r e ~atts ~st -> coqpp_body n r e
         (Attributes.parse Attributes.Notations.(deprecation ++ locality) atts) ~st), Some 
         (fun n r e -> 
# 497 "g_ltac.mlg"
    VtSideff [], VtNow 
         )))]

let () = Vernacextend.vernac_extend ~command:"VernacPrintLtac" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Print", 
                                     Vernacextend.TyTerminal ("Ltac", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_reference), 
                                     Vernacextend.TyNil))), (let coqpp_body r
                                                            () ~st = 
                                                            let () = 
                                                            
# 506 "g_ltac.mlg"
    Feedback.msg_notice (Tacintern.print_ltac r) 
                                                             in st in fun r
                                                            ~atts ~st
                                                            -> coqpp_body r
                                                            (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"VernacLocateLtac" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Locate", 
                                     Vernacextend.TyTerminal ("Ltac", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_reference), 
                                     Vernacextend.TyNil))), (let coqpp_body r
                                                            () ~st = 
                                                            let () = 
                                                            
# 511 "g_ltac.mlg"
    Tacentries.print_located_tactic r 
                                                             in st in fun r
                                                            ~atts ~st
                                                            -> coqpp_body r
                                                            (Attributes.unsupported_attributes atts) ~st), None))]


# 514 "g_ltac.mlg"
 

let pr_ltac_ref = Libnames.pr_qualid

let pr_tacdef_body env sigma tacdef_body =
  let id, redef, body =
    match tacdef_body with
    | TacticDefinition ({CAst.v=id}, body) -> Id.print id, false, body
    | TacticRedefinition (id, body) -> pr_ltac_ref id, true, body
  in
  let idl, body =
    match body with
      | Tacexpr.TacFun (idl,b) -> idl,b
      | _ -> [], body in
  id ++
    prlist (function Name.Anonymous -> str " _"
      | Name.Name id -> spc () ++ Id.print id) idl
  ++ (if redef then str" ::=" else str" :=") ++ brk(1,1)
  ++ Pptactic.pr_raw_tactic env sigma body



let (wit_ltac_tacdef_body, ltac_tacdef_body) = Vernacextend.vernac_argument_extend ~name:"ltac_tacdef_body" 
                                               {
                                               Vernacextend.arg_parsing = 
                                               Vernacextend.Arg_alias (tacdef_body);
                                               Vernacextend.arg_printer = fun env sigma -> 
                                               
# 537 "g_ltac.mlg"
             pr_tacdef_body env sigma 
                                               ;
                                               }
let _ = (wit_ltac_tacdef_body, ltac_tacdef_body)

let () = Vernacextend.vernac_extend ~command:"VernacDeclareTacticDefinition"  ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Ltac", 
                                     Vernacextend.TyNonTerminal (Extend.TUlist1sep (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_ltac_tacdef_body), "with"), 
                                     Vernacextend.TyNil)), (let coqpp_body l
                                                           (deprecation, locality) ~st = 
                                                           let () = 
# 546 "g_ltac.mlg"
        
         Tacentries.register_ltac (Locality.make_module_locality locality) ?deprecation l;
  
                                                            in st in fun l
                                                           ~atts ~st
                                                           -> coqpp_body l
                                                           (Attributes.parse Attributes.Notations.(deprecation ++ locality) atts) ~st), Some 
         (fun l -> 
# 542 "g_ltac.mlg"
                                                                                     
    VtSideff (List.map (function
      | TacticDefinition ({CAst.v=r},_) -> r
      | TacticRedefinition (qid,_) -> qualid_basename qid) l), VtLater
  
         )))]

let () = Vernacextend.vernac_extend ~command:"VernacPrintLtacs" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Print", 
                                     Vernacextend.TyTerminal ("Ltac", 
                                     Vernacextend.TyTerminal ("Signatures", 
                                     Vernacextend.TyNil))), (let coqpp_body () ~st = 
                                                            let () = 
                                                            
# 552 "g_ltac.mlg"
                                       Tacentries.print_ltacs () 
                                                             in st in fun ~atts
                                                            ~st
                                                            -> coqpp_body (Attributes.unsupported_attributes atts) ~st), None))]

