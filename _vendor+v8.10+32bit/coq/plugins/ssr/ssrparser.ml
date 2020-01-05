
# 13 "ssrparser.mlg"
 

let _vmcast = Constr.VMcast
open Names
open Pp
open Pcoq
open Ltac_plugin
open Stdarg
open Tacarg
open Libnames
open Tactics
open Tacmach
open Util
open Locus
open Tacexpr
open Tacinterp
open Pltac
open Extraargs
open Ppconstr

open Namegen
open Tactypes
open Decl_kinds
open Constrexpr
open Constrexpr_ops

open Proofview
open Proofview.Notations

open Ssrprinters
open Ssrcommon
open Ssrtacticals
open Ssrbwd
open Ssrequality
open Ssripats

(** Ssreflect load check. *)

(* To allow ssrcoq to be fully compatible with the "plain" Coq, we only *)
(* turn on its incompatible features (the new rewrite syntax, and the   *)
(* reserved identifiers) when the theory library (ssreflect.v) has      *)
(* has actually been required, or is being defined. Because this check  *)
(* needs to be done often (for each identifier lookup), we implement    *)
(* some caching, repeating the test only when the environment changes.  *)
(*   We check for protect_term because it is the first constant loaded; *)
(* ssr_have would ultimately be a better choice.                        *)
let ssr_loaded = Summary.ref ~name:"SSR:loaded" false
let is_ssr_loaded () =
  !ssr_loaded ||
  (if CLexer.is_keyword "SsrSyntax_is_Imported" then ssr_loaded:=true;
   !ssr_loaded)



let __coq_plugin_name = "ssreflect_plugin"
let _ = Mltop.add_known_module __coq_plugin_name

# 69 "ssrparser.mlg"
 

(* Defining grammar rules with "xx" in it automatically declares keywords too,
 * we thus save the lexer to restore it at the end of the file *)
let frozen_lexer = CLexer.get_keyword_state () ;;

let tacltop = (5,Notation_gram.E)

let pr_ssrtacarg env sigma _ _ prt = prt env sigma tacltop



let (wit_ssrtacarg, ssrtacarg) = Tacentries.argument_extend ~name:"ssrtacarg" 
                                 {
                                 Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                          [(Extend.Rule
                                                            (Extend.Next 
                                                             (Extend.Stop,
                                                             (Extend.Atoken (CLexer.terminal "YouShouldNotTypeThis"))),
                                                            (fun _ loc -> 
# 82 "ssrparser.mlg"
                                  CErrors.anomaly (Pp.str "Grammar placeholder match") 
                                                                    )))]);
                                 Tacentries.arg_tag = Some
                                                      (Geninterp.val_tag (Genarg.topwit wit_tactic));
                                 Tacentries.arg_intern = Tacentries.ArgInternWit (wit_tactic);
                                 Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_tactic);
                                 Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_tactic);
                                 Tacentries.arg_printer = ((fun env sigma -> 
                                                          
# 81 "ssrparser.mlg"
                                                       pr_ssrtacarg env sigma 
                                                          ), (fun env sigma -> 
                                                          
# 81 "ssrparser.mlg"
                                                       pr_ssrtacarg env sigma 
                                                          ), (fun env sigma -> 
                                                          
# 81 "ssrparser.mlg"
                                                       pr_ssrtacarg env sigma 
                                                          ));
                                 }
let _ = (wit_ssrtacarg, ssrtacarg)

let _ = let () =
        Pcoq.grammar_extend ssrtacarg None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Aentryl (tactic_expr, "5"))),
                 (fun tac loc -> 
# 86 "ssrparser.mlg"
                                                 tac 
                                 ))])])
        in ()

let (wit_ssrtac3arg, ssrtac3arg) = Tacentries.argument_extend ~name:"ssrtac3arg" 
                                   {
                                   Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                            [(Extend.Rule
                                                              (Extend.Next 
                                                               (Extend.Stop,
                                                               (Extend.Atoken (CLexer.terminal "YouShouldNotTypeThis"))),
                                                              (fun _ loc -> 
# 91 "ssrparser.mlg"
                                  CErrors.anomaly (Pp.str "Grammar placeholder match") 
                                                                    )))]);
                                   Tacentries.arg_tag = Some
                                                        (Geninterp.val_tag (Genarg.topwit wit_tactic));
                                   Tacentries.arg_intern = Tacentries.ArgInternWit (wit_tactic);
                                   Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_tactic);
                                   Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_tactic);
                                   Tacentries.arg_printer = ((fun env sigma -> 
                                                            
# 90 "ssrparser.mlg"
                                                        pr_ssrtacarg env sigma 
                                                            ), (fun env sigma -> 
                                                            
# 90 "ssrparser.mlg"
                                                        pr_ssrtacarg env sigma 
                                                            ), (fun env sigma -> 
                                                            
# 90 "ssrparser.mlg"
                                                        pr_ssrtacarg env sigma 
                                                            ));
                                   }
let _ = (wit_ssrtac3arg, ssrtac3arg)

let _ = let () =
        Pcoq.grammar_extend ssrtac3arg None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Aentryl (tactic_expr, "3"))),
                 (fun tac loc -> 
# 95 "ssrparser.mlg"
                                                  tac 
                                 ))])])
        in ()


# 98 "ssrparser.mlg"
 

(* Lexically closed tactic for tacticals. *)
let pr_ssrtclarg env sigma _ _ prt tac = prt env sigma tacltop tac



let (wit_ssrtclarg, ssrtclarg) = Tacentries.argument_extend ~name:"ssrtclarg" 
                                 {
                                 Tacentries.arg_parsing = Vernacextend.Arg_alias (ssrtacarg);
                                 Tacentries.arg_tag = Some
                                                      (Geninterp.val_tag (Genarg.topwit wit_ssrtacarg));
                                 Tacentries.arg_intern = Tacentries.ArgInternWit (wit_ssrtacarg);
                                 Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ssrtacarg);
                                 Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_ssrtacarg);
                                 Tacentries.arg_printer = ((fun env sigma -> 
                                                          
# 106 "ssrparser.mlg"
                 pr_ssrtclarg env sigma 
                                                          ), (fun env sigma -> 
                                                          
# 106 "ssrparser.mlg"
                 pr_ssrtclarg env sigma 
                                                          ), (fun env sigma -> 
                                                          
# 106 "ssrparser.mlg"
                 pr_ssrtclarg env sigma 
                                                          ));
                                 }
let _ = (wit_ssrtclarg, ssrtclarg)


# 110 "ssrparser.mlg"
 

open Genarg

(** Adding a new uninterpreted generic argument type *)
let add_genarg tag pr =
  let wit = Genarg.make0 tag in
  let tag = Geninterp.Val.create tag in
  let glob ist x = (ist, x) in
  let subst _ x = x in
  let interp ist x = Ftactic.return (Geninterp.Val.Dyn (tag, x)) in
  let gen_pr env sigma _ _ _ = pr env sigma in
  let () = Genintern.register_intern0 wit glob in
  let () = Genintern.register_subst0 wit subst in
  let () = Geninterp.register_interp0 wit interp in
  let () = Geninterp.register_val0 wit (Some (Geninterp.Val.Base tag)) in
  Pptactic.declare_extra_genarg_pprule wit gen_pr gen_pr gen_pr;
  wit

(** Primitive parsing to avoid syntax conflicts with basic tactics. *)

let accept_before_syms syms strm =
  match Util.stream_nth 1 strm with
  | Tok.KEYWORD sym when List.mem sym syms -> ()
  | _ -> raise Stream.Failure

let accept_before_syms_or_any_id syms strm =
  match Util.stream_nth 1 strm with
  | Tok.KEYWORD sym when List.mem sym syms -> ()
  | Tok.IDENT _ -> ()
  | _ -> raise Stream.Failure

let accept_before_syms_or_ids syms ids strm =
  match Util.stream_nth 1 strm with
  | Tok.KEYWORD sym when List.mem sym syms -> ()
  | Tok.IDENT id when List.mem id ids -> ()
  | _ -> raise Stream.Failure

open Ssrast
let pr_id = Ppconstr.pr_id
let pr_name = function Name id -> pr_id id | Anonymous -> str "_"
let pr_spc () = str " "
let pr_list = prlist_with_sep

(**************************** ssrhyp **************************************)

let pr_ssrhyp _ _ _ = pr_hyp

let wit_ssrhyprep = add_genarg "ssrhyprep" (fun env sigma -> pr_hyp)

let intern_hyp ist (SsrHyp (loc, id) as hyp) =
  let _ = Tacintern.intern_genarg ist (in_gen (rawwit wit_var) CAst.(make ?loc id)) in
  if not_section_id id then hyp else
  hyp_err ?loc "Can't clear section hypothesis " id

open Pcoq.Prim



let (wit_ssrhyp, ssrhyp) = Tacentries.argument_extend ~name:"ssrhyp" 
                           {
                           Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                    [(Extend.Rule
                                                      (Extend.Next (Extend.Stop,
                                                                   (Extend.Aentry ident)),
                                                      (fun id loc -> 
# 172 "ssrparser.mlg"
                       SsrHyp (Loc.tag ~loc id) 
                                                                    )))]);
                           Tacentries.arg_tag = Some
                                                (Geninterp.val_tag (Genarg.topwit wit_ssrhyprep));
                           Tacentries.arg_intern = Tacentries.ArgInternFun ((fun f ist v -> (ist, f ist v)) (
                                                   
# 171 "ssrparser.mlg"
                                       intern_hyp 
                                                   ));
                           Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ssrhyprep);
                           Tacentries.arg_interp = Tacentries.ArgInterpLegacy (
                                                   
# 170 "ssrparser.mlg"
                                        interp_hyp 
                                                   );
                           Tacentries.arg_printer = ((fun env sigma -> 
                                                    
# 169 "ssrparser.mlg"
                                                       pr_ssrhyp 
                                                    ), (fun env sigma -> 
                                                    
# 169 "ssrparser.mlg"
                                                       pr_ssrhyp 
                                                    ), (fun env sigma -> 
                                                    
# 169 "ssrparser.mlg"
                                                       pr_ssrhyp 
                                                    ));
                           }
let _ = (wit_ssrhyp, ssrhyp)


# 175 "ssrparser.mlg"
 

let pr_hoi = hoik pr_hyp
let pr_ssrhoi _ _ _ = pr_hoi

let wit_ssrhoirep = add_genarg "ssrhoirep" (fun env sigma -> pr_hoi)

let intern_ssrhoi ist = function
  | Hyp h -> Hyp (intern_hyp ist h)
  | Id (SsrHyp (_, id)) as hyp ->
    let _ = Tacintern.intern_genarg ist (in_gen (rawwit wit_ident) id) in
    hyp

let interp_ssrhoi ist gl = function
  | Hyp h -> let s, h' = interp_hyp ist gl h in s, Hyp h'
  | Id (SsrHyp (loc, id)) ->
    let s, id' = interp_wit wit_ident ist gl id in
    s, Id (SsrHyp (loc, id'))



let (wit_ssrhoi_hyp, ssrhoi_hyp) = Tacentries.argument_extend ~name:"ssrhoi_hyp" 
                                   {
                                   Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                            [(Extend.Rule
                                                              (Extend.Next 
                                                               (Extend.Stop,
                                                               (Extend.Aentry ident)),
                                                              (fun id loc ->
                                                              
# 199 "ssrparser.mlg"
                       Hyp (SsrHyp(Loc.tag ~loc id)) 
                                                              )))]);
                                   Tacentries.arg_tag = Some
                                                        (Geninterp.val_tag (Genarg.topwit wit_ssrhoirep));
                                   Tacentries.arg_intern = Tacentries.ArgInternFun ((fun f ist v -> (ist, f ist v)) (
                                                           
# 198 "ssrparser.mlg"
                                       intern_ssrhoi 
                                                           ));
                                   Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ssrhoirep);
                                   Tacentries.arg_interp = Tacentries.ArgInterpLegacy (
                                                           
# 197 "ssrparser.mlg"
                                        interp_ssrhoi 
                                                           );
                                   Tacentries.arg_printer = ((fun env sigma -> 
                                                            
# 196 "ssrparser.mlg"
                                                           pr_ssrhoi 
                                                            ), (fun env sigma -> 
                                                            
# 196 "ssrparser.mlg"
                                                           pr_ssrhoi 
                                                            ), (fun env sigma -> 
                                                            
# 196 "ssrparser.mlg"
                                                           pr_ssrhoi 
                                                            ));
                                   }
let _ = (wit_ssrhoi_hyp, ssrhoi_hyp)

let (wit_ssrhoi_id, ssrhoi_id) = Tacentries.argument_extend ~name:"ssrhoi_id" 
                                 {
                                 Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                          [(Extend.Rule
                                                            (Extend.Next 
                                                             (Extend.Stop,
                                                             (Extend.Aentry ident)),
                                                            (fun id loc -> 
# 204 "ssrparser.mlg"
                       Id (SsrHyp(Loc.tag ~loc id)) 
                                                                    )))]);
                                 Tacentries.arg_tag = Some
                                                      (Geninterp.val_tag (Genarg.topwit wit_ssrhoirep));
                                 Tacentries.arg_intern = Tacentries.ArgInternFun ((fun f ist v -> (ist, f ist v)) (
                                                         
# 203 "ssrparser.mlg"
                                       intern_ssrhoi 
                                                         ));
                                 Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ssrhoirep);
                                 Tacentries.arg_interp = Tacentries.ArgInterpLegacy (
                                                         
# 202 "ssrparser.mlg"
                                        interp_ssrhoi 
                                                         );
                                 Tacentries.arg_printer = ((fun env sigma -> 
                                                          
# 201 "ssrparser.mlg"
                                                          pr_ssrhoi 
                                                          ), (fun env sigma -> 
                                                          
# 201 "ssrparser.mlg"
                                                          pr_ssrhoi 
                                                          ), (fun env sigma -> 
                                                          
# 201 "ssrparser.mlg"
                                                          pr_ssrhoi 
                                                          ));
                                 }
let _ = (wit_ssrhoi_id, ssrhoi_id)


# 207 "ssrparser.mlg"
 

let pr_ssrhyps _ _ _ = pr_hyps



let (wit_ssrhyps, ssrhyps) = Tacentries.argument_extend ~name:"ssrhyps" 
                             {
                             Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                      [(Extend.Rule
                                                        (Extend.Next 
                                                         (Extend.Stop,
                                                         (Extend.Alist0 (Extend.Aentry ssrhyp))),
                                                        (fun hyps loc -> 
# 215 "ssrparser.mlg"
                               check_hyps_uniq [] hyps; hyps 
                                                                    )))]);
                             Tacentries.arg_tag = Some
                                                  (Geninterp.Val.List 
                                                  (Geninterp.val_tag (Genarg.topwit wit_ssrhyp)));
                             Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.ListArg 
                                                     (wit_ssrhyp));
                             Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.ListArg 
                                                    (wit_ssrhyp));
                             Tacentries.arg_interp = Tacentries.ArgInterpLegacy (
                                                     
# 214 "ssrparser.mlg"
                                         interp_hyps 
                                                     );
                             Tacentries.arg_printer = ((fun env sigma -> 
                                                      
# 213 "ssrparser.mlg"
                                                          pr_ssrhyps 
                                                      ), (fun env sigma -> 
                                                      
# 213 "ssrparser.mlg"
                                                          pr_ssrhyps 
                                                      ), (fun env sigma -> 
                                                      
# 213 "ssrparser.mlg"
                                                          pr_ssrhyps 
                                                      ));
                             }
let _ = (wit_ssrhyps, ssrhyps)


# 220 "ssrparser.mlg"
 

let pr_rwdir = function L2R -> mt() | R2L -> str "-"

let wit_ssrdir = add_genarg "ssrdir" (fun env sigma -> pr_dir)

(** Simpl switch *)

let pr_ssrsimpl _ _ _ = pr_simpl

let wit_ssrsimplrep = add_genarg "ssrsimplrep" (fun env sigma -> pr_simpl)

let test_ssrslashnum b1 b2 strm =
  match Util.stream_nth 0 strm with
  | Tok.KEYWORD "/" ->
      (match Util.stream_nth 1 strm with
      | Tok.NUMERAL _ when b1 ->
         (match Util.stream_nth 2 strm with
         | Tok.KEYWORD "=" | Tok.KEYWORD "/=" when not b2 -> ()
         | Tok.KEYWORD "/" ->
             if not b2 then () else begin
               match Util.stream_nth 3 strm with
               | Tok.NUMERAL _ -> ()
               | _ -> raise Stream.Failure
             end
         | _ -> raise Stream.Failure)
      | Tok.KEYWORD "/" when not b1 ->
         (match Util.stream_nth 2 strm with
         | Tok.KEYWORD "=" when not b2 -> ()
         | Tok.NUMERAL _ when b2 ->
           (match Util.stream_nth 3 strm with
           | Tok.KEYWORD "=" -> ()
           | _ -> raise Stream.Failure)
         | _ when not b2 -> ()
         | _ -> raise Stream.Failure)
      | Tok.KEYWORD "=" when not b1 && not b2 -> ()
      | _ -> raise Stream.Failure)
  | Tok.KEYWORD "//" when not b1 ->
         (match Util.stream_nth 1 strm with
         | Tok.KEYWORD "=" when not b2 -> ()
         | Tok.NUMERAL _ when b2 ->
           (match Util.stream_nth 2 strm with
           | Tok.KEYWORD "=" -> ()
           | _ -> raise Stream.Failure)
         | _ when not b2 -> ()
         | _ -> raise Stream.Failure)
  | _ -> raise Stream.Failure

let test_ssrslashnum10 = test_ssrslashnum true false
let test_ssrslashnum11 = test_ssrslashnum true true
let test_ssrslashnum01 = test_ssrslashnum false true
let test_ssrslashnum00 = test_ssrslashnum false false

let negate_parser f x =
  let rc = try Some (f x) with Stream.Failure -> None in
  match rc with
  | None -> ()
  | Some _ -> raise Stream.Failure

let test_not_ssrslashnum =
  Pcoq.Entry.of_parser
    "test_not_ssrslashnum" (negate_parser test_ssrslashnum10)
let test_ssrslashnum00 =
  Pcoq.Entry.of_parser "test_ssrslashnum01" test_ssrslashnum00
let test_ssrslashnum10 =
  Pcoq.Entry.of_parser "test_ssrslashnum10" test_ssrslashnum10
let test_ssrslashnum11 =
  Pcoq.Entry.of_parser "test_ssrslashnum11" test_ssrslashnum11
let test_ssrslashnum01 =
  Pcoq.Entry.of_parser "test_ssrslashnum01" test_ssrslashnum01



let (wit_ssrsimpl_ne, ssrsimpl_ne) = Tacentries.argument_extend ~name:"ssrsimpl_ne" 
                                     {
                                     Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                              [(Extend.Rule
                                                                (Extend.Next 
                                                                 (Extend.Stop,
                                                                 (Extend.Atoken (CLexer.terminal "/="))),
                                                                (fun _ loc ->
                                                                
# 295 "ssrparser.mlg"
                Simpl ~-1 
                                                                )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "//="))),
                                                               (fun _ loc ->
                                                               
# 294 "ssrparser.mlg"
                 SimplCut (~-1,~-1) 
                                                               )))]);
                                     Tacentries.arg_tag = Some
                                                          (Geninterp.val_tag (Genarg.topwit wit_ssrsimplrep));
                                     Tacentries.arg_intern = Tacentries.ArgInternWit (wit_ssrsimplrep);
                                     Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ssrsimplrep);
                                     Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_ssrsimplrep);
                                     Tacentries.arg_printer = ((fun env sigma -> 
                                                              
# 293 "ssrparser.mlg"
                                                              pr_ssrsimpl 
                                                              ), (fun env sigma -> 
                                                              
# 293 "ssrparser.mlg"
                                                              pr_ssrsimpl 
                                                              ), (fun env sigma -> 
                                                              
# 293 "ssrparser.mlg"
                                                              pr_ssrsimpl 
                                                              ));
                                     }
let _ = (wit_ssrsimpl_ne, ssrsimpl_ne)

let _ = let () =
        Pcoq.grammar_extend ssrsimpl_ne None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry test_ssrslashnum00)),
                              (Extend.Atoken (Tok.PKEYWORD ("//")))),
                 (fun _ _ loc -> 
# 308 "ssrparser.mlg"
                                    Cut ~-1 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Aentry test_ssrslashnum01)),
                                                       (Extend.Atoken (Tok.PKEYWORD ("//")))),
                                          (Extend.Aentry natural)),
                             (Extend.Atoken (Tok.PKEYWORD ("=")))),
                (fun _ m _ _ loc -> 
# 307 "ssrparser.mlg"
                                                      SimplCut (~-1,m) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Aentry test_ssrslashnum10)),
                                                                    (Extend.Atoken (Tok.PKEYWORD ("/")))),
                                                       (Extend.Aentry natural)),
                                          (Extend.Atoken (Tok.PKEYWORD ("/")))),
                             (Extend.Atoken (Tok.PKEYWORD ("=")))),
                (fun _ _ n _ _ loc -> 
# 306 "ssrparser.mlg"
                                                          SimplCut (n,~-1) 
                                      ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Aentry test_ssrslashnum10)),
                                                       (Extend.Atoken (Tok.PKEYWORD ("/")))),
                                          (Extend.Aentry natural)),
                             (Extend.Atoken (Tok.PKEYWORD ("/=")))),
                (fun _ n _ _ loc -> 
# 305 "ssrparser.mlg"
                                                      SimplCut (n,~-1) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Aentry test_ssrslashnum10)),
                                                       (Extend.Atoken (Tok.PKEYWORD ("/")))),
                                          (Extend.Aentry natural)),
                             (Extend.Atoken (Tok.PKEYWORD ("=")))),
                (fun _ n _ _ loc -> 
# 304 "ssrparser.mlg"
                                                     Simpl n 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Aentry test_ssrslashnum10)),
                                                       (Extend.Atoken (Tok.PKEYWORD ("/")))),
                                          (Extend.Aentry natural)),
                             (Extend.Atoken (Tok.PKEYWORD ("/")))),
                (fun _ n _ _ loc -> 
# 303 "ssrparser.mlg"
                                                     Cut n 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Aentry test_ssrslashnum11)),
                                                                    (Extend.Atoken (Tok.PKEYWORD ("/")))),
                                                                    (Extend.Aentry natural)),
                                                       (Extend.Atoken (Tok.PKEYWORD ("/")))),
                                          (Extend.Aentry natural)),
                             (Extend.Atoken (Tok.PKEYWORD ("=")))),
                (fun _ m _ n _ _ loc -> 
# 302 "ssrparser.mlg"
                                                                       SimplCut(n,m) 
                                        ))])])
        in ()

let (wit_ssrsimpl, ssrsimpl) = Tacentries.argument_extend ~name:"ssrsimpl" 
                               {
                               Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                        [(Extend.Rule
                                                          (Extend.Stop,
                                                          (fun loc -> 
# 315 "ssrparser.mlg"
           Nop 
                                                                    )));
                                                        (Extend.Rule
                                                         (Extend.Next 
                                                          (Extend.Stop,
                                                          (Extend.Aentry ssrsimpl_ne)),
                                                         (fun sim loc -> 
# 314 "ssrparser.mlg"
                            sim 
                                                                    )))]);
                               Tacentries.arg_tag = Some
                                                    (Geninterp.val_tag (Genarg.topwit wit_ssrsimplrep));
                               Tacentries.arg_intern = Tacentries.ArgInternWit (wit_ssrsimplrep);
                               Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ssrsimplrep);
                               Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_ssrsimplrep);
                               Tacentries.arg_printer = ((fun env sigma -> 
                                                        
# 313 "ssrparser.mlg"
                                                           pr_ssrsimpl 
                                                        ), (fun env sigma -> 
                                                        
# 313 "ssrparser.mlg"
                                                           pr_ssrsimpl 
                                                        ), (fun env sigma -> 
                                                        
# 313 "ssrparser.mlg"
                                                           pr_ssrsimpl 
                                                        ));
                               }
let _ = (wit_ssrsimpl, ssrsimpl)


# 318 "ssrparser.mlg"
 

let pr_ssrclear _ _ _ = pr_clear mt



let (wit_ssrclear_ne, ssrclear_ne) = Tacentries.argument_extend ~name:"ssrclear_ne" 
                                     {
                                     Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                              [(Extend.Rule
                                                                (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Stop,
                                                                 (Extend.Atoken (CLexer.terminal "{"))),
                                                                 (Extend.Alist1 (Extend.Aentry ssrhyp))),
                                                                 (Extend.Atoken (CLexer.terminal "}"))),
                                                                (fun _ clr _
                                                                loc -> 
                                                                
# 325 "ssrparser.mlg"
                                       check_hyps_uniq [] clr; clr 
                                                                )))]);
                                     Tacentries.arg_tag = Some
                                                          (Geninterp.val_tag (Genarg.topwit wit_ssrhyps));
                                     Tacentries.arg_intern = Tacentries.ArgInternWit (wit_ssrhyps);
                                     Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ssrhyps);
                                     Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_ssrhyps);
                                     Tacentries.arg_printer = ((fun env sigma -> 
                                                              
# 324 "ssrparser.mlg"
                                                          pr_ssrclear 
                                                              ), (fun env sigma -> 
                                                              
# 324 "ssrparser.mlg"
                                                          pr_ssrclear 
                                                              ), (fun env sigma -> 
                                                              
# 324 "ssrparser.mlg"
                                                          pr_ssrclear 
                                                              ));
                                     }
let _ = (wit_ssrclear_ne, ssrclear_ne)

let (wit_ssrclear, ssrclear) = Tacentries.argument_extend ~name:"ssrclear" 
                               {
                               Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                        [(Extend.Rule
                                                          (Extend.Stop,
                                                          (fun loc -> 
# 330 "ssrparser.mlg"
           [] 
                                                                    )));
                                                        (Extend.Rule
                                                         (Extend.Next 
                                                          (Extend.Stop,
                                                          (Extend.Aentry ssrclear_ne)),
                                                         (fun clr loc -> 
# 329 "ssrparser.mlg"
                            clr 
                                                                    )))]);
                               Tacentries.arg_tag = Some
                                                    (Geninterp.val_tag (Genarg.topwit wit_ssrclear_ne));
                               Tacentries.arg_intern = Tacentries.ArgInternWit (wit_ssrclear_ne);
                               Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ssrclear_ne);
                               Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_ssrclear_ne);
                               Tacentries.arg_printer = ((fun env sigma -> 
                                                        
# 328 "ssrparser.mlg"
                                                           pr_ssrclear 
                                                        ), (fun env sigma -> 
                                                        
# 328 "ssrparser.mlg"
                                                           pr_ssrclear 
                                                        ), (fun env sigma -> 
                                                        
# 328 "ssrparser.mlg"
                                                           pr_ssrclear 
                                                        ));
                               }
let _ = (wit_ssrclear, ssrclear)


# 341 "ssrparser.mlg"
 

let pr_index = function
  | ArgVar {CAst.v=id} -> pr_id id
  | ArgArg n when n > 0 -> int n
  | _ -> mt ()
let pr_ssrindex _ _ _ = pr_index

let noindex = ArgArg 0

let check_index ?loc i =
  if i > 0 then i else CErrors.user_err ?loc (str"Index not positive")
let mk_index ?loc = function
  | ArgArg i -> ArgArg (check_index ?loc i)
  | iv -> iv

let interp_index ist gl idx =
  Tacmach.project gl,
  match idx with
  | ArgArg _ -> idx
  | ArgVar id ->
    let i =
      try
        let v = Id.Map.find id.CAst.v ist.Tacinterp.lfun in
        begin match Tacinterp.Value.to_int v with
        | Some i -> i
        | None ->
        begin match Tacinterp.Value.to_constr v with
        | Some c ->
          let rc = Detyping.detype Detyping.Now false Id.Set.empty (pf_env gl) (project gl) c in
          begin match Notation.uninterp_prim_token rc with
          | _, Constrexpr.Numeral (b,{NumTok.int = s; frac = ""; exp = ""}) ->
             let n = int_of_string s in (match b with SPlus -> n | SMinus -> -n)
          | _ -> raise Not_found
          end
        | None -> raise Not_found
        end end
    with _ -> CErrors.user_err ?loc:id.CAst.loc (str"Index not a number") in
    ArgArg (check_index ?loc:id.CAst.loc i)

open Pltac



let (wit_ssrindex, ssrindex) = Tacentries.argument_extend ~name:"ssrindex" 
                               {
                               Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                        [(Extend.Rule
                                                          (Extend.Next 
                                                           (Extend.Stop,
                                                           (Extend.Aentry int_or_var)),
                                                          (fun i loc -> 
# 387 "ssrparser.mlg"
                         mk_index ~loc i 
                                                                    )))]);
                               Tacentries.arg_tag = None;
                               Tacentries.arg_intern = Tacentries.ArgInternFun (fun ist v -> (ist, v));
                               Tacentries.arg_subst = Tacentries.ArgSubstFun (fun s v -> v);
                               Tacentries.arg_interp = Tacentries.ArgInterpLegacy (
                                                       
# 386 "ssrparser.mlg"
                   interp_index 
                                                       );
                               Tacentries.arg_printer = ((fun env sigma -> 
                                                        
# 385 "ssrparser.mlg"
                                      pr_ssrindex 
                                                        ), (fun env sigma -> 
                                                        
# 385 "ssrparser.mlg"
                                      pr_ssrindex 
                                                        ), (fun env sigma -> 
                                                        
# 385 "ssrparser.mlg"
                                      pr_ssrindex 
                                                        ));
                               }
let _ = (wit_ssrindex, ssrindex)


# 403 "ssrparser.mlg"
 

let pr_ssrocc _ _ _ = pr_occ

open Pcoq.Prim



let (wit_ssrocc, ssrocc) = Tacentries.argument_extend ~name:"ssrocc" 
                           {
                           Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                    [(Extend.Rule
                                                      (Extend.Next (Extend.Next 
                                                                   (Extend.Stop,
                                                                   (Extend.Atoken (CLexer.terminal "+"))),
                                                                   (Extend.Alist0 (Extend.Aentry natural))),
                                                      (fun occ _ loc -> 
# 415 "ssrparser.mlg"
                                     Some (false, occ) 
                                                                    )));
                                                    (Extend.Rule
                                                     (Extend.Next (Extend.Next 
                                                                  (Extend.Stop,
                                                                  (Extend.Atoken (CLexer.terminal "-"))),
                                                                  (Extend.Alist0 (Extend.Aentry natural))),
                                                     (fun occ _ loc -> 
# 414 "ssrparser.mlg"
                                     Some (true, occ) 
                                                                    )));
                                                    (Extend.Rule
                                                     (Extend.Next (Extend.Next 
                                                                  (Extend.Stop,
                                                                  (Extend.Aentry natural)),
                                                                  (Extend.Alist0 (Extend.Aentry natural))),
                                                     (fun occ n loc -> 
# 412 "ssrparser.mlg"
                                       
     Some (false, List.map (check_index ~loc) (n::occ)) 
                                                                    )))]);
                           Tacentries.arg_tag = Some
                                                (Geninterp.Val.Opt (Geninterp.Val.Pair (
                                                                   (Geninterp.val_tag (Genarg.topwit wit_bool)), 
                                                                   (Geninterp.Val.List 
                                                                   (Geninterp.val_tag (Genarg.topwit wit_int))))));
                           Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.OptArg 
                                                   (Genarg.PairArg ((wit_bool), 
                                                   (Genarg.ListArg (wit_int)))));
                           Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.OptArg 
                                                  (Genarg.PairArg ((wit_bool), 
                                                  (Genarg.ListArg (wit_int)))));
                           Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.OptArg 
                                                   (Genarg.PairArg ((wit_bool), 
                                                   (Genarg.ListArg (wit_int)))));
                           Tacentries.arg_printer = ((fun env sigma -> 
                                                    
# 411 "ssrparser.mlg"
                                                                      pr_ssrocc 
                                                    ), (fun env sigma -> 
                                                    
# 411 "ssrparser.mlg"
                                                                      pr_ssrocc 
                                                    ), (fun env sigma -> 
                                                    
# 411 "ssrparser.mlg"
                                                                      pr_ssrocc 
                                                    ));
                           }
let _ = (wit_ssrocc, ssrocc)


# 421 "ssrparser.mlg"
 

let pr_mmod = function May -> str "?" | Must -> str "!" | Once -> mt ()

let wit_ssrmmod = add_genarg "ssrmmod" (fun env sigma -> pr_mmod)
let ssrmmod = Pcoq.create_generic_entry Pcoq.utactic "ssrmmod" (Genarg.rawwit wit_ssrmmod);;



let _ = let () =
        Pcoq.grammar_extend ssrmmod None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Atoken (Tok.PKEYWORD ("?")))),
                 (fun _ loc -> 
# 432 "ssrparser.mlg"
                                                                May 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PLEFTQMARK))),
                (fun _ loc -> 
# 432 "ssrparser.mlg"
                                               May 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("!")))),
                (fun _ loc -> 
# 432 "ssrparser.mlg"
                       Must 
                              ))])])
        in ()


# 437 "ssrparser.mlg"
 

let pr_mult (n, m) =
  if n > 0 && m <> Once then int n ++ pr_mmod m else pr_mmod m

let pr_ssrmult _ _ _ = pr_mult



let (wit_ssrmult_ne, ssrmult_ne) = Tacentries.argument_extend ~name:"ssrmult_ne" 
                                   {
                                   Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                            [(Extend.Rule
                                                              (Extend.Next 
                                                               (Extend.Stop,
                                                               (Extend.Aentry ssrmmod)),
                                                              (fun m loc -> 
# 448 "ssrparser.mlg"
                                   notimes, m 
                                                                    )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Aentry natural)),
                                                              (Extend.Aentry ssrmmod)),
                                                             (fun m n loc ->
                                                             
# 447 "ssrparser.mlg"
                                   check_index ~loc n, m 
                                                             )))]);
                                   Tacentries.arg_tag = Some
                                                        (Geninterp.Val.Pair (
                                                        (Geninterp.val_tag (Genarg.topwit wit_int)), 
                                                        (Geninterp.val_tag (Genarg.topwit wit_ssrmmod))));
                                   Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                           (wit_int), 
                                                           (wit_ssrmmod)));
                                   Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                          (wit_int), 
                                                          (wit_ssrmmod)));
                                   Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                           (wit_int), 
                                                           (wit_ssrmmod)));
                                   Tacentries.arg_printer = ((fun env sigma -> 
                                                            
# 446 "ssrparser.mlg"
                                                                 pr_ssrmult 
                                                            ), (fun env sigma -> 
                                                            
# 446 "ssrparser.mlg"
                                                                 pr_ssrmult 
                                                            ), (fun env sigma -> 
                                                            
# 446 "ssrparser.mlg"
                                                                 pr_ssrmult 
                                                            ));
                                   }
let _ = (wit_ssrmult_ne, ssrmult_ne)

let (wit_ssrmult, ssrmult) = Tacentries.argument_extend ~name:"ssrmult" 
                             {
                             Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                      [(Extend.Rule
                                                        (Extend.Stop,
                                                        (fun loc -> 
# 453 "ssrparser.mlg"
             nomult 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Stop,
                                                                    (Extend.Aentry ssrmult_ne)),
                                                       (fun m loc -> 
# 452 "ssrparser.mlg"
                           m 
                                                                    )))]);
                             Tacentries.arg_tag = Some
                                                  (Geninterp.val_tag (Genarg.topwit wit_ssrmult_ne));
                             Tacentries.arg_intern = Tacentries.ArgInternWit (wit_ssrmult_ne);
                             Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ssrmult_ne);
                             Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_ssrmult_ne);
                             Tacentries.arg_printer = ((fun env sigma -> 
                                                      
# 451 "ssrparser.mlg"
                                                         pr_ssrmult 
                                                      ), (fun env sigma -> 
                                                      
# 451 "ssrparser.mlg"
                                                         pr_ssrmult 
                                                      ), (fun env sigma -> 
                                                      
# 451 "ssrparser.mlg"
                                                         pr_ssrmult 
                                                      ));
                             }
let _ = (wit_ssrmult, ssrmult)


# 456 "ssrparser.mlg"
 

(** Discharge occ switch (combined occurrence / clear switch *)

let pr_docc = function
  | None, occ -> pr_occ occ
  | Some clr, _ -> pr_clear mt clr

let pr_ssrdocc _ _ _ = pr_docc



let (wit_ssrdocc, ssrdocc) = Tacentries.argument_extend ~name:"ssrdocc" 
                             {
                             Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                      [(Extend.Rule
                                                        (Extend.Next 
                                                         (Extend.Next 
                                                         (Extend.Next 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (CLexer.terminal "{"))),
                                                         (Extend.Alist0 (Extend.Aentry ssrhyp))),
                                                         (Extend.Atoken (CLexer.terminal "}"))),
                                                        (fun _ clr _ loc -> 
# 470 "ssrparser.mlg"
                                    mkclr clr 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "{"))),
                                                                    (Extend.Aentry ssrocc)),
                                                                    (Extend.Atoken (CLexer.terminal "}"))),
                                                       (fun _ occ _ loc -> 
# 469 "ssrparser.mlg"
                               mkocc occ 
                                                                    )))]);
                             Tacentries.arg_tag = Some
                                                  (Geninterp.Val.Pair (
                                                  (Geninterp.Val.Opt 
                                                  (Geninterp.val_tag (Genarg.topwit wit_ssrclear))), 
                                                  (Geninterp.val_tag (Genarg.topwit wit_ssrocc))));
                             Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                     (Genarg.OptArg (wit_ssrclear)), 
                                                     (wit_ssrocc)));
                             Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                    (Genarg.OptArg (wit_ssrclear)), 
                                                    (wit_ssrocc)));
                             Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                     (Genarg.OptArg (wit_ssrclear)), 
                                                     (wit_ssrocc)));
                             Tacentries.arg_printer = ((fun env sigma -> 
                                                      
# 468 "ssrparser.mlg"
                                                                         pr_ssrdocc 
                                                      ), (fun env sigma -> 
                                                      
# 468 "ssrparser.mlg"
                                                                         pr_ssrdocc 
                                                      ), (fun env sigma -> 
                                                      
# 468 "ssrparser.mlg"
                                                                         pr_ssrdocc 
                                                      ));
                             }
let _ = (wit_ssrdocc, ssrdocc)


# 473 "ssrparser.mlg"
 

(* Old kinds of terms *)

let input_ssrtermkind strm = match Util.stream_nth 0 strm with
  | Tok.KEYWORD "(" -> xInParens
  | Tok.KEYWORD "@" -> xWithAt
  | _ -> xNoFlag

let ssrtermkind = Pcoq.Entry.of_parser "ssrtermkind" input_ssrtermkind

(* New kinds of terms *)

let input_term_annotation strm =
  match Stream.npeek 2 strm with
  | Tok.KEYWORD "(" :: Tok.KEYWORD "(" :: _ -> `DoubleParens
  | Tok.KEYWORD "(" :: _ -> `Parens
  | Tok.KEYWORD "@" :: _ -> `At
  | _ -> `None
let term_annotation =
  Pcoq.Entry.of_parser "term_annotation" input_term_annotation

(* terms *)

(** Terms parsing. ********************************************************)

(* Because we allow wildcards in term references, we need to stage the *)
(* interpretation of terms so that it occurs at the right time during  *)
(* the execution of the tactic (e.g., so that we don't report an error *)
(* for a term that isn't actually used in the execution).              *)
(*   The term representation tracks whether the concrete initial term  *)
(* started with an opening paren, which might avoid a conflict between *)
(* the ssrreflect term syntax and Gallina notation.                    *)

(* Old terms *)
let pr_ssrterm _ _ _ = pr_term
let glob_ssrterm gs = function
  | k, (_, Some c) -> k, Tacintern.intern_constr gs c
  | ct -> ct
let subst_ssrterm s (k, c) = k, Tacsubst.subst_glob_constr_and_expr s c
let interp_ssrterm _ gl t = Tacmach.project gl, t

open Pcoq.Constr



let (wit_ssrterm, ssrterm) = Tacentries.argument_extend ~name:"ssrterm" 
                             {
                             Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                      [(Extend.Rule
                                                        (Extend.Next 
                                                         (Extend.Next 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (CLexer.terminal "YouShouldNotTypeThis"))),
                                                         (Extend.Aentry constr)),
                                                        (fun c _ loc -> 
# 525 "ssrparser.mlg"
                                            mk_lterm c 
                                                                    )))]);
                             Tacentries.arg_tag = None;
                             Tacentries.arg_intern = Tacentries.ArgInternFun ((fun f ist v -> (ist, f ist v)) (
                                                     
# 522 "ssrparser.mlg"
                     glob_ssrterm 
                                                     ));
                             Tacentries.arg_subst = Tacentries.ArgSubstFun (
# 522 "ssrparser.mlg"
                                                     subst_ssrterm 
                                                    );
                             Tacentries.arg_interp = Tacentries.ArgInterpLegacy (
                                                     
# 521 "ssrparser.mlg"
                      interp_ssrterm 
                                                     );
                             Tacentries.arg_printer = ((fun env sigma -> 
                                                      
# 523 "ssrparser.mlg"
                      pr_ssrterm 
                                                      ), (fun env sigma -> 
                                                      
# 524 "ssrparser.mlg"
                       pr_ssrterm 
                                                      ), (fun env sigma -> 
                                                      
# 520 "ssrparser.mlg"
                  pr_ssrterm 
                                                      ));
                             }
let _ = (wit_ssrterm, ssrterm)

let _ = let () =
        Pcoq.grammar_extend ssrterm None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry ssrtermkind)),
                              (Extend.Aentry Pcoq.Constr.constr)),
                 (fun c k loc -> 
# 530 "ssrparser.mlg"
                                                           mk_term k c 
                                 ))])])
        in ()


# 535 "ssrparser.mlg"
 

let pp_ast_closure_term _ _ _ = pr_ast_closure_term



let (wit_ast_closure_term, ast_closure_term) = Tacentries.argument_extend ~name:"ast_closure_term" 
                                               {
                                               Tacentries.arg_parsing = 
                                               Vernacextend.Arg_rules (
                                               [(Extend.Rule
                                                 (Extend.Next (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Aentry term_annotation)),
                                                              (Extend.Aentry constr)),
                                                 (fun c a loc -> 
# 548 "ssrparser.mlg"
                                          mk_ast_closure_term a c 
                                                                 )))]);
                                               Tacentries.arg_tag = None;
                                               Tacentries.arg_intern = 
                                               Tacentries.ArgInternFun ((fun f ist v -> (ist, f ist v)) (
                                               
# 544 "ssrparser.mlg"
                     glob_ast_closure_term 
                                               ));
                                               Tacentries.arg_subst = 
                                               Tacentries.ArgSubstFun (
# 545 "ssrparser.mlg"
                      subst_ast_closure_term 
                                               );
                                               Tacentries.arg_interp = 
                                               Tacentries.ArgInterpLegacy (
# 543 "ssrparser.mlg"
                      interp_ast_closure_term 
                                               );
                                               Tacentries.arg_printer = 
                                               ((fun env sigma -> 
# 546 "ssrparser.mlg"
                      pp_ast_closure_term 
                                               ), (fun env sigma -> 
# 547 "ssrparser.mlg"
                       pp_ast_closure_term 
                                               ), (fun env sigma -> 
# 542 "ssrparser.mlg"
                  pp_ast_closure_term 
                                               ));
                                               }
let _ = (wit_ast_closure_term, ast_closure_term)

let (wit_ast_closure_lterm, ast_closure_lterm) = Tacentries.argument_extend ~name:"ast_closure_lterm" 
                                                 {
                                                 Tacentries.arg_parsing = 
                                                 Vernacextend.Arg_rules (
                                                 [(Extend.Rule
                                                   (Extend.Next (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Aentry term_annotation)),
                                                                (Extend.Aentry lconstr)),
                                                   (fun c a loc -> 
# 557 "ssrparser.mlg"
                                           mk_ast_closure_term a c 
                                                                   )))]);
                                                 Tacentries.arg_tag = 
                                                 None;
                                                 Tacentries.arg_intern = 
                                                 Tacentries.ArgInternFun ((fun f ist v -> (ist, f ist v)) (
                                                 
# 553 "ssrparser.mlg"
                     glob_ast_closure_term 
                                                 ));
                                                 Tacentries.arg_subst = 
                                                 Tacentries.ArgSubstFun (
# 554 "ssrparser.mlg"
                      subst_ast_closure_term 
                                                 );
                                                 Tacentries.arg_interp = 
                                                 Tacentries.ArgInterpLegacy (
                                                 
# 552 "ssrparser.mlg"
                      interp_ast_closure_term 
                                                 );
                                                 Tacentries.arg_printer = 
                                                 ((fun env sigma -> 
# 555 "ssrparser.mlg"
                      pp_ast_closure_term 
                                                 ), (fun env sigma -> 
                                                 
# 556 "ssrparser.mlg"
                       pp_ast_closure_term 
                                                 ), (fun env sigma -> 
                                                 
# 551 "ssrparser.mlg"
                  pp_ast_closure_term 
                                                 ));
                                                 }
let _ = (wit_ast_closure_lterm, ast_closure_lterm)


# 562 "ssrparser.mlg"
 

let pr_view = pr_list mt (fun c -> str "/" ++ pr_term c)

let pr_ssrbwdview _ _ _ = pr_view



let (wit_ssrbwdview, ssrbwdview) = Tacentries.argument_extend ~name:"ssrbwdview" 
                                   {
                                   Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                            [(Extend.Rule
                                                              (Extend.Next 
                                                               (Extend.Stop,
                                                               (Extend.Atoken (CLexer.terminal "YouShouldNotTypeThis"))),
                                                              (fun _ loc -> 
# 572 "ssrparser.mlg"
                                  [] 
                                                                    )))]);
                                   Tacentries.arg_tag = Some
                                                        (Geninterp.Val.List 
                                                        (Geninterp.val_tag (Genarg.topwit wit_ssrterm)));
                                   Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.ListArg 
                                                           (wit_ssrterm));
                                   Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.ListArg 
                                                          (wit_ssrterm));
                                   Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.ListArg 
                                                           (wit_ssrterm));
                                   Tacentries.arg_printer = ((fun env sigma -> 
                                                            
# 571 "ssrparser.mlg"
                pr_ssrbwdview 
                                                            ), (fun env sigma -> 
                                                            
# 571 "ssrparser.mlg"
                pr_ssrbwdview 
                                                            ), (fun env sigma -> 
                                                            
# 571 "ssrparser.mlg"
                pr_ssrbwdview 
                                                            ));
                                   }
let _ = (wit_ssrbwdview, ssrbwdview)

let _ = let () =
        Pcoq.grammar_extend ssrbwdview None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Aentry test_not_ssrslashnum)),
                                                        (Extend.Atoken (Tok.PKEYWORD ("/")))),
                                           (Extend.Aentry Pcoq.Constr.constr)),
                              (Extend.Aentry ssrbwdview)),
                 (fun w c _ _ loc -> 
# 580 "ssrparser.mlg"
                                                                             
                    (mk_term xNoFlag c) :: w 
                                     ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry test_not_ssrslashnum)),
                                          (Extend.Atoken (Tok.PKEYWORD ("/")))),
                             (Extend.Aentry Pcoq.Constr.constr)),
                (fun c _ _ loc -> 
# 579 "ssrparser.mlg"
                                                              [mk_term xNoFlag c] 
                                  ))])])
        in ()


# 586 "ssrparser.mlg"
 

type ssrfwdview = ast_closure_term list

let pr_ssrfwdview _ _ _ = pr_view2



let (wit_ssrfwdview, ssrfwdview) = Tacentries.argument_extend ~name:"ssrfwdview" 
                                   {
                                   Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                            [(Extend.Rule
                                                              (Extend.Next 
                                                               (Extend.Stop,
                                                               (Extend.Atoken (CLexer.terminal "YouShouldNotTypeThis"))),
                                                              (fun _ loc -> 
# 596 "ssrparser.mlg"
                                  [] 
                                                                    )))]);
                                   Tacentries.arg_tag = Some
                                                        (Geninterp.Val.List 
                                                        (Geninterp.val_tag (Genarg.topwit wit_ast_closure_term)));
                                   Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.ListArg 
                                                           (wit_ast_closure_term));
                                   Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.ListArg 
                                                          (wit_ast_closure_term));
                                   Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.ListArg 
                                                           (wit_ast_closure_term));
                                   Tacentries.arg_printer = ((fun env sigma -> 
                                                            
# 595 "ssrparser.mlg"
                pr_ssrfwdview 
                                                            ), (fun env sigma -> 
                                                            
# 595 "ssrparser.mlg"
                pr_ssrfwdview 
                                                            ), (fun env sigma -> 
                                                            
# 595 "ssrparser.mlg"
                pr_ssrfwdview 
                                                            ));
                                   }
let _ = (wit_ssrfwdview, ssrfwdview)

let _ = let () =
        Pcoq.grammar_extend ssrfwdview None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Aentry test_not_ssrslashnum)),
                                                        (Extend.Atoken (Tok.PKEYWORD ("/")))),
                                           (Extend.Aentry ast_closure_term)),
                              (Extend.Aentry ssrfwdview)),
                 (fun w c _ _ loc -> 
# 604 "ssrparser.mlg"
                                                                            c :: w 
                                     ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry test_not_ssrslashnum)),
                                          (Extend.Atoken (Tok.PKEYWORD ("/")))),
                             (Extend.Aentry ast_closure_term)),
                (fun c _ _ loc -> 
# 603 "ssrparser.mlg"
                                                            [c] 
                                  ))])])
        in ()


# 609 "ssrparser.mlg"
 

let remove_loc x = x.CAst.v

let ipat_of_intro_pattern p = Tactypes.(
  let rec ipat_of_intro_pattern = function
    | IntroNaming (IntroIdentifier id) -> IPatId id
    | IntroAction IntroWildcard -> IPatAnon Drop
    | IntroAction (IntroOrAndPattern (IntroOrPattern iorpat)) ->
      IPatCase (Regular(
       List.map (List.map ipat_of_intro_pattern)
         (List.map (List.map remove_loc) iorpat)))
    | IntroAction (IntroOrAndPattern (IntroAndPattern iandpat)) ->
      IPatCase
       (Regular [List.map ipat_of_intro_pattern (List.map remove_loc iandpat)])
    | IntroNaming IntroAnonymous -> IPatAnon (One None)
    | IntroAction (IntroRewrite b) -> IPatRewrite (allocc, if b then L2R else R2L)
    | IntroNaming (IntroFresh id) -> IPatAnon (One None)
    | IntroAction (IntroApplyOn _) -> (* to do *) CErrors.user_err (Pp.str "TO DO")
    | IntroAction (IntroInjection ips) ->
        IPatInj [List.map ipat_of_intro_pattern (List.map remove_loc ips)]
    | IntroForthcoming _ ->
        (* Unable to determine which kind of ipat interp_introid could
         * return [HH] *)
        assert false
  in
  ipat_of_intro_pattern p
)

let rec map_ipat map_id map_ssrhyp map_ast_closure_term = function
  | (IPatSimpl _ | IPatAnon _ | IPatRewrite _ | IPatNoop | IPatFastNondep) as x -> x
  | IPatId id -> IPatId (map_id id)
  | IPatAbstractVars l -> IPatAbstractVars (List.map map_id l)
  | IPatClear clr -> IPatClear (List.map map_ssrhyp clr)
  | IPatCase (Regular iorpat) -> IPatCase (Regular (List.map (List.map (map_ipat map_id map_ssrhyp map_ast_closure_term)) iorpat))
  | IPatCase (Block(hat)) -> IPatCase (Block(map_block map_id hat))
  | IPatDispatch (Regular iorpat) -> IPatDispatch (Regular (List.map (List.map (map_ipat map_id map_ssrhyp map_ast_closure_term)) iorpat))
  | IPatDispatch (Block (hat)) -> IPatDispatch (Block(map_block map_id hat))
  | IPatInj iorpat -> IPatInj (List.map (List.map (map_ipat map_id map_ssrhyp map_ast_closure_term)) iorpat)
  | IPatView v -> IPatView (List.map map_ast_closure_term v)
and map_block map_id = function
  | Prefix id -> Prefix (map_id id)
  | SuffixId id -> SuffixId (map_id id)
  | SuffixNum _ as x -> x

type ssripatrep = ssripat
let wit_ssripatrep = add_genarg "ssripatrep" (fun env sigma -> pr_ipat)

let pr_ssripat _ _ _ = pr_ipat
let pr_ssripats _ _ _ = pr_ipats
let pr_ssriorpat _ _ _ = pr_iorpat

let intern_ipat ist =
  map_ipat
    (fun id -> id)
    (intern_hyp ist)
    (glob_ast_closure_term ist)

let intern_ipats ist = List.map (intern_ipat ist)

let interp_intro_pattern = interp_wit wit_intro_pattern

let interp_introid ist gl id =
 try IntroNaming (IntroIdentifier (hyp_id (snd (interp_hyp ist gl (SsrHyp (Loc.tag id))))))
 with _ -> (snd (interp_intro_pattern ist gl (CAst.make @@ IntroNaming (IntroIdentifier id)))).CAst.v

let get_intro_id = function
  | IntroNaming (IntroIdentifier id) -> id
  | _ -> assert false

let rec add_intro_pattern_hyps ipat hyps =
  let {CAst.loc=loc;v=ipat} = ipat in
  match ipat with
  | IntroNaming (IntroIdentifier id) ->
    if not_section_id id then SsrHyp (loc, id) :: hyps else
    hyp_err ?loc "Can't delete section hypothesis " id
  | IntroAction IntroWildcard -> hyps
  | IntroAction (IntroOrAndPattern (IntroOrPattern iorpat)) ->
     List.fold_right (List.fold_right add_intro_pattern_hyps) iorpat hyps
  | IntroAction (IntroOrAndPattern (IntroAndPattern iandpat)) ->
    List.fold_right add_intro_pattern_hyps iandpat hyps
  | IntroNaming IntroAnonymous -> []
  | IntroNaming (IntroFresh _) -> []
  | IntroAction (IntroRewrite _) -> hyps
  | IntroAction (IntroInjection ips) -> List.fold_right add_intro_pattern_hyps ips hyps
  | IntroAction (IntroApplyOn (c,pat)) -> add_intro_pattern_hyps pat hyps
  | IntroForthcoming _ ->
    (* As in ipat_of_intro_pattern, was unable to determine which kind
      of ipat interp_introid could return [HH] *) assert false

(* We interp the ipat using the standard ltac machinery for ids, since
 * we have no clue what a name could be bound to (maybe another ipat) *)
let interp_ipat ist gl =
  let ltacvar id = Id.Map.mem id ist.Tacinterp.lfun in
  let interp_block = function
    | Prefix id when ltacvar id ->
        begin match interp_introid ist gl id with
        | IntroNaming (IntroIdentifier id) -> Prefix id
        | _ -> Ssrcommon.errorstrm Pp.(str"Variable " ++ Id.print id ++ str" in block intro pattern should be bound to an identifier.")
        end
    | SuffixId id when ltacvar id ->
        begin match interp_introid ist gl id with
        | IntroNaming (IntroIdentifier id) -> SuffixId id
        | _ -> Ssrcommon.errorstrm Pp.(str"Variable " ++ Id.print id ++ str" in block intro pattern should be bound to an identifier.")
        end
    | x -> x in
  let rec interp = function
  | IPatId id when ltacvar id ->
    ipat_of_intro_pattern (interp_introid ist gl id)
  | IPatId _ as x -> x
  | IPatClear clr ->
    let add_hyps (SsrHyp (loc, id) as hyp) hyps =
      if not (ltacvar id) then hyp :: hyps else
      add_intro_pattern_hyps CAst.(make ?loc (interp_introid ist gl id)) hyps in
    let clr' = List.fold_right add_hyps clr [] in
    check_hyps_uniq [] clr';
    IPatClear clr'
  | IPatCase(Regular iorpat) ->
      IPatCase(Regular(List.map (List.map interp) iorpat))
  | IPatCase(Block(hat)) -> IPatCase(Block(interp_block hat))

  | IPatDispatch(Regular iorpat) ->
      IPatDispatch(Regular (List.map (List.map interp) iorpat))
  | IPatDispatch(Block(hat)) -> IPatDispatch(Block(interp_block hat))

  | IPatInj iorpat -> IPatInj (List.map (List.map interp) iorpat)
  | IPatAbstractVars l ->
     IPatAbstractVars (List.map get_intro_id (List.map (interp_introid ist gl) l))
  | IPatView l -> IPatView (List.map (fun x -> snd(interp_ast_closure_term ist
     gl x)) l)
  | (IPatSimpl _ | IPatAnon _ | IPatRewrite _ | IPatNoop | IPatFastNondep) as x -> x
    in
  interp

let interp_ipats ist gl l = project gl, List.map (interp_ipat ist gl) l

let pushIPatRewrite = function
  | pats :: orpat -> (IPatRewrite (allocc, L2R) :: pats) :: orpat
  | [] -> []

let pushIPatNoop = function
  | pats :: orpat -> (IPatNoop :: pats) :: orpat
  | [] -> []

let test_ident_no_do strm =
  match Util.stream_nth 0 strm with
  | Tok.IDENT s when s <> "do" -> ()
  | _ -> raise Stream.Failure

let test_ident_no_do =
  Pcoq.Entry.of_parser "test_ident_no_do" test_ident_no_do



let (wit_ident_no_do, ident_no_do) = Tacentries.argument_extend ~name:"ident_no_do" 
                                     {
                                     Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                              [(Extend.Rule
                                                                (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Stop,
                                                                 (Extend.Atoken (CLexer.terminal "YouShouldNotTypeThis"))),
                                                                 (Extend.Aentry ident)),
                                                                (fun id _
                                                                loc -> 
                                                                
# 764 "ssrparser.mlg"
                                            id 
                                                                )))]);
                                     Tacentries.arg_tag = None;
                                     Tacentries.arg_intern = Tacentries.ArgInternFun (fun ist v -> (ist, v));
                                     Tacentries.arg_subst = Tacentries.ArgSubstFun (fun s v -> v);
                                     Tacentries.arg_interp = Tacentries.ArgInterpRet;
                                     Tacentries.arg_printer = ((fun env sigma -> 
                                                              
# 763 "ssrparser.mlg"
                                         fun _ _ _ -> Names.Id.print 
                                                              ), (fun env sigma -> 
                                                              
# 763 "ssrparser.mlg"
                                         fun _ _ _ -> Names.Id.print 
                                                              ), (fun env sigma -> 
                                                              
# 763 "ssrparser.mlg"
                                         fun _ _ _ -> Names.Id.print 
                                                              ));
                                     }
let _ = (wit_ident_no_do, ident_no_do)

let _ = let () =
        Pcoq.grammar_extend ident_no_do None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry test_ident_no_do)),
                              (Extend.Atoken (Tok.PIDENT (None)))),
                 (fun id _ loc -> 
# 770 "ssrparser.mlg"
                                                     Id.of_string id 
                                  ))])])
        in ()

let (wit_ssripat, ssripat) = Tacentries.argument_extend ~name:"ssripat" 
                             {
                             Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                      [(Extend.Rule
                                                        (Extend.Next 
                                                         (Extend.Next 
                                                         (Extend.Next 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (CLexer.terminal "[:"))),
                                                         (Extend.Alist0 (Extend.Aentry ident))),
                                                         (Extend.Atoken (CLexer.terminal "]"))),
                                                        (fun _ idl _ loc -> 
# 811 "ssrparser.mlg"
                                      [IPatAbstractVars idl] 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "["))),
                                                                    (Extend.Atoken (CLexer.terminal ":"))),
                                                                    (Extend.Alist0 (Extend.Aentry ident))),
                                                                    (Extend.Atoken (CLexer.terminal "]"))),
                                                       (fun _ idl _ _ loc ->
                                                       
# 810 "ssrparser.mlg"
                                         [IPatAbstractVars idl] 
                                                       )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Stop,
                                                                    (Extend.Aentry ssrfwdview)),
                                                       (fun v loc -> 
# 809 "ssrparser.mlg"
                           [IPatView v] 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "-/"))),
                                                                    (Extend.Aentry integer)),
                                                                    (Extend.Atoken (CLexer.terminal "/"))),
                                                                    (Extend.Aentry integer)),
                                                                    (Extend.Atoken (CLexer.terminal "="))),
                                                       (fun _ m _ n _ loc ->
                                                       
# 808 "ssrparser.mlg"
        [IPatNoop;IPatSimpl(SimplCut(n,m))] 
                                                       )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "-/"))),
                                                                    (Extend.Aentry integer)),
                                                                    (Extend.Atoken (CLexer.terminal "/="))),
                                                       (fun _ n _ loc -> 
# 806 "ssrparser.mlg"
                                  [IPatNoop;IPatSimpl(SimplCut (n,~-1))] 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "-//="))),
                                                       (fun _ loc -> 
# 805 "ssrparser.mlg"
                    [IPatNoop;IPatSimpl(SimplCut (~-1,~-1))] 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "-//"))),
                                                                    (Extend.Atoken (CLexer.terminal "="))),
                                                       (fun _ _ loc -> 
# 804 "ssrparser.mlg"
                       [IPatNoop;IPatSimpl(SimplCut (~-1,~-1))] 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "-/"))),
                                                                    (Extend.Atoken (CLexer.terminal "/="))),
                                                       (fun _ _ loc -> 
# 803 "ssrparser.mlg"
                       [IPatNoop;IPatSimpl(SimplCut (~-1,~-1))] 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "-/"))),
                                                                    (Extend.Aentry integer)),
                                                                    (Extend.Atoken (CLexer.terminal "/"))),
                                                       (fun _ n _ loc -> 
# 802 "ssrparser.mlg"
                                 [IPatNoop;IPatSimpl(Cut n)] 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "-//"))),
                                                       (fun _ loc -> 
# 801 "ssrparser.mlg"
                   [IPatNoop;IPatSimpl(Cut ~-1)] 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "-/"))),
                                                                    (Extend.Atoken (CLexer.terminal "/"))),
                                                       (fun _ _ loc -> 
# 800 "ssrparser.mlg"
                      [IPatNoop;IPatSimpl(Cut ~-1)] 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "-/="))),
                                                       (fun _ loc -> 
# 799 "ssrparser.mlg"
                   [IPatNoop;IPatSimpl(Simpl ~-1)] 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "-/"))),
                                                                    (Extend.Atoken (CLexer.terminal "="))),
                                                       (fun _ _ loc -> 
# 798 "ssrparser.mlg"
                      [IPatNoop;IPatSimpl(Simpl ~-1)] 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "-"))),
                                                       (fun _ loc -> 
# 797 "ssrparser.mlg"
                 [IPatNoop] 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "<-"))),
                                                       (fun _ loc -> 
# 796 "ssrparser.mlg"
                  [IPatRewrite (allocc, R2L)] 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "->"))),
                                                       (fun _ loc -> 
# 795 "ssrparser.mlg"
                  [IPatRewrite (allocc, L2R)] 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Stop,
                                                                    (Extend.Aentry ssrdocc)),
                                                       (fun occ loc -> 
# 792 "ssrparser.mlg"
                          match occ with
      | Some cl, _ -> check_hyps_uniq [] cl; [IPatClear cl]
      | _ -> CErrors.user_err ~loc (str"Only identifiers are allowed here") 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Aentry ssrdocc)),
                                                                    (Extend.Atoken (CLexer.terminal "<-"))),
                                                       (fun _ occ loc -> 
# 788 "ssrparser.mlg"
                               match occ with
      | Some [], _ -> CErrors.user_err ~loc (str"occ_switch expected")
      | None, occ ->  [IPatRewrite (occ, R2L)]
      | Some clr, _ -> [IPatClear clr; IPatRewrite (allocc, R2L)] 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Aentry ssrdocc)),
                                                                    (Extend.Atoken (CLexer.terminal "->"))),
                                                       (fun _ occ loc -> 
# 784 "ssrparser.mlg"
                               match occ with
      | Some [], _ -> CErrors.user_err ~loc (str"occ_switch expected")
      | None, occ -> [IPatRewrite (occ, L2R)]
      | Some clr, _ -> [IPatClear clr; IPatRewrite (allocc, L2R)] 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Stop,
                                                                    (Extend.Aentry ssrsimpl_ne)),
                                                       (fun sim loc -> 
# 783 "ssrparser.mlg"
                              [IPatSimpl sim] 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "++"))),
                                                       (fun _ loc -> 
# 782 "ssrparser.mlg"
                  [IPatAnon Temporary; IPatAnon Temporary] 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "+"))),
                                                       (fun _ loc -> 
# 781 "ssrparser.mlg"
                 [IPatAnon Temporary] 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "?"))),
                                                       (fun _ loc -> 
# 780 "ssrparser.mlg"
                 [IPatAnon (One None)] 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Stop,
                                                                    (Extend.Aentry ident_no_do)),
                                                       (fun id loc -> 
# 779 "ssrparser.mlg"
                             [IPatId id] 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal ">"))),
                                                       (fun _ loc -> 
# 778 "ssrparser.mlg"
                 [IPatFastNondep] 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "*"))),
                                                       (fun _ loc -> 
# 777 "ssrparser.mlg"
                 [IPatAnon All] 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "_"))),
                                                       (fun _ loc -> 
# 776 "ssrparser.mlg"
                 [IPatAnon Drop] 
                                                                    )))]);
                             Tacentries.arg_tag = Some
                                                  (Geninterp.Val.List 
                                                  (Geninterp.val_tag (Genarg.topwit wit_ssripatrep)));
                             Tacentries.arg_intern = Tacentries.ArgInternFun ((fun f ist v -> (ist, f ist v)) (
                                                     
# 775 "ssrparser.mlg"
                  intern_ipats 
                                                     ));
                             Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.ListArg 
                                                    (wit_ssripatrep));
                             Tacentries.arg_interp = Tacentries.ArgInterpLegacy (
                                                     
# 774 "ssrparser.mlg"
                   interp_ipats 
                                                     );
                             Tacentries.arg_printer = ((fun env sigma -> 
                                                      
# 773 "ssrparser.mlg"
                                                              pr_ssripats 
                                                      ), (fun env sigma -> 
                                                      
# 773 "ssrparser.mlg"
                                                              pr_ssripats 
                                                      ), (fun env sigma -> 
                                                      
# 773 "ssrparser.mlg"
                                                              pr_ssripats 
                                                      ));
                             }
let _ = (wit_ssripat, ssripat)

let (wit_ssripats, ssripats) = Tacentries.argument_extend ~name:"ssripats" 
                               {
                               Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                        [(Extend.Rule
                                                          (Extend.Stop,
                                                          (fun loc -> 
# 816 "ssrparser.mlg"
             [] 
                                                                    )));
                                                        (Extend.Rule
                                                         (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Stop,
                                                          (Extend.Aentry ssripat)),
                                                          Extend.Aself),
                                                         (fun tl i loc -> 
# 815 "ssrparser.mlg"
                                     i @ tl 
                                                                    )))]);
                               Tacentries.arg_tag = Some
                                                    (Geninterp.val_tag (Genarg.topwit wit_ssripat));
                               Tacentries.arg_intern = Tacentries.ArgInternWit (wit_ssripat);
                               Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ssripat);
                               Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_ssripat);
                               Tacentries.arg_printer = ((fun env sigma -> 
                                                        
# 814 "ssrparser.mlg"
                                                       pr_ssripats 
                                                        ), (fun env sigma -> 
                                                        
# 814 "ssrparser.mlg"
                                                       pr_ssripats 
                                                        ), (fun env sigma -> 
                                                        
# 814 "ssrparser.mlg"
                                                       pr_ssripats 
                                                        ));
                               }
let _ = (wit_ssripats, ssripats)

let (wit_ssriorpat, ssriorpat) = Tacentries.argument_extend ~name:"ssriorpat" 
                                 {
                                 Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                          [(Extend.Rule
                                                            (Extend.Next 
                                                             (Extend.Stop,
                                                             (Extend.Aentry ssripats)),
                                                            (fun pats loc ->
                                                            
# 827 "ssrparser.mlg"
                          [pats] 
                                                            )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Aentry ssripats)),
                                                            (Extend.Atoken (CLexer.terminal "||||"))),
                                                            Extend.Aself),
                                                           (fun orpat _ pats
                                                           loc -> 
# 826 "ssrparser.mlg"
                                                  [pats; []; []; []] @ orpat 
                                                                  )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Aentry ssripats)),
                                                            (Extend.Atoken (CLexer.terminal "|||"))),
                                                            Extend.Aself),
                                                           (fun orpat _ pats
                                                           loc -> 
# 825 "ssrparser.mlg"
                                                 pats :: [] :: [] :: orpat 
                                                                  )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Aentry ssripats)),
                                                            (Extend.Atoken (CLexer.terminal "||"))),
                                                            Extend.Aself),
                                                           (fun orpat _ pats
                                                           loc -> 
# 824 "ssrparser.mlg"
                                                pats :: [] :: orpat 
                                                                  )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Aentry ssripats)),
                                                            (Extend.Atoken (CLexer.terminal "|->"))),
                                                            Extend.Aself),
                                                           (fun orpat _ pats
                                                           loc -> 
# 823 "ssrparser.mlg"
                                                 pats :: pushIPatRewrite orpat 
                                                                  )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Aentry ssripats)),
                                                            (Extend.Atoken (CLexer.terminal "|-"))),
                                                            Extend.Aself),
                                                           (fun orpat _ pats
                                                           loc -> 
# 822 "ssrparser.mlg"
                                                pats :: pushIPatNoop orpat 
                                                                  )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Aentry ssripats)),
                                                            (Extend.Atoken (CLexer.terminal "|-"))),
                                                            (Extend.Atoken (CLexer.terminal ">"))),
                                                            Extend.Aself),
                                                           (fun orpat _ _
                                                           pats loc -> 
                                                           
# 821 "ssrparser.mlg"
                                                    pats :: pushIPatRewrite orpat 
                                                           )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Aentry ssripats)),
                                                            (Extend.Atoken (CLexer.terminal "|"))),
                                                            Extend.Aself),
                                                           (fun orpat _ pats
                                                           loc -> 
# 820 "ssrparser.mlg"
                                               pats :: orpat 
                                                                  )))]);
                                 Tacentries.arg_tag = Some
                                                      (Geninterp.Val.List 
                                                      (Geninterp.val_tag (Genarg.topwit wit_ssripat)));
                                 Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.ListArg 
                                                         (wit_ssripat));
                                 Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.ListArg 
                                                        (wit_ssripat));
                                 Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.ListArg 
                                                         (wit_ssripat));
                                 Tacentries.arg_printer = ((fun env sigma -> 
                                                          
# 819 "ssrparser.mlg"
                                                             pr_ssriorpat 
                                                          ), (fun env sigma -> 
                                                          
# 819 "ssrparser.mlg"
                                                             pr_ssriorpat 
                                                          ), (fun env sigma -> 
                                                          
# 819 "ssrparser.mlg"
                                                             pr_ssriorpat 
                                                          ));
                                 }
let _ = (wit_ssriorpat, ssriorpat)


# 830 "ssrparser.mlg"
 

let reject_ssrhid strm =
  match Util.stream_nth 0 strm with
  | Tok.KEYWORD "[" ->
      (match Util.stream_nth 1 strm with
      | Tok.KEYWORD ":" -> raise Stream.Failure
      | _ -> ())
  | _ -> ()

let test_nohidden = Pcoq.Entry.of_parser "test_ssrhid" reject_ssrhid

let rec reject_binder crossed_paren k s =
  match
    try Some (Util.stream_nth k s)
    with Stream.Failure -> None
  with
  | Some (Tok.KEYWORD "(") when not crossed_paren -> reject_binder true (k+1) s
  | Some (Tok.IDENT _) when crossed_paren -> reject_binder true (k+1) s
  | Some (Tok.KEYWORD ":" | Tok.KEYWORD ":=") when crossed_paren ->
      raise Stream.Failure
  | Some (Tok.KEYWORD ")") when crossed_paren -> raise Stream.Failure
  | _ -> if crossed_paren then () else raise Stream.Failure

let _test_nobinder = Pcoq.Entry.of_parser "test_nobinder" (reject_binder false 0)



let (wit_ssrcpat, ssrcpat) = Tacentries.argument_extend ~name:"ssrcpat" 
                             {
                             Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                      [(Extend.Rule
                                                        (Extend.Next 
                                                         (Extend.Next 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (CLexer.terminal "YouShouldNotTypeThis"))),
                                                         (Extend.Aentry ssriorpat)),
                                                        (fun x _ loc -> 
# 859 "ssrparser.mlg"
                                                 IPatCase(Regular x) 
                                                                    )))]);
                             Tacentries.arg_tag = Some
                                                  (Geninterp.val_tag (Genarg.topwit wit_ssripatrep));
                             Tacentries.arg_intern = Tacentries.ArgInternWit (wit_ssripatrep);
                             Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ssripatrep);
                             Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_ssripatrep);
                             Tacentries.arg_printer = ((fun env sigma -> 
                                                      
# 858 "ssrparser.mlg"
                                                         pr_ssripat 
                                                      ), (fun env sigma -> 
                                                      
# 858 "ssrparser.mlg"
                                                         pr_ssripat 
                                                      ), (fun env sigma -> 
                                                      
# 858 "ssrparser.mlg"
                                                         pr_ssripat 
                                                      ));
                             }
let _ = (wit_ssrcpat, ssrcpat)

let _ = let hat = Pcoq.Entry.create "hat"
        in
        let () =
        Pcoq.grammar_extend hat None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PKEYWORD ("^~")))),
                              (Extend.Aentry natural)),
                 (fun n _ loc -> 
# 870 "ssrparser.mlg"
                           SuffixNum n 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("^~")))),
                             (Extend.Aentry ident)),
                (fun id _ loc -> 
# 869 "ssrparser.mlg"
                          SuffixId id 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD ("^")))),
                                          (Extend.Atoken (Tok.PKEYWORD ("~")))),
                             (Extend.Aentry natural)),
                (fun n _ _ loc -> 
# 868 "ssrparser.mlg"
                               SuffixNum n 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD ("^")))),
                                          (Extend.Atoken (Tok.PKEYWORD ("~")))),
                             (Extend.Aentry ident)),
                (fun id _ _ loc -> 
# 867 "ssrparser.mlg"
                              SuffixId id 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("^")))),
                             (Extend.Aentry ident)),
                (fun id _ loc -> 
# 866 "ssrparser.mlg"
                         Prefix id 
                                 ))])])
        in let () =
        Pcoq.grammar_extend ssrcpat None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Aentry test_nohidden)),
                                                        (Extend.Atoken (Tok.PKEYWORD ("[=")))),
                                           (Extend.Aentry ssriorpat)),
                              (Extend.Atoken (Tok.PKEYWORD ("]")))),
                 (fun _ iorpat _ _ loc -> 
# 877 "ssrparser.mlg"
                                                      
      IPatInj iorpat 
                                          ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Aentry test_nohidden)),
                                                       (Extend.Atoken (Tok.PKEYWORD ("[")))),
                                          (Extend.Aentry ssriorpat)),
                             (Extend.Atoken (Tok.PKEYWORD ("]")))),
                (fun _ iorpat _ _ loc -> 
# 875 "ssrparser.mlg"
                                                     
      IPatCase (Regular iorpat) 
                                         ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Aentry test_nohidden)),
                                                       (Extend.Atoken (Tok.PKEYWORD ("[")))),
                                          (Extend.Aentry hat)),
                             (Extend.Atoken (Tok.PKEYWORD ("]")))),
                (fun _ hat_id _ _ loc -> 
# 873 "ssrparser.mlg"
                                               
      IPatCase (Block(hat_id)) 
                                         ))])])
        in ()

let _ = let () =
        Pcoq.grammar_extend ssripat None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry ssrcpat)),
                 (fun pat loc -> 
# 883 "ssrparser.mlg"
                                 [pat] 
                                 ))])])
        in ()

let (wit_ssripats_ne, ssripats_ne) = Tacentries.argument_extend ~name:"ssripats_ne" 
                                     {
                                     Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                              [(Extend.Rule
                                                                (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Stop,
                                                                 (Extend.Aentry ssripat)),
                                                                 (Extend.Aentry ssripats)),
                                                                (fun tl i
                                                                loc -> 
                                                                
# 887 "ssrparser.mlg"
                                     i @ tl 
                                                                )))]);
                                     Tacentries.arg_tag = Some
                                                          (Geninterp.val_tag (Genarg.topwit wit_ssripat));
                                     Tacentries.arg_intern = Tacentries.ArgInternWit (wit_ssripat);
                                     Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ssripat);
                                     Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_ssripat);
                                     Tacentries.arg_printer = ((fun env sigma -> 
                                                              
# 886 "ssrparser.mlg"
                                                          pr_ssripats 
                                                              ), (fun env sigma -> 
                                                              
# 886 "ssrparser.mlg"
                                                          pr_ssripats 
                                                              ), (fun env sigma -> 
                                                              
# 886 "ssrparser.mlg"
                                                          pr_ssripats 
                                                              ));
                                     }
let _ = (wit_ssripats_ne, ssripats_ne)


# 892 "ssrparser.mlg"
 

(* TODO: review what this function does, it looks suspicious *)
let check_ssrhpats loc w_binders ipats =
  let err_loc s = CErrors.user_err ~loc ~hdr:"ssreflect" s in
  let clr, ipats =
    let opt_app = function None -> fun l -> Some l
      | Some l1 -> fun l2 -> Some (l1 @ l2) in
    let rec aux clr = function
      | IPatClear cl :: tl -> aux (opt_app clr cl) tl
      | tl -> clr, tl
    in aux None ipats in
  let simpl, ipats =
    match List.rev ipats with
    | IPatSimpl _ as s :: tl -> [s], List.rev tl
    | _ -> [],  ipats in
  if simpl <> [] && not w_binders then
    err_loc (str "No s-item allowed here: " ++ pr_ipats simpl);
  let ipat, binders =
    let rec loop ipat = function
      | [] -> ipat, []
      | ( IPatId _| IPatAnon _| IPatCase _ | IPatDispatch _ | IPatRewrite _ as i) :: tl ->
        if w_binders then
          if simpl <> [] && tl <> [] then
            err_loc(str"binders XOR s-item allowed here: "++pr_ipats(tl@simpl))
          else if not (List.for_all (function IPatId _ -> true | _ -> false) tl)
          then err_loc (str "Only binders allowed here: " ++ pr_ipats tl)
          else ipat @ [i], tl
        else
          if tl = [] then  ipat @ [i], []
          else err_loc (str "No binder or s-item allowed here: " ++ pr_ipats tl)
      | hd :: tl -> loop (ipat @ [hd]) tl
    in loop [] ipats in
  ((clr, ipat), binders), simpl

let pr_clear_opt sep = function None -> mt () | Some x -> pr_clear sep x

let pr_hpats (((clr, ipat), binders), simpl) =
   pr_clear_opt mt clr ++ pr_ipats ipat ++ pr_ipats binders ++ pr_ipats simpl
let pr_ssrhpats _ _ _ = pr_hpats
let pr_ssrhpats_wtransp _ _ _ (_, x) = pr_hpats x



let (wit_ssrhpats, ssrhpats) = Tacentries.argument_extend ~name:"ssrhpats" 
                               {
                               Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                        [(Extend.Rule
                                                          (Extend.Next 
                                                           (Extend.Stop,
                                                           (Extend.Aentry ssripats)),
                                                          (fun i loc -> 
# 938 "ssrparser.mlg"
                         check_ssrhpats loc true i 
                                                                    )))]);
                               Tacentries.arg_tag = Some
                                                    (Geninterp.Val.Pair (
                                                    (Geninterp.Val.Pair (
                                                    (Geninterp.Val.Pair (
                                                    (Geninterp.Val.Opt 
                                                    (Geninterp.val_tag (Genarg.topwit wit_ssrclear))), 
                                                    (Geninterp.val_tag (Genarg.topwit wit_ssripat)))), 
                                                    (Geninterp.val_tag (Genarg.topwit wit_ssripat)))), 
                                                    (Geninterp.val_tag (Genarg.topwit wit_ssripat))));
                               Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                       (Genarg.PairArg (
                                                       (Genarg.PairArg (
                                                       (Genarg.OptArg 
                                                       (wit_ssrclear)), 
                                                       (wit_ssripat))), 
                                                       (wit_ssripat))), 
                                                       (wit_ssripat)));
                               Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                      (Genarg.PairArg (
                                                      (Genarg.PairArg (
                                                      (Genarg.OptArg 
                                                      (wit_ssrclear)), 
                                                      (wit_ssripat))), 
                                                      (wit_ssripat))), 
                                                      (wit_ssripat)));
                               Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                       (Genarg.PairArg (
                                                       (Genarg.PairArg (
                                                       (Genarg.OptArg 
                                                       (wit_ssrclear)), 
                                                       (wit_ssripat))), 
                                                       (wit_ssripat))), 
                                                       (wit_ssripat)));
                               Tacentries.arg_printer = ((fun env sigma -> 
                                                        
# 937 "ssrparser.mlg"
             pr_ssrhpats 
                                                        ), (fun env sigma -> 
                                                        
# 937 "ssrparser.mlg"
             pr_ssrhpats 
                                                        ), (fun env sigma -> 
                                                        
# 937 "ssrparser.mlg"
             pr_ssrhpats 
                                                        ));
                               }
let _ = (wit_ssrhpats, ssrhpats)

let (wit_ssrhpats_wtransp, ssrhpats_wtransp) = Tacentries.argument_extend ~name:"ssrhpats_wtransp" 
                                               {
                                               Tacentries.arg_parsing = 
                                               Vernacextend.Arg_rules (
                                               [(Extend.Rule
                                                 (Extend.Next (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Aentry ssripats)),
                                                              (Extend.Atoken (CLexer.terminal "@"))),
                                                              (Extend.Aentry ssripats)),
                                                 (fun j _ i loc -> 
# 945 "ssrparser.mlg"
                                         true,check_ssrhpats loc true (i @ j) 
                                                                   )));
                                               (Extend.Rule
                                                (Extend.Next (Extend.Stop,
                                                             (Extend.Aentry ssripats)),
                                                (fun i loc -> 
# 944 "ssrparser.mlg"
                         false,check_ssrhpats loc true i 
                                                              )))]);
                                               Tacentries.arg_tag = Some
                                                                    (Geninterp.Val.Pair (
                                                                    (Geninterp.val_tag (Genarg.topwit wit_bool)), 
                                                                    (Geninterp.Val.Pair (
                                                                    (Geninterp.Val.Pair (
                                                                    (Geninterp.Val.Pair (
                                                                    (Geninterp.Val.Opt 
                                                                    (Geninterp.val_tag (Genarg.topwit wit_ssrclear))), 
                                                                    (Geninterp.val_tag (Genarg.topwit wit_ssripats)))), 
                                                                    (Geninterp.val_tag (Genarg.topwit wit_ssripats)))), 
                                                                    (Geninterp.val_tag (Genarg.topwit wit_ssripats))))));
                                               Tacentries.arg_intern = 
                                               Tacentries.ArgInternWit (Genarg.PairArg (
                                               (wit_bool), (Genarg.PairArg (
                                                           (Genarg.PairArg (
                                                           (Genarg.PairArg (
                                                           (Genarg.OptArg 
                                                           (wit_ssrclear)), 
                                                           (wit_ssripats))), 
                                                           (wit_ssripats))), 
                                                           (wit_ssripats)))));
                                               Tacentries.arg_subst = 
                                               Tacentries.ArgSubstWit (Genarg.PairArg (
                                               (wit_bool), (Genarg.PairArg (
                                                           (Genarg.PairArg (
                                                           (Genarg.PairArg (
                                                           (Genarg.OptArg 
                                                           (wit_ssrclear)), 
                                                           (wit_ssripats))), 
                                                           (wit_ssripats))), 
                                                           (wit_ssripats)))));
                                               Tacentries.arg_interp = 
                                               Tacentries.ArgInterpWit (Genarg.PairArg (
                                               (wit_bool), (Genarg.PairArg (
                                                           (Genarg.PairArg (
                                                           (Genarg.PairArg (
                                                           (Genarg.OptArg 
                                                           (wit_ssrclear)), 
                                                           (wit_ssripats))), 
                                                           (wit_ssripats))), 
                                                           (wit_ssripats)))));
                                               Tacentries.arg_printer = 
                                               ((fun env sigma -> 
# 943 "ssrparser.mlg"
               pr_ssrhpats_wtransp 
                                               ), (fun env sigma -> 
# 943 "ssrparser.mlg"
               pr_ssrhpats_wtransp 
                                               ), (fun env sigma -> 
# 943 "ssrparser.mlg"
               pr_ssrhpats_wtransp 
                                               ));
                                               }
let _ = (wit_ssrhpats_wtransp, ssrhpats_wtransp)

let (wit_ssrhpats_nobs, ssrhpats_nobs) = Tacentries.argument_extend ~name:"ssrhpats_nobs" 
                                         {
                                         Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                                  [(Extend.Rule
                                                                    (
                                                                    Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Aentry ssripats)),
                                                                    (fun i
                                                                    loc -> 
                                                                    
# 950 "ssrparser.mlg"
                         check_ssrhpats loc false i 
                                                                    )))]);
                                         Tacentries.arg_tag = Some
                                                              (Geninterp.Val.Pair (
                                                              (Geninterp.Val.Pair (
                                                              (Geninterp.Val.Pair (
                                                              (Geninterp.Val.Opt 
                                                              (Geninterp.val_tag (Genarg.topwit wit_ssrclear))), 
                                                              (Geninterp.val_tag (Genarg.topwit wit_ssripats)))), 
                                                              (Geninterp.val_tag (Genarg.topwit wit_ssripats)))), 
                                                              (Geninterp.val_tag (Genarg.topwit wit_ssripats))));
                                         Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                                 (Genarg.PairArg (
                                                                 (Genarg.PairArg (
                                                                 (Genarg.OptArg 
                                                                 (wit_ssrclear)), 
                                                                 (wit_ssripats))), 
                                                                 (wit_ssripats))), 
                                                                 (wit_ssripats)));
                                         Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                                (Genarg.PairArg (
                                                                (Genarg.PairArg (
                                                                (Genarg.OptArg 
                                                                (wit_ssrclear)), 
                                                                (wit_ssripats))), 
                                                                (wit_ssripats))), 
                                                                (wit_ssripats)));
                                         Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                                 (Genarg.PairArg (
                                                                 (Genarg.PairArg (
                                                                 (Genarg.OptArg 
                                                                 (wit_ssrclear)), 
                                                                 (wit_ssripats))), 
                                                                 (wit_ssripats))), 
                                                                 (wit_ssripats)));
                                         Tacentries.arg_printer = ((fun env sigma -> 
                                                                  
# 949 "ssrparser.mlg"
                                                                             pr_ssrhpats 
                                                                  ), (fun env sigma -> 
                                                                  
# 949 "ssrparser.mlg"
                                                                             pr_ssrhpats 
                                                                  ), (fun env sigma -> 
                                                                  
# 949 "ssrparser.mlg"
                                                                             pr_ssrhpats 
                                                                  ));
                                         }
let _ = (wit_ssrhpats_nobs, ssrhpats_nobs)

let (wit_ssrrpat, ssrrpat) = Tacentries.argument_extend ~name:"ssrrpat" 
                             {
                             Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                      [(Extend.Rule
                                                        (Extend.Next 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (CLexer.terminal "<-"))),
                                                        (fun _ loc -> 
# 955 "ssrparser.mlg"
                  IPatRewrite (allocc, R2L) 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "->"))),
                                                       (fun _ loc -> 
# 954 "ssrparser.mlg"
                  IPatRewrite (allocc, L2R) 
                                                                    )))]);
                             Tacentries.arg_tag = Some
                                                  (Geninterp.val_tag (Genarg.topwit wit_ssripatrep));
                             Tacentries.arg_intern = Tacentries.ArgInternWit (wit_ssripatrep);
                             Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ssripatrep);
                             Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_ssripatrep);
                             Tacentries.arg_printer = ((fun env sigma -> 
                                                      
# 953 "ssrparser.mlg"
                                                         pr_ssripat 
                                                      ), (fun env sigma -> 
                                                      
# 953 "ssrparser.mlg"
                                                         pr_ssripat 
                                                      ), (fun env sigma -> 
                                                      
# 953 "ssrparser.mlg"
                                                         pr_ssripat 
                                                      ));
                             }
let _ = (wit_ssrrpat, ssrrpat)


# 958 "ssrparser.mlg"
 

let pr_intros sep intrs =
  if intrs = [] then mt() else sep () ++ str "=>" ++ pr_ipats intrs
let pr_ssrintros _ _ _ = pr_intros mt



let (wit_ssrintros_ne, ssrintros_ne) = Tacentries.argument_extend ~name:"ssrintros_ne" 
                                       {
                                       Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                                [(Extend.Rule
                                                                  (Extend.Next 
                                                                   (Extend.Next 
                                                                   (Extend.Stop,
                                                                   (Extend.Atoken (CLexer.terminal "=>"))),
                                                                   (Extend.Aentry ssripats_ne)),
                                                                  (fun pats _
                                                                  loc -> 
                                                                  
# 968 "ssrparser.mlg"
                                    pats 
                                                                  )))]);
                                       Tacentries.arg_tag = Some
                                                            (Geninterp.val_tag (Genarg.topwit wit_ssripat));
                                       Tacentries.arg_intern = Tacentries.ArgInternWit (wit_ssripat);
                                       Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ssripat);
                                       Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_ssripat);
                                       Tacentries.arg_printer = ((fun env sigma -> 
                                                                
# 967 "ssrparser.mlg"
              pr_ssrintros 
                                                                ), (fun env sigma -> 
                                                                
# 967 "ssrparser.mlg"
              pr_ssrintros 
                                                                ), (fun env sigma -> 
                                                                
# 967 "ssrparser.mlg"
              pr_ssrintros 
                                                                ));
                                       }
let _ = (wit_ssrintros_ne, ssrintros_ne)

let (wit_ssrintros, ssrintros) = Tacentries.argument_extend ~name:"ssrintros" 
                                 {
                                 Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                          [(Extend.Rule
                                                            (Extend.Stop,
                                                            (fun loc -> 
# 975 "ssrparser.mlg"
             [] 
                                                                    )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Aentry ssrintros_ne)),
                                                           (fun intrs loc ->
                                                           
# 974 "ssrparser.mlg"
                                 intrs 
                                                           )))]);
                                 Tacentries.arg_tag = Some
                                                      (Geninterp.val_tag (Genarg.topwit wit_ssrintros_ne));
                                 Tacentries.arg_intern = Tacentries.ArgInternWit (wit_ssrintros_ne);
                                 Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ssrintros_ne);
                                 Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_ssrintros_ne);
                                 Tacentries.arg_printer = ((fun env sigma -> 
                                                          
# 973 "ssrparser.mlg"
                                                             pr_ssrintros 
                                                          ), (fun env sigma -> 
                                                          
# 973 "ssrparser.mlg"
                                                             pr_ssrintros 
                                                          ), (fun env sigma -> 
                                                          
# 973 "ssrparser.mlg"
                                                             pr_ssrintros 
                                                          ));
                                 }
let _ = (wit_ssrintros, ssrintros)


# 978 "ssrparser.mlg"
 

let pr_ssrintrosarg env sigma _ _ prt (tac, ipats) =
  prt env sigma tacltop tac ++ pr_intros spc ipats



let (wit_ssrintrosarg, ssrintrosarg) = Tacentries.argument_extend ~name:"ssrintrosarg" 
                                       {
                                       Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                                [(Extend.Rule
                                                                  (Extend.Next 
                                                                   (Extend.Next 
                                                                   (Extend.Next 
                                                                   (Extend.Stop,
                                                                   (Extend.Atoken (CLexer.terminal "YouShouldNotTypeThis"))),
                                                                   (Extend.Aentry ssrtacarg)),
                                                                   (Extend.Aentry ssrintros_ne)),
                                                                  (fun ipats
                                                                  arg _
                                                                  loc -> 
                                                                  
# 987 "ssrparser.mlg"
                                                                     arg, ipats 
                                                                  )))]);
                                       Tacentries.arg_tag = Some
                                                            (Geninterp.Val.Pair (
                                                            (Geninterp.val_tag (Genarg.topwit wit_tactic)), 
                                                            (Geninterp.val_tag (Genarg.topwit wit_ssrintros))));
                                       Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                               (wit_tactic), 
                                                               (wit_ssrintros)));
                                       Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                              (wit_tactic), 
                                                              (wit_ssrintros)));
                                       Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                               (wit_tactic), 
                                                               (wit_ssrintros)));
                                       Tacentries.arg_printer = ((fun env sigma -> 
                                                                
# 986 "ssrparser.mlg"
                pr_ssrintrosarg env sigma 
                                                                ), (fun env sigma -> 
                                                                
# 986 "ssrparser.mlg"
                pr_ssrintrosarg env sigma 
                                                                ), (fun env sigma -> 
                                                                
# 986 "ssrparser.mlg"
                pr_ssrintrosarg env sigma 
                                                                ));
                                       }
let _ = (wit_ssrintrosarg, ssrintrosarg)

let () = Tacentries.tactic_extend __coq_plugin_name "ssrtclintros" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("YouShouldNotTypeThis", 
                            Tacentries.TyArg (Extend.TUentry (Genarg.get_arg_tag wit_ssrintrosarg), 
                            Tacentries.TyNil)), (fun arg ist -> 
# 992 "ssrparser.mlg"
    let tac, intros = arg in
    ssrevaltac ist tac <*> tclIPATssr intros 
                                                )))]


# 996 "ssrparser.mlg"
 

(** Defined identifier *)
let pr_ssrfwdid id = pr_spc () ++ pr_id id

let pr_ssrfwdidx _ _ _ = pr_ssrfwdid



let (wit_ssrfwdid, ssrfwdid) = Tacentries.argument_extend ~name:"ssrfwdid" 
                               {
                               Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                        [(Extend.Rule
                                                          (Extend.Next 
                                                           (Extend.Stop,
                                                           (Extend.Atoken (CLexer.terminal "YouShouldNotTypeThis"))),
                                                          (fun _ loc -> 
# 1008 "ssrparser.mlg"
                                    anomaly "Grammar placeholder match" 
                                                                    )))]);
                               Tacentries.arg_tag = Some
                                                    (Geninterp.val_tag (Genarg.topwit wit_ident));
                               Tacentries.arg_intern = Tacentries.ArgInternWit (wit_ident);
                               Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ident);
                               Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_ident);
                               Tacentries.arg_printer = ((fun env sigma -> 
                                                        
# 1007 "ssrparser.mlg"
                                                     pr_ssrfwdidx 
                                                        ), (fun env sigma -> 
                                                        
# 1007 "ssrparser.mlg"
                                                     pr_ssrfwdidx 
                                                        ), (fun env sigma -> 
                                                        
# 1007 "ssrparser.mlg"
                                                     pr_ssrfwdidx 
                                                        ));
                               }
let _ = (wit_ssrfwdid, ssrfwdid)


# 1011 "ssrparser.mlg"
 

let accept_ssrfwdid strm =
  match stream_nth 0 strm with
  | Tok.IDENT id -> accept_before_syms_or_any_id [":"; ":="; "("] strm
  | _ -> raise Stream.Failure

let test_ssrfwdid = Pcoq.Entry.of_parser "test_ssrfwdid" accept_ssrfwdid



let _ = let () =
        Pcoq.grammar_extend ssrfwdid None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry test_ssrfwdid)),
                              (Extend.Aentry Prim.ident)),
                 (fun id _ loc -> 
# 1024 "ssrparser.mlg"
                                                   id 
                                  ))])])
        in ()


# 1035 "ssrparser.mlg"
 

let pr_ortacs env sigma prt =
  let rec pr_rec = function
  | [None]           -> spc() ++ str "|" ++ spc()
  | None :: tacs     -> spc() ++ str "|" ++ pr_rec tacs
  | Some tac :: tacs -> spc() ++ str "| " ++ prt env sigma tacltop tac ++  pr_rec tacs
  | []                -> mt() in
  function
  | [None]           -> spc()
  | None :: tacs     -> pr_rec tacs
  | Some tac :: tacs -> prt env sigma tacltop tac ++ pr_rec tacs
  | []                -> mt()
let pr_ssrortacs env sigma _ _ = pr_ortacs env sigma



let (wit_ssrortacs, ssrortacs) = Tacentries.argument_extend ~name:"ssrortacs" 
                                 {
                                 Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                          [(Extend.Rule
                                                            (Extend.Next 
                                                             (Extend.Stop,
                                                             (Extend.Atoken (CLexer.terminal "|"))),
                                                            (fun _ loc -> 
# 1057 "ssrparser.mlg"
               [None; None] 
                                                                    )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (CLexer.terminal "|"))),
                                                            Extend.Aself),
                                                           (fun tacs _ loc ->
                                                           
# 1056 "ssrparser.mlg"
                               None :: tacs 
                                                           )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Aentry ssrtacarg)),
                                                           (fun tac loc -> 
# 1055 "ssrparser.mlg"
                          [Some tac] 
                                                                    )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Aentry ssrtacarg)),
                                                            (Extend.Atoken (CLexer.terminal "|"))),
                                                           (fun _ tac loc ->
                                                           
# 1054 "ssrparser.mlg"
                              [Some tac; None] 
                                                           )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Aentry ssrtacarg)),
                                                            (Extend.Atoken (CLexer.terminal "|"))),
                                                            Extend.Aself),
                                                           (fun tacs _ tac
                                                           loc -> 
# 1053 "ssrparser.mlg"
                                              Some tac :: tacs 
                                                                  )))]);
                                 Tacentries.arg_tag = Some
                                                      (Geninterp.Val.List 
                                                      (Geninterp.Val.Opt 
                                                      (Geninterp.val_tag (Genarg.topwit wit_tactic))));
                                 Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.ListArg 
                                                         (Genarg.OptArg 
                                                         (wit_tactic)));
                                 Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.ListArg 
                                                        (Genarg.OptArg 
                                                        (wit_tactic)));
                                 Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.ListArg 
                                                         (Genarg.OptArg 
                                                         (wit_tactic)));
                                 Tacentries.arg_printer = ((fun env sigma -> 
                                                          
# 1052 "ssrparser.mlg"
                                                                   pr_ssrortacs env sigma 
                                                          ), (fun env sigma -> 
                                                          
# 1052 "ssrparser.mlg"
                                                                   pr_ssrortacs env sigma 
                                                          ), (fun env sigma -> 
                                                          
# 1052 "ssrparser.mlg"
                                                                   pr_ssrortacs env sigma 
                                                          ));
                                 }
let _ = (wit_ssrortacs, ssrortacs)


# 1060 "ssrparser.mlg"
 

let pr_hintarg env sigma prt = function
  | true, tacs -> hv 0 (str "[ " ++ pr_ortacs env sigma prt tacs ++ str " ]")
  | false, [Some tac] -> prt env sigma tacltop tac
  | _, _ -> mt()

let pr_ssrhintarg env sigma _ _ = pr_hintarg env sigma



let (wit_ssrhintarg, ssrhintarg) = Tacentries.argument_extend ~name:"ssrhintarg" 
                                   {
                                   Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                            [(Extend.Rule
                                                              (Extend.Next 
                                                               (Extend.Stop,
                                                               (Extend.Aentry ssrtacarg)),
                                                              (fun arg loc ->
                                                              
# 1074 "ssrparser.mlg"
                          mk_hint arg 
                                                              )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Atoken (CLexer.terminal "["))),
                                                              (Extend.Aentry ssrortacs)),
                                                              (Extend.Atoken (CLexer.terminal "]"))),
                                                             (fun _ tacs _
                                                             loc -> 
# 1073 "ssrparser.mlg"
                                   mk_orhint tacs 
                                                                    )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Atoken (CLexer.terminal "["))),
                                                              (Extend.Atoken (CLexer.terminal "]"))),
                                                             (fun _ _ loc ->
                                                             
# 1072 "ssrparser.mlg"
                   nullhint 
                                                             )))]);
                                   Tacentries.arg_tag = Some
                                                        (Geninterp.Val.Pair (
                                                        (Geninterp.val_tag (Genarg.topwit wit_bool)), 
                                                        (Geninterp.val_tag (Genarg.topwit wit_ssrortacs))));
                                   Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                           (wit_bool), 
                                                           (wit_ssrortacs)));
                                   Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                          (wit_bool), 
                                                          (wit_ssrortacs)));
                                   Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                           (wit_bool), 
                                                           (wit_ssrortacs)));
                                   Tacentries.arg_printer = ((fun env sigma -> 
                                                            
# 1071 "ssrparser.mlg"
                                                                    pr_ssrhintarg env sigma 
                                                            ), (fun env sigma -> 
                                                            
# 1071 "ssrparser.mlg"
                                                                    pr_ssrhintarg env sigma 
                                                            ), (fun env sigma -> 
                                                            
# 1071 "ssrparser.mlg"
                                                                    pr_ssrhintarg env sigma 
                                                            ));
                                   }
let _ = (wit_ssrhintarg, ssrhintarg)

let (wit_ssrhint3arg, ssrhint3arg) = Tacentries.argument_extend ~name:"ssrhint3arg" 
                                     {
                                     Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                              [(Extend.Rule
                                                                (Extend.Next 
                                                                 (Extend.Stop,
                                                                 (Extend.Aentry ssrtac3arg)),
                                                                (fun arg
                                                                loc -> 
                                                                
# 1081 "ssrparser.mlg"
                           mk_hint arg 
                                                                )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "["))),
                                                                (Extend.Aentry ssrortacs)),
                                                                (Extend.Atoken (CLexer.terminal "]"))),
                                                               (fun _ tacs _
                                                               loc -> 
                                                               
# 1080 "ssrparser.mlg"
                                   mk_orhint tacs 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "["))),
                                                                (Extend.Atoken (CLexer.terminal "]"))),
                                                               (fun _ _
                                                               loc -> 
                                                               
# 1079 "ssrparser.mlg"
                   nullhint 
                                                               )))]);
                                     Tacentries.arg_tag = Some
                                                          (Geninterp.Val.Pair (
                                                          (Geninterp.val_tag (Genarg.topwit wit_bool)), 
                                                          (Geninterp.val_tag (Genarg.topwit wit_ssrortacs))));
                                     Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                             (wit_bool), 
                                                             (wit_ssrortacs)));
                                     Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                            (wit_bool), 
                                                            (wit_ssrortacs)));
                                     Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                             (wit_bool), 
                                                             (wit_ssrortacs)));
                                     Tacentries.arg_printer = ((fun env sigma -> 
                                                              
# 1078 "ssrparser.mlg"
                                                                     pr_ssrhintarg env sigma 
                                                              ), (fun env sigma -> 
                                                              
# 1078 "ssrparser.mlg"
                                                                     pr_ssrhintarg env sigma 
                                                              ), (fun env sigma -> 
                                                              
# 1078 "ssrparser.mlg"
                                                                     pr_ssrhintarg env sigma 
                                                              ));
                                     }
let _ = (wit_ssrhint3arg, ssrhint3arg)

let (wit_ssrortacarg, ssrortacarg) = Tacentries.argument_extend ~name:"ssrortacarg" 
                                     {
                                     Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                              [(Extend.Rule
                                                                (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Stop,
                                                                 (Extend.Atoken (CLexer.terminal "["))),
                                                                 (Extend.Aentry ssrortacs)),
                                                                 (Extend.Atoken (CLexer.terminal "]"))),
                                                                (fun _ tacs _
                                                                loc -> 
                                                                
# 1085 "ssrparser.mlg"
                                   mk_orhint tacs 
                                                                )))]);
                                     Tacentries.arg_tag = Some
                                                          (Geninterp.val_tag (Genarg.topwit wit_ssrhintarg));
                                     Tacentries.arg_intern = Tacentries.ArgInternWit (wit_ssrhintarg);
                                     Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ssrhintarg);
                                     Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_ssrhintarg);
                                     Tacentries.arg_printer = ((fun env sigma -> 
                                                              
# 1084 "ssrparser.mlg"
                                                             pr_ssrhintarg env sigma 
                                                              ), (fun env sigma -> 
                                                              
# 1084 "ssrparser.mlg"
                                                             pr_ssrhintarg env sigma 
                                                              ), (fun env sigma -> 
                                                              
# 1084 "ssrparser.mlg"
                                                             pr_ssrhintarg env sigma 
                                                              ));
                                     }
let _ = (wit_ssrortacarg, ssrortacarg)


# 1088 "ssrparser.mlg"
 

let pr_hint env sigma prt arg =
  if arg = nohint then mt() else str "by " ++ pr_hintarg env sigma prt arg
let pr_ssrhint env sigma _ _ = pr_hint env sigma



let (wit_ssrhint, ssrhint) = Tacentries.argument_extend ~name:"ssrhint" 
                             {
                             Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                      [(Extend.Rule
                                                        (Extend.Stop,
                                                        (fun loc -> 
# 1097 "ssrparser.mlg"
                                 nohint 
                                                                    )))]);
                             Tacentries.arg_tag = Some
                                                  (Geninterp.val_tag (Genarg.topwit wit_ssrhintarg));
                             Tacentries.arg_intern = Tacentries.ArgInternWit (wit_ssrhintarg);
                             Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ssrhintarg);
                             Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_ssrhintarg);
                             Tacentries.arg_printer = ((fun env sigma -> 
                                                      
# 1096 "ssrparser.mlg"
                                                         pr_ssrhint env sigma 
                                                      ), (fun env sigma -> 
                                                      
# 1096 "ssrparser.mlg"
                                                         pr_ssrhint env sigma 
                                                      ), (fun env sigma -> 
                                                      
# 1096 "ssrparser.mlg"
                                                         pr_ssrhint env sigma 
                                                      ));
                             }
let _ = (wit_ssrhint, ssrhint)


# 1112 "ssrparser.mlg"
 

open Ssrmatching_plugin.Ssrmatching
open Ssrmatching_plugin.G_ssrmatching

let pr_wgen = function
  | (clr, Some((id,k),None)) -> spc() ++ pr_clear mt clr ++ str k ++ pr_hoi id
  | (clr, Some((id,k),Some p)) ->
      spc() ++ pr_clear mt clr ++ str"(" ++ str k ++ pr_hoi id ++ str ":=" ++
        pr_cpattern p ++ str ")"
  | (clr, None) -> spc () ++ pr_clear mt clr
let pr_ssrwgen _ _ _ = pr_wgen



let (wit_ssrwgen, ssrwgen) = Tacentries.argument_extend ~name:"ssrwgen" 
                             {
                             Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                      [(Extend.Rule
                                                        (Extend.Next 
                                                         (Extend.Next 
                                                         (Extend.Next 
                                                         (Extend.Next 
                                                         (Extend.Next 
                                                         (Extend.Next 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (CLexer.terminal "("))),
                                                         (Extend.Atoken (CLexer.terminal "@"))),
                                                         (Extend.Aentry ssrhoi_id)),
                                                         (Extend.Atoken (CLexer.terminal ":="))),
                                                         (Extend.Aentry lcpattern)),
                                                         (Extend.Atoken (CLexer.terminal ")"))),
                                                        (fun _ p _ id _ _
                                                        loc -> 
# 1140 "ssrparser.mlg"
    [], Some ((id,"@"),Some p) 
                                                               )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "(@"))),
                                                                    (Extend.Aentry ssrhoi_id)),
                                                                    (Extend.Atoken (CLexer.terminal ":="))),
                                                                    (Extend.Aentry lcpattern)),
                                                                    (Extend.Atoken (CLexer.terminal ")"))),
                                                       (fun _ p _ id _ loc ->
                                                       
# 1138 "ssrparser.mlg"
    [], Some ((id,"@"),Some p) 
                                                       )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "("))),
                                                                    (Extend.Aentry ssrhoi_id)),
                                                                    (Extend.Atoken (CLexer.terminal ")"))),
                                                       (fun _ id _ loc -> 
# 1136 "ssrparser.mlg"
                                 [], Some ((id,"("), None) 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "("))),
                                                                    (Extend.Aentry ssrhoi_id)),
                                                                    (Extend.Atoken (CLexer.terminal ":="))),
                                                                    (Extend.Aentry lcpattern)),
                                                                    (Extend.Atoken (CLexer.terminal ")"))),
                                                       (fun _ p _ id _ loc ->
                                                       
# 1135 "ssrparser.mlg"
    [], Some ((id," "),Some p) 
                                                       )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "@"))),
                                                                    (Extend.Aentry ssrhoi_hyp)),
                                                       (fun hyp _ loc -> 
# 1133 "ssrparser.mlg"
                               [], Some((hyp, "@"), None) 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Stop,
                                                                    (Extend.Aentry ssrhoi_hyp)),
                                                       (fun hyp loc -> 
# 1132 "ssrparser.mlg"
                           [], Some((hyp, " "), None) 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Stop,
                                                                    (Extend.Aentry ssrclear_ne)),
                                                       (fun clr loc -> 
# 1131 "ssrparser.mlg"
                            clr, None 
                                                                    )))]);
                             Tacentries.arg_tag = Some
                                                  (Geninterp.Val.Pair (
                                                  (Geninterp.val_tag (Genarg.topwit wit_ssrclear)), 
                                                  (Geninterp.Val.Opt 
                                                  (Geninterp.Val.Pair (
                                                  (Geninterp.Val.Pair (
                                                  (Geninterp.val_tag (Genarg.topwit wit_ssrhoi_hyp)), 
                                                  (Geninterp.val_tag (Genarg.topwit wit_string)))), 
                                                  (Geninterp.Val.Opt 
                                                  (Geninterp.val_tag (Genarg.topwit wit_cpattern))))))));
                             Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                     (wit_ssrclear), 
                                                     (Genarg.OptArg (Genarg.PairArg (
                                                                    (Genarg.PairArg (
                                                                    (wit_ssrhoi_hyp), 
                                                                    (wit_string))), 
                                                                    (Genarg.OptArg 
                                                                    (wit_cpattern)))))));
                             Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                    (wit_ssrclear), (Genarg.OptArg 
                                                                    (Genarg.PairArg (
                                                                    (Genarg.PairArg (
                                                                    (wit_ssrhoi_hyp), 
                                                                    (wit_string))), 
                                                                    (Genarg.OptArg 
                                                                    (wit_cpattern)))))));
                             Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                     (wit_ssrclear), 
                                                     (Genarg.OptArg (Genarg.PairArg (
                                                                    (Genarg.PairArg (
                                                                    (wit_ssrhoi_hyp), 
                                                                    (wit_string))), 
                                                                    (Genarg.OptArg 
                                                                    (wit_cpattern)))))));
                             Tacentries.arg_printer = ((fun env sigma -> 
                                                      
# 1130 "ssrparser.mlg"
               pr_ssrwgen 
                                                      ), (fun env sigma -> 
                                                      
# 1130 "ssrparser.mlg"
               pr_ssrwgen 
                                                      ), (fun env sigma -> 
                                                      
# 1130 "ssrparser.mlg"
               pr_ssrwgen 
                                                      ));
                             }
let _ = (wit_ssrwgen, ssrwgen)


# 1143 "ssrparser.mlg"
 

let pr_clseq = function
  | InGoal | InHyps -> mt ()
  | InSeqGoal       -> str "|- *"
  | InHypsSeqGoal   -> str " |- *"
  | InHypsGoal      -> str " *"
  | InAll           -> str "*"
  | InHypsSeq       -> str " |-"
  | InAllHyps       -> str "* |-"

let wit_ssrclseq = add_genarg "ssrclseq" (fun env sigma -> pr_clseq)
let pr_clausehyps = pr_list pr_spc pr_wgen
let pr_ssrclausehyps _ _ _ = pr_clausehyps



let (wit_ssrclausehyps, ssrclausehyps) = Tacentries.argument_extend ~name:"ssrclausehyps" 
                                         {
                                         Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                                  [(Extend.Rule
                                                                    (
                                                                    Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Aentry ssrwgen)),
                                                                    (fun hyp
                                                                    loc -> 
                                                                    
# 1164 "ssrparser.mlg"
                        [hyp] 
                                                                    )));
                                                                  (Extend.Rule
                                                                   (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Aentry ssrwgen)),
                                                                    Extend.Aself),
                                                                   (fun hyps
                                                                   hyp loc ->
                                                                   
# 1163 "ssrparser.mlg"
                                            hyp :: hyps 
                                                                   )));
                                                                  (Extend.Rule
                                                                   (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Aentry ssrwgen)),
                                                                    (Extend.Atoken (CLexer.terminal ","))),
                                                                    Extend.Aself),
                                                                   (fun hyps
                                                                   _ hyp
                                                                   loc -> 
                                                                   
# 1162 "ssrparser.mlg"
                                                hyp :: hyps 
                                                                   )))]);
                                         Tacentries.arg_tag = Some
                                                              (Geninterp.Val.List 
                                                              (Geninterp.val_tag (Genarg.topwit wit_ssrwgen)));
                                         Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.ListArg 
                                                                 (wit_ssrwgen));
                                         Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.ListArg 
                                                                (wit_ssrwgen));
                                         Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.ListArg 
                                                                 (wit_ssrwgen));
                                         Tacentries.arg_printer = ((fun env sigma -> 
                                                                  
# 1161 "ssrparser.mlg"
                                   pr_ssrclausehyps 
                                                                  ), (fun env sigma -> 
                                                                  
# 1161 "ssrparser.mlg"
                                   pr_ssrclausehyps 
                                                                  ), (fun env sigma -> 
                                                                  
# 1161 "ssrparser.mlg"
                                   pr_ssrclausehyps 
                                                                  ));
                                         }
let _ = (wit_ssrclausehyps, ssrclausehyps)


# 1167 "ssrparser.mlg"
 

(* type ssrclauses = ssrahyps * ssrclseq *)

let pr_clauses (hyps, clseq) =
  if clseq = InGoal then mt ()
  else str "in " ++ pr_clausehyps hyps ++ pr_clseq clseq
let pr_ssrclauses _ _ _ = pr_clauses



let (wit_ssrclauses, ssrclauses) = Tacentries.argument_extend ~name:"ssrclauses" 
                                   {
                                   Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                            [(Extend.Rule
                                                              (Extend.Stop,
                                                              (fun loc -> 
# 1187 "ssrparser.mlg"
                                               [], InGoal 
                                                                    )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Atoken (CLexer.terminal "in"))),
                                                              (Extend.Atoken (CLexer.terminal "*"))),
                                                              (Extend.Atoken (CLexer.terminal "|-"))),
                                                             (fun _ _ _
                                                             loc -> 
# 1186 "ssrparser.mlg"
                                               [], InAllHyps 
                                                                    )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Atoken (CLexer.terminal "in"))),
                                                              (Extend.Atoken (CLexer.terminal "*"))),
                                                             (fun _ _ loc ->
                                                             
# 1185 "ssrparser.mlg"
                                               [], InAll 
                                                             )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Atoken (CLexer.terminal "in"))),
                                                              (Extend.Atoken (CLexer.terminal "|-"))),
                                                              (Extend.Atoken (CLexer.terminal "*"))),
                                                             (fun _ _ _
                                                             loc -> 
# 1184 "ssrparser.mlg"
                                               [], InSeqGoal 
                                                                    )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Atoken (CLexer.terminal "in"))),
                                                              (Extend.Aentry ssrclausehyps)),
                                                             (fun hyps _
                                                             loc -> 
# 1183 "ssrparser.mlg"
                                               hyps, InHyps 
                                                                    )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Atoken (CLexer.terminal "in"))),
                                                              (Extend.Aentry ssrclausehyps)),
                                                              (Extend.Atoken (CLexer.terminal "*"))),
                                                             (fun _ hyps _
                                                             loc -> 
# 1182 "ssrparser.mlg"
                                               hyps, InHypsGoal 
                                                                    )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Atoken (CLexer.terminal "in"))),
                                                              (Extend.Aentry ssrclausehyps)),
                                                              (Extend.Atoken (CLexer.terminal "|-"))),
                                                             (fun _ hyps _
                                                             loc -> 
# 1181 "ssrparser.mlg"
                                               hyps, InHypsSeq 
                                                                    )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Atoken (CLexer.terminal "in"))),
                                                              (Extend.Aentry ssrclausehyps)),
                                                              (Extend.Atoken (CLexer.terminal "|-"))),
                                                              (Extend.Atoken (CLexer.terminal "*"))),
                                                             (fun _ _ hyps _
                                                             loc -> 
# 1180 "ssrparser.mlg"
                                               hyps, InHypsSeqGoal 
                                                                    )))]);
                                   Tacentries.arg_tag = Some
                                                        (Geninterp.Val.Pair (
                                                        (Geninterp.Val.List 
                                                        (Geninterp.val_tag (Genarg.topwit wit_ssrwgen))), 
                                                        (Geninterp.val_tag (Genarg.topwit wit_ssrclseq))));
                                   Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                           (Genarg.ListArg 
                                                           (wit_ssrwgen)), 
                                                           (wit_ssrclseq)));
                                   Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                          (Genarg.ListArg 
                                                          (wit_ssrwgen)), 
                                                          (wit_ssrclseq)));
                                   Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                           (Genarg.ListArg 
                                                           (wit_ssrwgen)), 
                                                           (wit_ssrclseq)));
                                   Tacentries.arg_printer = ((fun env sigma -> 
                                                            
# 1179 "ssrparser.mlg"
                 pr_ssrclauses 
                                                            ), (fun env sigma -> 
                                                            
# 1179 "ssrparser.mlg"
                 pr_ssrclauses 
                                                            ), (fun env sigma -> 
                                                            
# 1179 "ssrparser.mlg"
                 pr_ssrclauses 
                                                            ));
                                   }
let _ = (wit_ssrclauses, ssrclauses)


# 1191 "ssrparser.mlg"
 

(** Definition value formatting *)

(* We use an intermediate structure to correctly render the binder list  *)
(* abbreviations. We use a list of hints to extract the binders and      *)
(* base term from a term, for the two first levels of representation of  *)
(* of constr terms.                                                      *)

let pr_binder prl = function
  | Bvar x ->
    pr_name x
  | Bdecl (xs, t) ->
    str "(" ++ pr_list pr_spc pr_name xs ++ str " : " ++ prl t ++ str ")"
  | Bdef (x, None, v) ->
    str "(" ++ pr_name x ++ str " := " ++ prl v ++ str ")"
  | Bdef (x, Some t, v) ->
    str "(" ++ pr_name x ++ str " : " ++ prl t ++
                            str " := " ++ prl v ++ str ")"
  | Bstruct x ->
    str "{struct " ++ pr_name x ++ str "}"
  | Bcast t ->
    str ": " ++ prl t

let rec format_local_binders h0 bl0 = match h0, bl0 with
  | BFvar :: h, CLocalAssum ([{CAst.v=x}], _,  _) :: bl ->
    Bvar x :: format_local_binders h bl
  | BFdecl _ :: h, CLocalAssum (lxs, _, t) :: bl ->
    Bdecl (List.map (fun x -> x.CAst.v) lxs, t) :: format_local_binders h bl
  | BFdef :: h, CLocalDef ({CAst.v=x}, v, oty) :: bl ->
    Bdef (x, oty, v) :: format_local_binders h bl
  | _ -> []

let rec format_constr_expr h0 c0 = let open CAst in match h0, c0 with
  | BFvar :: h, { v = CLambdaN ([CLocalAssum([{CAst.v=x}], _, _)], c) } ->
    let bs, c' = format_constr_expr h c in
    Bvar x :: bs, c'
  | BFdecl _:: h, { v = CLambdaN ([CLocalAssum(lxs, _, t)], c) } ->
    let bs, c' = format_constr_expr h c in
    Bdecl (List.map (fun x -> x.CAst.v) lxs, t) :: bs, c'
  | BFdef :: h, { v = CLetIn({CAst.v=x}, v, oty, c) } ->
    let bs, c' = format_constr_expr h c in
    Bdef (x, oty, v) :: bs, c'
  | [BFcast], { v = CCast (c, Glob_term.CastConv t) } ->
    [Bcast t], c
  | BFrec (has_str, has_cast) :: h,
    { v = CFix ( _, [_, Some {CAst.v = CStructRec locn}, bl, t, c]) } ->
    let bs = format_local_binders h bl in
    let bstr = if has_str then [Bstruct (Name locn.CAst.v)] else [] in
    bs @ bstr @ (if has_cast then [Bcast t] else []), c
  | BFrec (_, has_cast) :: h, { v = CCoFix ( _, [_, bl, t, c]) } ->
    format_local_binders h bl @ (if has_cast then [Bcast t] else []), c
  | _, c ->
    [], c

(** Forward chaining argument *)

(* There are three kinds of forward definitions:           *)
(*   - Hint: type only, cast to Type, may have proof hint. *)
(*   - Have: type option + value, no space before type     *)
(*   - Pose: binders + value, space before binders.        *)

let pr_fwdkind = function
  | FwdHint (s,_) -> str (s ^ " ") | _ -> str " :=" ++ spc ()
let pr_fwdfmt (fk, _ : ssrfwdfmt) = pr_fwdkind fk

let wit_ssrfwdfmt = add_genarg "ssrfwdfmt" (fun env sigma -> pr_fwdfmt)

(* type ssrfwd = ssrfwdfmt * ssrterm *)

let mkFwdVal fk c = ((fk, []), c)
let mkssrFwdVal fk c = ((fk, []), (c,None))
let dC t = Glob_term.CastConv t

let same_ist { interp_env = x } { interp_env = y } =
  match x,y with
  | None, None -> true
  | Some a, Some b -> a == b
  | _ -> false

let mkFwdCast fk ?loc ?c t =
  let c = match c with
    | None -> mkCHole loc
    | Some c -> assert (same_ist t c); c.body in
  ((fk, [BFcast]),
   { t with annotation = `None;
            body = (CAst.make ?loc @@ CCast (c, dC t.body)) })

let mkssrFwdCast fk loc t c = ((fk, [BFcast]), (c, Some t))

let mkFwdHint s t =
  let loc =  Constrexpr_ops.constr_loc t.body in
  mkFwdCast (FwdHint (s,false)) ?loc t
let mkFwdHintNoTC s t =
  let loc =  Constrexpr_ops.constr_loc t.body in
  mkFwdCast (FwdHint (s,true)) ?loc t

let pr_gen_fwd prval prc prlc fk (bs, c) =
  let prc s = str s ++ spc () ++ prval prc prlc c in
  match fk, bs with
  | FwdHint (s,_), [Bcast t] -> str s ++ spc () ++ prlc t
  | FwdHint (s,_), _ ->  prc (s ^ "(* typeof *)")
  | FwdHave, [Bcast t] -> str ":" ++ spc () ++ prlc t ++ prc " :="
  | _, [] -> prc " :="
  | _, _ -> spc () ++ pr_list spc (pr_binder prlc) bs ++ prc " :="

let pr_fwd_guarded prval prval' = function
| (fk, h), c ->
  pr_gen_fwd prval pr_constr_expr prl_constr_expr fk (format_constr_expr h c.body)

let pr_unguarded prc prlc = prlc

let pr_fwd = pr_fwd_guarded pr_unguarded pr_unguarded
let pr_ssrfwd _ _ _ = pr_fwd



let (wit_ssrfwd, ssrfwd) = Tacentries.argument_extend ~name:"ssrfwd" 
                           {
                           Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                    [(Extend.Rule
                                                      (Extend.Next (Extend.Next 
                                                                   (Extend.Next 
                                                                   (Extend.Next 
                                                                   (Extend.Stop,
                                                                   (Extend.Atoken (CLexer.terminal ":"))),
                                                                   (Extend.Aentry ast_closure_lterm)),
                                                                   (Extend.Atoken (CLexer.terminal ":="))),
                                                                   (Extend.Aentry ast_closure_lterm)),
                                                      (fun c _ t _ loc -> 
# 1310 "ssrparser.mlg"
                                                                 mkFwdCast FwdPose ~loc t ~c 
                                                                    )));
                                                    (Extend.Rule
                                                     (Extend.Next (Extend.Next 
                                                                  (Extend.Stop,
                                                                  (Extend.Atoken (CLexer.terminal ":="))),
                                                                  (Extend.Aentry ast_closure_lterm)),
                                                     (fun c _ loc -> 
# 1309 "ssrparser.mlg"
                                       mkFwdVal FwdPose c 
                                                                    )))]);
                           Tacentries.arg_tag = Some
                                                (Geninterp.Val.Pair (
                                                (Geninterp.val_tag (Genarg.topwit wit_ssrfwdfmt)), 
                                                (Geninterp.val_tag (Genarg.topwit wit_ast_closure_lterm))));
                           Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                   (wit_ssrfwdfmt), (wit_ast_closure_lterm)));
                           Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                  (wit_ssrfwdfmt), (wit_ast_closure_lterm)));
                           Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                   (wit_ssrfwdfmt), (wit_ast_closure_lterm)));
                           Tacentries.arg_printer = ((fun env sigma -> 
                                                    
# 1308 "ssrparser.mlg"
                                                                             pr_ssrfwd 
                                                    ), (fun env sigma -> 
                                                    
# 1308 "ssrparser.mlg"
                                                                             pr_ssrfwd 
                                                    ), (fun env sigma -> 
                                                    
# 1308 "ssrparser.mlg"
                                                                             pr_ssrfwd 
                                                    ));
                           }
let _ = (wit_ssrfwd, ssrfwd)


# 1318 "ssrparser.mlg"
 

let pr_ssrbvar env sigma prc _ _ v = prc env sigma v



let (wit_ssrbvar, ssrbvar) = Tacentries.argument_extend ~name:"ssrbvar" 
                             {
                             Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                      [(Extend.Rule
                                                        (Extend.Next 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (CLexer.terminal "_"))),
                                                        (fun _ loc -> 
# 1326 "ssrparser.mlg"
               mkCHole (Some loc) 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Stop,
                                                                    (Extend.Aentry ident)),
                                                       (fun id loc -> 
# 1325 "ssrparser.mlg"
                     mkCVar ~loc id 
                                                                    )))]);
                             Tacentries.arg_tag = Some
                                                  (Geninterp.val_tag (Genarg.topwit wit_constr));
                             Tacentries.arg_intern = Tacentries.ArgInternWit (wit_constr);
                             Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_constr);
                             Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_constr);
                             Tacentries.arg_printer = ((fun env sigma -> 
                                                      
# 1324 "ssrparser.mlg"
                                                     pr_ssrbvar env sigma 
                                                      ), (fun env sigma -> 
                                                      
# 1324 "ssrparser.mlg"
                                                     pr_ssrbvar env sigma 
                                                      ), (fun env sigma -> 
                                                      
# 1324 "ssrparser.mlg"
                                                     pr_ssrbvar env sigma 
                                                      ));
                             }
let _ = (wit_ssrbvar, ssrbvar)


# 1329 "ssrparser.mlg"
 

let bvar_lname = let open CAst in function
  | { v = CRef (qid, _) } when qualid_is_ident qid ->
    CAst.make ?loc:qid.CAst.loc @@ Name (qualid_basename qid)
  | { loc = loc } -> CAst.make ?loc Anonymous

let pr_ssrbinder env sigma prc _ _ (_, c) = prc env sigma c



let (wit_ssrbinder, ssrbinder) = Tacentries.argument_extend ~name:"ssrbinder" 
                                 {
                                 Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                          [(Extend.Rule
                                                            (Extend.Next 
                                                             (Extend.Next 
                                                             (Extend.Next 
                                                             (Extend.Next 
                                                             (Extend.Next 
                                                             (Extend.Stop,
                                                             (Extend.Atoken (CLexer.terminal "("))),
                                                             (Extend.Aentry ssrbvar)),
                                                             (Extend.Atoken (CLexer.terminal ":="))),
                                                             (Extend.Aentry lconstr)),
                                                             (Extend.Atoken (CLexer.terminal ")"))),
                                                            (fun _ v _ id _
                                                            loc -> 
# 1361 "ssrparser.mlg"
     (FwdPose,[BFdef]), CAst.make ~loc @@ CLetIn (bvar_lname id, v, None, mkCHole (Some loc)) 
                                                                   )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (CLexer.terminal "("))),
                                                            (Extend.Aentry ssrbvar)),
                                                            (Extend.Atoken (CLexer.terminal ":"))),
                                                            (Extend.Aentry lconstr)),
                                                            (Extend.Atoken (CLexer.terminal ":="))),
                                                            (Extend.Aentry lconstr)),
                                                            (Extend.Atoken (CLexer.terminal ")"))),
                                                           (fun _ v _ t _ id
                                                           _ loc -> 
# 1359 "ssrparser.mlg"
     (FwdPose,[BFdef]), CAst.make ~loc @@ CLetIn (bvar_lname id, v, Some t, mkCHole (Some loc)) 
                                                                    )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (CLexer.terminal "("))),
                                                            (Extend.Aentry ssrbvar)),
                                                            (Extend.Alist1 (Extend.Aentry ssrbvar))),
                                                            (Extend.Atoken (CLexer.terminal ":"))),
                                                            (Extend.Aentry lconstr)),
                                                            (Extend.Atoken (CLexer.terminal ")"))),
                                                           (fun _ t _ bvs bv
                                                           _ loc -> 
# 1354 "ssrparser.mlg"
     let xs = List.map bvar_lname (bv :: bvs) in
     let n = List.length xs in
     (FwdPose, [BFdecl n]),
     CAst.make ~loc @@ CLambdaN ([CLocalAssum (xs, Default Explicit, t)], mkCHole (Some loc)) 
                                                                    )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (CLexer.terminal "("))),
                                                            (Extend.Aentry ssrbvar)),
                                                            (Extend.Atoken (CLexer.terminal ":"))),
                                                            (Extend.Aentry lconstr)),
                                                            (Extend.Atoken (CLexer.terminal ")"))),
                                                           (fun _ t _ bv _
                                                           loc -> 
# 1350 "ssrparser.mlg"
     let x = bvar_lname bv in
     (FwdPose, [BFdecl 1]),
     CAst.make ~loc @@ CLambdaN ([CLocalAssum([x], Default Explicit, t)], mkCHole (Some loc)) 
                                                                  )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (CLexer.terminal "("))),
                                                            (Extend.Aentry ssrbvar)),
                                                            (Extend.Atoken (CLexer.terminal ")"))),
                                                           (fun _ bv _ loc ->
                                                           
# 1346 "ssrparser.mlg"
     let { CAst.loc=xloc } as x = bvar_lname bv in
     (FwdPose, [BFvar]),
     CAst.make ~loc @@ CLambdaN ([CLocalAssum([x],Default Explicit,mkCHole xloc)],mkCHole (Some loc)) 
                                                           )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Aentry ssrbvar)),
                                                           (fun bv loc -> 
# 1342 "ssrparser.mlg"
     let { CAst.loc=xloc } as x = bvar_lname bv in
     (FwdPose, [BFvar]),
     CAst.make ~loc @@ CLambdaN ([CLocalAssum([x],Default Explicit,mkCHole xloc)],mkCHole (Some loc)) 
                                                                    )))]);
                                 Tacentries.arg_tag = Some
                                                      (Geninterp.Val.Pair (
                                                      (Geninterp.val_tag (Genarg.topwit wit_ssrfwdfmt)), 
                                                      (Geninterp.val_tag (Genarg.topwit wit_constr))));
                                 Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                         (wit_ssrfwdfmt), 
                                                         (wit_constr)));
                                 Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                        (wit_ssrfwdfmt), 
                                                        (wit_constr)));
                                 Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                         (wit_ssrfwdfmt), 
                                                         (wit_constr)));
                                 Tacentries.arg_printer = ((fun env sigma -> 
                                                          
# 1340 "ssrparser.mlg"
                                                                     pr_ssrbinder env sigma 
                                                          ), (fun env sigma -> 
                                                          
# 1340 "ssrparser.mlg"
                                                                     pr_ssrbinder env sigma 
                                                          ), (fun env sigma -> 
                                                          
# 1340 "ssrparser.mlg"
                                                                     pr_ssrbinder env sigma 
                                                          ));
                                 }
let _ = (wit_ssrbinder, ssrbinder)

let _ = let () =
        Pcoq.grammar_extend ssrbinder None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Arules [Extend.Rules 
                                                          (Extend.NextNoRec 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (Tok.PKEYWORD ("&")))),
                                                          (fun _ loc -> 
                                                          
# 1367 "ssrparser.mlg"
                                () 
                                                          ));
                                                          Extend.Rules 
                                                          (Extend.NextNoRec 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (Tok.PKEYWORD ("of")))),
                                                          (fun _ loc -> 
                                                          
# 1367 "ssrparser.mlg"
                () 
                                                          ))])),
                              (Extend.Aentryl (operconstr, "99"))),
                 (fun c _ loc -> 
# 1367 "ssrparser.mlg"
                                                                      
     (FwdPose, [BFvar]),
     CAst.make ~loc @@ CLambdaN ([CLocalAssum ([CAst.make ~loc Anonymous],Default Explicit,c)],mkCHole (Some loc)) 
                                 ))])])
        in ()


# 1373 "ssrparser.mlg"
 

let rec binders_fmts = function
  | ((_, h), _) :: bs -> h @ binders_fmts bs
  | _ -> []

let push_binders c2 bs =
  let loc2 = constr_loc c2 in let mkloc loc1 = Loc.merge_opt loc1 loc2 in
  let open CAst in
  let rec loop ty c = function
  | (_, { loc = loc1; v = CLambdaN (b, _) } ) :: bs when ty ->
      CAst.make ?loc:(mkloc loc1) @@ CProdN (b, loop ty c bs)
  | (_, { loc = loc1; v = CLambdaN (b, _) } ) :: bs ->
      CAst.make ?loc:(mkloc loc1) @@ CLambdaN (b, loop ty c bs)
  | (_, { loc = loc1; v = CLetIn (x, v, oty, _) } ) :: bs ->
      CAst.make ?loc:(mkloc loc1) @@ CLetIn (x, v, oty, loop ty c bs)
  | [] -> c
  | _ -> anomaly "binder not a lambda nor a let in" in
  match c2 with
  | { loc; v = CCast (ct, Glob_term.CastConv cty) } ->
      CAst.make ?loc @@ (CCast (loop false ct bs, Glob_term.CastConv (loop true cty bs)))
  | ct -> loop false ct bs

let rec fix_binders = let open CAst in function
  | (_, { v = CLambdaN ([CLocalAssum(xs, _, t)], _) } ) :: bs ->
      CLocalAssum (xs, Default Explicit, t) :: fix_binders bs
  | (_, { v = CLetIn (x, v, oty, _) } ) :: bs ->
    CLocalDef (x, v, oty) :: fix_binders bs
  | _ -> []

let pr_ssrstruct _ _ _ = function
  | Some id -> str "{struct " ++ pr_id id ++ str "}"
  | None -> mt ()



let (wit_ssrstruct, ssrstruct) = Tacentries.argument_extend ~name:"ssrstruct" 
                                 {
                                 Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                          [(Extend.Rule
                                                            (Extend.Stop,
                                                            (fun loc -> 
# 1411 "ssrparser.mlg"
           None 
                                                                    )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (CLexer.terminal "{"))),
                                                            (Extend.Atoken (CLexer.terminal "struct"))),
                                                            (Extend.Aentry ident)),
                                                            (Extend.Atoken (CLexer.terminal "}"))),
                                                           (fun _ id _ _
                                                           loc -> 
# 1410 "ssrparser.mlg"
                                      Some id 
                                                                  )))]);
                                 Tacentries.arg_tag = Some
                                                      (Geninterp.Val.Opt 
                                                      (Geninterp.val_tag (Genarg.topwit wit_ident)));
                                 Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.OptArg 
                                                         (wit_ident));
                                 Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.OptArg 
                                                        (wit_ident));
                                 Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.OptArg 
                                                         (wit_ident));
                                 Tacentries.arg_printer = ((fun env sigma -> 
                                                          
# 1409 "ssrparser.mlg"
                                                             pr_ssrstruct 
                                                          ), (fun env sigma -> 
                                                          
# 1409 "ssrparser.mlg"
                                                             pr_ssrstruct 
                                                          ), (fun env sigma -> 
                                                          
# 1409 "ssrparser.mlg"
                                                             pr_ssrstruct 
                                                          ));
                                 }
let _ = (wit_ssrstruct, ssrstruct)


# 1418 "ssrparser.mlg"
 

let bind_fwd bs ((fk, h), c) =
 (fk,binders_fmts bs @ h), { c with body = push_binders c.body bs }



let (wit_ssrposefwd, ssrposefwd) = Tacentries.argument_extend ~name:"ssrposefwd" 
                                   {
                                   Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                            [(Extend.Rule
                                                              (Extend.Next 
                                                               (Extend.Next 
                                                               (Extend.Stop,
                                                               (Extend.Alist0 (Extend.Aentry ssrbinder))),
                                                               (Extend.Aentry ssrfwd)),
                                                              (fun fwd bs
                                                              loc -> 
                                                              
# 1426 "ssrparser.mlg"
                                            bind_fwd bs fwd 
                                                              )))]);
                                   Tacentries.arg_tag = Some
                                                        (Geninterp.val_tag (Genarg.topwit wit_ssrfwd));
                                   Tacentries.arg_intern = Tacentries.ArgInternWit (wit_ssrfwd);
                                   Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ssrfwd);
                                   Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_ssrfwd);
                                   Tacentries.arg_printer = ((fun env sigma -> 
                                                            
# 1425 "ssrparser.mlg"
                                                        pr_ssrfwd 
                                                            ), (fun env sigma -> 
                                                            
# 1425 "ssrparser.mlg"
                                                        pr_ssrfwd 
                                                            ), (fun env sigma -> 
                                                            
# 1425 "ssrparser.mlg"
                                                        pr_ssrfwd 
                                                            ));
                                   }
let _ = (wit_ssrposefwd, ssrposefwd)


# 1431 "ssrparser.mlg"
 

let pr_ssrfixfwd _ _ _ (id, fwd) = str " fix " ++ pr_id id ++ pr_fwd fwd

let bvar_locid = function
  | { CAst.v = CRef (qid, _) } when qualid_is_ident qid ->
    CAst.make ?loc:qid.CAst.loc (qualid_basename qid)
  | _ -> CErrors.user_err (Pp.str "Missing identifier after \"(co)fix\"")



let (wit_ssrfixfwd, ssrfixfwd) = Tacentries.argument_extend ~name:"ssrfixfwd" 
                                 {
                                 Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                          [(Extend.Rule
                                                            (Extend.Next 
                                                             (Extend.Next 
                                                             (Extend.Next 
                                                             (Extend.Next 
                                                             (Extend.Next 
                                                             (Extend.Stop,
                                                             (Extend.Atoken (CLexer.terminal "fix"))),
                                                             (Extend.Aentry ssrbvar)),
                                                             (Extend.Alist0 (Extend.Aentry ssrbinder))),
                                                             (Extend.Aentry ssrstruct)),
                                                             (Extend.Aentry ssrfwd)),
                                                            (fun fwd sid bs
                                                            bv _ loc -> 
                                                            
# 1444 "ssrparser.mlg"
      let { CAst.v=id } as lid = bvar_locid bv in
      let (fk, h), ac = fwd in
      let c = ac.body in
      let has_cast, t', c' = match format_constr_expr h c with
      | [Bcast t'], c' -> true, t', c'
      | _ -> false, mkCHole (constr_loc c), c in
      let lb = fix_binders bs in
      let has_struct, i =
        let rec loop = function
          | {CAst.loc=l'; v=Name id'} :: _ when Option.equal Id.equal sid (Some id') ->
            true, CAst.make ?loc:l' id'
          | [{CAst.loc=l';v=Name id'}] when sid = None ->
            false, CAst.make ?loc:l' id'
          | _ :: bn -> loop bn
          | [] -> CErrors.user_err (Pp.str "Bad structural argument") in
        loop (names_of_local_assums lb) in
      let h' = BFrec (has_struct, has_cast) :: binders_fmts bs in
      let fix = CAst.make ~loc @@ CFix (lid, [lid, (Some (CAst.make (CStructRec i))), lb, t', c']) in
      id, ((fk, h'),  { ac with body = fix }) 
                                                            )))]);
                                 Tacentries.arg_tag = Some
                                                      (Geninterp.Val.Pair (
                                                      (Geninterp.val_tag (Genarg.topwit wit_ident)), 
                                                      (Geninterp.val_tag (Genarg.topwit wit_ssrfwd))));
                                 Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                         (wit_ident), 
                                                         (wit_ssrfwd)));
                                 Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                        (wit_ident), 
                                                        (wit_ssrfwd)));
                                 Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                         (wit_ident), 
                                                         (wit_ssrfwd)));
                                 Tacentries.arg_printer = ((fun env sigma -> 
                                                          
# 1442 "ssrparser.mlg"
                                                                 pr_ssrfixfwd 
                                                          ), (fun env sigma -> 
                                                          
# 1442 "ssrparser.mlg"
                                                                 pr_ssrfixfwd 
                                                          ), (fun env sigma -> 
                                                          
# 1442 "ssrparser.mlg"
                                                                 pr_ssrfixfwd 
                                                          ));
                                 }
let _ = (wit_ssrfixfwd, ssrfixfwd)


# 1468 "ssrparser.mlg"
 

let pr_ssrcofixfwd _ _ _ (id, fwd) = str " cofix " ++ pr_id id ++ pr_fwd fwd



let (wit_ssrcofixfwd, ssrcofixfwd) = Tacentries.argument_extend ~name:"ssrcofixfwd" 
                                     {
                                     Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                              [(Extend.Rule
                                                                (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Stop,
                                                                 (Extend.Atoken (CLexer.terminal "cofix"))),
                                                                 (Extend.Aentry ssrbvar)),
                                                                 (Extend.Alist0 (Extend.Aentry ssrbinder))),
                                                                 (Extend.Aentry ssrfwd)),
                                                                (fun fwd bs
                                                                bv _ loc -> 
                                                                
# 1476 "ssrparser.mlg"
      let { CAst.v=id } as lid = bvar_locid bv in
      let (fk, h), ac = fwd in
      let c = ac.body in
      let has_cast, t', c' = match format_constr_expr h c with
      | [Bcast t'], c' -> true, t', c'
      | _ -> false, mkCHole (constr_loc c), c in
      let h' = BFrec (false, has_cast) :: binders_fmts bs in
      let cofix = CAst.make ~loc @@ CCoFix (lid, [lid, fix_binders bs, t', c']) in
      id, ((fk, h'), { ac with body = cofix })
    
                                                                )))]);
                                     Tacentries.arg_tag = Some
                                                          (Geninterp.val_tag (Genarg.topwit wit_ssrfixfwd));
                                     Tacentries.arg_intern = Tacentries.ArgInternWit (wit_ssrfixfwd);
                                     Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ssrfixfwd);
                                     Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_ssrfixfwd);
                                     Tacentries.arg_printer = ((fun env sigma -> 
                                                              
# 1474 "ssrparser.mlg"
                                                            pr_ssrcofixfwd 
                                                              ), (fun env sigma -> 
                                                              
# 1474 "ssrparser.mlg"
                                                            pr_ssrcofixfwd 
                                                              ), (fun env sigma -> 
                                                              
# 1474 "ssrparser.mlg"
                                                            pr_ssrcofixfwd 
                                                              ));
                                     }
let _ = (wit_ssrcofixfwd, ssrcofixfwd)


# 1488 "ssrparser.mlg"
 

(* This does not print the type, it should be fixed... *)
let pr_ssrsetfwd _ _ _ (((fk,_),(t,_)), docc) =
  pr_gen_fwd (fun _ _ -> pr_cpattern)
    (fun _ -> mt()) (fun _ -> mt()) fk ([Bcast ()],t)



let (wit_ssrsetfwd, ssrsetfwd) = Tacentries.argument_extend ~name:"ssrsetfwd" 
                                 {
                                 Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                          [(Extend.Rule
                                                            (Extend.Next 
                                                             (Extend.Next 
                                                             (Extend.Stop,
                                                             (Extend.Atoken (CLexer.terminal ":="))),
                                                             (Extend.Aentry lcpattern)),
                                                            (fun c _ loc -> 
# 1506 "ssrparser.mlg"
                             mkssrFwdVal FwdPose c, nodocc 
                                                                    )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (CLexer.terminal ":="))),
                                                            (Extend.Atoken (CLexer.terminal "{"))),
                                                            (Extend.Aentry ssrocc)),
                                                            (Extend.Atoken (CLexer.terminal "}"))),
                                                            (Extend.Aentry cpattern)),
                                                           (fun c _ occ _ _
                                                           loc -> 
# 1505 "ssrparser.mlg"
    mkssrFwdVal FwdPose c, mkocc occ 
                                                                  )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (CLexer.terminal ":"))),
                                                            (Extend.Aentry ast_closure_lterm)),
                                                            (Extend.Atoken (CLexer.terminal ":="))),
                                                            (Extend.Aentry lcpattern)),
                                                           (fun c _ t _
                                                           loc -> 
# 1503 "ssrparser.mlg"
    mkssrFwdCast FwdPose loc t c, nodocc 
                                                                  )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (CLexer.terminal ":"))),
                                                            (Extend.Aentry ast_closure_lterm)),
                                                            (Extend.Atoken (CLexer.terminal ":="))),
                                                            (Extend.Atoken (CLexer.terminal "{"))),
                                                            (Extend.Aentry ssrocc)),
                                                            (Extend.Atoken (CLexer.terminal "}"))),
                                                            (Extend.Aentry cpattern)),
                                                           (fun c _ occ _ _ t
                                                           _ loc -> 
# 1501 "ssrparser.mlg"
    mkssrFwdCast FwdPose loc t c, mkocc occ 
                                                                    )))]);
                                 Tacentries.arg_tag = Some
                                                      (Geninterp.Val.Pair (
                                                      (Geninterp.Val.Pair (
                                                      (Geninterp.val_tag (Genarg.topwit wit_ssrfwdfmt)), 
                                                      (Geninterp.Val.Pair (
                                                      (Geninterp.val_tag (Genarg.topwit wit_lcpattern)), 
                                                      (Geninterp.Val.Opt 
                                                      (Geninterp.val_tag (Genarg.topwit wit_ast_closure_lterm))))))), 
                                                      (Geninterp.val_tag (Genarg.topwit wit_ssrdocc))));
                                 Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                         (Genarg.PairArg (
                                                         (wit_ssrfwdfmt), 
                                                         (Genarg.PairArg (
                                                         (wit_lcpattern), 
                                                         (Genarg.OptArg 
                                                         (wit_ast_closure_lterm)))))), 
                                                         (wit_ssrdocc)));
                                 Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                        (Genarg.PairArg (
                                                        (wit_ssrfwdfmt), 
                                                        (Genarg.PairArg (
                                                        (wit_lcpattern), 
                                                        (Genarg.OptArg 
                                                        (wit_ast_closure_lterm)))))), 
                                                        (wit_ssrdocc)));
                                 Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                         (Genarg.PairArg (
                                                         (wit_ssrfwdfmt), 
                                                         (Genarg.PairArg (
                                                         (wit_lcpattern), 
                                                         (Genarg.OptArg 
                                                         (wit_ast_closure_lterm)))))), 
                                                         (wit_ssrdocc)));
                                 Tacentries.arg_printer = ((fun env sigma -> 
                                                          
# 1499 "ssrparser.mlg"
             pr_ssrsetfwd 
                                                          ), (fun env sigma -> 
                                                          
# 1499 "ssrparser.mlg"
             pr_ssrsetfwd 
                                                          ), (fun env sigma -> 
                                                          
# 1499 "ssrparser.mlg"
             pr_ssrsetfwd 
                                                          ));
                                 }
let _ = (wit_ssrsetfwd, ssrsetfwd)


# 1509 "ssrparser.mlg"
 

let pr_ssrhavefwd env sigma _ _ prt (fwd, hint) = pr_fwd fwd ++ pr_hint env sigma prt hint



let (wit_ssrhavefwd, ssrhavefwd) = Tacentries.argument_extend ~name:"ssrhavefwd" 
                                   {
                                   Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                            [(Extend.Rule
                                                              (Extend.Next 
                                                               (Extend.Next 
                                                               (Extend.Stop,
                                                               (Extend.Atoken (CLexer.terminal ":="))),
                                                               (Extend.Aentry ast_closure_lterm)),
                                                              (fun c _ loc ->
                                                              
# 1519 "ssrparser.mlg"
                                     mkFwdVal FwdHave c, nohint 
                                                              )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Atoken (CLexer.terminal ":"))),
                                                              (Extend.Aentry ast_closure_lterm)),
                                                              (Extend.Atoken (CLexer.terminal ":="))),
                                                             (fun _ t _
                                                             loc -> 
# 1518 "ssrparser.mlg"
                                         mkFwdHintNoTC ":" t, nohint 
                                                                    )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Atoken (CLexer.terminal ":"))),
                                                              (Extend.Aentry ast_closure_lterm)),
                                                              (Extend.Atoken (CLexer.terminal ":="))),
                                                              (Extend.Aentry ast_closure_lterm)),
                                                             (fun c _ t _
                                                             loc -> 
# 1517 "ssrparser.mlg"
                                                              mkFwdCast FwdHave ~loc t ~c, nohint 
                                                                    )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Atoken (CLexer.terminal ":"))),
                                                              (Extend.Aentry ast_closure_lterm)),
                                                              (Extend.Aentry ssrhint)),
                                                             (fun hint t _
                                                             loc -> 
# 1516 "ssrparser.mlg"
                                                  mkFwdHint ":" t, hint 
                                                                    )))]);
                                   Tacentries.arg_tag = Some
                                                        (Geninterp.Val.Pair (
                                                        (Geninterp.val_tag (Genarg.topwit wit_ssrfwd)), 
                                                        (Geninterp.val_tag (Genarg.topwit wit_ssrhint))));
                                   Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                           (wit_ssrfwd), 
                                                           (wit_ssrhint)));
                                   Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                          (wit_ssrfwd), 
                                                          (wit_ssrhint)));
                                   Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                           (wit_ssrfwd), 
                                                           (wit_ssrhint)));
                                   Tacentries.arg_printer = ((fun env sigma -> 
                                                            
# 1515 "ssrparser.mlg"
                                                                    pr_ssrhavefwd env sigma 
                                                            ), (fun env sigma -> 
                                                            
# 1515 "ssrparser.mlg"
                                                                    pr_ssrhavefwd env sigma 
                                                            ), (fun env sigma -> 
                                                            
# 1515 "ssrparser.mlg"
                                                                    pr_ssrhavefwd env sigma 
                                                            ));
                                   }
let _ = (wit_ssrhavefwd, ssrhavefwd)


# 1522 "ssrparser.mlg"
 

let intro_id_to_binder = List.map (function
  | IPatId id ->
      let { CAst.loc=xloc } as x = bvar_lname (mkCVar id) in
      (FwdPose, [BFvar]),
        CAst.make @@ CLambdaN ([CLocalAssum([x], Default Explicit, mkCHole xloc)],
          mkCHole None)
  | _ -> anomaly "non-id accepted as binder")

let binder_to_intro_id = CAst.(List.map (function
  | (FwdPose, [BFvar]), { v = CLambdaN ([CLocalAssum(ids,_,_)],_) }
  | (FwdPose, [BFdecl _]), { v = CLambdaN ([CLocalAssum(ids,_,_)],_) } ->
      List.map (function {v=Name id} -> IPatId id | _ -> IPatAnon (One None)) ids
  | (FwdPose, [BFdef]), { v = CLetIn ({v=Name id},_,_,_) } -> [IPatId id]
  | (FwdPose, [BFdef]), { v = CLetIn ({v=Anonymous},_,_,_) } -> [IPatAnon (One None)]
  | _ -> anomaly "ssrbinder is not a binder"))

let pr_ssrhavefwdwbinders env sigma _ _ prt (tr,((hpats, (fwd, hint)))) =
  pr_hpats hpats ++ pr_fwd fwd ++ pr_hint env sigma prt hint



let (wit_ssrhavefwdwbinders, ssrhavefwdwbinders) = Tacentries.argument_extend ~name:"ssrhavefwdwbinders" 
                                                   {
                                                   Tacentries.arg_parsing = 
                                                   Vernacextend.Arg_rules (
                                                   [(Extend.Rule
                                                     (Extend.Next (Extend.Next 
                                                                  (Extend.Next 
                                                                  (Extend.Stop,
                                                                  (Extend.Aentry ssrhpats_wtransp)),
                                                                  (Extend.Alist0 (Extend.Aentry ssrbinder))),
                                                                  (Extend.Aentry ssrhavefwd)),
                                                     (fun fwd bs trpats
                                                     loc -> 
# 1549 "ssrparser.mlg"
    let tr, pats = trpats in
    let ((clr, pats), binders), simpl = pats in
    let allbs = intro_id_to_binder binders @ bs in
    let allbinders = binders @ List.flatten (binder_to_intro_id bs) in
    let hint = bind_fwd allbs (fst fwd), snd fwd in
    tr, ((((clr, pats), allbinders), simpl), hint) 
                                                            )))]);
                                                   Tacentries.arg_tag = 
                                                   Some
                                                   (Geninterp.Val.Pair (
                                                   (Geninterp.val_tag (Genarg.topwit wit_bool)), 
                                                   (Geninterp.Val.Pair (
                                                   (Geninterp.val_tag (Genarg.topwit wit_ssrhpats)), 
                                                   (Geninterp.Val.Pair (
                                                   (Geninterp.val_tag (Genarg.topwit wit_ssrfwd)), 
                                                   (Geninterp.val_tag (Genarg.topwit wit_ssrhint))))))));
                                                   Tacentries.arg_intern = 
                                                   Tacentries.ArgInternWit (Genarg.PairArg (
                                                   (wit_bool), (Genarg.PairArg (
                                                               (wit_ssrhpats), 
                                                               (Genarg.PairArg (
                                                               (wit_ssrfwd), 
                                                               (wit_ssrhint)))))));
                                                   Tacentries.arg_subst = 
                                                   Tacentries.ArgSubstWit (Genarg.PairArg (
                                                   (wit_bool), (Genarg.PairArg (
                                                               (wit_ssrhpats), 
                                                               (Genarg.PairArg (
                                                               (wit_ssrfwd), 
                                                               (wit_ssrhint)))))));
                                                   Tacentries.arg_interp = 
                                                   Tacentries.ArgInterpWit (Genarg.PairArg (
                                                   (wit_bool), (Genarg.PairArg (
                                                               (wit_ssrhpats), 
                                                               (Genarg.PairArg (
                                                               (wit_ssrfwd), 
                                                               (wit_ssrhint)))))));
                                                   Tacentries.arg_printer = 
                                                   ((fun env sigma -> 
                                                   
# 1547 "ssrparser.mlg"
               pr_ssrhavefwdwbinders env sigma 
                                                   ), (fun env sigma -> 
                                                   
# 1547 "ssrparser.mlg"
               pr_ssrhavefwdwbinders env sigma 
                                                   ), (fun env sigma -> 
                                                   
# 1547 "ssrparser.mlg"
               pr_ssrhavefwdwbinders env sigma 
                                                   ));
                                                   }
let _ = (wit_ssrhavefwdwbinders, ssrhavefwdwbinders)


# 1557 "ssrparser.mlg"
 

let pr_ssrdoarg env sigma prc _ prt (((n, m), tac), clauses) =
  pr_index n ++ pr_mmod m ++ pr_hintarg env sigma prt tac ++ pr_clauses clauses



let (wit_ssrdoarg, ssrdoarg) = Tacentries.argument_extend ~name:"ssrdoarg" 
                               {
                               Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                        [(Extend.Rule
                                                          (Extend.Next 
                                                           (Extend.Stop,
                                                           (Extend.Atoken (CLexer.terminal "YouShouldNotTypeThis"))),
                                                          (fun _ loc -> 
# 1567 "ssrparser.mlg"
                                  anomaly "Grammar placeholder match" 
                                                                    )))]);
                               Tacentries.arg_tag = Some
                                                    (Geninterp.Val.Pair (
                                                    (Geninterp.Val.Pair (
                                                    (Geninterp.Val.Pair (
                                                    (Geninterp.val_tag (Genarg.topwit wit_ssrindex)), 
                                                    (Geninterp.val_tag (Genarg.topwit wit_ssrmmod)))), 
                                                    (Geninterp.val_tag (Genarg.topwit wit_ssrhintarg)))), 
                                                    (Geninterp.val_tag (Genarg.topwit wit_ssrclauses))));
                               Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                       (Genarg.PairArg (
                                                       (Genarg.PairArg (
                                                       (wit_ssrindex), 
                                                       (wit_ssrmmod))), 
                                                       (wit_ssrhintarg))), 
                                                       (wit_ssrclauses)));
                               Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                      (Genarg.PairArg (
                                                      (Genarg.PairArg (
                                                      (wit_ssrindex), 
                                                      (wit_ssrmmod))), 
                                                      (wit_ssrhintarg))), 
                                                      (wit_ssrclauses)));
                               Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                       (Genarg.PairArg (
                                                       (Genarg.PairArg (
                                                       (wit_ssrindex), 
                                                       (wit_ssrmmod))), 
                                                       (wit_ssrhintarg))), 
                                                       (wit_ssrclauses)));
                               Tacentries.arg_printer = ((fun env sigma -> 
                                                        
# 1566 "ssrparser.mlg"
               pr_ssrdoarg env sigma 
                                                        ), (fun env sigma -> 
                                                        
# 1566 "ssrparser.mlg"
               pr_ssrdoarg env sigma 
                                                        ), (fun env sigma -> 
                                                        
# 1566 "ssrparser.mlg"
               pr_ssrdoarg env sigma 
                                                        ));
                               }
let _ = (wit_ssrdoarg, ssrdoarg)


# 1570 "ssrparser.mlg"
 

(* type ssrseqarg = ssrindex * (ssrtacarg * ssrtac option) *)

let pr_seqtacarg env sigma prt = function
  | (is_first, []), _ -> str (if is_first then "first" else "last")
  | tac, Some dtac ->
    hv 0 (pr_hintarg env sigma prt tac ++ spc() ++ str "|| " ++ prt env sigma tacltop dtac)
  | tac, _ -> pr_hintarg env sigma prt tac

let pr_ssrseqarg env sigma _ _ prt = function
  | ArgArg 0, tac -> pr_seqtacarg env sigma prt tac
  | i, tac -> pr_index i ++ str " " ++ pr_seqtacarg env sigma prt tac



let (wit_ssrseqarg, ssrseqarg) = Tacentries.argument_extend ~name:"ssrseqarg" 
                                 {
                                 Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                          [(Extend.Rule
                                                            (Extend.Next 
                                                             (Extend.Stop,
                                                             (Extend.Atoken (CLexer.terminal "YouShouldNotTypeThis"))),
                                                            (fun _ loc -> 
# 1590 "ssrparser.mlg"
                                  anomaly "Grammar placeholder match" 
                                                                    )))]);
                                 Tacentries.arg_tag = Some
                                                      (Geninterp.Val.Pair (
                                                      (Geninterp.val_tag (Genarg.topwit wit_ssrindex)), 
                                                      (Geninterp.Val.Pair (
                                                      (Geninterp.val_tag (Genarg.topwit wit_ssrhintarg)), 
                                                      (Geninterp.Val.Opt 
                                                      (Geninterp.val_tag (Genarg.topwit wit_tactic)))))));
                                 Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                         (wit_ssrindex), 
                                                         (Genarg.PairArg (
                                                         (wit_ssrhintarg), 
                                                         (Genarg.OptArg 
                                                         (wit_tactic))))));
                                 Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                        (wit_ssrindex), 
                                                        (Genarg.PairArg (
                                                        (wit_ssrhintarg), 
                                                        (Genarg.OptArg 
                                                        (wit_tactic))))));
                                 Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                         (wit_ssrindex), 
                                                         (Genarg.PairArg (
                                                         (wit_ssrhintarg), 
                                                         (Genarg.OptArg 
                                                         (wit_tactic))))));
                                 Tacentries.arg_printer = ((fun env sigma -> 
                                                          
# 1589 "ssrparser.mlg"
                                       pr_ssrseqarg env sigma 
                                                          ), (fun env sigma -> 
                                                          
# 1589 "ssrparser.mlg"
                                       pr_ssrseqarg env sigma 
                                                          ), (fun env sigma -> 
                                                          
# 1589 "ssrparser.mlg"
                                       pr_ssrseqarg env sigma 
                                                          ));
                                 }
let _ = (wit_ssrseqarg, ssrseqarg)


# 1593 "ssrparser.mlg"
 

let sq_brace_tacnames =
   ["first"; "solve"; "do"; "rewrite"; "have"; "suffices"; "wlog"]
   (* "by" is a keyword *)
let accept_ssrseqvar strm =
  match stream_nth 0 strm with
  | Tok.IDENT id when not (List.mem id sq_brace_tacnames) ->
     accept_before_syms_or_ids ["["] ["first";"last"] strm
  | _ -> raise Stream.Failure

let test_ssrseqvar = Pcoq.Entry.of_parser "test_ssrseqvar" accept_ssrseqvar

let swaptacarg (loc, b) = (b, []), Some (TacId [])

let check_seqtacarg dir arg = match snd arg, dir with
  | ((true, []), Some (TacAtom { CAst.loc })), L2R ->
    CErrors.user_err ?loc (str "expected \"last\"")
  | ((false, []), Some (TacAtom { CAst.loc })), R2L ->
    CErrors.user_err ?loc (str "expected \"first\"")
  | _, _ -> arg

let ssrorelse = Entry.create "ssrorelse"



let _ = let ssrseqidx = Pcoq.Entry.create "ssrseqidx"
        and ssrswap = Pcoq.Entry.create "ssrswap"
        in
        let () =
        Pcoq.grammar_extend ssrseqidx None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry Prim.natural)),
                 (fun n loc -> 
# 1623 "ssrparser.mlg"
                            ArgArg (check_index ~loc n) 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry test_ssrseqvar)),
                             (Extend.Aentry Prim.ident)),
                (fun id _ loc -> 
# 1622 "ssrparser.mlg"
                                           ArgVar (CAst.make ~loc id) 
                                 ))])])
        in let () =
        Pcoq.grammar_extend ssrswap None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                              ("last"))))),
                 (fun _ loc -> 
# 1625 "ssrparser.mlg"
                                                                 loc, false 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("first"))))),
                (fun _ loc -> 
# 1625 "ssrparser.mlg"
                                 loc, true 
                              ))])])
        in let () =
        Pcoq.grammar_extend ssrorelse None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PKEYWORD ("||")))),
                              (Extend.Aentryl (tactic_expr, "2"))),
                 (fun tac _ loc -> 
# 1626 "ssrparser.mlg"
                                                       tac 
                                   ))])])
        in let () =
        Pcoq.grammar_extend ssrseqarg None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Aentryl (tactic_expr, "3"))),
                 (fun tac loc -> 
# 1631 "ssrparser.mlg"
                                       noindex, (mk_hint tac, None) 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry ssrseqidx)),
                             (Extend.Aentry ssrswap)),
                (fun arg i loc -> 
# 1630 "ssrparser.mlg"
                                        i, swaptacarg arg 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry ssrseqidx)),
                                          (Extend.Aentry ssrortacarg)),
                             (Extend.Aopt (Extend.Aentry ssrorelse))),
                (fun def tac i loc -> 
# 1629 "ssrparser.mlg"
                                                                 i, (tac, def) 
                                      ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry ssrswap)),
                (fun arg loc -> 
# 1628 "ssrparser.mlg"
                         noindex, swaptacarg arg 
                                ))])])
        in ()


# 1635 "ssrparser.mlg"
 

let tactic_expr = Pltac.tactic_expr




# 1647 "ssrparser.mlg"
 

(* Let's play with the new proof engine API *)
let old_tac = V82.tactic




# 1665 "ssrparser.mlg"
 

let ssr_reserved_ids = Summary.ref ~name:"SSR:idents" true

let () =
  Goptions.(declare_bool_option
    { optname  = "ssreflect identifiers";
      optkey   = ["SsrIdents"];
      optdepr  = false;
      optread  = (fun _ -> !ssr_reserved_ids);
      optwrite = (fun b -> ssr_reserved_ids := b)
    })

let is_ssr_reserved s =
  let n = String.length s in n > 2 && s.[0] = '_' && s.[n - 1] = '_'

let ssr_id_of_string loc s =
  if is_ssr_reserved s && is_ssr_loaded () then begin
    if !ssr_reserved_ids then
      CErrors.user_err ~loc (str ("The identifier " ^ s ^ " is reserved."))
    else if is_internal_name s then
      Feedback.msg_warning (str ("Conflict between " ^ s ^ " and ssreflect internal names."))
    else Feedback.msg_warning (str (
     "The name " ^ s ^ " fits the _xxx_ format used for anonymous variables.\n"
  ^ "Scripts with explicit references to anonymous variables are fragile."))
    end; Id.of_string s

let ssr_null_entry = Pcoq.Entry.of_parser "ssr_null" (fun _ -> ())



let _ = let () =
        Pcoq.grammar_extend Prim.ident None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PIDENT (None)))),
                              (Extend.Aentry ssr_null_entry)),
                 (fun _ s loc -> 
# 1698 "ssrparser.mlg"
                                                ssr_id_of_string loc s 
                                 ))])])
        in ()


# 1701 "ssrparser.mlg"
 

let perm_tag = "_perm_Hyp_"
let _ = add_internal_name (is_tagged perm_tag)




# 1720 "ssrparser.mlg"
 

type ssrargfmt = ArgSsr of string | ArgSep of string

let ssrtac_name name = {
  mltac_plugin = "ssreflect_plugin";
  mltac_tactic = "ssr" ^ name;
}

let ssrtac_entry name n = {
  mltac_name = ssrtac_name name;
  mltac_index = n;
}

let set_pr_ssrtac name prec afmt = (* FIXME *) () (*
  let fmt = List.map (function
    | ArgSep s -> Egramml.GramTerminal s
    | ArgSsr s -> Egramml.GramTerminal s
    | ArgCoq at -> Egramml.GramTerminal "COQ_ARG") afmt in
  let tacname = ssrtac_name name in () *)

let ssrtac_atom ?loc name args = TacML (CAst.make ?loc (ssrtac_entry name 0, args))
let ssrtac_expr ?loc name args = ssrtac_atom ?loc name args

let tclintros_expr ?loc tac ipats =
  let args = [Tacexpr.TacGeneric (in_gen (rawwit wit_ssrintrosarg) (tac, ipats))] in
  ssrtac_expr ?loc "tclintros" args



let _ = let () =
        Pcoq.grammar_extend tactic_expr None
        (Some
        (Gramlib.Gramext.Level "1"), [(None, Some (Gramlib.Gramext.RightA),
                                      [Extend.Rule
                                       (Extend.Next (Extend.Next (Extend.Stop,
                                                                 (Extend.Aentry tactic_expr)),
                                                    (Extend.Aentry ssrintros_ne)),
                                       (fun intros tac loc -> 
# 1753 "ssrparser.mlg"
                                                    tclintros_expr ~loc tac intros 
                                                              ))])])
        in ()

let _ = let ssrparentacarg = Pcoq.Entry.create "ssrparentacarg"
        in
        let () =
        Pcoq.grammar_extend ssrparentacarg None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                           (Extend.Aentry tactic_expr)),
                              (Extend.Atoken (Tok.PKEYWORD (")")))),
                 (fun _ tac _ loc -> 
# 1768 "ssrparser.mlg"
                                                      CAst.make ~loc (Tacexp tac) 
                                     ))])])
        in let () =
        Pcoq.grammar_extend tactic_expr None
        (Some
        (Gramlib.Gramext.Level "0"), [(None, None,
                                      [Extend.Rule
                                       (Extend.Next (Extend.Stop,
                                                    (Extend.Aentry ssrparentacarg)),
                                       (fun arg loc -> 
# 1769 "ssrparser.mlg"
                                                      TacArg arg 
                                                       ))])])
        in ()


# 1781 "ssrparser.mlg"
 

let ssrautoprop gl =
  try
    let tacname =
      try Tacenv.locate_tactic (qualid_of_ident (Id.of_string "ssrautoprop"))
      with Not_found -> Tacenv.locate_tactic (ssrqid "ssrautoprop") in
    let tacexpr = CAst.make @@ Tacexpr.Reference (ArgArg (Loc.tag @@ tacname)) in
    V82.of_tactic (eval_tactic (Tacexpr.TacArg tacexpr)) gl
  with Not_found -> V82.of_tactic (Auto.full_trivial []) gl

let () = ssrautoprop_tac := ssrautoprop

let tclBY tac = Tacticals.tclTHEN tac (donetac ~-1)

(** Tactical arguments. *)

(* We have four kinds: simple tactics, [|]-bracketed lists, hints, and swaps *)
(* The latter two are used in forward-chaining tactics (have, suffice, wlog) *)
(* and subgoal reordering tacticals (; first & ; last), respectively.        *)

(* Force use of the tactic_expr parsing entry, to rule out tick marks. *)

(** The "by" tactical. *)


open Ssrfwd



let () = Tacentries.tactic_extend __coq_plugin_name "ssrtclby" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("by", Tacentries.TyArg (
                                                      Extend.TUentry (Genarg.get_arg_tag wit_ssrhintarg), 
                                                      Tacentries.TyNil)), 
           (fun tac ist -> 
# 1812 "ssrparser.mlg"
                                V82.tactic (hinttac ist true tac) 
           )))]

let _ = let () =
        Pcoq.grammar_extend ssrhint None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PKEYWORD ("by")))),
                              (Extend.Aentry ssrhintarg)),
                 (fun arg _ loc -> 
# 1820 "ssrparser.mlg"
                                          arg 
                                   ))])])
        in ()

let () = Tacentries.tactic_extend __coq_plugin_name "ssrtcldo" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("YouShouldNotTypeThis", 
                            Tacentries.TyIdent ("do", Tacentries.TyArg (
                                                      Extend.TUentry (Genarg.get_arg_tag wit_ssrdoarg), 
                                                      Tacentries.TyNil))), 
           (fun arg ist -> 
# 1829 "ssrparser.mlg"
                                                     V82.tactic (ssrdotac ist arg) 
           )))]


# 1832 "ssrparser.mlg"
 

let _ = set_pr_ssrtac "tcldo" 3 [ArgSep "do "; ArgSsr "doarg"]

let ssrdotac_expr ?loc n m tac clauses =
  let arg = ((n, m), tac), clauses in
  ssrtac_expr ?loc "tcldo" [Tacexpr.TacGeneric (in_gen (rawwit wit_ssrdoarg) arg)]



let _ = let ssrdotac = Pcoq.Entry.create "ssrdotac"
        in
        let () =
        Pcoq.grammar_extend ssrdotac None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry ssrortacarg)),
                 (fun tacs loc -> 
# 1846 "ssrparser.mlg"
                              tacs 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Aentryl (tactic_expr, "3"))),
                (fun tac loc -> 
# 1845 "ssrparser.mlg"
                                       mk_hint tac 
                                ))])])
        in let () =
        Pcoq.grammar_extend tactic_expr None
        (Some
        (Gramlib.Gramext.Level "3"), [(None, Some (Gramlib.Gramext.RightA),
                                      [Extend.Rule
                                       (Extend.Next (Extend.Next (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Stop,
                                                                 (Extend.Atoken (Tok.PIDENT (Some
                                                                 ("do"))))),
                                                                 (Extend.Aentry int_or_var)),
                                                                 (Extend.Aentry ssrmmod)),
                                                                 (Extend.Aentry ssrdotac)),
                                                    (Extend.Aentry ssrclauses)),
                                       (fun clauses tac m n _ loc -> 
# 1855 "ssrparser.mlg"
        ssrdotac_expr ~loc (mk_index ~loc n) m tac clauses 
                                                                    ));
                                      Extend.Rule
                                      (Extend.Next (Extend.Next (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (Tok.PIDENT (Some
                                                                ("do"))))),
                                                                (Extend.Aentry ssrortacarg)),
                                                   (Extend.Aentry ssrclauses)),
                                      (fun clauses tac _ loc -> 
# 1852 "ssrparser.mlg"
        ssrdotac_expr ~loc noindex Once tac clauses 
                                                                ));
                                      Extend.Rule
                                      (Extend.Next (Extend.Next (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (Tok.PIDENT (Some
                                                                ("do"))))),
                                                                (Extend.Aentry ssrmmod)),
                                                                (Extend.Aentry ssrdotac)),
                                                   (Extend.Aentry ssrclauses)),
                                      (fun clauses tac m _ loc -> 
# 1850 "ssrparser.mlg"
        ssrdotac_expr ~loc noindex m tac clauses 
                                                                  ))])])
        in ()


# 1859 "ssrparser.mlg"
 

(* We can't actually parse the direction separately because this   *)
(* would introduce conflicts with the basic ltac syntax.           *)
let pr_ssrseqdir _ _ _ = function
  | L2R -> str ";" ++ spc () ++ str "first "
  | R2L -> str ";" ++ spc () ++ str "last "



let (wit_ssrseqdir, ssrseqdir) = Tacentries.argument_extend ~name:"ssrseqdir" 
                                 {
                                 Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                          [(Extend.Rule
                                                            (Extend.Next 
                                                             (Extend.Stop,
                                                             (Extend.Atoken (CLexer.terminal "YouShouldNotTypeThis"))),
                                                            (fun _ loc -> 
# 1870 "ssrparser.mlg"
                                  anomaly "Grammar placeholder match" 
                                                                    )))]);
                                 Tacentries.arg_tag = Some
                                                      (Geninterp.val_tag (Genarg.topwit wit_ssrdir));
                                 Tacentries.arg_intern = Tacentries.ArgInternWit (wit_ssrdir);
                                 Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ssrdir);
                                 Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_ssrdir);
                                 Tacentries.arg_printer = ((fun env sigma -> 
                                                          
# 1869 "ssrparser.mlg"
                                                       pr_ssrseqdir 
                                                          ), (fun env sigma -> 
                                                          
# 1869 "ssrparser.mlg"
                                                       pr_ssrseqdir 
                                                          ), (fun env sigma -> 
                                                          
# 1869 "ssrparser.mlg"
                                                       pr_ssrseqdir 
                                                          ));
                                 }
let _ = (wit_ssrseqdir, ssrseqdir)

let () = Tacentries.tactic_extend __coq_plugin_name "ssrtclseq" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("YouShouldNotTypeThis", 
                            Tacentries.TyArg (Extend.TUentry (Genarg.get_arg_tag wit_ssrtclarg), 
                            Tacentries.TyArg (Extend.TUentry (Genarg.get_arg_tag wit_ssrseqdir), 
                            Tacentries.TyArg (Extend.TUentry (Genarg.get_arg_tag wit_ssrseqarg), 
                            Tacentries.TyNil)))), (fun tac dir arg ist -> 
# 1875 "ssrparser.mlg"
    V82.tactic (tclSEQAT ist tac dir arg) 
                                                  )))]


# 1878 "ssrparser.mlg"
 

let _ = set_pr_ssrtac "tclseq" 5 [ArgSsr "tclarg"; ArgSsr "seqdir"; ArgSsr "seqarg"]

let tclseq_expr ?loc tac dir arg =
  let arg1 = in_gen (rawwit wit_ssrtclarg) tac in
  let arg2 = in_gen (rawwit wit_ssrseqdir) dir in
  let arg3 = in_gen (rawwit wit_ssrseqarg) (check_seqtacarg dir arg) in
  ssrtac_expr ?loc "tclseq" (List.map (fun x -> Tacexpr.TacGeneric x) [arg1; arg2; arg3])



let _ = let ssr_first = Pcoq.Entry.create "ssr_first"
        and ssr_first_else = Pcoq.Entry.create "ssr_first_else"
        in
        let () =
        Pcoq.grammar_extend ssr_first None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("[")))),
                                           (Extend.Alist0sep ((Extend.Aentry tactic_expr), (Extend.Atoken (Tok.PKEYWORD ("|")))))),
                              (Extend.Atoken (Tok.PKEYWORD ("]")))),
                 (fun _ tacl _ loc -> 
# 1894 "ssrparser.mlg"
                                                      TacFirst tacl 
                                      ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry ssr_first)),
                             (Extend.Aentry ssrintros_ne)),
                (fun ipats tac loc -> 
# 1893 "ssrparser.mlg"
                                                 tclintros_expr ~loc tac ipats 
                                      ))])])
        in let () =
        Pcoq.grammar_extend ssr_first_else None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry ssr_first)),
                 (fun tac loc -> 
# 1898 "ssrparser.mlg"
                           tac 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry ssr_first)),
                             (Extend.Aentry ssrorelse)),
                (fun tac2 tac1 loc -> 
# 1897 "ssrparser.mlg"
                                              TacOrelse (tac1, tac2) 
                                      ))])])
        in let () =
        Pcoq.grammar_extend tactic_expr None
        (Some
        (Gramlib.Gramext.Level "4"), [(None, Some (Gramlib.Gramext.LeftA),
                                      [Extend.Rule
                                       (Extend.Next (Extend.Next (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Stop,
                                                                 (Extend.Aentry tactic_expr)),
                                                                 (Extend.Atoken (Tok.PKEYWORD (";")))),
                                                                 (Extend.Atoken (Tok.PIDENT (Some
                                                                 ("last"))))),
                                                    (Extend.Aentry ssrseqarg)),
                                       (fun arg _ _ tac loc -> 
# 1905 "ssrparser.mlg"
        tclseq_expr ~loc tac R2L arg 
                                                               ));
                                      Extend.Rule
                                      (Extend.Next (Extend.Next (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Aentry tactic_expr)),
                                                                (Extend.Atoken (Tok.PKEYWORD (";")))),
                                                                (Extend.Atoken (Tok.PIDENT (Some
                                                                ("first"))))),
                                                   (Extend.Aentry ssrseqarg)),
                                      (fun arg _ _ tac loc -> 
# 1903 "ssrparser.mlg"
        tclseq_expr ~loc tac L2R arg 
                                                              ));
                                      Extend.Rule
                                      (Extend.Next (Extend.Next (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Aentry tactic_expr)),
                                                                (Extend.Atoken (Tok.PKEYWORD (";")))),
                                                                (Extend.Atoken (Tok.PIDENT (Some
                                                                ("first"))))),
                                                   (Extend.Aentry ssr_first_else)),
                                      (fun tac2 _ _ tac1 loc -> 
# 1901 "ssrparser.mlg"
        TacThen (tac1, tac2) 
                                                                ))])])
        in ()


# 1917 "ssrparser.mlg"
 

let pr_gen (docc, dt) = pr_docc docc ++ pr_cpattern dt

let pr_ssrgen _ _ _ = pr_gen



let (wit_ssrgen, ssrgen) = Tacentries.argument_extend ~name:"ssrgen" 
                           {
                           Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                    [(Extend.Rule
                                                      (Extend.Next (Extend.Stop,
                                                                   (Extend.Aentry cpattern)),
                                                      (fun dt loc -> 
# 1930 "ssrparser.mlg"
                        nodocc, dt 
                                                                    )));
                                                    (Extend.Rule
                                                     (Extend.Next (Extend.Next 
                                                                  (Extend.Stop,
                                                                  (Extend.Aentry ssrdocc)),
                                                                  (Extend.Aentry cpattern)),
                                                     (fun dt docc loc -> 
# 1926 "ssrparser.mlg"
                                     
     match docc with
     | Some [], _ -> CErrors.user_err ~loc (str"Clear flag {} not allowed here")
     | _ -> docc, dt 
                                                                    )))]);
                           Tacentries.arg_tag = Some
                                                (Geninterp.Val.Pair (
                                                (Geninterp.val_tag (Genarg.topwit wit_ssrdocc)), 
                                                (Geninterp.val_tag (Genarg.topwit wit_cpattern))));
                           Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                   (wit_ssrdocc), (wit_cpattern)));
                           Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                  (wit_ssrdocc), (wit_cpattern)));
                           Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                   (wit_ssrdocc), (wit_cpattern)));
                           Tacentries.arg_printer = ((fun env sigma -> 
                                                    
# 1925 "ssrparser.mlg"
                                                                  pr_ssrgen 
                                                    ), (fun env sigma -> 
                                                    
# 1925 "ssrparser.mlg"
                                                                  pr_ssrgen 
                                                    ), (fun env sigma -> 
                                                    
# 1925 "ssrparser.mlg"
                                                                  pr_ssrgen 
                                                    ));
                           }
let _ = (wit_ssrgen, ssrgen)


# 1933 "ssrparser.mlg"
 

let has_occ ((_, occ), _) = occ <> None

(** Generalization (discharge) sequence *)

(* A discharge sequence is represented as a list of up to two   *)
(* lists of d-items, plus an ident list set (the possibly empty *)
(* final clear switch). The main list is empty iff the command  *)
(* is defective, and has length two if there is a sequence of   *)
(* dependent terms (and in that case it is the first of the two *)
(* lists). Thus, the first of the two lists is never empty.     *)

(* type ssrgens = ssrgen list *)
(* type ssrdgens = ssrgens list * ssrclear *)

let gens_sep = function [], [] -> mt | _ -> spc

let pr_dgens pr_gen (gensl, clr) =
  let prgens s gens = str s ++ pr_list spc pr_gen gens in
  let prdeps deps = prgens ": " deps ++ spc () ++ str "/" in
  match gensl with
  | [deps; []] -> prdeps deps ++ pr_clear pr_spc clr
  | [deps; gens] -> prdeps deps ++ prgens " " gens ++ pr_clear spc clr
  | [gens] -> prgens ": " gens ++ pr_clear spc clr
  | _ -> pr_clear pr_spc clr

let pr_ssrdgens _ _ _ = pr_dgens pr_gen

let cons_gen gen = function
  | gens :: gensl, clr -> (gen :: gens) :: gensl, clr
  | _ -> anomaly "missing gen list"

let cons_dep (gensl, clr) =
  if List.length gensl = 1 then ([] :: gensl, clr) else
  CErrors.user_err (Pp.str "multiple dependents switches '/'")



let (wit_ssrdgens_tl, ssrdgens_tl) = Tacentries.argument_extend ~name:"ssrdgens_tl" 
                                     {
                                     Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                              [(Extend.Rule
                                                                (Extend.Stop,
                                                                (fun loc -> 
# 1985 "ssrparser.mlg"
    [[]], [] 
                                                                    )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Aentry cpattern)),
                                                                Extend.Aself),
                                                               (fun dgens dt
                                                               loc -> 
                                                               
# 1983 "ssrparser.mlg"
    cons_gen (nodocc, dt) dgens 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "/"))),
                                                                Extend.Aself),
                                                               (fun dgens _
                                                               loc -> 
                                                               
# 1981 "ssrparser.mlg"
    cons_dep dgens 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "{"))),
                                                                (Extend.Aentry ssrocc)),
                                                                (Extend.Atoken (CLexer.terminal "}"))),
                                                                (Extend.Aentry cpattern)),
                                                                Extend.Aself),
                                                               (fun dgens dt
                                                               _ occ _ loc ->
                                                               
# 1979 "ssrparser.mlg"
    cons_gen (mkocc occ, dt) dgens 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "{"))),
                                                                (Extend.Alist1 (Extend.Aentry ssrhyp))),
                                                                (Extend.Atoken (CLexer.terminal "}"))),
                                                               (fun _ clr _
                                                               loc -> 
                                                               
# 1977 "ssrparser.mlg"
    [[]], clr 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "{"))),
                                                                (Extend.Alist1 (Extend.Aentry ssrhyp))),
                                                                (Extend.Atoken (CLexer.terminal "}"))),
                                                                (Extend.Aentry cpattern)),
                                                                Extend.Aself),
                                                               (fun dgens dt
                                                               _ clr _ loc ->
                                                               
# 1975 "ssrparser.mlg"
    cons_gen (mkclr clr, dt) dgens 
                                                               )))]);
                                     Tacentries.arg_tag = Some
                                                          (Geninterp.Val.Pair (
                                                          (Geninterp.Val.List 
                                                          (Geninterp.Val.List 
                                                          (Geninterp.val_tag (Genarg.topwit wit_ssrgen)))), 
                                                          (Geninterp.val_tag (Genarg.topwit wit_ssrclear))));
                                     Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                             (Genarg.ListArg 
                                                             (Genarg.ListArg 
                                                             (wit_ssrgen))), 
                                                             (wit_ssrclear)));
                                     Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                            (Genarg.ListArg 
                                                            (Genarg.ListArg 
                                                            (wit_ssrgen))), 
                                                            (wit_ssrclear)));
                                     Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                             (Genarg.ListArg 
                                                             (Genarg.ListArg 
                                                             (wit_ssrgen))), 
                                                             (wit_ssrclear)));
                                     Tacentries.arg_printer = ((fun env sigma -> 
                                                              
# 1973 "ssrparser.mlg"
                                         pr_ssrdgens 
                                                              ), (fun env sigma -> 
                                                              
# 1973 "ssrparser.mlg"
                                         pr_ssrdgens 
                                                              ), (fun env sigma -> 
                                                              
# 1973 "ssrparser.mlg"
                                         pr_ssrdgens 
                                                              ));
                                     }
let _ = (wit_ssrdgens_tl, ssrdgens_tl)

let (wit_ssrdgens, ssrdgens) = Tacentries.argument_extend ~name:"ssrdgens" 
                               {
                               Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                        [(Extend.Rule
                                                          (Extend.Next 
                                                           (Extend.Next 
                                                           (Extend.Next 
                                                           (Extend.Stop,
                                                           (Extend.Atoken (CLexer.terminal ":"))),
                                                           (Extend.Aentry ssrgen)),
                                                           (Extend.Aentry ssrdgens_tl)),
                                                          (fun dgens gen _
                                                          loc -> 
# 1989 "ssrparser.mlg"
                                              cons_gen gen dgens 
                                                                 )))]);
                               Tacentries.arg_tag = Some
                                                    (Geninterp.val_tag (Genarg.topwit wit_ssrdgens_tl));
                               Tacentries.arg_intern = Tacentries.ArgInternWit (wit_ssrdgens_tl);
                               Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ssrdgens_tl);
                               Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_ssrdgens_tl);
                               Tacentries.arg_printer = ((fun env sigma -> 
                                                        
# 1988 "ssrparser.mlg"
                                                           pr_ssrdgens 
                                                        ), (fun env sigma -> 
                                                        
# 1988 "ssrparser.mlg"
                                                           pr_ssrdgens 
                                                        ), (fun env sigma -> 
                                                        
# 1988 "ssrparser.mlg"
                                                           pr_ssrdgens 
                                                        ));
                               }
let _ = (wit_ssrdgens, ssrdgens)


# 1996 "ssrparser.mlg"
 
type ssreqid = ssripatrep option

let pr_eqid = function Some pat -> str " " ++ pr_ipat pat | None -> mt ()
let pr_ssreqid _ _ _ = pr_eqid



let (wit_ssreqid, ssreqid) = Tacentries.argument_extend ~name:"ssreqid" 
                             {
                             Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                      [(Extend.Rule
                                                        (Extend.Next 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (CLexer.terminal "YouShouldNotTypeThis"))),
                                                        (fun _ loc -> 
# 2007 "ssrparser.mlg"
                                  anomaly "Grammar placeholder match" 
                                                                    )))]);
                             Tacentries.arg_tag = Some
                                                  (Geninterp.Val.Opt 
                                                  (Geninterp.val_tag (Genarg.topwit wit_ssripatrep)));
                             Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.OptArg 
                                                     (wit_ssripatrep));
                             Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.OptArg 
                                                    (wit_ssripatrep));
                             Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.OptArg 
                                                     (wit_ssripatrep));
                             Tacentries.arg_printer = ((fun env sigma -> 
                                                      
# 2006 "ssrparser.mlg"
                                                                pr_ssreqid 
                                                      ), (fun env sigma -> 
                                                      
# 2006 "ssrparser.mlg"
                                                                pr_ssreqid 
                                                      ), (fun env sigma -> 
                                                      
# 2006 "ssrparser.mlg"
                                                                pr_ssreqid 
                                                      ));
                             }
let _ = (wit_ssreqid, ssreqid)


# 2010 "ssrparser.mlg"
 

let accept_ssreqid strm =
  match Util.stream_nth 0 strm with
  | Tok.IDENT _ -> accept_before_syms [":"] strm
  | Tok.KEYWORD ":" -> ()
  | Tok.KEYWORD pat when List.mem pat ["_"; "?"; "->"; "<-"] ->
                      accept_before_syms [":"] strm
  | _ -> raise Stream.Failure

let test_ssreqid = Pcoq.Entry.of_parser "test_ssreqid" accept_ssreqid



let _ = let ssreqpat = Pcoq.Entry.create "ssreqpat"
        in
        let () =
        Pcoq.grammar_extend ssreqpat None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Atoken (Tok.PKEYWORD ("<-")))),
                 (fun _ loc -> 
# 2038 "ssrparser.mlg"
                IPatRewrite (allocc, R2L) 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("->")))),
                (fun _ loc -> 
# 2037 "ssrparser.mlg"
                IPatRewrite (allocc, L2R) 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry ssrdocc)),
                             (Extend.Atoken (Tok.PKEYWORD ("<-")))),
                (fun _ occ loc -> 
# 2034 "ssrparser.mlg"
                               match occ with
      | None, occ ->  IPatRewrite (occ, R2L)
      | _ -> CErrors.user_err ~loc (str "Only occurrences are allowed here") 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry ssrdocc)),
                             (Extend.Atoken (Tok.PKEYWORD ("->")))),
                (fun _ occ loc -> 
# 2031 "ssrparser.mlg"
                               match occ with
      | None, occ -> IPatRewrite (occ, L2R)
      | _ -> CErrors.user_err ~loc (str"Only occurrences are allowed here") 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("+")))),
                (fun _ loc -> 
# 2030 "ssrparser.mlg"
               IPatAnon Temporary 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("?")))),
                (fun _ loc -> 
# 2029 "ssrparser.mlg"
               IPatAnon (One None) 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("_")))),
                (fun _ loc -> 
# 2028 "ssrparser.mlg"
               IPatAnon Drop 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry Prim.ident)),
                (fun id loc -> 
# 2027 "ssrparser.mlg"
                           IPatId id 
                               ))])])
        in let () =
        Pcoq.grammar_extend ssreqid None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry test_ssreqid)),
                 (fun _ loc -> 
# 2042 "ssrparser.mlg"
                        None 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry test_ssreqid)),
                             (Extend.Aentry ssreqpat)),
                (fun pat _ loc -> 
# 2041 "ssrparser.mlg"
                                        Some pat 
                                  ))])])
        in ()


# 2053 "ssrparser.mlg"
 

type ssrarg = ssrfwdview * (ssreqid * (cpattern ssragens * ssripats))

(* type ssrarg = ssrbwdview * (ssreqid * (ssrdgens * ssripats)) *)

let pr_ssrarg _ _ _ (view, (eqid, (dgens, ipats))) =
  let pri = pr_intros (gens_sep dgens) in
  pr_view2 view ++ pr_eqid eqid ++ pr_dgens pr_gen dgens ++ pri ipats



let (wit_ssrarg, ssrarg) = Tacentries.argument_extend ~name:"ssrarg" 
                           {
                           Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                    [(Extend.Rule
                                                      (Extend.Next (Extend.Stop,
                                                                   (Extend.Aentry ssrintros_ne)),
                                                      (fun ipats loc -> 
# 2076 "ssrparser.mlg"
    [], (None, (([], []), ipats)) 
                                                                    )));
                                                    (Extend.Rule
                                                     (Extend.Next (Extend.Next 
                                                                  (Extend.Stop,
                                                                  (Extend.Aentry ssrclear_ne)),
                                                                  (Extend.Aentry ssrintros)),
                                                     (fun ipats clr loc -> 
# 2074 "ssrparser.mlg"
    [], (None, (([], clr), ipats)) 
                                                                    )));
                                                    (Extend.Rule
                                                     (Extend.Next (Extend.Next 
                                                                  (Extend.Next 
                                                                  (Extend.Stop,
                                                                  (Extend.Aentry ssreqid)),
                                                                  (Extend.Aentry ssrdgens)),
                                                                  (Extend.Aentry ssrintros)),
                                                     (fun ipats dgens eqid
                                                     loc -> 
# 2072 "ssrparser.mlg"
    [], (eqid, (dgens, ipats)) 
                                                            )));
                                                    (Extend.Rule
                                                     (Extend.Next (Extend.Next 
                                                                  (Extend.Next 
                                                                  (Extend.Stop,
                                                                  (Extend.Aentry ssrfwdview)),
                                                                  (Extend.Aentry ssrclear)),
                                                                  (Extend.Aentry ssrintros)),
                                                     (fun ipats clr view
                                                     loc -> 
# 2070 "ssrparser.mlg"
    view, (None, (([], clr), ipats)) 
                                                            )));
                                                    (Extend.Rule
                                                     (Extend.Next (Extend.Next 
                                                                  (Extend.Next 
                                                                  (Extend.Next 
                                                                  (Extend.Stop,
                                                                  (Extend.Aentry ssrfwdview)),
                                                                  (Extend.Aentry ssreqid)),
                                                                  (Extend.Aentry ssrdgens)),
                                                                  (Extend.Aentry ssrintros)),
                                                     (fun ipats dgens eqid
                                                     view loc -> 
# 2068 "ssrparser.mlg"
    view, (eqid, (dgens, ipats)) 
                                                                 )))]);
                           Tacentries.arg_tag = Some
                                                (Geninterp.Val.Pair (
                                                (Geninterp.val_tag (Genarg.topwit wit_ssrfwdview)), 
                                                (Geninterp.Val.Pair (
                                                (Geninterp.val_tag (Genarg.topwit wit_ssreqid)), 
                                                (Geninterp.Val.Pair (
                                                (Geninterp.val_tag (Genarg.topwit wit_ssrdgens)), 
                                                (Geninterp.val_tag (Genarg.topwit wit_ssrintros))))))));
                           Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                   (wit_ssrfwdview), 
                                                   (Genarg.PairArg ((wit_ssreqid), 
                                                   (Genarg.PairArg ((wit_ssrdgens), 
                                                   (wit_ssrintros)))))));
                           Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                  (wit_ssrfwdview), (Genarg.PairArg (
                                                                    (wit_ssreqid), 
                                                                    (Genarg.PairArg (
                                                                    (wit_ssrdgens), 
                                                                    (wit_ssrintros)))))));
                           Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                   (wit_ssrfwdview), 
                                                   (Genarg.PairArg ((wit_ssreqid), 
                                                   (Genarg.PairArg ((wit_ssrdgens), 
                                                   (wit_ssrintros)))))));
                           Tacentries.arg_printer = ((fun env sigma -> 
                                                    
# 2066 "ssrparser.mlg"
                pr_ssrarg 
                                                    ), (fun env sigma -> 
                                                    
# 2066 "ssrparser.mlg"
                pr_ssrarg 
                                                    ), (fun env sigma -> 
                                                    
# 2066 "ssrparser.mlg"
                pr_ssrarg 
                                                    ));
                           }
let _ = (wit_ssrarg, ssrarg)

let () = Tacentries.tactic_extend __coq_plugin_name "ssrclear" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("clear", Tacentries.TyArg (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_natural), 
                                                         Tacentries.TyNil)), 
           (fun n ist -> 
# 2084 "ssrparser.mlg"
                                tclIPAT (List.init n (fun _ -> IOpDrop)) 
           )))]


# 2089 "ssrparser.mlg"
 

(* TODO: review this, in particular the => _ and => [] cases *)
let rec improper_intros = function
  | IPatSimpl _ :: ipats -> improper_intros ipats
  | (IPatId _ | IPatAnon _ | IPatCase _ | IPatDispatch _) :: _ -> false
  | _ -> true (* FIXME *)

let check_movearg = function
  | view, (eqid, _) when view <> [] && eqid <> None ->
    CErrors.user_err (Pp.str "incompatible view and equation in move tactic")
  | view, (_, (([gen :: _], _), _)) when view <> [] && has_occ gen ->
    CErrors.user_err (Pp.str "incompatible view and occurrence switch in move tactic")
  | _, (_, ((dgens, _), _)) when List.length dgens > 1 ->
    CErrors.user_err (Pp.str "dependents switch `/' in move tactic")
  | _, (eqid, (_, ipats)) when eqid <> None && improper_intros ipats ->
    CErrors.user_err (Pp.str "no proper intro pattern for equation in move tactic")
  | arg -> arg



let (wit_ssrmovearg, ssrmovearg) = Tacentries.argument_extend ~name:"ssrmovearg" 
                                   {
                                   Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                            [(Extend.Rule
                                                              (Extend.Next 
                                                               (Extend.Stop,
                                                               (Extend.Aentry ssrarg)),
                                                              (fun arg loc ->
                                                              
# 2111 "ssrparser.mlg"
                       check_movearg arg 
                                                              )))]);
                                   Tacentries.arg_tag = Some
                                                        (Geninterp.val_tag (Genarg.topwit wit_ssrarg));
                                   Tacentries.arg_intern = Tacentries.ArgInternWit (wit_ssrarg);
                                   Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ssrarg);
                                   Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_ssrarg);
                                   Tacentries.arg_printer = ((fun env sigma -> 
                                                            
# 2110 "ssrparser.mlg"
                                                        pr_ssrarg 
                                                            ), (fun env sigma -> 
                                                            
# 2110 "ssrparser.mlg"
                                                        pr_ssrarg 
                                                            ), (fun env sigma -> 
                                                            
# 2110 "ssrparser.mlg"
                                                        pr_ssrarg 
                                                            ));
                                   }
let _ = (wit_ssrmovearg, ssrmovearg)


# 2114 "ssrparser.mlg"
 

let movearg_of_parsed_movearg (v,(eq,(dg,ip))) =
  (v,(eq,(ssrdgens_of_parsed_dgens dg,ip)))



let () = Tacentries.tactic_extend __coq_plugin_name "ssrmove" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("move", Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrmovearg), 
                                                        Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrrpat), 
                                                        Tacentries.TyNil))), 
           (fun arg pat ist -> 
# 2123 "ssrparser.mlg"
    ssrmovetac (movearg_of_parsed_movearg arg) <*> tclIPAT (tclCompileIPats [pat]) 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("move", Tacentries.TyArg (
                                                       Extend.TUentry (Genarg.get_arg_tag wit_ssrmovearg), 
                                                       Tacentries.TyArg (
                                                       Extend.TUentry (Genarg.get_arg_tag wit_ssrclauses), 
                                                       Tacentries.TyNil))), 
          (fun arg clauses ist -> 
# 2125 "ssrparser.mlg"
    tclCLAUSES (ssrmovetac (movearg_of_parsed_movearg arg)) clauses 
          )));
         (Tacentries.TyML (Tacentries.TyIdent ("move", Tacentries.TyArg (
                                                       Extend.TUentry (Genarg.get_arg_tag wit_ssrrpat), 
                                                       Tacentries.TyNil)), 
          (fun pat ist -> 
# 2126 "ssrparser.mlg"
                               tclIPAT (tclCompileIPats [pat]) 
          )));
         (Tacentries.TyML (Tacentries.TyIdent ("move", Tacentries.TyNil), 
          (fun ist -> 
# 2127 "ssrparser.mlg"
                  ssrsmovetac 
          )))]


# 2130 "ssrparser.mlg"
 

let check_casearg = function
| view, (_, (([_; gen :: _], _), _)) when view <> [] && has_occ gen ->
  CErrors.user_err (Pp.str "incompatible view and occurrence switch in dependent case tactic")
| arg -> arg



let (wit_ssrcasearg, ssrcasearg) = Tacentries.argument_extend ~name:"ssrcasearg" 
                                   {
                                   Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                            [(Extend.Rule
                                                              (Extend.Next 
                                                               (Extend.Stop,
                                                               (Extend.Aentry ssrarg)),
                                                              (fun arg loc ->
                                                              
# 2140 "ssrparser.mlg"
                       check_casearg arg 
                                                              )))]);
                                   Tacentries.arg_tag = Some
                                                        (Geninterp.val_tag (Genarg.topwit wit_ssrarg));
                                   Tacentries.arg_intern = Tacentries.ArgInternWit (wit_ssrarg);
                                   Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ssrarg);
                                   Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_ssrarg);
                                   Tacentries.arg_printer = ((fun env sigma -> 
                                                            
# 2139 "ssrparser.mlg"
                                                        pr_ssrarg 
                                                            ), (fun env sigma -> 
                                                            
# 2139 "ssrparser.mlg"
                                                        pr_ssrarg 
                                                            ), (fun env sigma -> 
                                                            
# 2139 "ssrparser.mlg"
                                                        pr_ssrarg 
                                                            ));
                                   }
let _ = (wit_ssrcasearg, ssrcasearg)

let () = Tacentries.tactic_extend __coq_plugin_name "ssrcase" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("case", Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrcasearg), 
                                                        Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrclauses), 
                                                        Tacentries.TyNil))), 
           (fun arg clauses ist -> 
# 2145 "ssrparser.mlg"
    tclCLAUSES (ssrcasetac (movearg_of_parsed_movearg arg)) clauses 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("case", Tacentries.TyNil), 
          (fun ist -> 
# 2146 "ssrparser.mlg"
                  ssrscasetoptac 
          )))]

let () = Tacentries.tactic_extend __coq_plugin_name "ssrelim" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("elim", Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrarg), 
                                                        Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrclauses), 
                                                        Tacentries.TyNil))), 
           (fun arg clauses ist -> 
# 2153 "ssrparser.mlg"
    tclCLAUSES (ssrelimtac (movearg_of_parsed_movearg arg)) clauses 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("elim", Tacentries.TyNil), 
          (fun ist -> 
# 2154 "ssrparser.mlg"
                  ssrselimtoptac 
          )))]


# 2161 "ssrparser.mlg"
 

let pr_agen (docc, dt) = pr_docc docc ++ pr_term dt
let pr_ssragen _ _ _ = pr_agen
let pr_ssragens _ _ _ = pr_dgens pr_agen



let (wit_ssragen, ssragen) = Tacentries.argument_extend ~name:"ssragen" 
                             {
                             Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                      [(Extend.Rule
                                                        (Extend.Next 
                                                         (Extend.Stop,
                                                         (Extend.Aentry ssrterm)),
                                                        (fun dt loc -> 
# 2171 "ssrparser.mlg"
                       nodocc, dt 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "{"))),
                                                                    (Extend.Alist1 (Extend.Aentry ssrhyp))),
                                                                    (Extend.Atoken (CLexer.terminal "}"))),
                                                                    (Extend.Aentry ssrterm)),
                                                       (fun dt _ clr _ loc ->
                                                       
# 2170 "ssrparser.mlg"
                                                   mkclr clr, dt 
                                                       )))]);
                             Tacentries.arg_tag = Some
                                                  (Geninterp.Val.Pair (
                                                  (Geninterp.val_tag (Genarg.topwit wit_ssrdocc)), 
                                                  (Geninterp.val_tag (Genarg.topwit wit_ssrterm))));
                             Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                     (wit_ssrdocc), (wit_ssrterm)));
                             Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                    (wit_ssrdocc), (wit_ssrterm)));
                             Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                     (wit_ssrdocc), (wit_ssrterm)));
                             Tacentries.arg_printer = ((fun env sigma -> 
                                                      
# 2169 "ssrparser.mlg"
                                                                  pr_ssragen 
                                                      ), (fun env sigma -> 
                                                      
# 2169 "ssrparser.mlg"
                                                                  pr_ssragen 
                                                      ), (fun env sigma -> 
                                                      
# 2169 "ssrparser.mlg"
                                                                  pr_ssragen 
                                                      ));
                             }
let _ = (wit_ssragen, ssragen)

let (wit_ssragens, ssragens) = Tacentries.argument_extend ~name:"ssragens" 
                               {
                               Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                        [(Extend.Rule
                                                          (Extend.Stop,
                                                          (fun loc -> 
# 2181 "ssrparser.mlg"
           [[]], [] 
                                                                    )));
                                                        (Extend.Rule
                                                         (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Stop,
                                                          (Extend.Aentry ssrterm)),
                                                          Extend.Aself),
                                                         (fun agens dt loc ->
                                                         
# 2180 "ssrparser.mlg"
    cons_gen (nodocc, dt) agens 
                                                         )));
                                                        (Extend.Rule
                                                         (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (CLexer.terminal "{"))),
                                                          (Extend.Alist1 (Extend.Aentry ssrhyp))),
                                                          (Extend.Atoken (CLexer.terminal "}"))),
                                                         (fun _ clr _ loc ->
                                                         
# 2178 "ssrparser.mlg"
                                       [[]], clr
                                                         )));
                                                        (Extend.Rule
                                                         (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (CLexer.terminal "{"))),
                                                          (Extend.Alist1 (Extend.Aentry ssrhyp))),
                                                          (Extend.Atoken (CLexer.terminal "}"))),
                                                          (Extend.Aentry ssrterm)),
                                                          Extend.Aself),
                                                         (fun agens dt _ clr
                                                         _ loc -> 
# 2177 "ssrparser.mlg"
    cons_gen (mkclr clr, dt) agens 
                                                                  )))]);
                               Tacentries.arg_tag = Some
                                                    (Geninterp.Val.Pair (
                                                    (Geninterp.Val.List 
                                                    (Geninterp.Val.List 
                                                    (Geninterp.val_tag (Genarg.topwit wit_ssragen)))), 
                                                    (Geninterp.val_tag (Genarg.topwit wit_ssrclear))));
                               Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                       (Genarg.ListArg 
                                                       (Genarg.ListArg 
                                                       (wit_ssragen))), 
                                                       (wit_ssrclear)));
                               Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                      (Genarg.ListArg 
                                                      (Genarg.ListArg 
                                                      (wit_ssragen))), 
                                                      (wit_ssrclear)));
                               Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                       (Genarg.ListArg 
                                                       (Genarg.ListArg 
                                                       (wit_ssragen))), 
                                                       (wit_ssrclear)));
                               Tacentries.arg_printer = ((fun env sigma -> 
                                                        
# 2175 "ssrparser.mlg"
             pr_ssragens 
                                                        ), (fun env sigma -> 
                                                        
# 2175 "ssrparser.mlg"
             pr_ssragens 
                                                        ), (fun env sigma -> 
                                                        
# 2175 "ssrparser.mlg"
             pr_ssragens 
                                                        ));
                               }
let _ = (wit_ssragens, ssragens)


# 2184 "ssrparser.mlg"
 

let mk_applyarg views agens intros = views, (agens, intros)

let pr_ssraarg _ _ _ (view, (dgens, ipats)) =
  let pri = pr_intros (gens_sep dgens) in
  pr_view view ++ pr_dgens pr_agen dgens ++ pri ipats



let (wit_ssrapplyarg, ssrapplyarg) = Tacentries.argument_extend ~name:"ssrapplyarg" 
                                     {
                                     Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                              [(Extend.Rule
                                                                (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Stop,
                                                                 (Extend.Aentry ssrbwdview)),
                                                                 (Extend.Aentry ssrclear)),
                                                                 (Extend.Aentry ssrintros)),
                                                                (fun intros
                                                                clr view
                                                                loc -> 
                                                                
# 2206 "ssrparser.mlg"
    mk_applyarg view ([], clr) intros 
                                                                )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Aentry ssrbwdview)),
                                                                (Extend.Atoken (CLexer.terminal ":"))),
                                                                (Extend.Aentry ssragen)),
                                                                (Extend.Aentry ssragens)),
                                                                (Extend.Aentry ssrintros)),
                                                               (fun intros
                                                               dgens gen _
                                                               view loc -> 
                                                               
# 2204 "ssrparser.mlg"
    mk_applyarg view (cons_gen gen dgens) intros 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Aentry ssrintros_ne)),
                                                               (fun intros
                                                               loc -> 
                                                               
# 2202 "ssrparser.mlg"
    mk_applyarg [] ([], []) intros 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Aentry ssrclear_ne)),
                                                                (Extend.Aentry ssrintros)),
                                                               (fun intros
                                                               clr loc -> 
                                                               
# 2200 "ssrparser.mlg"
    mk_applyarg [] ([], clr) intros 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal ":"))),
                                                                (Extend.Aentry ssragen)),
                                                                (Extend.Aentry ssragens)),
                                                                (Extend.Aentry ssrintros)),
                                                               (fun intros
                                                               dgens gen _
                                                               loc -> 
                                                               
# 2198 "ssrparser.mlg"
    mk_applyarg [] (cons_gen gen dgens) intros 
                                                               )))]);
                                     Tacentries.arg_tag = Some
                                                          (Geninterp.Val.Pair (
                                                          (Geninterp.val_tag (Genarg.topwit wit_ssrbwdview)), 
                                                          (Geninterp.Val.Pair (
                                                          (Geninterp.val_tag (Genarg.topwit wit_ssragens)), 
                                                          (Geninterp.val_tag (Genarg.topwit wit_ssrintros))))));
                                     Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                             (wit_ssrbwdview), 
                                                             (Genarg.PairArg (
                                                             (wit_ssragens), 
                                                             (wit_ssrintros)))));
                                     Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                            (wit_ssrbwdview), 
                                                            (Genarg.PairArg (
                                                            (wit_ssragens), 
                                                            (wit_ssrintros)))));
                                     Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                             (wit_ssrbwdview), 
                                                             (Genarg.PairArg (
                                                             (wit_ssragens), 
                                                             (wit_ssrintros)))));
                                     Tacentries.arg_printer = ((fun env sigma -> 
                                                              
# 2196 "ssrparser.mlg"
             pr_ssraarg 
                                                              ), (fun env sigma -> 
                                                              
# 2196 "ssrparser.mlg"
             pr_ssraarg 
                                                              ), (fun env sigma -> 
                                                              
# 2196 "ssrparser.mlg"
             pr_ssraarg 
                                                              ));
                                     }
let _ = (wit_ssrapplyarg, ssrapplyarg)

let () = Tacentries.tactic_extend __coq_plugin_name "ssrapply" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("apply", Tacentries.TyArg (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_ssrapplyarg), 
                                                         Tacentries.TyNil)), 
           (fun arg ist -> 
# 2210 "ssrparser.mlg"
                                   
     let views, (gens_clr, intros) = arg in
     inner_ssrapplytac views gens_clr ist <*> tclIPATssr intros 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("apply", Tacentries.TyNil), 
          (fun ist -> 
# 2213 "ssrparser.mlg"
                   apply_top_tac 
          )))]


# 2218 "ssrparser.mlg"
 

let mk_exactarg views dgens = mk_applyarg views dgens []



let (wit_ssrexactarg, ssrexactarg) = Tacentries.argument_extend ~name:"ssrexactarg" 
                                     {
                                     Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                              [(Extend.Rule
                                                                (Extend.Next 
                                                                 (Extend.Stop,
                                                                 (Extend.Aentry ssrclear_ne)),
                                                                (fun clr
                                                                loc -> 
                                                                
# 2230 "ssrparser.mlg"
    mk_exactarg [] ([], clr) 
                                                                )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Aentry ssrbwdview)),
                                                                (Extend.Aentry ssrclear)),
                                                               (fun clr view
                                                               loc -> 
                                                               
# 2228 "ssrparser.mlg"
    mk_exactarg view ([], clr) 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal ":"))),
                                                                (Extend.Aentry ssragen)),
                                                                (Extend.Aentry ssragens)),
                                                               (fun dgens gen
                                                               _ loc -> 
                                                               
# 2226 "ssrparser.mlg"
    mk_exactarg [] (cons_gen gen dgens) 
                                                               )))]);
                                     Tacentries.arg_tag = Some
                                                          (Geninterp.val_tag (Genarg.topwit wit_ssrapplyarg));
                                     Tacentries.arg_intern = Tacentries.ArgInternWit (wit_ssrapplyarg);
                                     Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ssrapplyarg);
                                     Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_ssrapplyarg);
                                     Tacentries.arg_printer = ((fun env sigma -> 
                                                              
# 2224 "ssrparser.mlg"
                                                              pr_ssraarg 
                                                              ), (fun env sigma -> 
                                                              
# 2224 "ssrparser.mlg"
                                                              pr_ssraarg 
                                                              ), (fun env sigma -> 
                                                              
# 2224 "ssrparser.mlg"
                                                              pr_ssraarg 
                                                              ));
                                     }
let _ = (wit_ssrexactarg, ssrexactarg)


# 2233 "ssrparser.mlg"
 

let vmexacttac pf =
  Goal.enter begin fun gl ->
  exact_no_check (EConstr.mkCast (pf, _vmcast, Tacmach.New.pf_concl gl))
  end



let () = Tacentries.tactic_extend __coq_plugin_name "ssrexact" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("exact", Tacentries.TyArg (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_ssrexactarg), 
                                                         Tacentries.TyNil)), 
           (fun arg ist -> 
# 2243 "ssrparser.mlg"
                                   
     let views, (gens_clr, _) = arg in
     V82.tactic (tclBY (V82.of_tactic (inner_ssrapplytac views gens_clr ist))) 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("exact", Tacentries.TyNil), 
          (fun ist -> 
# 2246 "ssrparser.mlg"
                  
     V82.tactic (Tacticals.tclORELSE (donetac ~-1) (tclBY (V82.of_tactic apply_top_tac))) 
          )));
         (Tacentries.TyML (Tacentries.TyIdent ("exact", Tacentries.TyIdent ("<:", 
                                                        Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_lconstr), 
                                                        Tacentries.TyNil))), 
          (fun pf ist -> 
# 2248 "ssrparser.mlg"
                                    vmexacttac pf 
          )))]


# 2255 "ssrparser.mlg"
 

let pr_ssrcongrarg _ _ _ ((n, f), dgens) =
  (if n <= 0 then mt () else str " " ++ int n) ++
  str " " ++ pr_term f ++ pr_dgens pr_gen dgens



let (wit_ssrcongrarg, ssrcongrarg) = Tacentries.argument_extend ~name:"ssrcongrarg" 
                                     {
                                     Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                              [(Extend.Rule
                                                                (Extend.Next 
                                                                 (Extend.Stop,
                                                                 (Extend.Aentry constr)),
                                                                (fun c loc ->
                                                                
# 2268 "ssrparser.mlg"
                     (0, mk_term xNoFlag c), ([[]],[]) 
                                                                )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Aentry constr)),
                                                                (Extend.Aentry ssrdgens)),
                                                               (fun dgens c
                                                               loc -> 
                                                               
# 2267 "ssrparser.mlg"
                                     (0, mk_term xNoFlag c), dgens 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Aentry natural)),
                                                                (Extend.Aentry constr)),
                                                               (fun c n
                                                               loc -> 
                                                               
# 2266 "ssrparser.mlg"
                                (n, mk_term xNoFlag c),([[]],[]) 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Aentry natural)),
                                                                (Extend.Aentry constr)),
                                                                (Extend.Aentry ssrdgens)),
                                                               (fun dgens c n
                                                               loc -> 
                                                               
# 2265 "ssrparser.mlg"
                                                (n, mk_term xNoFlag c), dgens 
                                                               )))]);
                                     Tacentries.arg_tag = Some
                                                          (Geninterp.Val.Pair (
                                                          (Geninterp.Val.Pair (
                                                          (Geninterp.val_tag (Genarg.topwit wit_int)), 
                                                          (Geninterp.val_tag (Genarg.topwit wit_ssrterm)))), 
                                                          (Geninterp.val_tag (Genarg.topwit wit_ssrdgens))));
                                     Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                             (Genarg.PairArg (
                                                             (wit_int), 
                                                             (wit_ssrterm))), 
                                                             (wit_ssrdgens)));
                                     Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                            (Genarg.PairArg (
                                                            (wit_int), 
                                                            (wit_ssrterm))), 
                                                            (wit_ssrdgens)));
                                     Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                             (Genarg.PairArg (
                                                             (wit_int), 
                                                             (wit_ssrterm))), 
                                                             (wit_ssrdgens)));
                                     Tacentries.arg_printer = ((fun env sigma -> 
                                                              
# 2264 "ssrparser.mlg"
               pr_ssrcongrarg 
                                                              ), (fun env sigma -> 
                                                              
# 2264 "ssrparser.mlg"
               pr_ssrcongrarg 
                                                              ), (fun env sigma -> 
                                                              
# 2264 "ssrparser.mlg"
               pr_ssrcongrarg 
                                                              ));
                                     }
let _ = (wit_ssrcongrarg, ssrcongrarg)

let () = Tacentries.tactic_extend __coq_plugin_name "ssrcongr" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("congr", Tacentries.TyArg (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_ssrcongrarg), 
                                                         Tacentries.TyNil)), 
           (fun arg ist -> 
# 2275 "ssrparser.mlg"
  let arg, dgens = arg in
  V82.tactic begin
    match dgens with
    | [gens], clr -> Tacticals.tclTHEN (genstac (gens,clr)) (newssrcongrtac arg ist)
    | _ -> errorstrm (str"Dependent family abstractions not allowed in congr")
  end 
           )))]


# 2289 "ssrparser.mlg"
 

let pr_rwocc = function
  | None, None -> mt ()
  | None, occ -> pr_occ occ
  | Some clr,  _ ->  pr_clear_ne clr

let pr_ssrrwocc _ _ _ = pr_rwocc



let (wit_ssrrwocc, ssrrwocc) = Tacentries.argument_extend ~name:"ssrrwocc" 
                               {
                               Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                        [(Extend.Rule
                                                          (Extend.Stop,
                                                          (fun loc -> 
# 2303 "ssrparser.mlg"
           noclr 
                                                                    )));
                                                        (Extend.Rule
                                                         (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (CLexer.terminal "{"))),
                                                          (Extend.Aentry ssrocc)),
                                                          (Extend.Atoken (CLexer.terminal "}"))),
                                                         (fun _ occ _ loc ->
                                                         
# 2302 "ssrparser.mlg"
                               mkocc occ 
                                                         )));
                                                        (Extend.Rule
                                                         (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (CLexer.terminal "{"))),
                                                          (Extend.Alist0 (Extend.Aentry ssrhyp))),
                                                          (Extend.Atoken (CLexer.terminal "}"))),
                                                         (fun _ clr _ loc ->
                                                         
# 2301 "ssrparser.mlg"
                                    mkclr clr 
                                                         )))]);
                               Tacentries.arg_tag = Some
                                                    (Geninterp.val_tag (Genarg.topwit wit_ssrdocc));
                               Tacentries.arg_intern = Tacentries.ArgInternWit (wit_ssrdocc);
                               Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ssrdocc);
                               Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_ssrdocc);
                               Tacentries.arg_printer = ((fun env sigma -> 
                                                        
# 2300 "ssrparser.mlg"
                                                       pr_ssrrwocc 
                                                        ), (fun env sigma -> 
                                                        
# 2300 "ssrparser.mlg"
                                                       pr_ssrrwocc 
                                                        ), (fun env sigma -> 
                                                        
# 2300 "ssrparser.mlg"
                                                       pr_ssrrwocc 
                                                        ));
                               }
let _ = (wit_ssrrwocc, ssrrwocc)


# 2308 "ssrparser.mlg"
 

let pr_rwkind = function
  | RWred s -> pr_simpl s
  | RWdef -> str "/"
  | RWeq -> mt ()

let wit_ssrrwkind = add_genarg "ssrrwkind" (fun env sigma -> pr_rwkind)

let pr_rule = function
  | RWred s, _ -> pr_simpl s
  | RWdef, r-> str "/" ++ pr_term r
  | RWeq, r -> pr_term r

let pr_ssrrule _ _ _ = pr_rule

let noruleterm loc = mk_term xNoFlag (mkCProp loc)



let (wit_ssrrule_ne, ssrrule_ne) = Tacentries.argument_extend ~name:"ssrrule_ne" 
                                   {
                                   Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                            [(Extend.Rule
                                                              (Extend.Next 
                                                               (Extend.Stop,
                                                               (Extend.Atoken (CLexer.terminal "YouShouldNotTypeThis"))),
                                                              (fun _ loc -> 
# 2329 "ssrparser.mlg"
                                    anomaly "Grammar placeholder match" 
                                                                    )))]);
                                   Tacentries.arg_tag = Some
                                                        (Geninterp.Val.Pair (
                                                        (Geninterp.val_tag (Genarg.topwit wit_ssrrwkind)), 
                                                        (Geninterp.val_tag (Genarg.topwit wit_ssrterm))));
                                   Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                           (wit_ssrrwkind), 
                                                           (wit_ssrterm)));
                                   Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                          (wit_ssrrwkind), 
                                                          (wit_ssrterm)));
                                   Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                           (wit_ssrrwkind), 
                                                           (wit_ssrterm)));
                                   Tacentries.arg_printer = ((fun env sigma -> 
                                                            
# 2328 "ssrparser.mlg"
                                                                       pr_ssrrule 
                                                            ), (fun env sigma -> 
                                                            
# 2328 "ssrparser.mlg"
                                                                       pr_ssrrule 
                                                            ), (fun env sigma -> 
                                                            
# 2328 "ssrparser.mlg"
                                                                       pr_ssrrule 
                                                            ));
                                   }
let _ = (wit_ssrrule_ne, ssrrule_ne)

let _ = let () =
        Pcoq.grammar_extend ssrrule_ne None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry ssrsimpl_ne)),
                 (fun s loc -> 
# 2340 "ssrparser.mlg"
                           RWred s, noruleterm (Some loc) 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry test_not_ssrslashnum)),
                             (Extend.Arules [Extend.Rules (Extend.NextNoRec 
                                                          (Extend.Stop,
                                                          (Extend.Aentry ssrsimpl_ne)),
                                                          (fun s loc -> 
                                                          
# 2338 "ssrparser.mlg"
                               RWred s, noruleterm (Some loc) 
                                                          ));
                                            Extend.Rules (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Aentry ssrterm)),
                                                         (fun t loc -> 
                                                         
# 2337 "ssrparser.mlg"
                           RWeq, t 
                                                         ));
                                            Extend.Rules (Extend.NextNoRec 
                                                         (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (Tok.PKEYWORD ("/")))),
                                                         (Extend.Aentry ssrterm)),
                                                         (fun t _ loc -> 
                                                         
# 2336 "ssrparser.mlg"
                                RWdef, t 
                                                         ))])),
                (fun x _ loc -> 
# 2339 "ssrparser.mlg"
               x 
                                ))])])
        in ()

let (wit_ssrrule, ssrrule) = Tacentries.argument_extend ~name:"ssrrule" 
                             {
                             Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                      [(Extend.Rule
                                                        (Extend.Stop,
                                                        (fun loc -> 
# 2346 "ssrparser.mlg"
             RWred Nop, noruleterm (Some loc) 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Stop,
                                                                    (Extend.Aentry ssrrule_ne)),
                                                       (fun r loc -> 
# 2345 "ssrparser.mlg"
                           r 
                                                                    )))]);
                             Tacentries.arg_tag = Some
                                                  (Geninterp.val_tag (Genarg.topwit wit_ssrrule_ne));
                             Tacentries.arg_intern = Tacentries.ArgInternWit (wit_ssrrule_ne);
                             Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ssrrule_ne);
                             Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_ssrrule_ne);
                             Tacentries.arg_printer = ((fun env sigma -> 
                                                      
# 2344 "ssrparser.mlg"
                                                         pr_ssrrule 
                                                      ), (fun env sigma -> 
                                                      
# 2344 "ssrparser.mlg"
                                                         pr_ssrrule 
                                                      ), (fun env sigma -> 
                                                      
# 2344 "ssrparser.mlg"
                                                         pr_ssrrule 
                                                      ));
                             }
let _ = (wit_ssrrule, ssrrule)


# 2351 "ssrparser.mlg"
 

let pr_option f = function None -> mt() | Some x -> f x
let pr_pattern_squarep= pr_option (fun r -> str "[" ++ pr_rpattern r ++ str "]")
let pr_ssrpattern_squarep _ _ _ = pr_pattern_squarep
let pr_rwarg ((d, m), ((docc, rx), r)) =
  pr_rwdir d ++ pr_mult m ++ pr_rwocc docc ++ pr_pattern_squarep rx ++ pr_rule r

let pr_ssrrwarg _ _ _ = pr_rwarg



let (wit_ssrpattern_squarep, ssrpattern_squarep) = Tacentries.argument_extend ~name:"ssrpattern_squarep" 
                                                   {
                                                   Tacentries.arg_parsing = 
                                                   Vernacextend.Arg_rules (
                                                   [(Extend.Rule
                                                     (Extend.Stop,
                                                     (fun loc -> 
# 2366 "ssrparser.mlg"
             None 
                                                                 )));
                                                   (Extend.Rule
                                                    (Extend.Next (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Stop,
                                                                 (Extend.Atoken (CLexer.terminal "["))),
                                                                 (Extend.Aentry rpattern)),
                                                                 (Extend.Atoken (CLexer.terminal "]"))),
                                                    (fun _ rdx _ loc -> 
# 2365 "ssrparser.mlg"
                                   Some rdx 
                                                                    )))]);
                                                   Tacentries.arg_tag = 
                                                   Some
                                                   (Geninterp.Val.Opt 
                                                   (Geninterp.val_tag (Genarg.topwit wit_rpattern)));
                                                   Tacentries.arg_intern = 
                                                   Tacentries.ArgInternWit (Genarg.OptArg 
                                                   (wit_rpattern));
                                                   Tacentries.arg_subst = 
                                                   Tacentries.ArgSubstWit (Genarg.OptArg 
                                                   (wit_rpattern));
                                                   Tacentries.arg_interp = 
                                                   Tacentries.ArgInterpWit (Genarg.OptArg 
                                                   (wit_rpattern));
                                                   Tacentries.arg_printer = 
                                                   ((fun env sigma -> 
                                                   
# 2364 "ssrparser.mlg"
                                      pr_ssrpattern_squarep 
                                                   ), (fun env sigma -> 
                                                   
# 2364 "ssrparser.mlg"
                                      pr_ssrpattern_squarep 
                                                   ), (fun env sigma -> 
                                                   
# 2364 "ssrparser.mlg"
                                      pr_ssrpattern_squarep 
                                                   ));
                                                   }
let _ = (wit_ssrpattern_squarep, ssrpattern_squarep)

let (wit_ssrpattern_ne_squarep, ssrpattern_ne_squarep) = Tacentries.argument_extend ~name:"ssrpattern_ne_squarep" 
                                                         {
                                                         Tacentries.arg_parsing = 
                                                         Vernacextend.Arg_rules (
                                                         [(Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (CLexer.terminal "["))),
                                                            (Extend.Aentry rpattern)),
                                                            (Extend.Atoken (CLexer.terminal "]"))),
                                                           (fun _ rdx _
                                                           loc -> 
# 2371 "ssrparser.mlg"
                                   Some rdx 
                                                                  )))]);
                                                         Tacentries.arg_tag = 
                                                         Some
                                                         (Geninterp.Val.Opt 
                                                         (Geninterp.val_tag (Genarg.topwit wit_rpattern)));
                                                         Tacentries.arg_intern = 
                                                         Tacentries.ArgInternWit (Genarg.OptArg 
                                                         (wit_rpattern));
                                                         Tacentries.arg_subst = 
                                                         Tacentries.ArgSubstWit (Genarg.OptArg 
                                                         (wit_rpattern));
                                                         Tacentries.arg_interp = 
                                                         Tacentries.ArgInterpWit (Genarg.OptArg 
                                                         (wit_rpattern));
                                                         Tacentries.arg_printer = 
                                                         ((fun env sigma -> 
                                                         
# 2370 "ssrparser.mlg"
                                      pr_ssrpattern_squarep 
                                                         ), (fun env sigma -> 
                                                         
# 2370 "ssrparser.mlg"
                                      pr_ssrpattern_squarep 
                                                         ), (fun env sigma -> 
                                                         
# 2370 "ssrparser.mlg"
                                      pr_ssrpattern_squarep 
                                                         ));
                                                         }
let _ = (wit_ssrpattern_ne_squarep, ssrpattern_ne_squarep)

let (wit_ssrrwarg, ssrrwarg) = Tacentries.argument_extend ~name:"ssrrwarg" 
                               {
                               Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                        [(Extend.Rule
                                                          (Extend.Next 
                                                           (Extend.Stop,
                                                           (Extend.Aentry ssrrule_ne)),
                                                          (fun r loc -> 
# 2395 "ssrparser.mlg"
      mk_rwarg norwmult norwocc r 
                                                                    )));
                                                        (Extend.Rule
                                                         (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Stop,
                                                          (Extend.Aentry ssrpattern_ne_squarep)),
                                                          (Extend.Aentry ssrrule_ne)),
                                                         (fun r rx loc -> 
# 2393 "ssrparser.mlg"
      mk_rwarg norwmult (noclr, rx) r 
                                                                    )));
                                                        (Extend.Rule
                                                         (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (CLexer.terminal "{"))),
                                                          (Extend.Atoken (CLexer.terminal "}"))),
                                                          (Extend.Aentry ssrpattern_squarep)),
                                                          (Extend.Aentry ssrrule_ne)),
                                                         (fun r rx _ _ loc ->
                                                         
# 2391 "ssrparser.mlg"
      mk_rwarg norwmult (nodocc, rx) r 
                                                         )));
                                                        (Extend.Rule
                                                         (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (CLexer.terminal "{"))),
                                                          (Extend.Aentry ssrocc)),
                                                          (Extend.Atoken (CLexer.terminal "}"))),
                                                          (Extend.Aentry ssrpattern_squarep)),
                                                          (Extend.Aentry ssrrule_ne)),
                                                         (fun r rx _ occ _
                                                         loc -> 
# 2389 "ssrparser.mlg"
      mk_rwarg norwmult (mkocc occ, rx) r 
                                                                )));
                                                        (Extend.Rule
                                                         (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (CLexer.terminal "{"))),
                                                          (Extend.Alist1 (Extend.Aentry ssrhyp))),
                                                          (Extend.Atoken (CLexer.terminal "}"))),
                                                          (Extend.Aentry ssrrule)),
                                                         (fun r _ clr _
                                                         loc -> 
# 2387 "ssrparser.mlg"
      mk_rwarg norwmult (mkclr clr, None) r 
                                                                )));
                                                        (Extend.Rule
                                                         (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (CLexer.terminal "{"))),
                                                          (Extend.Alist1 (Extend.Aentry ssrhyp))),
                                                          (Extend.Atoken (CLexer.terminal "}"))),
                                                          (Extend.Aentry ssrpattern_ne_squarep)),
                                                          (Extend.Aentry ssrrule_ne)),
                                                         (fun r rx _ clr _
                                                         loc -> 
# 2385 "ssrparser.mlg"
      mk_rwarg norwmult (mkclr clr, rx) r 
                                                                )));
                                                        (Extend.Rule
                                                         (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Stop,
                                                          (Extend.Aentry ssrmult_ne)),
                                                          (Extend.Aentry ssrrwocc)),
                                                          (Extend.Aentry ssrpattern_squarep)),
                                                          (Extend.Aentry ssrrule_ne)),
                                                         (fun r rx docc m
                                                         loc -> 
# 2383 "ssrparser.mlg"
      mk_rwarg (L2R, m) (docc, rx) r 
                                                                )));
                                                        (Extend.Rule
                                                         (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (CLexer.terminal "-/"))),
                                                          (Extend.Aentry ssrterm)),
                                                         (fun t _ loc -> 
# 2381 "ssrparser.mlg"
      mk_rwarg (R2L, nomult) norwocc (RWdef, t) 
                                                                    )));
                                                        (Extend.Rule
                                                         (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (CLexer.terminal "-"))),
                                                          (Extend.Aentry ssrmult)),
                                                          (Extend.Aentry ssrrwocc)),
                                                          (Extend.Aentry ssrpattern_squarep)),
                                                          (Extend.Aentry ssrrule_ne)),
                                                         (fun r rx docc m _
                                                         loc -> 
# 2379 "ssrparser.mlg"
      mk_rwarg (R2L, m) (docc, rx) r 
                                                                )))]);
                               Tacentries.arg_tag = Some
                                                    (Geninterp.Val.Pair (
                                                    (Geninterp.Val.Pair (
                                                    (Geninterp.val_tag (Genarg.topwit wit_ssrdir)), 
                                                    (Geninterp.val_tag (Genarg.topwit wit_ssrmult)))), 
                                                    (Geninterp.Val.Pair (
                                                    (Geninterp.Val.Pair (
                                                    (Geninterp.val_tag (Genarg.topwit wit_ssrdocc)), 
                                                    (Geninterp.Val.Opt 
                                                    (Geninterp.val_tag (Genarg.topwit wit_rpattern))))), 
                                                    (Geninterp.val_tag (Genarg.topwit wit_ssrrule))))));
                               Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                       (Genarg.PairArg (
                                                       (wit_ssrdir), 
                                                       (wit_ssrmult))), 
                                                       (Genarg.PairArg (
                                                       (Genarg.PairArg (
                                                       (wit_ssrdocc), 
                                                       (Genarg.OptArg 
                                                       (wit_rpattern)))), 
                                                       (wit_ssrrule)))));
                               Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                      (Genarg.PairArg (
                                                      (wit_ssrdir), (wit_ssrmult))), 
                                                      (Genarg.PairArg (
                                                      (Genarg.PairArg (
                                                      (wit_ssrdocc), 
                                                      (Genarg.OptArg 
                                                      (wit_rpattern)))), 
                                                      (wit_ssrrule)))));
                               Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                       (Genarg.PairArg (
                                                       (wit_ssrdir), 
                                                       (wit_ssrmult))), 
                                                       (Genarg.PairArg (
                                                       (Genarg.PairArg (
                                                       (wit_ssrdocc), 
                                                       (Genarg.OptArg 
                                                       (wit_rpattern)))), 
                                                       (wit_ssrrule)))));
                               Tacentries.arg_printer = ((fun env sigma -> 
                                                        
# 2377 "ssrparser.mlg"
               pr_ssrrwarg 
                                                        ), (fun env sigma -> 
                                                        
# 2377 "ssrparser.mlg"
               pr_ssrrwarg 
                                                        ), (fun env sigma -> 
                                                        
# 2377 "ssrparser.mlg"
               pr_ssrrwarg 
                                                        ));
                               }
let _ = (wit_ssrrwarg, ssrrwarg)

let () = Tacentries.tactic_extend __coq_plugin_name "ssrinstofruleL2R" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("ssrinstancesofruleL2R", 
                            Tacentries.TyArg (Extend.TUentry (Genarg.get_arg_tag wit_ssrterm), 
                            Tacentries.TyNil)), (fun arg ist -> 
# 2399 "ssrparser.mlg"
                                                V82.tactic (ssrinstancesofrule ist L2R arg) 
                                                )))]

let () = Tacentries.tactic_extend __coq_plugin_name "ssrinstofruleR2L" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("ssrinstancesofruleR2L", 
                            Tacentries.TyArg (Extend.TUentry (Genarg.get_arg_tag wit_ssrterm), 
                            Tacentries.TyNil)), (fun arg ist -> 
# 2402 "ssrparser.mlg"
                                                V82.tactic (ssrinstancesofrule ist R2L arg) 
                                                )))]


# 2409 "ssrparser.mlg"
 

let pr_ssrrwargs _ _ _ rwargs = pr_list spc pr_rwarg rwargs



let (wit_ssrrwargs, ssrrwargs) = Tacentries.argument_extend ~name:"ssrrwargs" 
                                 {
                                 Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                          [(Extend.Rule
                                                            (Extend.Next 
                                                             (Extend.Stop,
                                                             (Extend.Atoken (CLexer.terminal "YouShouldNotTypeThis"))),
                                                            (fun _ loc -> 
# 2416 "ssrparser.mlg"
                                    anomaly "Grammar placeholder match" 
                                                                    )))]);
                                 Tacentries.arg_tag = Some
                                                      (Geninterp.Val.List 
                                                      (Geninterp.val_tag (Genarg.topwit wit_ssrrwarg)));
                                 Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.ListArg 
                                                         (wit_ssrrwarg));
                                 Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.ListArg 
                                                        (wit_ssrrwarg));
                                 Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.ListArg 
                                                         (wit_ssrrwarg));
                                 Tacentries.arg_printer = ((fun env sigma -> 
                                                          
# 2415 "ssrparser.mlg"
                                                              pr_ssrrwargs 
                                                          ), (fun env sigma -> 
                                                          
# 2415 "ssrparser.mlg"
                                                              pr_ssrrwargs 
                                                          ), (fun env sigma -> 
                                                          
# 2415 "ssrparser.mlg"
                                                              pr_ssrrwargs 
                                                          ));
                                 }
let _ = (wit_ssrrwargs, ssrrwargs)


# 2419 "ssrparser.mlg"
 

let ssr_rw_syntax = Summary.ref ~name:"SSR:rewrite" true

let () =
  Goptions.(declare_bool_option
    { optname  = "ssreflect rewrite";
      optkey   = ["SsrRewrite"];
      optread  = (fun _ -> !ssr_rw_syntax);
      optdepr  = false;
      optwrite = (fun b -> ssr_rw_syntax := b) })

let lbrace = Char.chr 123
(** Workaround to a limitation of coqpp *)

let test_ssr_rw_syntax =
  let test strm  =
    if not !ssr_rw_syntax then raise Stream.Failure else
    if is_ssr_loaded () then () else
    match Util.stream_nth 0 strm with
    | Tok.KEYWORD key when List.mem key.[0] [lbrace; '['; '/'] -> ()
    | _ -> raise Stream.Failure in
  Pcoq.Entry.of_parser "test_ssr_rw_syntax" test



let _ = let () =
        Pcoq.grammar_extend ssrrwargs None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry test_ssr_rw_syntax)),
                              (Extend.Alist1 (Extend.Aentry ssrrwarg))),
                 (fun a _ loc -> 
# 2447 "ssrparser.mlg"
                                                            a 
                                 ))])])
        in ()

let () = Tacentries.tactic_extend __coq_plugin_name "ssrrewrite" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("rewrite", Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_ssrrwargs), 
                                                           Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_ssrclauses), 
                                                           Tacentries.TyNil))), 
           (fun args clauses ist -> 
# 2454 "ssrparser.mlg"
      tclCLAUSES (old_tac (ssrrewritetac ist args)) clauses 
           )))]


# 2459 "ssrparser.mlg"
 

let pr_unlockarg (occ, t) = pr_occ occ ++ pr_term t
let pr_ssrunlockarg _ _ _ = pr_unlockarg



let (wit_ssrunlockarg, ssrunlockarg) = Tacentries.argument_extend ~name:"ssrunlockarg" 
                                       {
                                       Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                                [(Extend.Rule
                                                                  (Extend.Next 
                                                                   (Extend.Stop,
                                                                   (Extend.Aentry ssrterm)),
                                                                  (fun t
                                                                  loc -> 
                                                                  
# 2469 "ssrparser.mlg"
                         None, t 
                                                                  )));
                                                                (Extend.Rule
                                                                 (Extend.Next 
                                                                  (Extend.Next 
                                                                  (Extend.Next 
                                                                  (Extend.Next 
                                                                  (Extend.Stop,
                                                                  (Extend.Atoken (CLexer.terminal "{"))),
                                                                  (Extend.Aentry ssrocc)),
                                                                  (Extend.Atoken (CLexer.terminal "}"))),
                                                                  (Extend.Aentry ssrterm)),
                                                                 (fun t _ occ
                                                                 _ loc -> 
                                                                 
# 2468 "ssrparser.mlg"
                                             occ, t 
                                                                 )))]);
                                       Tacentries.arg_tag = Some
                                                            (Geninterp.Val.Pair (
                                                            (Geninterp.val_tag (Genarg.topwit wit_ssrocc)), 
                                                            (Geninterp.val_tag (Genarg.topwit wit_ssrterm))));
                                       Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                               (wit_ssrocc), 
                                                               (wit_ssrterm)));
                                       Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                              (wit_ssrocc), 
                                                              (wit_ssrterm)));
                                       Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                               (wit_ssrocc), 
                                                               (wit_ssrterm)));
                                       Tacentries.arg_printer = ((fun env sigma -> 
                                                                
# 2467 "ssrparser.mlg"
               pr_ssrunlockarg 
                                                                ), (fun env sigma -> 
                                                                
# 2467 "ssrparser.mlg"
               pr_ssrunlockarg 
                                                                ), (fun env sigma -> 
                                                                
# 2467 "ssrparser.mlg"
               pr_ssrunlockarg 
                                                                ));
                                       }
let _ = (wit_ssrunlockarg, ssrunlockarg)


# 2472 "ssrparser.mlg"
 

let pr_ssrunlockargs _ _ _ args = pr_list spc pr_unlockarg args



let (wit_ssrunlockargs, ssrunlockargs) = Tacentries.argument_extend ~name:"ssrunlockargs" 
                                         {
                                         Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                                  [(Extend.Rule
                                                                    (
                                                                    Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Alist0 (Extend.Aentry ssrunlockarg))),
                                                                    (fun args
                                                                    loc -> 
                                                                    
# 2480 "ssrparser.mlg"
                                      args 
                                                                    )))]);
                                         Tacentries.arg_tag = Some
                                                              (Geninterp.Val.List 
                                                              (Geninterp.val_tag (Genarg.topwit wit_ssrunlockarg)));
                                         Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.ListArg 
                                                                 (wit_ssrunlockarg));
                                         Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.ListArg 
                                                                (wit_ssrunlockarg));
                                         Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.ListArg 
                                                                 (wit_ssrunlockarg));
                                         Tacentries.arg_printer = ((fun env sigma -> 
                                                                  
# 2479 "ssrparser.mlg"
               pr_ssrunlockargs 
                                                                  ), (fun env sigma -> 
                                                                  
# 2479 "ssrparser.mlg"
               pr_ssrunlockargs 
                                                                  ), (fun env sigma -> 
                                                                  
# 2479 "ssrparser.mlg"
               pr_ssrunlockargs 
                                                                  ));
                                         }
let _ = (wit_ssrunlockargs, ssrunlockargs)

let () = Tacentries.tactic_extend __coq_plugin_name "ssrunlock" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("unlock", Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_ssrunlockargs), 
                                                          Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_ssrclauses), 
                                                          Tacentries.TyNil))), 
           (fun args clauses ist -> 
# 2485 "ssrparser.mlg"
      tclCLAUSES (old_tac (unlocktac ist args)) clauses 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "ssrpose" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("pose", Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrfixfwd), 
                                                        Tacentries.TyNil)), 
           (fun ffwd ist -> 
# 2492 "ssrparser.mlg"
                                  V82.tactic (ssrposetac ffwd) 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("pose", Tacentries.TyArg (
                                                       Extend.TUentry (Genarg.get_arg_tag wit_ssrcofixfwd), 
                                                       Tacentries.TyNil)), 
          (fun ffwd ist -> 
# 2493 "ssrparser.mlg"
                                    V82.tactic (ssrposetac ffwd) 
          )));
         (Tacentries.TyML (Tacentries.TyIdent ("pose", Tacentries.TyArg (
                                                       Extend.TUentry (Genarg.get_arg_tag wit_ssrfwdid), 
                                                       Tacentries.TyArg (
                                                       Extend.TUentry (Genarg.get_arg_tag wit_ssrposefwd), 
                                                       Tacentries.TyNil))), 
          (fun id fwd ist -> 
# 2494 "ssrparser.mlg"
                                               V82.tactic (ssrposetac (id, fwd)) 
          )))]

let () = Tacentries.tactic_extend __coq_plugin_name "ssrset" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("set", Tacentries.TyArg (
                                                       Extend.TUentry (Genarg.get_arg_tag wit_ssrfwdid), 
                                                       Tacentries.TyArg (
                                                       Extend.TUentry (Genarg.get_arg_tag wit_ssrsetfwd), 
                                                       Tacentries.TyArg (
                                                       Extend.TUentry (Genarg.get_arg_tag wit_ssrclauses), 
                                                       Tacentries.TyNil)))), 
           (fun id fwd clauses ist -> 
# 2503 "ssrparser.mlg"
    tclCLAUSES (old_tac (ssrsettac id fwd)) clauses 
           )))]

let _ = let () =
        Pcoq.grammar_extend tactic_expr None
        (Some
        (Gramlib.Gramext.Level "3"), [(None, Some (Gramlib.Gramext.RightA),
                                      [Extend.Rule
                                       (Extend.Next (Extend.Next (Extend.Stop,
                                                                 (Extend.Atoken (Tok.PIDENT (Some
                                                                 ("abstract"))))),
                                                    (Extend.Aentry ssrdgens)),
                                       (fun gens _ loc -> 
# 2518 "ssrparser.mlg"
                 ssrtac_expr ~loc "abstract"
                [Tacexpr.TacGeneric (Genarg.in_gen (Genarg.rawwit wit_ssrdgens) gens)] 
                                                          ))])])
        in ()

let () = Tacentries.tactic_extend __coq_plugin_name "ssrabstract" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("abstract", Tacentries.TyArg (
                                                            Extend.TUentry (Genarg.get_arg_tag wit_ssrdgens), 
                                                            Tacentries.TyNil)), 
           (fun gens ist -> 
# 2522 "ssrparser.mlg"
                                    
    if List.length (fst gens) <> 1 then
      errorstrm (str"dependents switches '/' not allowed here");
    Ssripats.ssrabstract (ssrdgens_of_parsed_dgens gens) 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "ssrhave" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("have", Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrhavefwdwbinders), 
                                                        Tacentries.TyNil)), 
           (fun fwd ist -> 
# 2530 "ssrparser.mlg"
    V82.tactic (havetac ist fwd false false) 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "ssrhavesuff" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("have", Tacentries.TyIdent ("suff", 
                                                        Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrhpats_nobs), 
                                                        Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrhavefwd), 
                                                        Tacentries.TyNil)))), 
           (fun pats fwd ist -> 
# 2535 "ssrparser.mlg"
    V82.tactic (havetac ist (false,(pats,fwd)) true false) 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "ssrhavesuffices" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("have", Tacentries.TyIdent ("suffices", 
                                                        Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrhpats_nobs), 
                                                        Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrhavefwd), 
                                                        Tacentries.TyNil)))), 
           (fun pats fwd ist -> 
# 2540 "ssrparser.mlg"
    V82.tactic (havetac ist (false,(pats,fwd)) true false) 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "ssrsuffhave" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("suff", Tacentries.TyIdent ("have", 
                                                        Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrhpats_nobs), 
                                                        Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrhavefwd), 
                                                        Tacentries.TyNil)))), 
           (fun pats fwd ist -> 
# 2545 "ssrparser.mlg"
    V82.tactic (havetac ist (false,(pats,fwd)) true true) 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "ssrsufficeshave" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("suffices", Tacentries.TyIdent ("have", 
                                                            Tacentries.TyArg (
                                                            Extend.TUentry (Genarg.get_arg_tag wit_ssrhpats_nobs), 
                                                            Tacentries.TyArg (
                                                            Extend.TUentry (Genarg.get_arg_tag wit_ssrhavefwd), 
                                                            Tacentries.TyNil)))), 
           (fun pats fwd ist -> 
# 2550 "ssrparser.mlg"
    V82.tactic (havetac ist (false,(pats,fwd)) true true) 
           )))]


# 2555 "ssrparser.mlg"
 

let pr_ssrsufffwdwbinders env sigma _ _ prt (hpats, (fwd, hint)) =
  pr_hpats hpats ++ pr_fwd fwd ++ pr_hint env sigma prt hint



let (wit_ssrsufffwd, ssrsufffwd) = Tacentries.argument_extend ~name:"ssrsufffwd" 
                                   {
                                   Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                            [(Extend.Rule
                                                              (Extend.Next 
                                                               (Extend.Next 
                                                               (Extend.Next 
                                                               (Extend.Next 
                                                               (Extend.Next 
                                                               (Extend.Stop,
                                                               (Extend.Aentry ssrhpats)),
                                                               (Extend.Alist0 (Extend.Aentry ssrbinder))),
                                                               (Extend.Atoken (CLexer.terminal ":"))),
                                                               (Extend.Aentry ast_closure_lterm)),
                                                               (Extend.Aentry ssrhint)),
                                                              (fun hint t _
                                                              bs pats loc ->
                                                              
# 2565 "ssrparser.mlg"
    let ((clr, pats), binders), simpl = pats in
    let allbs = intro_id_to_binder binders @ bs in
    let allbinders = binders @ List.flatten (binder_to_intro_id bs) in
    let fwd = mkFwdHint ":" t in
    (((clr, pats), allbinders), simpl), (bind_fwd allbs fwd, hint) 
                                                              )))]);
                                   Tacentries.arg_tag = Some
                                                        (Geninterp.Val.Pair (
                                                        (Geninterp.val_tag (Genarg.topwit wit_ssrhpats)), 
                                                        (Geninterp.Val.Pair (
                                                        (Geninterp.val_tag (Genarg.topwit wit_ssrfwd)), 
                                                        (Geninterp.val_tag (Genarg.topwit wit_ssrhint))))));
                                   Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                           (wit_ssrhpats), 
                                                           (Genarg.PairArg (
                                                           (wit_ssrfwd), 
                                                           (wit_ssrhint)))));
                                   Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                          (wit_ssrhpats), 
                                                          (Genarg.PairArg (
                                                          (wit_ssrfwd), 
                                                          (wit_ssrhint)))));
                                   Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                           (wit_ssrhpats), 
                                                           (Genarg.PairArg (
                                                           (wit_ssrfwd), 
                                                           (wit_ssrhint)))));
                                   Tacentries.arg_printer = ((fun env sigma -> 
                                                            
# 2563 "ssrparser.mlg"
                                                        pr_ssrsufffwdwbinders env sigma 
                                                            ), (fun env sigma -> 
                                                            
# 2563 "ssrparser.mlg"
                                                        pr_ssrsufffwdwbinders env sigma 
                                                            ), (fun env sigma -> 
                                                            
# 2563 "ssrparser.mlg"
                                                        pr_ssrsufffwdwbinders env sigma 
                                                            ));
                                   }
let _ = (wit_ssrsufffwd, ssrsufffwd)

let () = Tacentries.tactic_extend __coq_plugin_name "ssrsuff" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("suff", Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrsufffwd), 
                                                        Tacentries.TyNil)), 
           (fun fwd ist -> 
# 2574 "ssrparser.mlg"
                                  V82.tactic (sufftac ist fwd) 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "ssrsuffices" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("suffices", Tacentries.TyArg (
                                                            Extend.TUentry (Genarg.get_arg_tag wit_ssrsufffwd), 
                                                            Tacentries.TyNil)), 
           (fun fwd ist -> 
# 2578 "ssrparser.mlg"
                                      V82.tactic (sufftac ist fwd) 
           )))]


# 2585 "ssrparser.mlg"
 

let pr_ssrwlogfwd _ _ _ (gens, t) =
  str ":" ++ pr_list mt pr_wgen gens ++ spc() ++ pr_fwd t



let (wit_ssrwlogfwd, ssrwlogfwd) = Tacentries.argument_extend ~name:"ssrwlogfwd" 
                                   {
                                   Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                            [(Extend.Rule
                                                              (Extend.Next 
                                                               (Extend.Next 
                                                               (Extend.Next 
                                                               (Extend.Next 
                                                               (Extend.Stop,
                                                               (Extend.Atoken (CLexer.terminal ":"))),
                                                               (Extend.Alist0 (Extend.Aentry ssrwgen))),
                                                               (Extend.Atoken (CLexer.terminal "/"))),
                                                               (Extend.Aentry ast_closure_lterm)),
                                                              (fun t _ gens _
                                                              loc -> 
                                                              
# 2594 "ssrparser.mlg"
                                                           gens, mkFwdHint "/" t
                                                              )))]);
                                   Tacentries.arg_tag = Some
                                                        (Geninterp.Val.Pair (
                                                        (Geninterp.Val.List 
                                                        (Geninterp.val_tag (Genarg.topwit wit_ssrwgen))), 
                                                        (Geninterp.val_tag (Genarg.topwit wit_ssrfwd))));
                                   Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                           (Genarg.ListArg 
                                                           (wit_ssrwgen)), 
                                                           (wit_ssrfwd)));
                                   Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                          (Genarg.ListArg 
                                                          (wit_ssrwgen)), 
                                                          (wit_ssrfwd)));
                                   Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                           (Genarg.ListArg 
                                                           (wit_ssrwgen)), 
                                                           (wit_ssrfwd)));
                                   Tacentries.arg_printer = ((fun env sigma -> 
                                                            
# 2593 "ssrparser.mlg"
                                      pr_ssrwlogfwd 
                                                            ), (fun env sigma -> 
                                                            
# 2593 "ssrparser.mlg"
                                      pr_ssrwlogfwd 
                                                            ), (fun env sigma -> 
                                                            
# 2593 "ssrparser.mlg"
                                      pr_ssrwlogfwd 
                                                            ));
                                   }
let _ = (wit_ssrwlogfwd, ssrwlogfwd)

let () = Tacentries.tactic_extend __coq_plugin_name "ssrwlog" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("wlog", Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrhpats_nobs), 
                                                        Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrwlogfwd), 
                                                        Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrhint), 
                                                        Tacentries.TyNil)))), 
           (fun pats fwd hint ist -> 
# 2600 "ssrparser.mlg"
    V82.tactic (wlogtac ist pats fwd hint false `NoGen) 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "ssrwlogs" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("wlog", Tacentries.TyIdent ("suff", 
                                                        Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrhpats_nobs), 
                                                        Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrwlogfwd), 
                                                        Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrhint), 
                                                        Tacentries.TyNil))))), 
           (fun pats fwd hint ist -> 
# 2605 "ssrparser.mlg"
    V82.tactic (wlogtac ist pats fwd hint true `NoGen) 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "ssrwlogss" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("wlog", Tacentries.TyIdent ("suffices", 
                                                        Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrhpats_nobs), 
                                                        Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrwlogfwd), 
                                                        Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrhint), 
                                                        Tacentries.TyNil))))), 
           (fun pats fwd hint ist -> 
# 2610 "ssrparser.mlg"
    V82.tactic (wlogtac ist pats fwd hint true `NoGen) 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "ssrwithoutloss" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("without", Tacentries.TyIdent ("loss", 
                                                           Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_ssrhpats_nobs), 
                                                           Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_ssrwlogfwd), 
                                                           Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_ssrhint), 
                                                           Tacentries.TyNil))))), 
           (fun pats fwd hint ist -> 
# 2615 "ssrparser.mlg"
    V82.tactic (wlogtac ist pats fwd hint false `NoGen) 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "ssrwithoutlosss" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("without", Tacentries.TyIdent ("loss", 
                                                           Tacentries.TyIdent ("suff", 
                                                           Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_ssrhpats_nobs), 
                                                           Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_ssrwlogfwd), 
                                                           Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_ssrhint), 
                                                           Tacentries.TyNil)))))), 
           (fun pats fwd hint ist -> 
# 2621 "ssrparser.mlg"
    V82.tactic (wlogtac ist pats fwd hint true `NoGen) 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "ssrwithoutlossss" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("without", Tacentries.TyIdent ("loss", 
                                                           Tacentries.TyIdent ("suffices", 
                                                           Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_ssrhpats_nobs), 
                                                           Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_ssrwlogfwd), 
                                                           Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_ssrhint), 
                                                           Tacentries.TyNil)))))), 
           (fun pats fwd hint ist -> 
# 2627 "ssrparser.mlg"
    V82.tactic (wlogtac ist pats fwd hint true `NoGen) 
           )))]


# 2630 "ssrparser.mlg"
 

(* Generally have *)
let pr_idcomma _ _ _ = function
  | None -> mt()
  | Some None -> str"_, "
  | Some (Some id) -> pr_id id ++ str", "



let (wit_ssr_idcomma, ssr_idcomma) = Tacentries.argument_extend ~name:"ssr_idcomma" 
                                     {
                                     Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                              [(Extend.Rule
                                                                (Extend.Stop,
                                                                (fun loc -> 
# 2641 "ssrparser.mlg"
             None 
                                                                    )))]);
                                     Tacentries.arg_tag = Some
                                                          (Geninterp.Val.Opt 
                                                          (Geninterp.Val.Opt 
                                                          (Geninterp.val_tag (Genarg.topwit wit_ident))));
                                     Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.OptArg 
                                                             (Genarg.OptArg 
                                                             (wit_ident)));
                                     Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.OptArg 
                                                            (Genarg.OptArg 
                                                            (wit_ident)));
                                     Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.OptArg 
                                                             (Genarg.OptArg 
                                                             (wit_ident)));
                                     Tacentries.arg_printer = ((fun env sigma -> 
                                                              
# 2640 "ssrparser.mlg"
                                                                      pr_idcomma 
                                                              ), (fun env sigma -> 
                                                              
# 2640 "ssrparser.mlg"
                                                                      pr_idcomma 
                                                              ), (fun env sigma -> 
                                                              
# 2640 "ssrparser.mlg"
                                                                      pr_idcomma 
                                                              ));
                                     }
let _ = (wit_ssr_idcomma, ssr_idcomma)


# 2644 "ssrparser.mlg"
 

let accept_idcomma strm =
  match stream_nth 0 strm with
  | Tok.IDENT _ | Tok.KEYWORD "_" -> accept_before_syms [","] strm
  | _ -> raise Stream.Failure

let test_idcomma = Pcoq.Entry.of_parser "test_idcomma" accept_idcomma



let _ = let () =
        Pcoq.grammar_extend ssr_idcomma None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Aentry test_idcomma)),
                                           (Extend.Arules [Extend.Rules 
                                                          (Extend.NextNoRec 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (Tok.PKEYWORD ("_")))),
                                                          (fun _ loc -> 
                                                          
# 2658 "ssrparser.mlg"
                                                               None 
                                                          ));
                                                          Extend.Rules 
                                                          (Extend.NextNoRec 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (Tok.PIDENT (None)))),
                                                          (fun id loc -> 
                                                          
# 2658 "ssrparser.mlg"
                           Some (Id.of_string id) 
                                                          ))])),
                              (Extend.Atoken (Tok.PKEYWORD (",")))),
                 (fun _ ip _ loc -> 
# 2659 "ssrparser.mlg"
      Some ip 
                                    ))])])
        in ()


# 2663 "ssrparser.mlg"
 

let augment_preclr clr1 (((clr0, x),y),z) =
  let cl = match clr0 with
    | None -> if clr1 = [] then None else Some clr1
    | Some clr0 -> Some (clr1 @ clr0) in
  (((cl, x),y),z)



let () = Tacentries.tactic_extend __coq_plugin_name "ssrgenhave" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("gen", Tacentries.TyIdent ("have", 
                                                       Tacentries.TyArg (
                                                       Extend.TUentry (Genarg.get_arg_tag wit_ssrclear), 
                                                       Tacentries.TyArg (
                                                       Extend.TUentry (Genarg.get_arg_tag wit_ssr_idcomma), 
                                                       Tacentries.TyArg (
                                                       Extend.TUentry (Genarg.get_arg_tag wit_ssrhpats_nobs), 
                                                       Tacentries.TyArg (
                                                       Extend.TUentry (Genarg.get_arg_tag wit_ssrwlogfwd), 
                                                       Tacentries.TyArg (
                                                       Extend.TUentry (Genarg.get_arg_tag wit_ssrhint), 
                                                       Tacentries.TyNil))))))), 
           (fun clr id pats fwd hint ist -> 
# 2676 "ssrparser.mlg"
    let pats = augment_preclr clr pats in
    V82.tactic (wlogtac ist pats fwd hint false (`Gen id)) 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "ssrgenhave2" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("generally", Tacentries.TyIdent ("have", 
                                                             Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_ssrclear), 
                                                             Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_ssr_idcomma), 
                                                             Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_ssrhpats_nobs), 
                                                             Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_ssrwlogfwd), 
                                                             Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_ssrhint), 
                                                             Tacentries.TyNil))))))), 
           (fun clr id pats fwd hint ist -> 
# 2683 "ssrparser.mlg"
    let pats = augment_preclr clr pats in
    V82.tactic (wlogtac ist pats fwd hint false (`Gen id)) 
           )))]


# 2687 "ssrparser.mlg"
 

let check_under_arg ((_dir,mult),((_occ,_rpattern),_rule)) =
  if mult <> nomult then
    CErrors.user_err Pp.(str"under does not support multipliers")



let () = Tacentries.tactic_extend __coq_plugin_name "under" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("under", Tacentries.TyArg (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_ssrrwarg), 
                                                         Tacentries.TyNil)), 
           (fun arg ist -> 
# 2697 "ssrparser.mlg"
                                  
    check_under_arg arg;
    Ssrfwd.undertac ist None arg nohint
    
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("under", Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrrwarg), 
                                                        Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrintros_ne), 
                                                        Tacentries.TyNil))), 
          (fun arg ipats ist -> 
# 2701 "ssrparser.mlg"
                                                      
    check_under_arg arg;
    Ssrfwd.undertac ist (Some ipats) arg nohint
    
          )));
         (Tacentries.TyML (Tacentries.TyIdent ("under", Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrrwarg), 
                                                        Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrintros_ne), 
                                                        Tacentries.TyIdent ("do", 
                                                        Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrhint3arg), 
                                                        Tacentries.TyNil))))), 
          (fun arg ipats h ist -> 
# 2705 "ssrparser.mlg"
                                                                          
    check_under_arg arg;
    Ssrfwd.undertac ist (Some ipats) arg h
    
          )));
         (Tacentries.TyML (Tacentries.TyIdent ("under", Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrrwarg), 
                                                        Tacentries.TyIdent ("do", 
                                                        Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ssrhint3arg), 
                                                        Tacentries.TyNil)))), 
          (fun arg h ist -> 
# 2709 "ssrparser.mlg"
                                                       (* implicit "=> [*|*]" *)
    check_under_arg arg;
    Ssrfwd.undertac ~pad_intro:true ist (Some [IPatAnon All]) arg h
    
          )))]


# 2715 "ssrparser.mlg"
 

(* We wipe out all the keywords generated by the grammar rules we defined. *)
(* The user is supposed to Require Import ssreflect or Require ssreflect   *)
(* and Import ssreflect.SsrSyntax to obtain these keywords and as a         *)
(* consequence the extended ssreflect grammar.                             *)
let () = CLexer.set_keyword_state frozen_lexer ;;



