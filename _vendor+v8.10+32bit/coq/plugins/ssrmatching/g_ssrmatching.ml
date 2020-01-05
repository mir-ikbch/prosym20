
# 13 "g_ssrmatching.mlg"
 

open Ltac_plugin
open Pcoq.Constr
open Ssrmatching
open Ssrmatching.Internal

(* Defining grammar rules with "xx" in it automatically declares keywords too,
 * we thus save the lexer to restore it at the end of the file *)
let frozen_lexer = CLexer.get_keyword_state () ;;



let __coq_plugin_name = "ssrmatching_plugin"
let _ = Mltop.add_known_module __coq_plugin_name

# 28 "g_ssrmatching.mlg"
 

let pr_rpattern _ _ _ = pr_rpattern



let (wit_rpattern, rpattern) = Tacentries.argument_extend ~name:"rpattern" 
                               {
                               Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                        [(Extend.Rule
                                                          (Extend.Next 
                                                           (Extend.Next 
                                                           (Extend.Next 
                                                           (Extend.Next 
                                                           (Extend.Next 
                                                           (Extend.Stop,
                                                           (Extend.Aentry lconstr)),
                                                           (Extend.Atoken (CLexer.terminal "as"))),
                                                           (Extend.Aentry lconstr)),
                                                           (Extend.Atoken (CLexer.terminal "in"))),
                                                           (Extend.Aentry lconstr)),
                                                          (fun c _ x _ e
                                                          loc -> 
# 49 "g_ssrmatching.mlg"
      mk_rpattern (E_As_X_In_T (mk_lterm e None, mk_lterm x None, mk_lterm c None)) 
                                                                 )));
                                                        (Extend.Rule
                                                         (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Stop,
                                                          (Extend.Aentry lconstr)),
                                                          (Extend.Atoken (CLexer.terminal "in"))),
                                                          (Extend.Aentry lconstr)),
                                                          (Extend.Atoken (CLexer.terminal "in"))),
                                                          (Extend.Aentry lconstr)),
                                                         (fun c _ x _ e
                                                         loc -> 
# 47 "g_ssrmatching.mlg"
      mk_rpattern (E_In_X_In_T (mk_lterm e None, mk_lterm x None, mk_lterm c None)) 
                                                                )));
                                                        (Extend.Rule
                                                         (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (CLexer.terminal "in"))),
                                                          (Extend.Aentry lconstr)),
                                                          (Extend.Atoken (CLexer.terminal "in"))),
                                                          (Extend.Aentry lconstr)),
                                                         (fun c _ x _ loc ->
                                                         
# 45 "g_ssrmatching.mlg"
      mk_rpattern (In_X_In_T (mk_lterm x None, mk_lterm c None)) 
                                                         )));
                                                        (Extend.Rule
                                                         (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Stop,
                                                          (Extend.Aentry lconstr)),
                                                          (Extend.Atoken (CLexer.terminal "in"))),
                                                          (Extend.Aentry lconstr)),
                                                         (fun c _ x loc -> 
# 43 "g_ssrmatching.mlg"
      mk_rpattern (X_In_T (mk_lterm x None, mk_lterm c None)) 
                                                                    )));
                                                        (Extend.Rule
                                                         (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (CLexer.terminal "in"))),
                                                          (Extend.Aentry lconstr)),
                                                         (fun c _ loc -> 
# 41 "g_ssrmatching.mlg"
                             mk_rpattern (In_T (mk_lterm c None)) 
                                                                    )));
                                                        (Extend.Rule
                                                         (Extend.Next 
                                                          (Extend.Stop,
                                                          (Extend.Aentry lconstr)),
                                                         (fun c loc -> 
# 40 "g_ssrmatching.mlg"
                        mk_rpattern (T (mk_lterm c None)) 
                                                                    )))]);
                               Tacentries.arg_tag = Some
                                                    (Geninterp.val_tag (Genarg.topwit wit_rpatternty));
                               Tacentries.arg_intern = Tacentries.ArgInternFun ((fun f ist v -> (ist, f ist v)) (
                                                       
# 38 "g_ssrmatching.mlg"
                  glob_rpattern 
                                                       ));
                               Tacentries.arg_subst = Tacentries.ArgSubstFun (
                                                      
# 39 "g_ssrmatching.mlg"
                   subst_rpattern 
                                                      );
                               Tacentries.arg_interp = Tacentries.ArgInterpLegacy (
                                                       
# 37 "g_ssrmatching.mlg"
                   interp_rpattern 
                                                       );
                               Tacentries.arg_printer = ((fun env sigma -> 
                                                        
# 36 "g_ssrmatching.mlg"
               pr_rpattern 
                                                        ), (fun env sigma -> 
                                                        
# 36 "g_ssrmatching.mlg"
               pr_rpattern 
                                                        ), (fun env sigma -> 
                                                        
# 36 "g_ssrmatching.mlg"
               pr_rpattern 
                                                        ));
                               }
let _ = (wit_rpattern, rpattern)


# 52 "g_ssrmatching.mlg"
 

let pr_ssrterm _ _ _ = pr_ssrterm



let (wit_cpattern, cpattern) = Tacentries.argument_extend ~name:"cpattern" 
                               {
                               Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                        [(Extend.Rule
                                                          (Extend.Next 
                                                           (Extend.Next 
                                                           (Extend.Stop,
                                                           (Extend.Atoken (CLexer.terminal "Qed"))),
                                                           (Extend.Aentry constr)),
                                                          (fun c _ loc -> 
# 64 "g_ssrmatching.mlg"
                           mk_lterm c None 
                                                                    )))]);
                               Tacentries.arg_tag = None;
                               Tacentries.arg_intern = Tacentries.ArgInternFun ((fun f ist v -> (ist, f ist v)) (
                                                       
# 61 "g_ssrmatching.mlg"
                     glob_cpattern 
                                                       ));
                               Tacentries.arg_subst = Tacentries.ArgSubstFun (
                                                      
# 61 "g_ssrmatching.mlg"
                                                      subst_ssrterm 
                                                      );
                               Tacentries.arg_interp = Tacentries.ArgInterpLegacy (
                                                       
# 60 "g_ssrmatching.mlg"
                      interp_ssrterm 
                                                       );
                               Tacentries.arg_printer = ((fun env sigma -> 
                                                        
# 62 "g_ssrmatching.mlg"
                      pr_ssrterm 
                                                        ), (fun env sigma -> 
                                                        
# 63 "g_ssrmatching.mlg"
                       pr_ssrterm 
                                                        ), (fun env sigma -> 
                                                        
# 59 "g_ssrmatching.mlg"
                  pr_ssrterm 
                                                        ));
                               }
let _ = (wit_cpattern, cpattern)


# 67 "g_ssrmatching.mlg"
 

let input_ssrtermkind strm = match Util.stream_nth 0 strm with
  | Tok.KEYWORD "(" -> '('
  | Tok.KEYWORD "@" -> '@'
  | _ -> ' '
let ssrtermkind = Pcoq.Entry.of_parser "ssrtermkind" input_ssrtermkind



let _ = let () =
        Pcoq.grammar_extend cpattern None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry ssrtermkind)),
                              (Extend.Aentry constr)),
                 (fun c k loc -> 
# 79 "g_ssrmatching.mlg"
                                               
    let pattern = mk_term k c None in
    if loc_of_cpattern pattern <> Some loc && k = '('
    then mk_term 'x' c None
    else pattern 
                                 ))])])
        in ()

let (wit_lcpattern, lcpattern) = Tacentries.argument_extend ~name:"lcpattern" 
                                 {
                                 Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                          [(Extend.Rule
                                                            (Extend.Next 
                                                             (Extend.Next 
                                                             (Extend.Stop,
                                                             (Extend.Atoken (CLexer.terminal "Qed"))),
                                                             (Extend.Aentry lconstr)),
                                                            (fun c _ loc -> 
# 93 "g_ssrmatching.mlg"
                            mk_lterm c None 
                                                                    )))]);
                                 Tacentries.arg_tag = Some
                                                      (Geninterp.val_tag (Genarg.topwit wit_cpattern));
                                 Tacentries.arg_intern = Tacentries.ArgInternFun ((fun f ist v -> (ist, f ist v)) (
                                                         
# 90 "g_ssrmatching.mlg"
                     glob_cpattern 
                                                         ));
                                 Tacentries.arg_subst = Tacentries.ArgSubstFun (
                                                        
# 90 "g_ssrmatching.mlg"
                                                      subst_ssrterm 
                                                        );
                                 Tacentries.arg_interp = Tacentries.ArgInterpLegacy (
                                                         
# 89 "g_ssrmatching.mlg"
                      interp_ssrterm 
                                                         );
                                 Tacentries.arg_printer = ((fun env sigma -> 
                                                          
# 91 "g_ssrmatching.mlg"
                      pr_ssrterm 
                                                          ), (fun env sigma -> 
                                                          
# 92 "g_ssrmatching.mlg"
                       pr_ssrterm 
                                                          ), (fun env sigma -> 
                                                          
# 88 "g_ssrmatching.mlg"
                  pr_ssrterm 
                                                          ));
                                 }
let _ = (wit_lcpattern, lcpattern)

let _ = let () =
        Pcoq.grammar_extend lcpattern None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry ssrtermkind)),
                              (Extend.Aentry lconstr)),
                 (fun c k loc -> 
# 98 "g_ssrmatching.mlg"
                                                 
    let pattern = mk_term k c None in
    if loc_of_cpattern pattern <> Some loc && k = '('
    then mk_term 'x' c None
    else pattern 
                                 ))])])
        in ()

let (wit_ssrpatternarg, ssrpatternarg) = Tacentries.argument_extend ~name:"ssrpatternarg" 
                                         {
                                         Tacentries.arg_parsing = Vernacextend.Arg_alias (rpattern);
                                         Tacentries.arg_tag = Some
                                                              (Geninterp.val_tag (Genarg.topwit wit_rpattern));
                                         Tacentries.arg_intern = Tacentries.ArgInternWit (wit_rpattern);
                                         Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_rpattern);
                                         Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_rpattern);
                                         Tacentries.arg_printer = ((fun env sigma -> 
                                                                  
# 105 "g_ssrmatching.mlg"
                                                             pr_rpattern 
                                                                  ), (fun env sigma -> 
                                                                  
# 105 "g_ssrmatching.mlg"
                                                             pr_rpattern 
                                                                  ), (fun env sigma -> 
                                                                  
# 105 "g_ssrmatching.mlg"
                                                             pr_rpattern 
                                                                  ));
                                         }
let _ = (wit_ssrpatternarg, ssrpatternarg)

let () = Tacentries.tactic_extend __coq_plugin_name "ssrinstoftpat" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("ssrinstancesoftpat", 
                            Tacentries.TyArg (Extend.TUentry (Genarg.get_arg_tag wit_cpattern), 
                            Tacentries.TyNil)), (fun arg ist -> 
# 110 "g_ssrmatching.mlg"
                                              Proofview.V82.tactic (ssrinstancesof arg) 
                                                )))]


# 113 "g_ssrmatching.mlg"
 

(* We wipe out all the keywords generated by the grammar rules we defined. *)
(* The user is supposed to Require Import ssreflect or Require ssreflect   *)
(* and Import ssreflect.SsrSyntax to obtain these keywords and as a         *)
(* consequence the extended ssreflect grammar.                             *)
let () = CLexer.set_keyword_state frozen_lexer ;;



