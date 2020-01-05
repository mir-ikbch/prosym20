
# 11 "extratactics.mlg"
 

open Pp
open Constr
open Context
open Genarg
open Stdarg
open Tacarg
open Extraargs
open Pcoq.Prim
open Pltac
open Mod_subst
open Names
open Tacexpr
open Glob_ops
open CErrors
open Util
open Termops
open Equality
open Namegen
open Tactypes
open Tactics
open Proofview.Notations
open Attributes
open Vernacextend

let wit_hyp = wit_var



let __coq_plugin_name = "ltac_plugin"
let _ = Mltop.add_known_module __coq_plugin_name

# 43 "extratactics.mlg"
 

(**********************************************************************)
(* replace, discriminate, injection, simplify_eq                      *)
(* cutrewrite, dependent rewrite                                      *)

let with_delayed_uconstr ist c tac =
  let flags = {
    Pretyping.use_typeclasses = false;
    solve_unification_constraints = true;
    fail_evar = false;
    expand_evars = true;
    program_mode = false;
    polymorphic = false;
 } in
  let c = Tacinterp.type_uconstr ~flags ist c in
  Tacticals.New.tclDELAYEDWITHHOLES false c tac

let replace_in_clause_maybe_by ist c1 c2 cl tac =
  with_delayed_uconstr ist c1
  (fun c1 -> replace_in_clause_maybe_by c1 c2 cl (Option.map (Tacinterp.tactic_of_value ist) tac))

let replace_term ist dir_opt c cl =
  with_delayed_uconstr ist c (fun c -> replace_term dir_opt c cl)



let () = Tacentries.tactic_extend __coq_plugin_name "replace" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("replace", Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_uconstr), 
                                                           Tacentries.TyIdent ("with", 
                                                           Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                           Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_clause), 
                                                           Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_by_arg_tac), 
                                                           Tacentries.TyNil)))))), 
           (fun c1 c2 cl tac ist -> 
# 72 "extratactics.mlg"
     replace_in_clause_maybe_by ist c1 c2 cl tac 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "replace_term_left" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("replace", Tacentries.TyIdent ("->", 
                                                           Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_uconstr), 
                                                           Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_clause), 
                                                           Tacentries.TyNil)))), 
           (fun c cl ist -> 
# 77 "extratactics.mlg"
       replace_term ist (Some true) c cl 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "replace_term_right" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("replace", Tacentries.TyIdent ("<-", 
                                                           Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_uconstr), 
                                                           Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_clause), 
                                                           Tacentries.TyNil)))), 
           (fun c cl ist -> 
# 82 "extratactics.mlg"
       replace_term ist (Some false) c cl 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "replace_term" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("replace", Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_uconstr), 
                                                           Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_clause), 
                                                           Tacentries.TyNil))), 
           (fun c cl ist -> 
# 87 "extratactics.mlg"
       replace_term ist None c cl 
           )))]


# 90 "extratactics.mlg"
 

let induction_arg_of_quantified_hyp = function
  | AnonHyp n -> None,ElimOnAnonHyp n
  | NamedHyp id -> None,ElimOnIdent (CAst.make id)

(* Versions *_main must come first!! so that "1" is interpreted as a
   ElimOnAnonHyp and not as a "constr", and "id" is interpreted as a
   ElimOnIdent and not as "constr" *)

let mytclWithHoles tac with_evars c =
  Proofview.Goal.enter begin fun gl ->
    let env = Tacmach.New.pf_env gl in
    let sigma = Tacmach.New.project gl in
    let sigma',c = Tactics.force_destruction_arg with_evars env sigma c in
    Tacticals.New.tclWITHHOLES with_evars (tac with_evars (Some c)) sigma'
  end

let elimOnConstrWithHoles tac with_evars c =
  Tacticals.New.tclDELAYEDWITHHOLES with_evars c
    (fun c -> tac with_evars (Some (None,ElimOnConstr c)))



let () = Tacentries.tactic_extend __coq_plugin_name "simplify_eq" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("simplify_eq", Tacentries.TyNil), 
           (fun ist -> 
# 115 "extratactics.mlg"
                         dEq ~keep_proofs:None false None 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("simplify_eq", Tacentries.TyArg (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_destruction_arg), 
                                                              Tacentries.TyNil)), 
          (fun c ist -> 
# 116 "extratactics.mlg"
                                            mytclWithHoles (dEq ~keep_proofs:None) false c 
          )))]

let () = Tacentries.tactic_extend __coq_plugin_name "esimplify_eq" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("esimplify_eq", Tacentries.TyNil), 
           (fun ist -> 
# 119 "extratactics.mlg"
                          dEq ~keep_proofs:None true None 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("esimplify_eq", Tacentries.TyArg (
                                                               Extend.TUentry (Genarg.get_arg_tag wit_destruction_arg), 
                                                               Tacentries.TyNil)), 
          (fun c ist -> 
# 120 "extratactics.mlg"
                                             mytclWithHoles (dEq ~keep_proofs:None) true c 
          )))]


# 123 "extratactics.mlg"
 

let discr_main c = elimOnConstrWithHoles discr_tac false c



let () = Tacentries.tactic_extend __coq_plugin_name "discriminate" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("discriminate", Tacentries.TyNil), 
           (fun ist -> 
# 130 "extratactics.mlg"
                          discr_tac false None 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("discriminate", Tacentries.TyArg (
                                                               Extend.TUentry (Genarg.get_arg_tag wit_destruction_arg), 
                                                               Tacentries.TyNil)), 
          (fun c ist -> 
# 132 "extratactics.mlg"
      mytclWithHoles discr_tac false c 
          )))]

let () = Tacentries.tactic_extend __coq_plugin_name "ediscriminate" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("ediscriminate", Tacentries.TyNil), 
           (fun ist -> 
# 135 "extratactics.mlg"
                           discr_tac true None 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("ediscriminate", Tacentries.TyArg (
                                                                Extend.TUentry (Genarg.get_arg_tag wit_destruction_arg), 
                                                                Tacentries.TyNil)), 
          (fun c ist -> 
# 137 "extratactics.mlg"
      mytclWithHoles discr_tac true c 
          )))]


# 140 "extratactics.mlg"
 

let discrHyp id =
  Proofview.tclEVARMAP >>= fun sigma ->
  discr_main (fun env sigma -> (sigma, (EConstr.mkVar id, NoBindings)))

let injection_main with_evars c =
 elimOnConstrWithHoles (injClause None None) with_evars c



let () = Tacentries.tactic_extend __coq_plugin_name "injection" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("injection", Tacentries.TyNil), 
           (fun ist -> 
# 152 "extratactics.mlg"
                       injClause None None false None 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("injection", Tacentries.TyArg (
                                                            Extend.TUentry (Genarg.get_arg_tag wit_destruction_arg), 
                                                            Tacentries.TyNil)), 
          (fun c ist -> 
# 153 "extratactics.mlg"
                                          mytclWithHoles (injClause None None) false c 
          )))]

let () = Tacentries.tactic_extend __coq_plugin_name "einjection" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("einjection", Tacentries.TyNil), 
           (fun ist -> 
# 156 "extratactics.mlg"
                        injClause None None true None 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("einjection", Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_destruction_arg), 
                                                             Tacentries.TyNil)), 
          (fun c ist -> 
# 157 "extratactics.mlg"
                                           mytclWithHoles (injClause None None) true c 
          )))]

let () = Tacentries.tactic_extend __coq_plugin_name "injection_as" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("injection", Tacentries.TyIdent ("as", 
                                                             Tacentries.TyArg (
                                                             Extend.TUlist0 (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_simple_intropattern)), 
                                                             Tacentries.TyNil))), 
           (fun ipat ist -> 
# 161 "extratactics.mlg"
      injClause None (Some ipat) false None 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("injection", Tacentries.TyArg (
                                                            Extend.TUentry (Genarg.get_arg_tag wit_destruction_arg), 
                                                            Tacentries.TyIdent ("as", 
                                                            Tacentries.TyArg (
                                                            Extend.TUlist0 (
                                                            Extend.TUentry (Genarg.get_arg_tag wit_simple_intropattern)), 
                                                            Tacentries.TyNil)))), 
          (fun c ipat ist -> 
# 163 "extratactics.mlg"
      mytclWithHoles (injClause None (Some ipat)) false c 
          )))]

let () = Tacentries.tactic_extend __coq_plugin_name "einjection_as" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("einjection", Tacentries.TyIdent ("as", 
                                                              Tacentries.TyArg (
                                                              Extend.TUlist0 (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_simple_intropattern)), 
                                                              Tacentries.TyNil))), 
           (fun ipat ist -> 
# 167 "extratactics.mlg"
      injClause None (Some ipat) true None 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("einjection", Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_destruction_arg), 
                                                             Tacentries.TyIdent ("as", 
                                                             Tacentries.TyArg (
                                                             Extend.TUlist0 (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_simple_intropattern)), 
                                                             Tacentries.TyNil)))), 
          (fun c ipat ist -> 
# 169 "extratactics.mlg"
      mytclWithHoles (injClause None (Some ipat)) true c 
          )))]

let () = Tacentries.tactic_extend __coq_plugin_name "simple_injection" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("simple", Tacentries.TyIdent ("injection", 
                                                          Tacentries.TyNil)), 
           (fun ist -> 
# 172 "extratactics.mlg"
                                simpleInjClause None false None 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("simple", Tacentries.TyIdent ("injection", 
                                                         Tacentries.TyArg (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_destruction_arg), 
                                                         Tacentries.TyNil))), 
          (fun c ist -> 
# 173 "extratactics.mlg"
                                                   mytclWithHoles (simpleInjClause None) false c 
          )))]


# 176 "extratactics.mlg"
 

let injHyp id =
  Proofview.tclEVARMAP >>= fun sigma ->
  injection_main false (fun env sigma -> (sigma, (EConstr.mkVar id, NoBindings)))



let () = Tacentries.tactic_extend __coq_plugin_name "dependent_rewrite" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("dependent", Tacentries.TyIdent ("rewrite", 
                                                             Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_orient), 
                                                             Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                             Tacentries.TyNil)))), 
           (fun b c ist -> 
# 185 "extratactics.mlg"
                                                     rewriteInConcl b c 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("dependent", Tacentries.TyIdent ("rewrite", 
                                                            Tacentries.TyArg (
                                                            Extend.TUentry (Genarg.get_arg_tag wit_orient), 
                                                            Tacentries.TyArg (
                                                            Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                            Tacentries.TyIdent ("in", 
                                                            Tacentries.TyArg (
                                                            Extend.TUentry (Genarg.get_arg_tag wit_hyp), 
                                                            Tacentries.TyNil)))))), 
          (fun b c id ist -> 
# 187 "extratactics.mlg"
         rewriteInHyp b c id 
          )))]

let () = Tacentries.tactic_extend __coq_plugin_name "cut_rewrite" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("cutrewrite", Tacentries.TyArg (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_orient), 
                                                              Tacentries.TyArg (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                              Tacentries.TyNil))), 
           (fun b eqn ist -> 
# 195 "extratactics.mlg"
                                              cutRewriteInConcl b eqn 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("cutrewrite", Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_orient), 
                                                             Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                             Tacentries.TyIdent ("in", 
                                                             Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_hyp), 
                                                             Tacentries.TyNil))))), 
          (fun b eqn id ist -> 
# 197 "extratactics.mlg"
         cutRewriteInHyp b eqn id 
          )))]

let () = Tacentries.tactic_extend __coq_plugin_name "decompose_sum" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("decompose", Tacentries.TyIdent ("sum", 
                                                             Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                             Tacentries.TyNil))), 
           (fun c ist -> 
# 204 "extratactics.mlg"
                                       Elim.h_decompose_or c 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "decompose_record" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("decompose", Tacentries.TyIdent ("record", 
                                                             Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                             Tacentries.TyNil))), 
           (fun c ist -> 
# 208 "extratactics.mlg"
                                          Elim.h_decompose_and c 
           )))]


# 214 "extratactics.mlg"
 

open Contradiction



let () = Tacentries.tactic_extend __coq_plugin_name "absurd" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("absurd", Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                          Tacentries.TyNil)), 
           (fun c ist -> 
# 221 "extratactics.mlg"
                              absurd c 
           )))]


# 224 "extratactics.mlg"
 

let onSomeWithHoles tac = function
  | None -> tac None
  | Some c -> Tacticals.New.tclDELAYEDWITHHOLES false c (fun c -> tac (Some c))



let () = Tacentries.tactic_extend __coq_plugin_name "contradiction" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("contradiction", Tacentries.TyArg (
                                                                 Extend.TUopt (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_constr_with_bindings)), 
                                                                 Tacentries.TyNil)), 
           (fun c ist -> 
# 234 "extratactics.mlg"
      onSomeWithHoles contradiction c 
           )))]


# 240 "extratactics.mlg"
 

open Autorewrite

let pr_orient _prc _prlc _prt = function
  | true -> Pp.mt ()
  | false -> Pp.str " <-"

let pr_orient_string _prc _prlc _prt (orient, s) =
  pr_orient _prc _prlc _prt orient ++ Pp.spc () ++ Pp.str s



let (wit_orient_string, orient_string) = Tacentries.argument_extend ~name:"orient_string" 
                                         {
                                         Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                                  [(Extend.Rule
                                                                    (
                                                                    Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Aentry orient)),
                                                                    (Extend.Aentry preident)),
                                                                    (fun i r
                                                                    loc -> 
                                                                    
# 254 "extratactics.mlg"
                                 r, i 
                                                                    )))]);
                                         Tacentries.arg_tag = Some
                                                              (Geninterp.Val.Pair (
                                                              (Geninterp.val_tag (Genarg.topwit wit_bool)), 
                                                              (Geninterp.val_tag (Genarg.topwit wit_string))));
                                         Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                                 (wit_bool), 
                                                                 (wit_string)));
                                         Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                                (wit_bool), 
                                                                (wit_string)));
                                         Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                                 (wit_bool), 
                                                                 (wit_string)));
                                         Tacentries.arg_printer = ((fun env sigma -> 
                                                                  
# 253 "extratactics.mlg"
                                                                    pr_orient_string 
                                                                  ), (fun env sigma -> 
                                                                  
# 253 "extratactics.mlg"
                                                                    pr_orient_string 
                                                                  ), (fun env sigma -> 
                                                                  
# 253 "extratactics.mlg"
                                                                    pr_orient_string 
                                                                  ));
                                         }
let _ = (wit_orient_string, orient_string)

let () = Tacentries.tactic_extend __coq_plugin_name "autorewrite" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("autorewrite", Tacentries.TyIdent ("with", 
                                                               Tacentries.TyArg (
                                                               Extend.TUlist1 (
                                                               Extend.TUentry (Genarg.get_arg_tag wit_preident)), 
                                                               Tacentries.TyArg (
                                                               Extend.TUentry (Genarg.get_arg_tag wit_clause), 
                                                               Tacentries.TyNil)))), 
           (fun l cl ist -> 
# 259 "extratactics.mlg"
      auto_multi_rewrite  l ( cl) 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("autorewrite", Tacentries.TyIdent ("with", 
                                                              Tacentries.TyArg (
                                                              Extend.TUlist1 (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_preident)), 
                                                              Tacentries.TyArg (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_clause), 
                                                              Tacentries.TyIdent ("using", 
                                                              Tacentries.TyArg (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                                              Tacentries.TyNil)))))), 
          (fun l cl t ist -> 
# 261 "extratactics.mlg"
     
      auto_multi_rewrite_with (Tacinterp.tactic_of_value ist t) l cl
   
          )))]

let () = Tacentries.tactic_extend __coq_plugin_name "autorewrite_star" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("autorewrite", Tacentries.TyIdent ("*", 
                                                               Tacentries.TyIdent ("with", 
                                                               Tacentries.TyArg (
                                                               Extend.TUlist1 (
                                                               Extend.TUentry (Genarg.get_arg_tag wit_preident)), 
                                                               Tacentries.TyArg (
                                                               Extend.TUentry (Genarg.get_arg_tag wit_clause), 
                                                               Tacentries.TyNil))))), 
           (fun l cl ist -> 
# 268 "extratactics.mlg"
      auto_multi_rewrite ~conds:AllMatches l cl 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("autorewrite", Tacentries.TyIdent ("*", 
                                                              Tacentries.TyIdent ("with", 
                                                              Tacentries.TyArg (
                                                              Extend.TUlist1 (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_preident)), 
                                                              Tacentries.TyArg (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_clause), 
                                                              Tacentries.TyIdent ("using", 
                                                              Tacentries.TyArg (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                                              Tacentries.TyNil))))))), 
          (fun l cl t ist -> 
# 270 "extratactics.mlg"
    auto_multi_rewrite_with ~conds:AllMatches (Tacinterp.tactic_of_value ist t) l cl 
          )))]


# 276 "extratactics.mlg"
 

let rewrite_star ist clause orient occs c (tac : Geninterp.Val.t option) =
  let tac' = Option.map (fun t -> Tacinterp.tactic_of_value ist t, FirstSolved) tac in
  with_delayed_uconstr ist c
    (fun c -> general_rewrite_ebindings_clause clause orient occs ?tac:tac' true true (c,NoBindings) true)



let () = Tacentries.tactic_extend __coq_plugin_name "rewrite_star" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("rewrite", Tacentries.TyIdent ("*", 
                                                           Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_orient), 
                                                           Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_uconstr), 
                                                           Tacentries.TyIdent ("in", 
                                                           Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_hyp), 
                                                           Tacentries.TyIdent ("at", 
                                                           Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_occurrences), 
                                                           Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_by_arg_tac), 
                                                           Tacentries.TyNil))))))))), 
           (fun o c id occ tac ist -> 
# 287 "extratactics.mlg"
      rewrite_star ist (Some id) o (occurrences_of occ) c tac 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("rewrite", Tacentries.TyIdent ("*", 
                                                          Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_orient), 
                                                          Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_uconstr), 
                                                          Tacentries.TyIdent ("at", 
                                                          Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_occurrences), 
                                                          Tacentries.TyIdent ("in", 
                                                          Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_hyp), 
                                                          Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_by_arg_tac), 
                                                          Tacentries.TyNil))))))))), 
          (fun o c occ id tac ist -> 
# 289 "extratactics.mlg"
      rewrite_star ist (Some id) o (occurrences_of occ) c tac 
          )));
         (Tacentries.TyML (Tacentries.TyIdent ("rewrite", Tacentries.TyIdent ("*", 
                                                          Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_orient), 
                                                          Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_uconstr), 
                                                          Tacentries.TyIdent ("in", 
                                                          Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_hyp), 
                                                          Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_by_arg_tac), 
                                                          Tacentries.TyNil))))))), 
          (fun o c id tac ist -> 
# 291 "extratactics.mlg"
      rewrite_star ist (Some id) o Locus.AllOccurrences c tac 
          )));
         (Tacentries.TyML (Tacentries.TyIdent ("rewrite", Tacentries.TyIdent ("*", 
                                                          Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_orient), 
                                                          Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_uconstr), 
                                                          Tacentries.TyIdent ("at", 
                                                          Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_occurrences), 
                                                          Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_by_arg_tac), 
                                                          Tacentries.TyNil))))))), 
          (fun o c occ tac ist -> 
# 293 "extratactics.mlg"
      rewrite_star ist None o (occurrences_of occ) c tac 
          )));
         (Tacentries.TyML (Tacentries.TyIdent ("rewrite", Tacentries.TyIdent ("*", 
                                                          Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_orient), 
                                                          Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_uconstr), 
                                                          Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_by_arg_tac), 
                                                          Tacentries.TyNil))))), 
          (fun o c tac ist -> 
# 295 "extratactics.mlg"
      rewrite_star ist None o Locus.AllOccurrences c tac 
          )))]


# 301 "extratactics.mlg"
 

let add_rewrite_hint ~poly bases ort t lcsr =
  let env = Global.env() in
  let sigma = Evd.from_env env in
  let f ce =
    let c, ctx = Constrintern.interp_constr env sigma ce in
    let c = EConstr.to_constr sigma c in
    let ctx =
      let ctx = UState.context_set ctx in
        if poly then ctx
        else (* This is a global universe context that shouldn't be
                refreshed at every use of the hint, declare it globally. *)
          (Declare.declare_universe_context false ctx;
           Univ.ContextSet.empty)
    in
      CAst.make ?loc:(Constrexpr_ops.constr_loc ce) ((c, ctx), ort, Option.map (in_gen (rawwit wit_ltac)) t) in
  let eqs = List.map f lcsr in
  let add_hints base = add_rew_rules base eqs in
  List.iter add_hints bases

let classify_hint _ = VtSideff [], VtLater



let () = Vernacextend.vernac_extend ~command:"HintRewrite" ~classifier:( classify_hint ) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Hint", 
                                     Vernacextend.TyTerminal ("Rewrite", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_orient), 
                                     Vernacextend.TyNonTerminal (Extend.TUlist1 (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_constr)), 
                                     Vernacextend.TyTerminal (":", Vernacextend.TyNonTerminal (
                                                                   Extend.TUlist0 (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_preident)), 
                                                                   Vernacextend.TyNil)))))), 
         (let coqpp_body o l bl polymorphic ~st = let () = 
# 328 "extratactics.mlg"
    add_rewrite_hint ~poly:polymorphic bl o None l 
                                                   in st in fun o
         l bl ~atts ~st -> coqpp_body o l bl
         (Attributes.parse polymorphic atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Hint", 
                                    Vernacextend.TyTerminal ("Rewrite", 
                                    Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_orient), 
                                    Vernacextend.TyNonTerminal (Extend.TUlist1 (
                                                                Extend.TUentry (Genarg.get_arg_tag wit_constr)), 
                                    Vernacextend.TyTerminal ("using", 
                                    Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                    Vernacextend.TyTerminal (":", Vernacextend.TyNonTerminal (
                                                                  Extend.TUlist0 (
                                                                  Extend.TUentry (Genarg.get_arg_tag wit_preident)), 
                                                                  Vernacextend.TyNil)))))))), 
         (let coqpp_body o l t bl polymorphic ~st = let () = 
# 331 "extratactics.mlg"
    add_rewrite_hint ~poly:polymorphic bl o (Some t) l 
                                                     in st in fun o
         l t bl ~atts ~st -> coqpp_body o l t bl
         (Attributes.parse polymorphic atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Hint", 
                                    Vernacextend.TyTerminal ("Rewrite", 
                                    Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_orient), 
                                    Vernacextend.TyNonTerminal (Extend.TUlist1 (
                                                                Extend.TUentry (Genarg.get_arg_tag wit_constr)), 
                                    Vernacextend.TyNil)))), (let coqpp_body o
                                                            l
                                                            polymorphic ~st = 
                                                            let () = 
                                                            
# 333 "extratactics.mlg"
    add_rewrite_hint ~poly:polymorphic ["core"] o None l 
                                                             in st in fun o l
                                                            ~atts ~st
                                                            -> coqpp_body o l
                                                            (Attributes.parse polymorphic atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Hint", 
                                    Vernacextend.TyTerminal ("Rewrite", 
                                    Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_orient), 
                                    Vernacextend.TyNonTerminal (Extend.TUlist1 (
                                                                Extend.TUentry (Genarg.get_arg_tag wit_constr)), 
                                    Vernacextend.TyTerminal ("using", 
                                    Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                    Vernacextend.TyNil)))))), (let coqpp_body o
                                                              l t
                                                              polymorphic ~st = 
                                                              let () = 
                                                              
# 335 "extratactics.mlg"
    add_rewrite_hint ~poly:polymorphic ["core"] o (Some t) l 
                                                               in st in fun o
                                                              l t ~atts ~st
                                                              -> coqpp_body o
                                                              l t
                                                              (Attributes.parse polymorphic atts) ~st), None))]


# 341 "extratactics.mlg"
 

open EConstr
open Vars

let constr_flags () = {
  Pretyping.use_typeclasses = true;
  Pretyping.solve_unification_constraints = Pfedit.use_unification_heuristics ();
  Pretyping.fail_evar = false;
  Pretyping.expand_evars = true;
  Pretyping.program_mode = false;
  Pretyping.polymorphic = false;
}

let refine_tac ist simple with_classes c =
  Proofview.Goal.enter begin fun gl ->
    let concl = Proofview.Goal.concl gl in
    let env = Proofview.Goal.env gl in
    let flags =
      { (constr_flags ()) with Pretyping.use_typeclasses = with_classes } in
    let expected_type = Pretyping.OfType concl in
    let c = Tacinterp.type_uconstr ~flags ~expected_type ist c in
    let update = begin fun sigma ->
      c env sigma
    end in
    let refine = Refine.refine ~typecheck:false update in
    if simple then refine
    else refine <*>
           Tactics.New.reduce_after_refine <*>
           Proofview.shelve_unifiable
  end



let () = Tacentries.tactic_extend __coq_plugin_name "refine" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("refine", Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_uconstr), 
                                                          Tacentries.TyNil)), 
           (fun c ist -> 
# 377 "extratactics.mlg"
     refine_tac ist false true c 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "simple_refine" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("simple", Tacentries.TyIdent ("refine", 
                                                          Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_uconstr), 
                                                          Tacentries.TyNil))), 
           (fun c ist -> 
# 382 "extratactics.mlg"
     refine_tac ist true true c 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "notcs_refine" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("notypeclasses", Tacentries.TyIdent ("refine", 
                                                                 Tacentries.TyArg (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_uconstr), 
                                                                 Tacentries.TyNil))), 
           (fun c ist -> 
# 387 "extratactics.mlg"
     refine_tac ist false false c 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "notcs_simple_refine" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("simple", Tacentries.TyIdent ("notypeclasses", 
                                                          Tacentries.TyIdent ("refine", 
                                                          Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_uconstr), 
                                                          Tacentries.TyNil)))), 
           (fun c ist -> 
# 392 "extratactics.mlg"
     refine_tac ist true false c 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "solve_constraints" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("solve_constraints", 
                            Tacentries.TyNil), (fun ist -> 
# 397 "extratactics.mlg"
                               Refine.solve_constraints 
                                               )))]


# 403 "extratactics.mlg"
 

open Inv
open Leminv

let seff id = VtSideff [id], VtLater



let () = Vernacextend.vernac_extend ~command:"DeriveInversionClear"  ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Derive", 
                                     Vernacextend.TyTerminal ("Inversion_clear", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                     Vernacextend.TyTerminal ("with", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                     Vernacextend.TyTerminal ("Sort", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_sort_family), 
                                     Vernacextend.TyNil))))))), (let coqpp_body na
                                                                c s
                                                                polymorphic ~st = 
                                                                let () = 
                                                                
# 421 "extratactics.mlg"
      
      add_inversion_lemma_exn ~poly:polymorphic na c s false inv_clear_tac 
                                                                 in st in fun na
                                                                c s ~atts ~st
                                                                -> coqpp_body na
                                                                c s
                                                                (Attributes.parse polymorphic atts) ~st), Some 
         (fun na c s -> 
# 420 "extratactics.mlg"
       seff na 
         )));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Derive", 
                                    Vernacextend.TyTerminal ("Inversion_clear", 
                                    Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                    Vernacextend.TyTerminal ("with", 
                                    Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                    Vernacextend.TyNil))))), (let coqpp_body na
                                                             c
                                                             polymorphic ~st = 
                                                             let () = 
                                                             
# 425 "extratactics.mlg"
      
      add_inversion_lemma_exn ~poly:polymorphic na c Sorts.InProp false inv_clear_tac 
                                                              in st in fun na
                                                             c ~atts ~st
                                                             -> coqpp_body na
                                                             c
                                                             (Attributes.parse polymorphic atts) ~st), Some 
         (fun na c -> 
# 424 "extratactics.mlg"
                                                                                   seff na 
         )))]

let () = Vernacextend.vernac_extend ~command:"DeriveInversion"  ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Derive", 
                                     Vernacextend.TyTerminal ("Inversion", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                     Vernacextend.TyTerminal ("with", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                     Vernacextend.TyTerminal ("Sort", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_sort_family), 
                                     Vernacextend.TyNil))))))), (let coqpp_body na
                                                                c s
                                                                polymorphic ~st = 
                                                                let () = 
                                                                
# 432 "extratactics.mlg"
      
      add_inversion_lemma_exn ~poly:polymorphic na c s false inv_tac 
                                                                 in st in fun na
                                                                c s ~atts ~st
                                                                -> coqpp_body na
                                                                c s
                                                                (Attributes.parse polymorphic atts) ~st), Some 
         (fun na c s -> 
# 431 "extratactics.mlg"
       seff na 
         )));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Derive", 
                                    Vernacextend.TyTerminal ("Inversion", 
                                    Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                    Vernacextend.TyTerminal ("with", 
                                    Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                    Vernacextend.TyNil))))), (let coqpp_body na
                                                             c
                                                             polymorphic ~st = 
                                                             let () = 
                                                             
# 436 "extratactics.mlg"
      
      add_inversion_lemma_exn ~poly:polymorphic na c Sorts.InProp false inv_tac 
                                                              in st in fun na
                                                             c ~atts ~st
                                                             -> coqpp_body na
                                                             c
                                                             (Attributes.parse polymorphic atts) ~st), Some 
         (fun na c -> 
# 435 "extratactics.mlg"
                                                                             seff na 
         )))]

let () = Vernacextend.vernac_extend ~command:"DeriveDependentInversion"  ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Derive", 
                                     Vernacextend.TyTerminal ("Dependent", 
                                     Vernacextend.TyTerminal ("Inversion", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                     Vernacextend.TyTerminal ("with", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                     Vernacextend.TyTerminal ("Sort", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_sort_family), 
                                     Vernacextend.TyNil)))))))), (let coqpp_body na
                                                                 c s
                                                                 polymorphic ~st = 
                                                                 let () = 
                                                                 
# 443 "extratactics.mlg"
      
      add_inversion_lemma_exn ~poly:polymorphic na c s true dinv_tac 
                                                                  in st in fun na
                                                                 c s ~atts
                                                                 ~st
                                                                 -> coqpp_body na
                                                                 c s
                                                                 (Attributes.parse polymorphic atts) ~st), Some 
         (fun na c s -> 
# 442 "extratactics.mlg"
       seff na 
         )))]

let () = Vernacextend.vernac_extend ~command:"DeriveDependentInversionClear"  ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Derive", 
                                     Vernacextend.TyTerminal ("Dependent", 
                                     Vernacextend.TyTerminal ("Inversion_clear", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                     Vernacextend.TyTerminal ("with", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                     Vernacextend.TyTerminal ("Sort", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_sort_family), 
                                     Vernacextend.TyNil)))))))), (let coqpp_body na
                                                                 c s
                                                                 polymorphic ~st = 
                                                                 let () = 
                                                                 
# 450 "extratactics.mlg"
      
      add_inversion_lemma_exn ~poly:polymorphic na c s true dinv_clear_tac 
                                                                  in st in fun na
                                                                 c s ~atts
                                                                 ~st
                                                                 -> coqpp_body na
                                                                 c s
                                                                 (Attributes.parse polymorphic atts) ~st), Some 
         (fun na c s -> 
# 449 "extratactics.mlg"
       seff na 
         )))]

let () = Tacentries.tactic_extend __coq_plugin_name "subst" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("subst", Tacentries.TyArg (
                                                         Extend.TUlist1 (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_var)), 
                                                         Tacentries.TyNil)), 
           (fun l ist -> 
# 458 "extratactics.mlg"
                                  subst l 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("subst", Tacentries.TyNil), 
          (fun ist -> 
# 459 "extratactics.mlg"
                   subst_all () 
          )))]


# 462 "extratactics.mlg"
 

let simple_subst_tactic_flags =
  { only_leibniz = true; rewrite_dependent_proof = false }



let () = Tacentries.tactic_extend __coq_plugin_name "simple_subst" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("simple", Tacentries.TyIdent ("subst", 
                                                          Tacentries.TyNil)), 
           (fun ist -> 
# 470 "extratactics.mlg"
                            subst_all ~flags:simple_subst_tactic_flags () 
           )))]


# 473 "extratactics.mlg"
 

open Evar_tactics



let () = Tacentries.tactic_extend __coq_plugin_name "evar" ~level:0 [(
                                                                    Tacentries.TyML (
                                                                    Tacentries.TyIdent ("evar", 
                                                                    Tacentries.TyArg (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_test_lpar_id_colon), 
                                                                    Tacentries.TyIdent ("(", 
                                                                    Tacentries.TyArg (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                    Tacentries.TyIdent (":", 
                                                                    Tacentries.TyArg (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_lconstr), 
                                                                    Tacentries.TyIdent (")", 
                                                                    Tacentries.TyNil))))))), 
                                                                    (fun _ id
                                                                    typ ist
                                                                    -> 
                                                                    
# 485 "extratactics.mlg"
                                                                        let_evar (Name.Name id) typ 
                                                                    )));
                                                                    (
                                                                    Tacentries.TyML (
                                                                    Tacentries.TyIdent ("evar", 
                                                                    Tacentries.TyArg (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Tacentries.TyNil)), 
                                                                    (fun typ
                                                                    ist -> 
                                                                    
# 486 "extratactics.mlg"
                              let_evar Name.Anonymous typ 
                                                                    )))]

let () = Tacentries.tactic_extend __coq_plugin_name "instantiate" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("instantiate", Tacentries.TyIdent ("(", 
                                                               Tacentries.TyArg (
                                                               Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                               Tacentries.TyIdent (":=", 
                                                               Tacentries.TyArg (
                                                               Extend.TUentry (Genarg.get_arg_tag wit_lglob), 
                                                               Tacentries.TyIdent (")", 
                                                               Tacentries.TyNil)))))), 
           (fun id c ist -> 
# 491 "extratactics.mlg"
      Tacticals.New.tclTHEN (instantiate_tac_by_name id c) Proofview.V82.nf_evar_goals 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("instantiate", Tacentries.TyIdent ("(", 
                                                              Tacentries.TyArg (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_integer), 
                                                              Tacentries.TyIdent (":=", 
                                                              Tacentries.TyArg (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_lglob), 
                                                              Tacentries.TyIdent (")", 
                                                              Tacentries.TyArg (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_hloc), 
                                                              Tacentries.TyNil))))))), 
          (fun i c hl ist -> 
# 493 "extratactics.mlg"
      Tacticals.New.tclTHEN (instantiate_tac i c hl) Proofview.V82.nf_evar_goals 
          )));
         (Tacentries.TyML (Tacentries.TyIdent ("instantiate", Tacentries.TyNil), 
          (fun ist -> 
# 494 "extratactics.mlg"
                         Proofview.V82.nf_evar_goals 
          )))]


# 500 "extratactics.mlg"
 

open Tactics
open Glob_term
open Libobject
open Lib

(* Registered lemmas are expected to be of the form
     x R y -> y == z -> x R z    (in the right table)
     x R y -> x == z -> z R y    (in the left table)
*)

let transitivity_right_table = Summary.ref [] ~name:"transitivity-steps-r"
let transitivity_left_table = Summary.ref [] ~name:"transitivity-steps-l"

(* [step] tries to apply a rewriting lemma; then apply [tac] intended to
   complete to proof of the last hypothesis (assumed to state an equality) *)

let step left x tac =
  let l =
    List.map (fun lem ->
      let lem = EConstr.of_constr lem in
      Tacticals.New.tclTHENLAST
        (apply_with_bindings (lem, ImplicitBindings [x]))
        tac)
      !(if left then transitivity_left_table else transitivity_right_table)
  in
  Tacticals.New.tclFIRST l

(* Main function to push lemmas in persistent environment *)

let cache_transitivity_lemma (_,(left,lem)) =
  if left then
    transitivity_left_table  := lem :: !transitivity_left_table
  else
    transitivity_right_table := lem :: !transitivity_right_table

let subst_transitivity_lemma (subst,(b,ref)) = (b,subst_mps subst ref)

let inTransitivity : bool * Constr.t -> obj =
  declare_object @@ global_object_nodischarge "TRANSITIVITY-STEPS"
    ~cache:cache_transitivity_lemma
    ~subst:(Some subst_transitivity_lemma)

(* Main entry points *)

let add_transitivity_lemma left lem =
  let env = Global.env () in
  let sigma = Evd.from_env env in
  let lem',ctx (*FIXME*) = Constrintern.interp_constr env sigma lem in
  let lem' = EConstr.to_constr sigma lem' in
  add_anonymous_leaf (inTransitivity (left,lem'))



let () = Tacentries.tactic_extend __coq_plugin_name "stepl" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("stepl", Tacentries.TyArg (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                         Tacentries.TyIdent ("by", 
                                                         Tacentries.TyArg (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                                         Tacentries.TyNil)))), 
           (fun c tac ist -> 
# 558 "extratactics.mlg"
                                             step true c (Tacinterp.tactic_of_value ist tac) 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("stepl", Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                        Tacentries.TyNil)), 
          (fun c ist -> 
# 559 "extratactics.mlg"
                            step true c (Proofview.tclUNIT ()) 
          )))]

let () = Tacentries.tactic_extend __coq_plugin_name "stepr" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("stepr", Tacentries.TyArg (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                         Tacentries.TyIdent ("by", 
                                                         Tacentries.TyArg (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                                         Tacentries.TyNil)))), 
           (fun c tac ist -> 
# 563 "extratactics.mlg"
                                             step false c (Tacinterp.tactic_of_value ist tac) 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("stepr", Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                        Tacentries.TyNil)), 
          (fun c ist -> 
# 564 "extratactics.mlg"
                            step false c (Proofview.tclUNIT ()) 
          )))]

let () = Vernacextend.vernac_extend ~command:"AddStepl" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Declare", 
                                     Vernacextend.TyTerminal ("Left", 
                                     Vernacextend.TyTerminal ("Step", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                     Vernacextend.TyNil)))), (let coqpp_body t
                                                             () ~st = 
                                                             let () = 
                                                             
# 569 "extratactics.mlg"
      add_transitivity_lemma true t 
                                                              in st in fun t
                                                             ~atts ~st
                                                             -> coqpp_body t
                                                             (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"AddStepr" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Declare", 
                                     Vernacextend.TyTerminal ("Right", 
                                     Vernacextend.TyTerminal ("Step", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                     Vernacextend.TyNil)))), (let coqpp_body t
                                                             () ~st = 
                                                             let () = 
                                                             
# 574 "extratactics.mlg"
      add_transitivity_lemma false t 
                                                              in st in fun t
                                                             ~atts ~st
                                                             -> coqpp_body t
                                                             (Attributes.unsupported_attributes atts) ~st), None))]

let () = Tacentries.tactic_extend __coq_plugin_name "generalize_eqs" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("generalize_eqs", Tacentries.TyArg (
                                                                  Extend.TUentry (Genarg.get_arg_tag wit_hyp), 
                                                                  Tacentries.TyNil)), 
           (fun id ist -> 
# 581 "extratactics.mlg"
                                   abstract_generalize ~generalize_vars:false id 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "dep_generalize_eqs" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("dependent", Tacentries.TyIdent ("generalize_eqs", 
                                                             Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_hyp), 
                                                             Tacentries.TyNil))), 
           (fun id ist -> 
# 584 "extratactics.mlg"
                                               abstract_generalize ~generalize_vars:false ~force_dep:true id 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "generalize_eqs_vars" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("generalize_eqs_vars", 
                            Tacentries.TyArg (Extend.TUentry (Genarg.get_arg_tag wit_hyp), 
                            Tacentries.TyNil)), (fun id ist -> 
# 587 "extratactics.mlg"
                                        abstract_generalize ~generalize_vars:true id 
                                                )))]

let () = Tacentries.tactic_extend __coq_plugin_name "dep_generalize_eqs_vars" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("dependent", Tacentries.TyIdent ("generalize_eqs_vars", 
                                                             Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_hyp), 
                                                             Tacentries.TyNil))), 
           (fun id ist -> 
# 590 "extratactics.mlg"
                                                    abstract_generalize ~force_dep:true ~generalize_vars:true id 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "specialize_eqs" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("specialize_eqs", Tacentries.TyArg (
                                                                  Extend.TUentry (Genarg.get_arg_tag wit_hyp), 
                                                                  Tacentries.TyNil)), 
           (fun id ist -> 
# 598 "extratactics.mlg"
                                    specialize_eqs id 
           )))]


# 609 "extratactics.mlg"
 

let subst_var_with_hole occ tid t =
  let occref = if occ > 0 then ref occ else Find_subterm.error_invalid_occurrence [occ] in
  let locref = ref 0 in
  let rec substrec x = match DAst.get x with
    | GVar id ->
        if Id.equal id tid
        then
          (decr occref;
           if Int.equal !occref 0 then x
           else
             (incr locref;
              DAst.make ~loc:(Loc.make_loc (!locref,0)) @@
              GHole (Evar_kinds.QuestionMark {
                  Evar_kinds.qm_obligation=Evar_kinds.Define true;
                  Evar_kinds.qm_name=Anonymous;
                  Evar_kinds.qm_record_field=None;
             }, IntroAnonymous, None)))
        else x
    | _ -> map_glob_constr_left_to_right substrec x in
  let t' = substrec t
  in
  if !occref > 0 then Find_subterm.error_invalid_occurrence [occ] else t'

let subst_hole_with_term occ tc t =
  let locref = ref 0 in
  let occref = ref occ in
  let rec substrec c = match DAst.get c with
    | GHole (Evar_kinds.QuestionMark {
                Evar_kinds.qm_obligation=Evar_kinds.Define true;
                Evar_kinds.qm_name=Anonymous;
                Evar_kinds.qm_record_field=None;
           }, IntroAnonymous, s) ->
        decr occref;
        if Int.equal !occref 0 then tc
        else
          (incr locref;
           DAst.make ~loc:(Loc.make_loc (!locref,0)) @@
           GHole (Evar_kinds.QuestionMark {
               Evar_kinds.qm_obligation=Evar_kinds.Define true;
               Evar_kinds.qm_name=Anonymous;
               Evar_kinds.qm_record_field=None;
          },IntroAnonymous,s))
    | _ -> map_glob_constr_left_to_right substrec c
  in
  substrec t

open Tacmach

let hResolve id c occ t =
  Proofview.Goal.enter begin fun gl ->
  let sigma = Proofview.Goal.sigma gl in
  let env = Termops.clear_named_body id (Proofview.Goal.env gl) in
  let concl = Proofview.Goal.concl gl in
  let env_ids = Termops.vars_of_env env in
  let c_raw = Detyping.detype Detyping.Now true env_ids env sigma c in
  let t_raw = Detyping.detype Detyping.Now true env_ids env sigma t in
  let rec resolve_hole t_hole =
    try
      Pretyping.understand env sigma t_hole
    with
      | Pretype_errors.PretypeError (_,_,Pretype_errors.UnsolvableImplicit _) as e ->
          let (e, info) = CErrors.push e in
          let loc_begin = Option.cata (fun l -> fst (Loc.unloc l)) 0 (Loc.get_loc info) in
          resolve_hole (subst_hole_with_term loc_begin c_raw t_hole)
  in
  let t_constr,ctx = resolve_hole (subst_var_with_hole occ id t_raw) in
  let sigma = Evd.merge_universe_context sigma ctx in
  let t_constr_type = Retyping.get_type_of env sigma t_constr in
  Proofview.tclTHEN (Proofview.Unsafe.tclEVARS sigma)
    (change_concl (mkLetIn (make_annot Name.Anonymous Sorts.Relevant,t_constr,t_constr_type,concl)))
  end

let hResolve_auto id c t =
  let rec resolve_auto n =
    try
      hResolve id c n t
    with
    | UserError _ as e -> raise e
    | e when CErrors.noncritical e -> resolve_auto (n+1)
  in
  resolve_auto 1



let () = Tacentries.tactic_extend __coq_plugin_name "hresolve_core" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("hresolve_core", Tacentries.TyIdent ("(", 
                                                                 Tacentries.TyArg (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                 Tacentries.TyIdent (":=", 
                                                                 Tacentries.TyArg (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                 Tacentries.TyIdent (")", 
                                                                 Tacentries.TyIdent ("at", 
                                                                 Tacentries.TyArg (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_int_or_var), 
                                                                 Tacentries.TyIdent ("in", 
                                                                 Tacentries.TyArg (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                 Tacentries.TyNil)))))))))), 
           (fun id c occ t ist -> 
# 696 "extratactics.mlg"
                                                                                                hResolve id c occ t 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("hresolve_core", Tacentries.TyIdent ("(", 
                                                                Tacentries.TyArg (
                                                                Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                Tacentries.TyIdent (":=", 
                                                                Tacentries.TyArg (
                                                                Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                Tacentries.TyIdent (")", 
                                                                Tacentries.TyIdent ("in", 
                                                                Tacentries.TyArg (
                                                                Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                Tacentries.TyNil)))))))), 
          (fun id c t ist -> 
# 697 "extratactics.mlg"
                                                                           hResolve_auto id c t 
          )))]

let () = Tacentries.tactic_extend __coq_plugin_name "hget_evar" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("hget_evar", Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_int_or_var), 
                                                             Tacentries.TyNil)), 
           (fun n ist -> 
# 705 "extratactics.mlg"
                                     Evar_tactics.hget_evar n 
           )))]


# 718 "extratactics.mlg"
 

exception Found of unit Proofview.tactic

let rewrite_except h =
  Proofview.Goal.enter begin fun gl ->
  let hyps = Tacmach.New.pf_ids_of_hyps gl in
  Tacticals.New.tclMAP (fun id -> if Id.equal id h then Proofview.tclUNIT () else
      Tacticals.New.tclTRY (Equality.general_rewrite_in true Locus.AllOccurrences true true id (mkVar h) false))
    hyps
  end


let refl_equal () = Coqlib.lib_ref "core.eq.type"

(* This is simply an implementation of the case_eq tactic.  this code
  should be replaced by a call to the tactic but I don't know how to
  call it before it is defined. *)
let  mkCaseEq a  : unit Proofview.tactic =
  Proofview.Goal.enter begin fun gl ->
    let type_of_a = Tacmach.New.pf_unsafe_type_of gl a in
    Tacticals.New.pf_constr_of_global (delayed_force refl_equal) >>= fun req ->
    Tacticals.New.tclTHENLIST
         [Tactics.generalize [(mkApp(req, [| type_of_a; a|]))];
          Proofview.Goal.enter begin fun gl ->
            let concl = Proofview.Goal.concl gl in
            let env = Proofview.Goal.env gl in
            (* FIXME: this looks really wrong. Does anybody really use
               this tactic? *)
            let (_, c) = Tacred.pattern_occs [Locus.OnlyOccurrences [1], a] env (Evd.from_env env) concl in
            change_concl c
          end;
          simplest_case a]
  end


let case_eq_intros_rewrite x =
  Proofview.Goal.enter begin fun gl ->
  let n = nb_prod (Tacmach.New.project gl) (Proofview.Goal.concl gl) in
  (* Pp.msgnl (Printer.pr_lconstr x); *)
  Tacticals.New.tclTHENLIST [
      mkCaseEq x;
    Proofview.Goal.enter begin fun gl ->
      let concl = Proofview.Goal.concl gl in
      let hyps = Tacmach.New.pf_ids_set_of_hyps gl in
      let n' = nb_prod (Tacmach.New.project gl) concl in
      let h = fresh_id_in_env hyps (Id.of_string "heq") (Proofview.Goal.env gl)  in
      Tacticals.New.tclTHENLIST [
                    Tacticals.New.tclDO (n'-n-1) intro;
                    introduction h;
                    rewrite_except h]
    end
  ]
  end

let rec find_a_destructable_match sigma t =
  let cl = induction_arg_of_quantified_hyp (NamedHyp (Id.of_string "x")) in
  let cl = [cl, (None, None), None], None in
  let dest = TacAtom (CAst.make @@ TacInductionDestruct(false, false, cl)) in
  match EConstr.kind sigma t with
    | Case (_,_,x,_) when closed0 sigma x ->
        if isVar sigma x then
          (* TODO check there is no rel n. *)
          raise (Found (Tacinterp.eval_tactic dest))
        else
          (* let _ = Pp.msgnl (Printer.pr_lconstr x)  in *)
          raise (Found (case_eq_intros_rewrite x))
    | _ -> EConstr.iter sigma (fun c -> find_a_destructable_match sigma c) t


let destauto t =
  Proofview.tclEVARMAP >>= fun sigma ->
  try find_a_destructable_match sigma t;
    Tacticals.New.tclZEROMSG (str "No destructable match found")
  with Found tac -> tac

let destauto_in id =
  Proofview.Goal.enter begin fun gl ->
  let ctype = Tacmach.New.pf_unsafe_type_of gl (mkVar id) in
(*  Pp.msgnl (Printer.pr_lconstr (mkVar id)); *)
(*  Pp.msgnl (Printer.pr_lconstr (ctype)); *)
  destauto ctype
  end



let () = Tacentries.tactic_extend __coq_plugin_name "destauto" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("destauto", Tacentries.TyNil), 
           (fun ist -> 
# 805 "extratactics.mlg"
                      Proofview.Goal.enter begin fun gl -> destauto (Proofview.Goal.concl gl) end 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("destauto", Tacentries.TyIdent ("in", 
                                                           Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_hyp), 
                                                           Tacentries.TyNil))), 
          (fun id ist -> 
# 806 "extratactics.mlg"
                                   destauto_in id 
          )))]

let () = Tacentries.tactic_extend __coq_plugin_name "transparent_abstract" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("transparent_abstract", 
                            Tacentries.TyArg (Extend.TUentryl (Genarg.get_arg_tag wit_tactic, 3), 
                            Tacentries.TyNil)), (fun t ist -> 
# 817 "extratactics.mlg"
                                             Proofview.Goal.enter begin fun gl ->
    Abstract.tclABSTRACT ~opaque:false None (Tacinterp.tactic_of_value ist t) end; 
                                                )));
         (Tacentries.TyML (Tacentries.TyIdent ("transparent_abstract", 
                           Tacentries.TyArg (Extend.TUentryl (Genarg.get_arg_tag wit_tactic, 3), 
                           Tacentries.TyIdent ("using", Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                        Tacentries.TyNil)))), 
          (fun t id ist -> 
# 819 "extratactics.mlg"
                                                               Proofview.Goal.enter begin fun gl ->
    Abstract.tclABSTRACT ~opaque:false (Some id) (Tacinterp.tactic_of_value ist t) end; 
          )))]

let () = Tacentries.tactic_extend __coq_plugin_name "constr_eq" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("constr_eq", Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                             Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                             Tacentries.TyNil))), 
           (fun x y ist -> 
# 826 "extratactics.mlg"
                                           Tactics.constr_eq ~strict:false x y 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "constr_eq_strict" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("constr_eq_strict", Tacentries.TyArg (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Tacentries.TyArg (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Tacentries.TyNil))), 
           (fun x y ist -> 
# 830 "extratactics.mlg"
                                                  Tactics.constr_eq ~strict:true x y 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "constr_eq_nounivs" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("constr_eq_nounivs", 
                            Tacentries.TyArg (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                            Tacentries.TyArg (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                            Tacentries.TyNil))), (fun x y ist -> 
# 834 "extratactics.mlg"
                                                  
    Proofview.tclEVARMAP >>= fun sigma ->
    if eq_constr_nounivs sigma x y then Proofview.tclUNIT () else Tacticals.New.tclFAIL 0 (str "Not equal") 
                                                 )))]

let () = Tacentries.tactic_extend __coq_plugin_name "is_evar" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("is_evar", Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                           Tacentries.TyNil)), 
           (fun x ist -> 
# 840 "extratactics.mlg"
                              
    Proofview.tclEVARMAP >>= fun sigma ->
    match EConstr.kind sigma x with
      | Evar _ -> Proofview.tclUNIT ()
      | _ -> Tacticals.New.tclFAIL 0 (str "Not an evar")
   
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "has_evar" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("has_evar", Tacentries.TyArg (
                                                            Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                            Tacentries.TyNil)), 
           (fun x ist -> 
# 849 "extratactics.mlg"
                               
  Proofview.tclEVARMAP >>= fun sigma ->
  if Evarutil.has_undefined_evars sigma x
  then Proofview.tclUNIT ()
  else Tacticals.New.tclFAIL 0 (str "No evars")

           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "is_hyp" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("is_var", Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                          Tacentries.TyNil)), 
           (fun x ist -> 
# 858 "extratactics.mlg"
                             
  Proofview.tclEVARMAP >>= fun sigma ->
  match EConstr.kind sigma x with
    | Var _ ->  Proofview.tclUNIT ()
    | _ -> Tacticals.New.tclFAIL 0 (str "Not a variable or hypothesis") 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "is_fix" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("is_fix", Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                          Tacentries.TyNil)), 
           (fun x ist -> 
# 866 "extratactics.mlg"
                             
  Proofview.tclEVARMAP >>= fun sigma ->
  match EConstr.kind sigma x with
    | Fix _ -> Proofview.tclUNIT ()
    | _ -> Tacticals.New.tclFAIL 0 (Pp.str "not a fix definition") 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "is_cofix" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("is_cofix", Tacentries.TyArg (
                                                            Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                            Tacentries.TyNil)), 
           (fun x ist -> 
# 874 "extratactics.mlg"
                               
  Proofview.tclEVARMAP >>= fun sigma ->
  match EConstr.kind sigma x with
    | CoFix _ -> Proofview.tclUNIT ()
    | _ -> Tacticals.New.tclFAIL 0 (Pp.str "not a cofix definition") 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "is_ind" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("is_ind", Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                          Tacentries.TyNil)), 
           (fun x ist -> 
# 882 "extratactics.mlg"
                             
  Proofview.tclEVARMAP >>= fun sigma ->
  match EConstr.kind sigma x with
    | Ind _ -> Proofview.tclUNIT ()
    | _ -> Tacticals.New.tclFAIL 0 (Pp.str "not an (co)inductive datatype") 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "is_constructor" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("is_constructor", Tacentries.TyArg (
                                                                  Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                  Tacentries.TyNil)), 
           (fun x ist -> 
# 890 "extratactics.mlg"
                                     
  Proofview.tclEVARMAP >>= fun sigma ->
  match EConstr.kind sigma x with
    | Construct _ -> Proofview.tclUNIT ()
    | _ -> Tacticals.New.tclFAIL 0 (Pp.str "not a constructor") 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "is_proj" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("is_proj", Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                           Tacentries.TyNil)), 
           (fun x ist -> 
# 898 "extratactics.mlg"
                              
  Proofview.tclEVARMAP >>= fun sigma ->
  match EConstr.kind sigma x with
    | Proj _ -> Proofview.tclUNIT ()
    | _ -> Tacticals.New.tclFAIL 0 (Pp.str "not a primitive projection") 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "is_const" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("is_const", Tacentries.TyArg (
                                                            Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                            Tacentries.TyNil)), 
           (fun x ist -> 
# 906 "extratactics.mlg"
                               
  Proofview.tclEVARMAP >>= fun sigma ->
  match EConstr.kind sigma x with
    | Const _ -> Proofview.tclUNIT ()
    | _ -> Tacticals.New.tclFAIL 0 (Pp.str "not a constant") 
           )))]

let () = Vernacextend.vernac_extend ~command:"GrabEvars"  ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Grab", 
                                     Vernacextend.TyTerminal ("Existential", 
                                     Vernacextend.TyTerminal ("Variables", 
                                     Vernacextend.TyNil))), (let coqpp_body () ~st = 
                                                            let proof = (
                                                            
# 920 "extratactics.mlg"
       fun ~pstate -> Option.map (Proof_global.simple_with_current_proof (fun _ p  -> Proof.V82.grab_evars p)) pstate 
                                                            ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun ~atts
                                                            ~st
                                                            -> coqpp_body (Attributes.unsupported_attributes atts) ~st), Some 
         
# 919 "extratactics.mlg"
       classify_as_proofstep 
         ))]

let () = Tacentries.tactic_extend __coq_plugin_name "shelve" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("shelve", Tacentries.TyNil), 
           (fun ist -> 
# 926 "extratactics.mlg"
      Proofview.shelve 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "shelve_unifiable" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("shelve_unifiable", Tacentries.TyNil), 
           (fun ist -> 
# 934 "extratactics.mlg"
      Proofview.shelve_unifiable 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "unshelve" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("unshelve", Tacentries.TyArg (
                                                            Extend.TUentryl (Genarg.get_arg_tag wit_tactic, 1), 
                                                            Tacentries.TyNil)), 
           (fun t ist -> 
# 940 "extratactics.mlg"
     
      Proofview.with_shelf (Tacinterp.tactic_of_value ist t) >>= fun (gls, ()) ->
      let gls = List.map Proofview.with_empty_state gls in
      Proofview.Unsafe.tclGETGOALS >>= fun ogls ->
      Proofview.Unsafe.tclSETGOALS (gls @ ogls)
   
           )))]

let () = Vernacextend.vernac_extend ~command:"Unshelve"  ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Unshelve", 
                                     Vernacextend.TyNil), (let coqpp_body () ~st = 
                                                          let proof = (
                                                          
# 952 "extratactics.mlg"
       fun ~pstate -> Option.map (Proof_global.simple_with_current_proof (fun _ p  -> Proof.unshelve p)) pstate  
                                                          ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun ~atts
                                                          ~st
                                                          -> coqpp_body (Attributes.unsupported_attributes atts) ~st), Some 
         
# 951 "extratactics.mlg"
       classify_as_proofstep 
         ))]

let () = Tacentries.tactic_extend __coq_plugin_name "give_up" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("give_up", Tacentries.TyNil), 
           (fun ist -> 
# 960 "extratactics.mlg"
      Proofview.give_up 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "cycle" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("cycle", Tacentries.TyArg (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_int_or_var), 
                                                         Tacentries.TyNil)), 
           (fun n ist -> 
# 965 "extratactics.mlg"
                                 Proofview.cycle n 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "swap" ~level:0 [(
                                                                    Tacentries.TyML (
                                                                    Tacentries.TyIdent ("swap", 
                                                                    Tacentries.TyArg (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_int_or_var), 
                                                                    Tacentries.TyArg (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_int_or_var), 
                                                                    Tacentries.TyNil))), 
                                                                    (fun i j
                                                                    ist -> 
                                                                    
# 970 "extratactics.mlg"
                                              Proofview.swap i j 
                                                                    )))]

let () = Tacentries.tactic_extend __coq_plugin_name "revgoals" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("revgoals", Tacentries.TyNil), 
           (fun ist -> 
# 975 "extratactics.mlg"
                      Proofview.revgoals 
           )))]


# 978 "extratactics.mlg"
 

type cmp =
  | Eq
  | Lt | Le
  | Gt | Ge

type 'i test =
  | Test of cmp * 'i * 'i

let pr_cmp = function
  | Eq -> Pp.str"="
  | Lt -> Pp.str"<"
  | Le -> Pp.str"<="
  | Gt -> Pp.str">"
  | Ge -> Pp.str">="

let pr_cmp' _prc _prlc _prt = pr_cmp

let pr_test_gen f (Test(c,x,y)) =
  Pp.(f x ++ pr_cmp c ++ f y)

let pr_test = pr_test_gen (Pputils.pr_or_var Pp.int)

let pr_test' _prc _prlc _prt = pr_test

let pr_itest = pr_test_gen Pp.int

let pr_itest' _prc _prlc _prt = pr_itest



let (wit_comparison, comparison) = Tacentries.argument_extend ~name:"comparison" 
                                   {
                                   Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                            [(Extend.Rule
                                                              (Extend.Next 
                                                               (Extend.Stop,
                                                               (Extend.Atoken (CLexer.terminal ">="))),
                                                              (fun _ loc -> 
# 1015 "extratactics.mlg"
                Ge 
                                                                    )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Atoken (CLexer.terminal ">"))),
                                                             (fun _ loc -> 
# 1014 "extratactics.mlg"
                Gt 
                                                                    )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Atoken (CLexer.terminal "<="))),
                                                             (fun _ loc -> 
# 1013 "extratactics.mlg"
                Le 
                                                                    )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Atoken (CLexer.terminal "<"))),
                                                             (fun _ loc -> 
# 1012 "extratactics.mlg"
                Lt 
                                                                    )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Atoken (CLexer.terminal "="))),
                                                             (fun _ loc -> 
# 1011 "extratactics.mlg"
                Eq 
                                                                    )))]);
                                   Tacentries.arg_tag = None;
                                   Tacentries.arg_intern = Tacentries.ArgInternFun (fun ist v -> (ist, v));
                                   Tacentries.arg_subst = Tacentries.ArgSubstFun (fun s v -> v);
                                   Tacentries.arg_interp = Tacentries.ArgInterpRet;
                                   Tacentries.arg_printer = ((fun env sigma -> 
                                                            
# 1010 "extratactics.mlg"
                                        pr_cmp' 
                                                            ), (fun env sigma -> 
                                                            
# 1010 "extratactics.mlg"
                                        pr_cmp' 
                                                            ), (fun env sigma -> 
                                                            
# 1010 "extratactics.mlg"
                                        pr_cmp' 
                                                            ));
                                   }
let _ = (wit_comparison, comparison)


# 1018 "extratactics.mlg"
 

let interp_test ist gls = function
  | Test (c,x,y) ->
      project gls ,
      Test(c,Tacinterp.interp_int_or_var ist x,Tacinterp.interp_int_or_var ist y)



let (wit_test, test) = Tacentries.argument_extend ~name:"test" {
                                                               Tacentries.arg_parsing = 
                                                               Vernacextend.Arg_rules (
                                                               [(Extend.Rule
                                                                 (Extend.Next 
                                                                  (Extend.Next 
                                                                  (Extend.Next 
                                                                  (Extend.Stop,
                                                                  (Extend.Aentry int_or_var)),
                                                                  (Extend.Aentry comparison)),
                                                                  (Extend.Aentry int_or_var)),
                                                                 (fun y c x
                                                                 loc -> 
                                                                 
# 1032 "extratactics.mlg"
                                                     Test(c,x,y) 
                                                                 )))]);
                                                               Tacentries.arg_tag = 
                                                               None;
                                                               Tacentries.arg_intern = 
                                                               Tacentries.ArgInternFun (fun ist v -> (ist, v));
                                                               Tacentries.arg_subst = 
                                                               Tacentries.ArgSubstFun (fun s v -> v);
                                                               Tacentries.arg_interp = 
                                                               Tacentries.ArgInterpLegacy (
                                                               
# 1029 "extratactics.mlg"
                   interp_test 
                                                               );
                                                               Tacentries.arg_printer = 
                                                               ((fun env sigma -> 
                                                               
# 1030 "extratactics.mlg"
                   pr_test' 
                                                               ), (fun env sigma -> 
                                                               
# 1031 "extratactics.mlg"
                    pr_test' 
                                                               ), (fun env sigma -> 
                                                               
# 1028 "extratactics.mlg"
               pr_itest' 
                                                               ));
                                                               }
let _ = (wit_test, test)


# 1035 "extratactics.mlg"
 

let interp_cmp = function
  | Eq -> Int.equal
  | Lt -> ((<):int->int->bool)
  | Le -> ((<=):int->int->bool)
  | Gt -> ((>):int->int->bool)
  | Ge -> ((>=):int->int->bool)

let run_test = function
  | Test(c,x,y) -> interp_cmp c x y

let guard tst =
  if run_test tst then
    Proofview.tclUNIT ()
  else
    let msg = Pp.(str"Condition not satisfied:"++ws 1++(pr_itest tst)) in
    Tacticals.New.tclZEROMSG msg



let () = Tacentries.tactic_extend __coq_plugin_name "guard" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("guard", Tacentries.TyArg (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_test), 
                                                         Tacentries.TyNil)), 
           (fun tst ist -> 
# 1057 "extratactics.mlg"
                             guard tst 
           )))]


# 1060 "extratactics.mlg"
 

let decompose l c =
  Proofview.Goal.enter begin fun gl ->
    let sigma = Tacmach.New.project gl in
    let to_ind c =
      if isInd sigma c then fst (destInd sigma c)
      else user_err Pp.(str "not an inductive type")
    in
    let l = List.map to_ind l in
    Elim.h_decompose l c
  end



let () = Tacentries.tactic_extend __coq_plugin_name "decompose" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("decompose", Tacentries.TyIdent ("[", 
                                                             Tacentries.TyArg (
                                                             Extend.TUlist1 (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_constr)), 
                                                             Tacentries.TyIdent ("]", 
                                                             Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                             Tacentries.TyNil))))), 
           (fun l c ist -> 
# 1076 "extratactics.mlg"
                                                           decompose l c 
           )))]

let () = Vernacextend.vernac_extend ~command:"Declare_keys" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Declare", 
                                     Vernacextend.TyTerminal ("Equivalent", 
                                     Vernacextend.TyTerminal ("Keys", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                     Vernacextend.TyNil))))), (let coqpp_body c
                                                              c'
                                                              () ~st = 
                                                              let () = 
                                                              
# 1082 "extratactics.mlg"
                                                             
  let get_key c =
    let env = Global.env () in
    let evd = Evd.from_env env in
    let (evd, c) = Constrintern.interp_open_constr env evd c in
    let kind c = EConstr.kind evd c in
    Keys.constr_key kind c
  in
  let k1 = get_key c in
  let k2 = get_key c' in
    match k1, k2 with
    | Some k1, Some k2 -> Keys.declare_equiv_keys k1 k2
    | _ -> () 
                                                               in st in fun c
                                                              c' ~atts ~st
                                                              -> coqpp_body c
                                                              c'
                                                              (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"Print_keys" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Print", 
                                     Vernacextend.TyTerminal ("Equivalent", 
                                     Vernacextend.TyTerminal ("Keys", 
                                     Vernacextend.TyNil))), (let coqpp_body () ~st = 
                                                            let () = 
                                                            
# 1098 "extratactics.mlg"
                                       Feedback.msg_notice (Keys.pr_keys Printer.pr_global) 
                                                             in st in fun ~atts
                                                            ~st
                                                            -> coqpp_body (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"OptimizeProof"  ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Optimize", 
                                     Vernacextend.TyTerminal ("Proof", 
                                     Vernacextend.TyNil)), (let coqpp_body () ~st = 
                                                           let proof = (
                                                           
# 1104 "extratactics.mlg"
    fun ~pstate -> Option.map Proof_global.compact_the_proof pstate 
                                                           ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun ~atts
                                                           ~st
                                                           -> coqpp_body (Attributes.unsupported_attributes atts) ~st), Some 
         
# 1103 "extratactics.mlg"
                                         classify_as_proofstep 
         ));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Optimize", 
                                    Vernacextend.TyTerminal ("Heap", 
                                    Vernacextend.TyNil)), (let coqpp_body () ~st = 
                                                          let () = 
# 1106 "extratactics.mlg"
    Gc.compact () 
                                                           in st in fun ~atts
                                                          ~st
                                                          -> coqpp_body (Attributes.unsupported_attributes atts) ~st), Some 
         
# 1105 "extratactics.mlg"
                             classify_as_proofstep 
         ))]


# 1111 "extratactics.mlg"
 

let tclOPTIMIZE_HEAP =
  Proofview.tclLIFT (Proofview.NonLogical.make (fun () -> Gc.compact ()))



let () = Tacentries.tactic_extend __coq_plugin_name "optimize_heap" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("optimize_heap", Tacentries.TyNil), 
           (fun ist -> 
# 1119 "extratactics.mlg"
                           tclOPTIMIZE_HEAP 
           )))]

