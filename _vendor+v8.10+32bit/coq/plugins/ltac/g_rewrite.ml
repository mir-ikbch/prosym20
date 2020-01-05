
# 13 "g_rewrite.mlg"
 

open Names
open Locus
open Constrexpr
open Glob_term
open Genintern
open Geninterp
open Extraargs
open Tacmach
open Rewrite
open Stdarg
open Tactypes
open Pcoq.Prim
open Pcoq.Constr
open Pvernac.Vernac_
open Pltac
open Vernacextend

let wit_hyp = wit_var



let __coq_plugin_name = "ltac_plugin"
let _ = Mltop.add_known_module __coq_plugin_name

# 38 "g_rewrite.mlg"
 

type constr_expr_with_bindings = constr_expr with_bindings
type glob_constr_with_bindings = glob_constr_and_expr with_bindings
type glob_constr_with_bindings_sign = interp_sign * glob_constr_and_expr with_bindings

let pr_glob_constr_with_bindings_sign env sigma _ _ _ (ge : glob_constr_with_bindings_sign) =
  Printer.pr_glob_constr_env env (fst (fst (snd ge)))
let pr_glob_constr_with_bindings env sigma _ _ _ (ge : glob_constr_with_bindings) =
  Printer.pr_glob_constr_env env (fst (fst ge))
let pr_constr_expr_with_bindings env sigma prc _ _ (ge : constr_expr_with_bindings) = prc env sigma (fst ge)
let interp_glob_constr_with_bindings ist gl c = Tacmach.project gl , (ist, c)
let glob_glob_constr_with_bindings ist l = Tacintern.intern_constr_with_bindings ist l
let subst_glob_constr_with_bindings s c =
  Tacsubst.subst_glob_with_bindings s c



let (wit_glob_constr_with_bindings, glob_constr_with_bindings) = Tacentries.argument_extend ~name:"glob_constr_with_bindings" 
                                                                 {
                                                                 Tacentries.arg_parsing = 
                                                                 Vernacextend.Arg_alias (constr_with_bindings);
                                                                 Tacentries.arg_tag = 
                                                                 None;
                                                                 Tacentries.arg_intern = 
                                                                 Tacentries.ArgInternFun ((fun f ist v -> (ist, f ist v)) (
                                                                 
# 60 "g_rewrite.mlg"
                    glob_glob_constr_with_bindings 
                                                                 ));
                                                                 Tacentries.arg_subst = 
                                                                 Tacentries.ArgSubstFun (
                                                                 
# 61 "g_rewrite.mlg"
                     subst_glob_constr_with_bindings 
                                                                 );
                                                                 Tacentries.arg_interp = 
                                                                 Tacentries.ArgInterpLegacy (
                                                                 
# 59 "g_rewrite.mlg"
                     interp_glob_constr_with_bindings 
                                                                 );
                                                                 Tacentries.arg_printer = 
                                                                 ((fun env sigma -> 
                                                                 
# 63 "g_rewrite.mlg"
                     pr_constr_expr_with_bindings env sigma 
                                                                 ), (fun env sigma -> 
                                                                 
# 64 "g_rewrite.mlg"
                      pr_glob_constr_with_bindings env sigma 
                                                                 ), (fun env sigma -> 
                                                                 
# 57 "g_rewrite.mlg"
                 pr_glob_constr_with_bindings_sign env sigma 
                                                                 ));
                                                                 }
let _ = (wit_glob_constr_with_bindings, glob_constr_with_bindings)


# 69 "g_rewrite.mlg"
 

type raw_strategy = (constr_expr, Tacexpr.raw_red_expr) strategy_ast
type glob_strategy = (glob_constr_and_expr, Tacexpr.raw_red_expr) strategy_ast

let interp_strategy ist gl s =
  let sigma = project gl in
    sigma, strategy_of_ast s
let glob_strategy ist s = map_strategy (Tacintern.intern_constr ist) (fun c -> c) s
let subst_strategy s str = str

let pr_strategy _ _ _ (s : strategy) = Pp.str "<strategy>"
let pr_raw_strategy env sigma prc prlc _ (s : raw_strategy) =
  let prr = Pptactic.pr_red_expr env sigma (prc, prlc, Pputils.pr_or_by_notation Libnames.pr_qualid, prc) in
  Rewrite.pr_strategy (prc env sigma) prr s
let pr_glob_strategy env sigma prc prlc _ (s : glob_strategy) =
  let prr = Pptactic.pr_red_expr env sigma
    (Ppconstr.pr_constr_expr,
    Ppconstr.pr_lconstr_expr,
    Pputils.pr_or_by_notation Libnames.pr_qualid,
    Ppconstr.pr_constr_expr)
  in
  Rewrite.pr_strategy (prc env sigma) prr s



let (wit_rewstrategy, rewstrategy) = Tacentries.argument_extend ~name:"rewstrategy" 
                                     {
                                     Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                              [(Extend.Rule
                                                                (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Stop,
                                                                 (Extend.Atoken (CLexer.terminal "fold"))),
                                                                 (Extend.Aentry constr)),
                                                                (fun c _
                                                                loc -> 
                                                                
# 127 "g_rewrite.mlg"
                              StratFold c 
                                                                )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "eval"))),
                                                                (Extend.Aentry red_expr)),
                                                               (fun r _
                                                               loc -> 
                                                               
# 126 "g_rewrite.mlg"
                                StratEval r 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "terms"))),
                                                                (Extend.Alist0 (Extend.Aentry constr))),
                                                               (fun h _
                                                               loc -> 
                                                               
# 125 "g_rewrite.mlg"
                                    StratTerms h 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "hints"))),
                                                                (Extend.Aentry preident)),
                                                               (fun h _
                                                               loc -> 
                                                               
# 124 "g_rewrite.mlg"
                                 StratHints (false, h) 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "old_hints"))),
                                                                (Extend.Aentry preident)),
                                                               (fun h _
                                                               loc -> 
                                                               
# 123 "g_rewrite.mlg"
                                     StratHints (true, h) 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "choice"))),
                                                                Extend.Aself),
                                                                Extend.Aself),
                                                               (fun h' h _
                                                               loc -> 
                                                               
# 122 "g_rewrite.mlg"
                                                     StratBinary (Choice, h, h') 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "("))),
                                                                Extend.Aself),
                                                                (Extend.Atoken (CLexer.terminal ")"))),
                                                               (fun _ h _
                                                               loc -> 
                                                               
# 121 "g_rewrite.mlg"
                                    h 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                Extend.Aself),
                                                                (Extend.Atoken (CLexer.terminal ";"))),
                                                                Extend.Aself),
                                                               (fun h' _ h
                                                               loc -> 
                                                               
# 120 "g_rewrite.mlg"
                                                StratBinary (Compose, h, h') 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "repeat"))),
                                                                Extend.Aself),
                                                               (fun h _
                                                               loc -> 
                                                               
# 119 "g_rewrite.mlg"
                                     StratUnary (Repeat, h) 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "any"))),
                                                                Extend.Aself),
                                                               (fun h _
                                                               loc -> 
                                                               
# 118 "g_rewrite.mlg"
                                  StratUnary (Any, h) 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "try"))),
                                                                Extend.Aself),
                                                               (fun h _
                                                               loc -> 
                                                               
# 117 "g_rewrite.mlg"
                                  StratUnary (Try, h) 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "progress"))),
                                                                Extend.Aself),
                                                               (fun h _
                                                               loc -> 
                                                               
# 116 "g_rewrite.mlg"
                                       StratUnary (Progress, h) 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "refl"))),
                                                               (fun _ loc ->
                                                               
# 115 "g_rewrite.mlg"
                    StratRefl 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "fail"))),
                                                               (fun _ loc ->
                                                               
# 114 "g_rewrite.mlg"
                    StratFail 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "id"))),
                                                               (fun _ loc ->
                                                               
# 113 "g_rewrite.mlg"
                  StratId 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "topdown"))),
                                                                Extend.Aself),
                                                               (fun h _
                                                               loc -> 
                                                               
# 112 "g_rewrite.mlg"
                                      StratUnary(Topdown, h) 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "bottomup"))),
                                                                Extend.Aself),
                                                               (fun h _
                                                               loc -> 
                                                               
# 111 "g_rewrite.mlg"
                                       StratUnary(Bottomup, h) 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "outermost"))),
                                                                Extend.Aself),
                                                               (fun h _
                                                               loc -> 
                                                               
# 110 "g_rewrite.mlg"
                                        StratUnary(Outermost, h) 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "innermost"))),
                                                                Extend.Aself),
                                                               (fun h _
                                                               loc -> 
                                                               
# 109 "g_rewrite.mlg"
                                        StratUnary(Innermost, h) 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "subterm"))),
                                                                Extend.Aself),
                                                               (fun h _
                                                               loc -> 
                                                               
# 108 "g_rewrite.mlg"
                                      StratUnary (Subterm, h) 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "subterms"))),
                                                                Extend.Aself),
                                                               (fun h _
                                                               loc -> 
                                                               
# 107 "g_rewrite.mlg"
                                       StratUnary (Subterms, h) 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "<-"))),
                                                                (Extend.Aentry constr)),
                                                               (fun c _
                                                               loc -> 
                                                               
# 106 "g_rewrite.mlg"
                            StratConstr (c, false) 
                                                               )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Aentry glob)),
                                                               (fun c loc ->
                                                               
# 105 "g_rewrite.mlg"
                     StratConstr (c, true) 
                                                               )))]);
                                     Tacentries.arg_tag = None;
                                     Tacentries.arg_intern = Tacentries.ArgInternFun ((fun f ist v -> (ist, f ist v)) (
                                                             
# 99 "g_rewrite.mlg"
                    glob_strategy 
                                                             ));
                                     Tacentries.arg_subst = Tacentries.ArgSubstFun (
                                                            
# 100 "g_rewrite.mlg"
                     subst_strategy 
                                                            );
                                     Tacentries.arg_interp = Tacentries.ArgInterpLegacy (
                                                             
# 98 "g_rewrite.mlg"
                     interp_strategy 
                                                             );
                                     Tacentries.arg_printer = ((fun env sigma -> 
                                                              
# 102 "g_rewrite.mlg"
                     pr_raw_strategy env sigma 
                                                              ), (fun env sigma -> 
                                                              
# 103 "g_rewrite.mlg"
                      pr_glob_strategy env sigma 
                                                              ), (fun env sigma -> 
                                                              
# 96 "g_rewrite.mlg"
                 pr_strategy 
                                                              ));
                                     }
let _ = (wit_rewstrategy, rewstrategy)


# 132 "g_rewrite.mlg"
 

let db_strat db = StratUnary (Topdown, StratHints (false, db))
let cl_rewrite_clause_db db = cl_rewrite_clause_strat (strategy_of_ast (db_strat db))



let () = Tacentries.tactic_extend __coq_plugin_name "rewrite_strat" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("rewrite_strat", Tacentries.TyArg (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_rewstrategy), 
                                                                 Tacentries.TyIdent ("in", 
                                                                 Tacentries.TyArg (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_hyp), 
                                                                 Tacentries.TyNil)))), 
           (fun s id ist -> 
# 140 "g_rewrite.mlg"
                                                       cl_rewrite_clause_strat s (Some id) 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("rewrite_strat", Tacentries.TyArg (
                                                                Extend.TUentry (Genarg.get_arg_tag wit_rewstrategy), 
                                                                Tacentries.TyNil)), 
          (fun s ist -> 
# 141 "g_rewrite.mlg"
                                          cl_rewrite_clause_strat s None 
          )));
         (Tacentries.TyML (Tacentries.TyIdent ("rewrite_db", Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_preident), 
                                                             Tacentries.TyIdent ("in", 
                                                             Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_hyp), 
                                                             Tacentries.TyNil)))), 
          (fun db id ist -> 
# 142 "g_rewrite.mlg"
                                                  cl_rewrite_clause_db db (Some id) 
          )));
         (Tacentries.TyML (Tacentries.TyIdent ("rewrite_db", Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_preident), 
                                                             Tacentries.TyNil)), 
          (fun db ist -> 
# 143 "g_rewrite.mlg"
                                     cl_rewrite_clause_db db None 
          )))]


# 146 "g_rewrite.mlg"
 

let clsubstitute o c =
  Proofview.Goal.enter begin fun gl ->
  let is_tac id = match DAst.get (fst (fst (snd c))) with GVar id' when Id.equal id' id -> true | _ -> false in
  let hyps = Tacmach.New.pf_ids_of_hyps gl in
    Tacticals.New.tclMAP
      (fun cl ->
        match cl with
          | Some id when is_tac id -> Tacticals.New.tclIDTAC
          | _ -> cl_rewrite_clause c o AllOccurrences cl)
      (None :: List.map (fun id -> Some id) hyps)
  end



let () = Tacentries.tactic_extend __coq_plugin_name "substitute" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("substitute", Tacentries.TyArg (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_orient), 
                                                              Tacentries.TyArg (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_glob_constr_with_bindings), 
                                                              Tacentries.TyNil))), 
           (fun o c ist -> 
# 163 "g_rewrite.mlg"
                                                               clsubstitute o c 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "setoid_rewrite" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("setoid_rewrite", Tacentries.TyArg (
                                                                  Extend.TUentry (Genarg.get_arg_tag wit_orient), 
                                                                  Tacentries.TyArg (
                                                                  Extend.TUentry (Genarg.get_arg_tag wit_glob_constr_with_bindings), 
                                                                  Tacentries.TyNil))), 
           (fun o c ist -> 
# 171 "g_rewrite.mlg"
        cl_rewrite_clause c o AllOccurrences None 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("setoid_rewrite", Tacentries.TyArg (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_orient), 
                                                                 Tacentries.TyArg (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_glob_constr_with_bindings), 
                                                                 Tacentries.TyIdent ("in", 
                                                                 Tacentries.TyArg (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_hyp), 
                                                                 Tacentries.TyNil))))), 
          (fun o c id ist -> 
# 173 "g_rewrite.mlg"
        cl_rewrite_clause c o AllOccurrences (Some id) 
          )));
         (Tacentries.TyML (Tacentries.TyIdent ("setoid_rewrite", Tacentries.TyArg (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_orient), 
                                                                 Tacentries.TyArg (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_glob_constr_with_bindings), 
                                                                 Tacentries.TyIdent ("at", 
                                                                 Tacentries.TyArg (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_occurrences), 
                                                                 Tacentries.TyNil))))), 
          (fun o c occ ist -> 
# 175 "g_rewrite.mlg"
        cl_rewrite_clause c o (occurrences_of occ) None 
          )));
         (Tacentries.TyML (Tacentries.TyIdent ("setoid_rewrite", Tacentries.TyArg (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_orient), 
                                                                 Tacentries.TyArg (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_glob_constr_with_bindings), 
                                                                 Tacentries.TyIdent ("at", 
                                                                 Tacentries.TyArg (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_occurrences), 
                                                                 Tacentries.TyIdent ("in", 
                                                                 Tacentries.TyArg (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_hyp), 
                                                                 Tacentries.TyNil))))))), 
          (fun o c occ id ist -> 
# 177 "g_rewrite.mlg"
        cl_rewrite_clause c o (occurrences_of occ) (Some id) 
          )));
         (Tacentries.TyML (Tacentries.TyIdent ("setoid_rewrite", Tacentries.TyArg (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_orient), 
                                                                 Tacentries.TyArg (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_glob_constr_with_bindings), 
                                                                 Tacentries.TyIdent ("in", 
                                                                 Tacentries.TyArg (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_hyp), 
                                                                 Tacentries.TyIdent ("at", 
                                                                 Tacentries.TyArg (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_occurrences), 
                                                                 Tacentries.TyNil))))))), 
          (fun o c id occ ist -> 
# 179 "g_rewrite.mlg"
        cl_rewrite_clause c o (occurrences_of occ) (Some id) 
          )))]

let () = Vernacextend.vernac_extend ~command:"AddRelation" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Add", 
                                     Vernacextend.TyTerminal ("Relation", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                     Vernacextend.TyTerminal ("reflexivity", 
                                     Vernacextend.TyTerminal ("proved", 
                                     Vernacextend.TyTerminal ("by", Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("symmetry", 
                                                                    Vernacextend.TyTerminal ("proved", 
                                                                    Vernacextend.TyTerminal ("by", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("as", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                    Vernacextend.TyNil)))))))))))))), 
         (let coqpp_body a aeq lemma1 lemma2 n
         atts ~st = let proof = (
# 185 "g_rewrite.mlg"
        declare_relation atts a aeq n (Some lemma1) (Some lemma2) None 
                    ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun a
         aeq lemma1 lemma2 n ~atts ~st -> coqpp_body a aeq lemma1 lemma2 n
         (Attributes.parse rewrite_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Add", Vernacextend.TyTerminal ("Relation", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("reflexivity", 
                                                                    Vernacextend.TyTerminal ("proved", 
                                                                    Vernacextend.TyTerminal ("by", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("as", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                    Vernacextend.TyNil)))))))))), 
         (let coqpp_body a aeq lemma1 n
         atts ~st = let proof = (
# 189 "g_rewrite.mlg"
        declare_relation atts a aeq n (Some lemma1) None None 
                    ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun a
         aeq lemma1 n ~atts ~st -> coqpp_body a aeq lemma1 n
         (Attributes.parse rewrite_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Add", Vernacextend.TyTerminal ("Relation", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("as", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                    Vernacextend.TyNil)))))), 
         (let coqpp_body a aeq n
         atts ~st = let proof = (
# 191 "g_rewrite.mlg"
        declare_relation atts a aeq n None None None 
                    ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun a
         aeq n ~atts ~st -> coqpp_body a aeq n
         (Attributes.parse rewrite_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"AddRelation2" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Add", 
                                     Vernacextend.TyTerminal ("Relation", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                     Vernacextend.TyTerminal ("symmetry", 
                                     Vernacextend.TyTerminal ("proved", 
                                     Vernacextend.TyTerminal ("by", Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("as", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                    Vernacextend.TyNil)))))))))), 
         (let coqpp_body a aeq lemma2 n
         atts ~st = let proof = (
# 197 "g_rewrite.mlg"
        declare_relation atts a aeq n None (Some lemma2) None 
                    ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun a
         aeq lemma2 n ~atts ~st -> coqpp_body a aeq lemma2 n
         (Attributes.parse rewrite_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Add", Vernacextend.TyTerminal ("Relation", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("symmetry", 
                                                                    Vernacextend.TyTerminal ("proved", 
                                                                    Vernacextend.TyTerminal ("by", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("transitivity", 
                                                                    Vernacextend.TyTerminal ("proved", 
                                                                    Vernacextend.TyTerminal ("by", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("as", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                    Vernacextend.TyNil)))))))))))))), 
         (let coqpp_body a aeq lemma2 lemma3 n
         atts ~st = let proof = (
# 199 "g_rewrite.mlg"
        declare_relation atts a aeq n None (Some lemma2) (Some lemma3) 
                    ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun a
         aeq lemma2 lemma3 n ~atts ~st -> coqpp_body a aeq lemma2 lemma3 n
         (Attributes.parse rewrite_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"AddRelation3" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Add", 
                                     Vernacextend.TyTerminal ("Relation", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                     Vernacextend.TyTerminal ("reflexivity", 
                                     Vernacextend.TyTerminal ("proved", 
                                     Vernacextend.TyTerminal ("by", Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("transitivity", 
                                                                    Vernacextend.TyTerminal ("proved", 
                                                                    Vernacextend.TyTerminal ("by", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("as", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                    Vernacextend.TyNil)))))))))))))), 
         (let coqpp_body a aeq lemma1 lemma3 n
         atts ~st = let proof = (
# 205 "g_rewrite.mlg"
        declare_relation atts a aeq n (Some lemma1) None (Some lemma3) 
                    ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun a
         aeq lemma1 lemma3 n ~atts ~st -> coqpp_body a aeq lemma1 lemma3 n
         (Attributes.parse rewrite_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Add", Vernacextend.TyTerminal ("Relation", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("reflexivity", 
                                                                    Vernacextend.TyTerminal ("proved", 
                                                                    Vernacextend.TyTerminal ("by", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("symmetry", 
                                                                    Vernacextend.TyTerminal ("proved", 
                                                                    Vernacextend.TyTerminal ("by", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("transitivity", 
                                                                    Vernacextend.TyTerminal ("proved", 
                                                                    Vernacextend.TyTerminal ("by", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("as", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                    Vernacextend.TyNil)))))))))))))))))), 
         (let coqpp_body a aeq lemma1 lemma2 lemma3 n
         atts ~st = let proof = (
# 209 "g_rewrite.mlg"
        declare_relation atts a aeq n (Some lemma1) (Some lemma2) (Some lemma3) 
                    ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun a
         aeq lemma1 lemma2 lemma3 n ~atts ~st -> coqpp_body a aeq lemma1
         lemma2 lemma3 n (Attributes.parse rewrite_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Add", Vernacextend.TyTerminal ("Relation", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("transitivity", 
                                                                    Vernacextend.TyTerminal ("proved", 
                                                                    Vernacextend.TyTerminal ("by", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("as", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                    Vernacextend.TyNil)))))))))), 
         (let coqpp_body a aeq lemma3 n
         atts ~st = let proof = (
# 212 "g_rewrite.mlg"
        declare_relation atts a aeq n None None (Some lemma3) 
                    ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun a
         aeq lemma3 n ~atts ~st -> coqpp_body a aeq lemma3 n
         (Attributes.parse rewrite_attributes atts) ~st), None))]


# 215 "g_rewrite.mlg"
 

type binders_argtype = local_binder_expr list

let wit_binders =
 (Genarg.create_arg "binders" : binders_argtype Genarg.uniform_genarg_type)

let binders = Pcoq.create_generic_entry Pcoq.utactic "binders" (Genarg.rawwit wit_binders)

let () =
  let raw_printer env sigma _ _ _ l = Pp.pr_non_empty_arg (Ppconstr.pr_binders env sigma) l in
  Pptactic.declare_extra_vernac_genarg_pprule wit_binders raw_printer



let _ = let () =
        Pcoq.grammar_extend binders None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Aentry Pcoq.Constr.binders)),
                 (fun b loc -> 
# 233 "g_rewrite.mlg"
                                     b 
                               ))])])
        in ()

let () = Vernacextend.vernac_extend ~command:"AddParametricRelation" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Add", 
                                     Vernacextend.TyTerminal ("Parametric", 
                                     Vernacextend.TyTerminal ("Relation", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_binders), 
                                     Vernacextend.TyTerminal (":", Vernacextend.TyNonTerminal (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                   Vernacextend.TyNonTerminal (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                   Vernacextend.TyTerminal ("reflexivity", 
                                                                   Vernacextend.TyTerminal ("proved", 
                                                                   Vernacextend.TyTerminal ("by", 
                                                                   Vernacextend.TyNonTerminal (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                   Vernacextend.TyTerminal ("symmetry", 
                                                                   Vernacextend.TyTerminal ("proved", 
                                                                   Vernacextend.TyTerminal ("by", 
                                                                   Vernacextend.TyNonTerminal (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                   Vernacextend.TyTerminal ("as", 
                                                                   Vernacextend.TyNonTerminal (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                   Vernacextend.TyNil))))))))))))))))), 
         (let coqpp_body b a aeq lemma1 lemma2 n
         atts ~st = let proof = (
# 240 "g_rewrite.mlg"
        declare_relation atts ~binders:b a aeq n (Some lemma1) (Some lemma2) None 
                    ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun b
         a aeq lemma1 lemma2 n ~atts ~st -> coqpp_body b a aeq lemma1 lemma2
         n (Attributes.parse rewrite_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Add", Vernacextend.TyTerminal ("Parametric", 
                                                                    Vernacextend.TyTerminal ("Relation", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_binders), 
                                                                    Vernacextend.TyTerminal (":", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("reflexivity", 
                                                                    Vernacextend.TyTerminal ("proved", 
                                                                    Vernacextend.TyTerminal ("by", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("as", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                    Vernacextend.TyNil))))))))))))), 
         (let coqpp_body b a aeq lemma1 n
         atts ~st = let proof = (
# 244 "g_rewrite.mlg"
        declare_relation atts ~binders:b a aeq n (Some lemma1) None None 
                    ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun b
         a aeq lemma1 n ~atts ~st -> coqpp_body b a aeq lemma1 n
         (Attributes.parse rewrite_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Add", Vernacextend.TyTerminal ("Parametric", 
                                                                    Vernacextend.TyTerminal ("Relation", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_binders), 
                                                                    Vernacextend.TyTerminal (":", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("as", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                    Vernacextend.TyNil))))))))), 
         (let coqpp_body b a aeq n
         atts ~st = let proof = (
# 246 "g_rewrite.mlg"
        declare_relation atts ~binders:b a aeq n None None None 
                    ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun b
         a aeq n ~atts ~st -> coqpp_body b a aeq n
         (Attributes.parse rewrite_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"AddParametricRelation2" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Add", 
                                     Vernacextend.TyTerminal ("Parametric", 
                                     Vernacextend.TyTerminal ("Relation", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_binders), 
                                     Vernacextend.TyTerminal (":", Vernacextend.TyNonTerminal (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                   Vernacextend.TyNonTerminal (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                   Vernacextend.TyTerminal ("symmetry", 
                                                                   Vernacextend.TyTerminal ("proved", 
                                                                   Vernacextend.TyTerminal ("by", 
                                                                   Vernacextend.TyNonTerminal (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                   Vernacextend.TyTerminal ("as", 
                                                                   Vernacextend.TyNonTerminal (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                   Vernacextend.TyNil))))))))))))), 
         (let coqpp_body b a aeq lemma2 n
         atts ~st = let proof = (
# 252 "g_rewrite.mlg"
        declare_relation atts ~binders:b a aeq n None (Some lemma2) None 
                    ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun b
         a aeq lemma2 n ~atts ~st -> coqpp_body b a aeq lemma2 n
         (Attributes.parse rewrite_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Add", Vernacextend.TyTerminal ("Parametric", 
                                                                    Vernacextend.TyTerminal ("Relation", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_binders), 
                                                                    Vernacextend.TyTerminal (":", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("symmetry", 
                                                                    Vernacextend.TyTerminal ("proved", 
                                                                    Vernacextend.TyTerminal ("by", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("transitivity", 
                                                                    Vernacextend.TyTerminal ("proved", 
                                                                    Vernacextend.TyTerminal ("by", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("as", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                    Vernacextend.TyNil))))))))))))))))), 
         (let coqpp_body b a aeq lemma2 lemma3 n
         atts ~st = let proof = (
# 254 "g_rewrite.mlg"
        declare_relation atts ~binders:b a aeq n None (Some lemma2) (Some lemma3) 
                    ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun b
         a aeq lemma2 lemma3 n ~atts ~st -> coqpp_body b a aeq lemma2 lemma3
         n (Attributes.parse rewrite_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"AddParametricRelation3" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Add", 
                                     Vernacextend.TyTerminal ("Parametric", 
                                     Vernacextend.TyTerminal ("Relation", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_binders), 
                                     Vernacextend.TyTerminal (":", Vernacextend.TyNonTerminal (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                   Vernacextend.TyNonTerminal (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                   Vernacextend.TyTerminal ("reflexivity", 
                                                                   Vernacextend.TyTerminal ("proved", 
                                                                   Vernacextend.TyTerminal ("by", 
                                                                   Vernacextend.TyNonTerminal (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                   Vernacextend.TyTerminal ("transitivity", 
                                                                   Vernacextend.TyTerminal ("proved", 
                                                                   Vernacextend.TyTerminal ("by", 
                                                                   Vernacextend.TyNonTerminal (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                   Vernacextend.TyTerminal ("as", 
                                                                   Vernacextend.TyNonTerminal (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                   Vernacextend.TyNil))))))))))))))))), 
         (let coqpp_body b a aeq lemma1 lemma3 n
         atts ~st = let proof = (
# 260 "g_rewrite.mlg"
        declare_relation atts ~binders:b a aeq n (Some lemma1) None (Some lemma3) 
                    ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun b
         a aeq lemma1 lemma3 n ~atts ~st -> coqpp_body b a aeq lemma1 lemma3
         n (Attributes.parse rewrite_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Add", Vernacextend.TyTerminal ("Parametric", 
                                                                    Vernacextend.TyTerminal ("Relation", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_binders), 
                                                                    Vernacextend.TyTerminal (":", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("reflexivity", 
                                                                    Vernacextend.TyTerminal ("proved", 
                                                                    Vernacextend.TyTerminal ("by", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("symmetry", 
                                                                    Vernacextend.TyTerminal ("proved", 
                                                                    Vernacextend.TyTerminal ("by", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("transitivity", 
                                                                    Vernacextend.TyTerminal ("proved", 
                                                                    Vernacextend.TyTerminal ("by", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("as", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                    Vernacextend.TyNil))))))))))))))))))))), 
         (let coqpp_body b a aeq lemma1 lemma2 lemma3 n
         atts ~st = let proof = (
# 264 "g_rewrite.mlg"
        declare_relation atts ~binders:b a aeq n (Some lemma1) (Some lemma2) (Some lemma3) 
                    ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun b
         a aeq lemma1 lemma2 lemma3 n ~atts ~st -> coqpp_body b a aeq lemma1
         lemma2 lemma3 n (Attributes.parse rewrite_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Add", Vernacextend.TyTerminal ("Parametric", 
                                                                    Vernacextend.TyTerminal ("Relation", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_binders), 
                                                                    Vernacextend.TyTerminal (":", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("transitivity", 
                                                                    Vernacextend.TyTerminal ("proved", 
                                                                    Vernacextend.TyTerminal ("by", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("as", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                    Vernacextend.TyNil))))))))))))), 
         (let coqpp_body b a aeq lemma3 n
         atts ~st = let proof = (
# 267 "g_rewrite.mlg"
        declare_relation atts ~binders:b a aeq n None None (Some lemma3) 
                    ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun b
         a aeq lemma3 n ~atts ~st -> coqpp_body b a aeq lemma3 n
         (Attributes.parse rewrite_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"AddSetoid1" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Add", 
                                     Vernacextend.TyTerminal ("Setoid", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                     Vernacextend.TyTerminal ("as", Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                    Vernacextend.TyNil))))))), 
         (let coqpp_body a aeq t n
         atts ~st = let proof = (
# 272 "g_rewrite.mlg"
      
         add_setoid atts [] a aeq t n
     
                    ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun a
         aeq t n ~atts ~st -> coqpp_body a aeq t n
         (Attributes.parse rewrite_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Add", Vernacextend.TyTerminal ("Parametric", 
                                                                    Vernacextend.TyTerminal ("Setoid", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_binders), 
                                                                    Vernacextend.TyTerminal (":", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("as", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                    Vernacextend.TyNil)))))))))), 
         (let coqpp_body binders a aeq t n
         atts ~st = let proof = (
# 276 "g_rewrite.mlg"
      
         add_setoid atts binders a aeq t n
     
                    ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun binders
         a aeq t n ~atts ~st -> coqpp_body binders a aeq t n
         (Attributes.parse rewrite_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Add", Vernacextend.TyTerminal ("Morphism", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal (":", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                    Vernacextend.TyNil))))), 
         (let coqpp_body m n
         atts ~st = let proof = (
# 282 "g_rewrite.mlg"
        
          add_morphism_infer atts m n
       
                    ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun m
         n ~atts ~st -> coqpp_body m n
         (Attributes.parse rewrite_attributes atts) ~st), Some (fun m n -> 
# 281 "g_rewrite.mlg"
         VtUnknown, VtNow 
                                                               )));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Add", Vernacextend.TyTerminal ("Morphism", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("with", 
                                                                    Vernacextend.TyTerminal ("signature", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_lconstr), 
                                                                    Vernacextend.TyTerminal ("as", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                    Vernacextend.TyNil)))))))), 
         (let coqpp_body m s n
         atts ~st = let proof = (
# 287 "g_rewrite.mlg"
        
          add_morphism atts [] m s n
       
                    ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun m
         s n ~atts ~st -> coqpp_body m s n
         (Attributes.parse rewrite_attributes atts) ~st), Some (fun m s n
                                                               -> 
# 286 "g_rewrite.mlg"
         VtStartProof(GuaranteesOpacity,[n]), VtLater 
                                                               )));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Add", Vernacextend.TyTerminal ("Parametric", 
                                                                    Vernacextend.TyTerminal ("Morphism", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_binders), 
                                                                    Vernacextend.TyTerminal (":", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                    Vernacextend.TyTerminal ("with", 
                                                                    Vernacextend.TyTerminal ("signature", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_lconstr), 
                                                                    Vernacextend.TyTerminal ("as", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                    Vernacextend.TyNil))))))))))), 
         (let coqpp_body binders m s n
         atts ~st = let proof = (
# 293 "g_rewrite.mlg"
        
          add_morphism atts binders m s n
       
                    ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun binders
         m s n ~atts ~st -> coqpp_body binders m s n
         (Attributes.parse rewrite_attributes atts) ~st), Some (fun binders m
                                                               s n -> 
                                                               
# 292 "g_rewrite.mlg"
         VtStartProof(GuaranteesOpacity,[n]), VtLater 
                                                               )))]

let () = Tacentries.tactic_extend __coq_plugin_name "setoid_symmetry" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("setoid_symmetry", Tacentries.TyNil), 
           (fun ist -> 
# 299 "g_rewrite.mlg"
                              setoid_symmetry 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("setoid_symmetry", Tacentries.TyIdent ("in", 
                                                                  Tacentries.TyArg (
                                                                  Extend.TUentry (Genarg.get_arg_tag wit_hyp), 
                                                                  Tacentries.TyNil))), 
          (fun n ist -> 
# 300 "g_rewrite.mlg"
                                          setoid_symmetry_in n 
          )))]

let () = Tacentries.tactic_extend __coq_plugin_name "setoid_reflexivity" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("setoid_reflexivity", 
                            Tacentries.TyNil), (fun ist -> 
# 304 "g_rewrite.mlg"
                                setoid_reflexivity 
                                               )))]

let () = Tacentries.tactic_extend __coq_plugin_name "setoid_transitivity" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("setoid_transitivity", 
                            Tacentries.TyArg (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                            Tacentries.TyNil)), (fun t ist -> 
# 308 "g_rewrite.mlg"
                                           setoid_transitivity (Some t) 
                                                )));
         (Tacentries.TyML (Tacentries.TyIdent ("setoid_etransitivity", 
                           Tacentries.TyNil), (fun ist -> 
# 309 "g_rewrite.mlg"
                                  setoid_transitivity None 
                                              )))]

let () = Vernacextend.vernac_extend ~command:"PrintRewriteHintDb" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Print", 
                                     Vernacextend.TyTerminal ("Rewrite", 
                                     Vernacextend.TyTerminal ("HintDb", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_preident), 
                                     Vernacextend.TyNil)))), (let coqpp_body s
                                                             () ~st = 
                                                             let () = 
                                                             
# 314 "g_rewrite.mlg"
    Feedback.msg_notice (Autorewrite.print_rewrite_hintdb s) 
                                                              in st in fun s
                                                             ~atts ~st
                                                             -> coqpp_body s
                                                             (Attributes.unsupported_attributes atts) ~st), None))]

