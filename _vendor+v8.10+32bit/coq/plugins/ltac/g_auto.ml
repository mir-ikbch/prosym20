
# 11 "g_auto.mlg"
 

open Pp
open Constr
open Stdarg
open Pcoq.Prim
open Pcoq.Constr
open Pltac
open Hints

let wit_hyp = wit_var



let __coq_plugin_name = "ltac_plugin"
let _ = Mltop.add_known_module __coq_plugin_name
let () = Tacentries.tactic_extend __coq_plugin_name "eassumption" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("eassumption", Tacentries.TyNil), 
           (fun ist -> 
# 31 "g_auto.mlg"
                         Eauto.e_assumption 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "eexact" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("eexact", Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                          Tacentries.TyNil)), 
           (fun c ist -> 
# 35 "g_auto.mlg"
                              Eauto.e_give_exact c 
           )))]


# 38 "g_auto.mlg"
 

let pr_hintbases _prc _prlc _prt = Pptactic.pr_hintbases



let (wit_hintbases, hintbases) = Tacentries.argument_extend ~name:"hintbases" 
                                 {
                                 Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                          [(Extend.Rule
                                                            (Extend.Stop,
                                                            (fun loc -> 
# 49 "g_auto.mlg"
           Some [] 
                                                                    )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (CLexer.terminal "with"))),
                                                            (Extend.Alist1 (Extend.Aentry preident))),
                                                           (fun l _ loc -> 
# 48 "g_auto.mlg"
                                      Some l 
                                                                    )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (CLexer.terminal "with"))),
                                                            (Extend.Atoken (CLexer.terminal "*"))),
                                                           (fun _ _ loc -> 
# 47 "g_auto.mlg"
                      None 
                                                                    )))]);
                                 Tacentries.arg_tag = Some
                                                      (Geninterp.Val.Opt 
                                                      (Geninterp.Val.List 
                                                      (Geninterp.val_tag (Genarg.topwit wit_preident))));
                                 Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.OptArg 
                                                         (Genarg.ListArg 
                                                         (wit_preident)));
                                 Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.OptArg 
                                                        (Genarg.ListArg 
                                                        (wit_preident)));
                                 Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.OptArg 
                                                         (Genarg.ListArg 
                                                         (wit_preident)));
                                 Tacentries.arg_printer = ((fun env sigma -> 
                                                          
# 46 "g_auto.mlg"
               pr_hintbases 
                                                          ), (fun env sigma -> 
                                                          
# 46 "g_auto.mlg"
               pr_hintbases 
                                                          ), (fun env sigma -> 
                                                          
# 46 "g_auto.mlg"
               pr_hintbases 
                                                          ));
                                 }
let _ = (wit_hintbases, hintbases)


# 52 "g_auto.mlg"
 

let eval_uconstrs ist cs =
  let flags = {
    Pretyping.use_typeclasses = false;
    solve_unification_constraints = true;
    fail_evar = false;
    expand_evars = true;
    program_mode = false;
    polymorphic = false;
  } in
  let map c env sigma = c env sigma in
  List.map (fun c -> map (Tacinterp.type_uconstr ~flags ist c)) cs

let pr_auto_using_raw env sigma _ _ _  = Pptactic.pr_auto_using @@ Ppconstr.pr_constr_expr env sigma
let pr_auto_using_glob env sigma _ _ _ = Pptactic.pr_auto_using (fun (c,_) ->
    Printer.pr_glob_constr_env env c)
let pr_auto_using env sigma _ _ _ = Pptactic.pr_auto_using @@
     Printer.pr_closed_glob_env env sigma



let (wit_auto_using, auto_using) = Tacentries.argument_extend ~name:"auto_using" 
                                   {
                                   Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                            [(Extend.Rule
                                                              (Extend.Stop,
                                                              (fun loc -> 
# 80 "g_auto.mlg"
           [] 
                                                                    )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Atoken (CLexer.terminal "using"))),
                                                              (Extend.Alist1sep ((Extend.Aentry uconstr), (Extend.Atoken (CLexer.terminal ","))))),
                                                             (fun l _ loc ->
                                                             
# 79 "g_auto.mlg"
                                               l 
                                                             )))]);
                                   Tacentries.arg_tag = Some
                                                        (Geninterp.Val.List 
                                                        (Geninterp.val_tag (Genarg.topwit wit_uconstr)));
                                   Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.ListArg 
                                                           (wit_uconstr));
                                   Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.ListArg 
                                                          (wit_uconstr));
                                   Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.ListArg 
                                                           (wit_uconstr));
                                   Tacentries.arg_printer = ((fun env sigma -> 
                                                            
# 77 "g_auto.mlg"
                   pr_auto_using_raw env sigma 
                                                            ), (fun env sigma -> 
                                                            
# 78 "g_auto.mlg"
                    pr_auto_using_glob env sigma 
                                                            ), (fun env sigma -> 
                                                            
# 76 "g_auto.mlg"
               pr_auto_using env sigma 
                                                            ));
                                   }
let _ = (wit_auto_using, auto_using)

let () = Tacentries.tactic_extend __coq_plugin_name "trivial" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("trivial", Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_auto_using), 
                                                           Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_hintbases), 
                                                           Tacentries.TyNil))), 
           (fun lems db ist -> 
# 87 "g_auto.mlg"
      Auto.h_trivial (eval_uconstrs ist lems) db 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "info_trivial" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("info_trivial", Tacentries.TyArg (
                                                                Extend.TUentry (Genarg.get_arg_tag wit_auto_using), 
                                                                Tacentries.TyArg (
                                                                Extend.TUentry (Genarg.get_arg_tag wit_hintbases), 
                                                                Tacentries.TyNil))), 
           (fun lems db ist -> 
# 92 "g_auto.mlg"
      Auto.h_trivial ~debug:Info (eval_uconstrs ist lems) db 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "debug_trivial" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("debug", Tacentries.TyIdent ("trivial", 
                                                         Tacentries.TyArg (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_auto_using), 
                                                         Tacentries.TyArg (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_hintbases), 
                                                         Tacentries.TyNil)))), 
           (fun lems db ist -> 
# 97 "g_auto.mlg"
      Auto.h_trivial ~debug:Debug (eval_uconstrs ist lems) db 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "auto" ~level:0 [(
                                                                    Tacentries.TyML (
                                                                    Tacentries.TyIdent ("auto", 
                                                                    Tacentries.TyArg (
                                                                    Extend.TUopt (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_int_or_var)), 
                                                                    Tacentries.TyArg (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_auto_using), 
                                                                    Tacentries.TyArg (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_hintbases), 
                                                                    Tacentries.TyNil)))), 
                                                                    (fun n
                                                                    lems db
                                                                    ist -> 
                                                                    
# 102 "g_auto.mlg"
      Auto.h_auto n (eval_uconstrs ist lems) db 
                                                                    )))]

let () = Tacentries.tactic_extend __coq_plugin_name "info_auto" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("info_auto", Tacentries.TyArg (
                                                             Extend.TUopt (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_int_or_var)), 
                                                             Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_auto_using), 
                                                             Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_hintbases), 
                                                             Tacentries.TyNil)))), 
           (fun n lems db ist -> 
# 107 "g_auto.mlg"
      Auto.h_auto ~debug:Info n (eval_uconstrs ist lems) db 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "debug_auto" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("debug", Tacentries.TyIdent ("auto", 
                                                         Tacentries.TyArg (
                                                         Extend.TUopt (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_int_or_var)), 
                                                         Tacentries.TyArg (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_auto_using), 
                                                         Tacentries.TyArg (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_hintbases), 
                                                         Tacentries.TyNil))))), 
           (fun n lems db ist -> 
# 112 "g_auto.mlg"
      Auto.h_auto ~debug:Debug n (eval_uconstrs ist lems) db 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "prolog" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("prolog", Tacentries.TyIdent ("[", 
                                                          Tacentries.TyArg (
                                                          Extend.TUlist0 (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_uconstr)), 
                                                          Tacentries.TyIdent ("]", 
                                                          Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_int_or_var), 
                                                          Tacentries.TyNil))))), 
           (fun l n ist -> 
# 119 "g_auto.mlg"
      Eauto.prolog_tac (eval_uconstrs ist l) n 
           )))]


# 122 "g_auto.mlg"
 

let make_depth n = snd (Eauto.make_dimension n None)



let () = Tacentries.tactic_extend __coq_plugin_name "eauto" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("eauto", Tacentries.TyArg (
                                                         Extend.TUopt (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_int_or_var)), 
                                                         Tacentries.TyArg (
                                                         Extend.TUopt (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_int_or_var)), 
                                                         Tacentries.TyArg (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_auto_using), 
                                                         Tacentries.TyArg (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_hintbases), 
                                                         Tacentries.TyNil))))), 
           (fun n p lems db ist -> 
# 131 "g_auto.mlg"
      Eauto.gen_eauto (Eauto.make_dimension n p) (eval_uconstrs ist lems) db 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "new_eauto" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("new", Tacentries.TyIdent ("auto", 
                                                       Tacentries.TyArg (
                                                       Extend.TUopt (
                                                       Extend.TUentry (Genarg.get_arg_tag wit_int_or_var)), 
                                                       Tacentries.TyArg (
                                                       Extend.TUentry (Genarg.get_arg_tag wit_auto_using), 
                                                       Tacentries.TyArg (
                                                       Extend.TUentry (Genarg.get_arg_tag wit_hintbases), 
                                                       Tacentries.TyNil))))), 
           (fun n lems db ist -> 
# 137 "g_auto.mlg"
      match db with
      | None -> Auto.new_full_auto (make_depth n) (eval_uconstrs ist lems)
      | Some l -> Auto.new_auto (make_depth n) (eval_uconstrs ist lems) l 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "debug_eauto" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("debug", Tacentries.TyIdent ("eauto", 
                                                         Tacentries.TyArg (
                                                         Extend.TUopt (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_int_or_var)), 
                                                         Tacentries.TyArg (
                                                         Extend.TUopt (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_int_or_var)), 
                                                         Tacentries.TyArg (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_auto_using), 
                                                         Tacentries.TyArg (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_hintbases), 
                                                         Tacentries.TyNil)))))), 
           (fun n p lems db ist -> 
# 145 "g_auto.mlg"
      Eauto.gen_eauto ~debug:Debug (Eauto.make_dimension n p) (eval_uconstrs ist lems) db 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "info_eauto" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("info_eauto", Tacentries.TyArg (
                                                              Extend.TUopt (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_int_or_var)), 
                                                              Tacentries.TyArg (
                                                              Extend.TUopt (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_int_or_var)), 
                                                              Tacentries.TyArg (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_auto_using), 
                                                              Tacentries.TyArg (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_hintbases), 
                                                              Tacentries.TyNil))))), 
           (fun n p lems db ist -> 
# 151 "g_auto.mlg"
      Eauto.gen_eauto ~debug:Info (Eauto.make_dimension n p) (eval_uconstrs ist lems) db 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "dfs_eauto" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("dfs", Tacentries.TyIdent ("eauto", 
                                                       Tacentries.TyArg (
                                                       Extend.TUopt (
                                                       Extend.TUentry (Genarg.get_arg_tag wit_int_or_var)), 
                                                       Tacentries.TyArg (
                                                       Extend.TUentry (Genarg.get_arg_tag wit_auto_using), 
                                                       Tacentries.TyArg (
                                                       Extend.TUentry (Genarg.get_arg_tag wit_hintbases), 
                                                       Tacentries.TyNil))))), 
           (fun p lems db ist -> 
# 157 "g_auto.mlg"
      Eauto.gen_eauto (Eauto.make_dimension p None) (eval_uconstrs ist lems) db 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "autounfold" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("autounfold", Tacentries.TyArg (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_hintbases), 
                                                              Tacentries.TyArg (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_clause_dft_concl), 
                                                              Tacentries.TyNil))), 
           (fun db cl ist -> 
# 161 "g_auto.mlg"
                                                           Eauto.autounfold_tac db cl 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "autounfold_one" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("autounfold_one", Tacentries.TyArg (
                                                                  Extend.TUentry (Genarg.get_arg_tag wit_hintbases), 
                                                                  Tacentries.TyIdent ("in", 
                                                                  Tacentries.TyArg (
                                                                  Extend.TUentry (Genarg.get_arg_tag wit_hyp), 
                                                                  Tacentries.TyNil)))), 
           (fun db id ist -> 
# 166 "g_auto.mlg"
      Eauto.autounfold_one (match db with None -> ["core"] | Some x -> "core"::x) (Some (id, Locus.InHyp)) 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("autounfold_one", Tacentries.TyArg (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_hintbases), 
                                                                 Tacentries.TyNil)), 
          (fun db ist -> 
# 168 "g_auto.mlg"
      Eauto.autounfold_one (match db with None -> ["core"] | Some x -> "core"::x) None 
          )))]

let () = Tacentries.tactic_extend __coq_plugin_name "unify" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("unify", Tacentries.TyArg (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                         Tacentries.TyArg (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                         Tacentries.TyNil))), 
           (fun x y ist -> 
# 172 "g_auto.mlg"
                                      Tactics.unify x y 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("unify", Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                        Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                        Tacentries.TyIdent ("with", 
                                                        Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_preident), 
                                                        Tacentries.TyNil))))), 
          (fun x y base ist -> 
# 173 "g_auto.mlg"
                                                            
    let table = try Some (Hints.searchtable_map base) with Not_found -> None in
    match table with
    | None ->
      let msg = str "Hint table " ++ str base ++ str " not found" in
      Tacticals.New.tclZEROMSG msg
    | Some t ->
      let state = Hints.Hint_db.transparent_state t in
      Tactics.unify ~state x y
  
          )))]


# 185 "g_auto.mlg"
 
let deprecated_convert_concl_no_check =
  CWarnings.create
    ~name:"convert_concl_no_check" ~category:"deprecated"
    (fun () -> Pp.str "The syntax [convert_concl_no_check] is deprecated. Use [change_no_check] instead.")


let () = Tacentries.tactic_extend __coq_plugin_name "convert_concl_no_check" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("convert_concl_no_check", 
                            Tacentries.TyArg (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                            Tacentries.TyNil)), (fun x ist -> 
# 193 "g_auto.mlg"
                                            
    deprecated_convert_concl_no_check ();
    Tactics.convert_concl ~check:false x DEFAULTcast
  
                                                )))]


# 199 "g_auto.mlg"
 

let pr_pre_hints_path_atom _ _ _ = Hints.pp_hints_path_atom Libnames.pr_qualid
let pr_hints_path_atom _ _ _ = Hints.pp_hints_path_atom Printer.pr_global
let glob_hints_path_atom ist = Hints.glob_hints_path_atom



let (wit_hints_path_atom, hints_path_atom) = Tacentries.argument_extend ~name:"hints_path_atom" 
                                             {
                                             Tacentries.arg_parsing = 
                                             Vernacextend.Arg_rules (
                                             [(Extend.Rule
                                               (Extend.Next (Extend.Stop,
                                                            (Extend.Atoken (CLexer.terminal "_"))),
                                               (fun _ loc -> 
# 215 "g_auto.mlg"
               Hints.PathAny 
                                                             )));
                                             (Extend.Rule
                                              (Extend.Next (Extend.Stop,
                                                           (Extend.Alist1 (Extend.Aentry global))),
                                              (fun g loc -> 
# 214 "g_auto.mlg"
                             Hints.PathHints g 
                                                            )))]);
                                             Tacentries.arg_tag = None;
                                             Tacentries.arg_intern = 
                                             Tacentries.ArgInternFun ((fun f ist v -> (ist, f ist v)) (
                                             
# 210 "g_auto.mlg"
                  glob_hints_path_atom 
                                             ));
                                             Tacentries.arg_subst = Tacentries.ArgSubstFun (fun s v -> v);
                                             Tacentries.arg_interp = 
                                             Tacentries.ArgInterpRet;
                                             Tacentries.arg_printer = 
                                             ((fun env sigma -> 
# 212 "g_auto.mlg"
                   pr_pre_hints_path_atom 
                                             ), (fun env sigma -> 
# 213 "g_auto.mlg"
                    pr_hints_path_atom 
                                             ), (fun env sigma -> 
# 208 "g_auto.mlg"
               pr_hints_path_atom 
                                             ));
                                             }
let _ = (wit_hints_path_atom, hints_path_atom)


# 218 "g_auto.mlg"
 

let pr_hints_path prc prx pry c = Hints.pp_hints_path c
let pr_pre_hints_path prc prx pry c = Hints.pp_hints_path_gen Libnames.pr_qualid c
let glob_hints_path ist = Hints.glob_hints_path



let (wit_hints_path, hints_path) = Tacentries.argument_extend ~name:"hints_path" 
                                   {
                                   Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                            [(Extend.Rule
                                                              (Extend.Next 
                                                               (Extend.Next 
                                                               (Extend.Stop,
                                                               Extend.Aself),
                                                               Extend.Aself),
                                                              (fun q p loc ->
                                                              
# 239 "g_auto.mlg"
                                       Hints.PathSeq (p, q) 
                                                              )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Aentry hints_path_atom)),
                                                             (fun a loc -> 
# 238 "g_auto.mlg"
                              Hints.PathAtom a 
                                                                    )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Stop,
                                                              Extend.Aself),
                                                              (Extend.Atoken (CLexer.terminal "|"))),
                                                              Extend.Aself),
                                                             (fun q _ p
                                                             loc -> 
# 237 "g_auto.mlg"
                                           Hints.PathOr (p, q) 
                                                                    )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Atoken (CLexer.terminal "eps"))),
                                                             (fun _ loc -> 
# 236 "g_auto.mlg"
                 Hints.PathEpsilon 
                                                                    )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Atoken (CLexer.terminal "emp"))),
                                                             (fun _ loc -> 
# 235 "g_auto.mlg"
                 Hints.PathEmpty 
                                                                    )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Stop,
                                                              Extend.Aself),
                                                              (Extend.Atoken (CLexer.terminal "*"))),
                                                             (fun _ p loc ->
                                                             
# 234 "g_auto.mlg"
                             Hints.PathStar p 
                                                             )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Atoken (CLexer.terminal "("))),
                                                              Extend.Aself),
                                                              (Extend.Atoken (CLexer.terminal ")"))),
                                                             (fun _ p _
                                                             loc -> 
# 233 "g_auto.mlg"
                                  p 
                                                                    )))]);
                                   Tacentries.arg_tag = None;
                                   Tacentries.arg_intern = Tacentries.ArgInternFun ((fun f ist v -> (ist, f ist v)) (
                                                           
# 229 "g_auto.mlg"
                glob_hints_path 
                                                           ));
                                   Tacentries.arg_subst = Tacentries.ArgSubstFun (fun s v -> v);
                                   Tacentries.arg_interp = Tacentries.ArgInterpRet;
                                   Tacentries.arg_printer = ((fun env sigma -> 
                                                            
# 230 "g_auto.mlg"
                 pr_pre_hints_path 
                                                            ), (fun env sigma -> 
                                                            
# 231 "g_auto.mlg"
                  pr_hints_path 
                                                            ), (fun env sigma -> 
                                                            
# 227 "g_auto.mlg"
             pr_hints_path 
                                                            ));
                                   }
let _ = (wit_hints_path, hints_path)

let (wit_opthints, opthints) = Tacentries.argument_extend ~name:"opthints" 
                               {
                               Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                        [(Extend.Rule
                                                          (Extend.Stop,
                                                          (fun loc -> 
# 246 "g_auto.mlg"
           None 
                                                                    )));
                                                        (Extend.Rule
                                                         (Extend.Next 
                                                          (Extend.Next 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (CLexer.terminal ":"))),
                                                          (Extend.Alist1 (Extend.Aentry preident))),
                                                         (fun l _ loc -> 
# 245 "g_auto.mlg"
                                   Some l 
                                                                    )))]);
                               Tacentries.arg_tag = Some
                                                    (Geninterp.Val.Opt 
                                                    (Geninterp.Val.List 
                                                    (Geninterp.val_tag (Genarg.topwit wit_preident))));
                               Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.OptArg 
                                                       (Genarg.ListArg 
                                                       (wit_preident)));
                               Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.OptArg 
                                                      (Genarg.ListArg 
                                                      (wit_preident)));
                               Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.OptArg 
                                                       (Genarg.ListArg 
                                                       (wit_preident)));
                               Tacentries.arg_printer = ((fun env sigma -> 
                                                        
# 244 "g_auto.mlg"
               pr_hintbases 
                                                        ), (fun env sigma -> 
                                                        
# 244 "g_auto.mlg"
               pr_hintbases 
                                                        ), (fun env sigma -> 
                                                        
# 244 "g_auto.mlg"
               pr_hintbases 
                                                        ));
                               }
let _ = (wit_opthints, opthints)

let () = Vernacextend.vernac_extend ~command:"HintCut" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Hint", 
                                     Vernacextend.TyTerminal ("Cut", 
                                     Vernacextend.TyTerminal ("[", Vernacextend.TyNonTerminal (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_hints_path), 
                                                                   Vernacextend.TyTerminal ("]", 
                                                                   Vernacextend.TyNonTerminal (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_opthints), 
                                                                   Vernacextend.TyNil)))))), 
         (let coqpp_body p dbnames locality ~st = let () = 
# 250 "g_auto.mlg"
                                                                                                    
        let entry = Hints.HintsCutEntry (Hints.glob_hints_path p) in
        Hints.add_hints ~local:(Locality.make_section_locality locality)
          (match dbnames with None -> ["core"] | Some l -> l) entry;
 
                                                   in st in fun p
         dbnames ~atts ~st -> coqpp_body p dbnames
         (Attributes.parse Attributes.locality atts) ~st), None))]

