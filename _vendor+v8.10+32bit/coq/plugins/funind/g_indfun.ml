
# 11 "g_indfun.mlg"
 

open Ltac_plugin
open Util
open Pp
open Constrexpr
open Indfun_common
open Indfun
open Stdarg
open Tacarg
open Tactypes
open Pcoq.Prim
open Pcoq.Constr
open Pltac



let __coq_plugin_name = "recdef_plugin"
let _ = Mltop.add_known_module __coq_plugin_name

# 30 "g_indfun.mlg"
 

let pr_fun_ind_using env sigma prc prlc _ opt_c =
  match opt_c with
    | None -> mt ()
    | Some b -> spc () ++ hov 2 (str "using" ++ spc () ++ Miscprint.pr_with_bindings (prc env sigma) (prlc env sigma) b)

(* Duplication of printing functions because "'a with_bindings" is
   (internally) not uniform in 'a: indeed constr_with_bindings at the
   "typed" level has type "open_constr with_bindings" instead of
   "constr with_bindings"; hence, its printer cannot be polymorphic in
   (prc,prlc)... *)

let pr_fun_ind_using_typed prc prlc _ opt_c =
  match opt_c with
    | None -> mt ()
    | Some b ->
      let env = Global.env () in
      let evd = Evd.from_env env in
      let (_, b) = b env evd in
      spc () ++ hov 2 (str "using" ++ spc () ++ Miscprint.pr_with_bindings (prc env evd) (prlc env evd) b)



let (wit_fun_ind_using, fun_ind_using) = Tacentries.argument_extend ~name:"fun_ind_using" 
                                         {
                                         Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                                  [(Extend.Rule
                                                                    (
                                                                    Extend.Stop,
                                                                    (fun
                                                                    loc -> 
                                                                    
# 60 "g_indfun.mlg"
           None 
                                                                    )));
                                                                  (Extend.Rule
                                                                   (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "using"))),
                                                                    (Extend.Aentry constr_with_bindings)),
                                                                   (fun c _
                                                                   loc -> 
                                                                   
# 59 "g_indfun.mlg"
                                           Some c 
                                                                   )))]);
                                         Tacentries.arg_tag = Some
                                                              (Geninterp.Val.Opt 
                                                              (Geninterp.val_tag (Genarg.topwit wit_constr_with_bindings)));
                                         Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.OptArg 
                                                                 (wit_constr_with_bindings));
                                         Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.OptArg 
                                                                (wit_constr_with_bindings));
                                         Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.OptArg 
                                                                 (wit_constr_with_bindings));
                                         Tacentries.arg_printer = ((fun env sigma -> 
                                                                  
# 57 "g_indfun.mlg"
                   pr_fun_ind_using env sigma 
                                                                  ), (fun env sigma -> 
                                                                  
# 58 "g_indfun.mlg"
                    pr_fun_ind_using env sigma 
                                                                  ), (fun env sigma -> 
                                                                  
# 56 "g_indfun.mlg"
               pr_fun_ind_using_typed 
                                                                  ));
                                         }
let _ = (wit_fun_ind_using, fun_ind_using)

let () = Tacentries.tactic_extend __coq_plugin_name "newfuninv" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("functional", Tacentries.TyIdent ("inversion", 
                                                              Tacentries.TyArg (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_quantified_hypothesis), 
                                                              Tacentries.TyArg (
                                                              Extend.TUopt (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_reference)), 
                                                              Tacentries.TyNil)))), 
           (fun hyp fname ist -> 
# 66 "g_indfun.mlg"
      
       Proofview.V82.tactic (Invfun.invfun hyp fname)
     
           )))]


# 71 "g_indfun.mlg"
 

let pr_intro_as_pat _prc _ _ pat =
  match pat with
    | Some pat ->
      spc () ++ str "as" ++ spc () ++ (* Miscprint.pr_intro_pattern prc  pat *)
        str"<simple_intropattern>"
    | None -> mt ()

let out_disjunctive = CAst.map (function
  | IntroAction (IntroOrAndPattern l) -> l
  | _ -> CErrors.user_err Pp.(str "Disjunctive or conjunctive intro pattern expected."))



let (wit_with_names, with_names) = Tacentries.argument_extend ~name:"with_names" 
                                   {
                                   Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                            [(Extend.Rule
                                                              (Extend.Stop,
                                                              (fun loc -> 
# 88 "g_indfun.mlg"
           None 
                                                                    )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Atoken (CLexer.terminal "as"))),
                                                              (Extend.Aentry simple_intropattern)),
                                                             (fun ipat _
                                                             loc -> 
# 87 "g_indfun.mlg"
                                           Some ipat 
                                                                    )))]);
                                   Tacentries.arg_tag = Some
                                                        (Geninterp.Val.Opt 
                                                        (Geninterp.val_tag (Genarg.topwit wit_intro_pattern)));
                                   Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.OptArg 
                                                           (wit_intro_pattern));
                                   Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.OptArg 
                                                          (wit_intro_pattern));
                                   Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.OptArg 
                                                           (wit_intro_pattern));
                                   Tacentries.arg_printer = ((fun env sigma -> 
                                                            
# 86 "g_indfun.mlg"
                                                                      pr_intro_as_pat 
                                                            ), (fun env sigma -> 
                                                            
# 86 "g_indfun.mlg"
                                                                      pr_intro_as_pat 
                                                            ), (fun env sigma -> 
                                                            
# 86 "g_indfun.mlg"
                                                                      pr_intro_as_pat 
                                                            ));
                                   }
let _ = (wit_with_names, with_names)


# 91 "g_indfun.mlg"
 

let functional_induction b c x pat =
  Proofview.V82.tactic (functional_induction true c x (Option.map out_disjunctive pat))



let () = Tacentries.tactic_extend __coq_plugin_name "newfunind" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("functional", Tacentries.TyIdent ("induction", 
                                                              Tacentries.TyArg (
                                                              Extend.TUlist1 (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_constr)), 
                                                              Tacentries.TyArg (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_fun_ind_using), 
                                                              Tacentries.TyArg (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_with_names), 
                                                              Tacentries.TyNil))))), 
           (fun cl princl pat ist -> 
# 100 "g_indfun.mlg"
      
       let c = match cl with
         | [] -> assert false
         | [c] -> c
         | c::cl -> EConstr.applist(c,cl)
       in
       Extratactics.onSomeWithHoles (fun x -> functional_induction true c x pat) princl 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "snewfunind" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("soft", Tacentries.TyIdent ("functional", 
                                                        Tacentries.TyIdent ("induction", 
                                                        Tacentries.TyArg (
                                                        Extend.TUlist1 (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_constr)), 
                                                        Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_fun_ind_using), 
                                                        Tacentries.TyArg (
                                                        Extend.TUentry (Genarg.get_arg_tag wit_with_names), 
                                                        Tacentries.TyNil)))))), 
           (fun cl princl pat ist -> 
# 111 "g_indfun.mlg"
      
       let c = match cl with
         | [] -> assert false
         | [c] -> c
         | c::cl -> EConstr.applist(c,cl)
       in
       Extratactics.onSomeWithHoles (fun x -> functional_induction false c x pat) princl 
           )))]


# 120 "g_indfun.mlg"
 

let pr_constr_comma_sequence env sigma prc _ _ = prlist_with_sep pr_comma (prc env sigma)



let (wit_constr_comma_sequence', constr_comma_sequence') = Tacentries.argument_extend ~name:"constr_comma_sequence'" 
                                                           {
                                                           Tacentries.arg_parsing = 
                                                           Vernacextend.Arg_rules (
                                                           [(Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Aentry constr)),
                                                             (fun c loc -> 
# 130 "g_indfun.mlg"
                     [c] 
                                                                    )));
                                                           (Extend.Rule
                                                            (Extend.Next 
                                                             (Extend.Next 
                                                             (Extend.Next 
                                                             (Extend.Stop,
                                                             (Extend.Aentry constr)),
                                                             (Extend.Atoken (CLexer.terminal ","))),
                                                             Extend.Aself),
                                                            (fun l _ c loc ->
                                                            
# 129 "g_indfun.mlg"
                                                   c::l 
                                                            )))]);
                                                           Tacentries.arg_tag = 
                                                           Some
                                                           (Geninterp.Val.List 
                                                           (Geninterp.val_tag (Genarg.topwit wit_constr)));
                                                           Tacentries.arg_intern = 
                                                           Tacentries.ArgInternWit (Genarg.ListArg 
                                                           (wit_constr));
                                                           Tacentries.arg_subst = 
                                                           Tacentries.ArgSubstWit (Genarg.ListArg 
                                                           (wit_constr));
                                                           Tacentries.arg_interp = 
                                                           Tacentries.ArgInterpWit (Genarg.ListArg 
                                                           (wit_constr));
                                                           Tacentries.arg_printer = 
                                                           ((fun env sigma -> 
                                                           
# 128 "g_indfun.mlg"
               pr_constr_comma_sequence env sigma 
                                                           ), (fun env sigma -> 
                                                           
# 128 "g_indfun.mlg"
               pr_constr_comma_sequence env sigma 
                                                           ), (fun env sigma -> 
                                                           
# 128 "g_indfun.mlg"
               pr_constr_comma_sequence env sigma 
                                                           ));
                                                           }
let _ = (wit_constr_comma_sequence', constr_comma_sequence')


# 133 "g_indfun.mlg"
 

let pr_auto_using env sigma prc _prlc _prt = Pptactic.pr_auto_using (prc env sigma)



let (wit_auto_using', auto_using') = Tacentries.argument_extend ~name:"auto_using'" 
                                     {
                                     Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                              [(Extend.Rule
                                                                (Extend.Stop,
                                                                (fun loc -> 
# 143 "g_indfun.mlg"
           [] 
                                                                    )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Atoken (CLexer.terminal "using"))),
                                                                (Extend.Aentry constr_comma_sequence')),
                                                               (fun l _
                                                               loc -> 
                                                               
# 142 "g_indfun.mlg"
                                             l 
                                                               )))]);
                                     Tacentries.arg_tag = Some
                                                          (Geninterp.Val.List 
                                                          (Geninterp.val_tag (Genarg.topwit wit_constr)));
                                     Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.ListArg 
                                                             (wit_constr));
                                     Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.ListArg 
                                                            (wit_constr));
                                     Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.ListArg 
                                                             (wit_constr));
                                     Tacentries.arg_printer = ((fun env sigma -> 
                                                              
# 141 "g_indfun.mlg"
               pr_auto_using env sigma 
                                                              ), (fun env sigma -> 
                                                              
# 141 "g_indfun.mlg"
               pr_auto_using env sigma 
                                                              ), (fun env sigma -> 
                                                              
# 141 "g_indfun.mlg"
               pr_auto_using env sigma 
                                                              ));
                                     }
let _ = (wit_auto_using', auto_using')


# 146 "g_indfun.mlg"
 

module Vernac = Pvernac.Vernac_
module Tactic = Pltac

type function_rec_definition_loc_argtype = (Vernacexpr.fixpoint_expr * Vernacexpr.decl_notation list) Loc.located

let (wit_function_rec_definition_loc : function_rec_definition_loc_argtype Genarg.uniform_genarg_type) =
  Genarg.create_arg "function_rec_definition_loc"

let function_rec_definition_loc =
  Pcoq.create_generic_entry Pcoq.utactic "function_rec_definition_loc" (Genarg.rawwit wit_function_rec_definition_loc)



let _ = let () =
        Pcoq.grammar_extend function_rec_definition_loc None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Aentry Vernac.rec_definition)),
                 (fun g loc -> 
# 165 "g_indfun.mlg"
                                       Loc.tag ~loc g 
                               ))])])
        in ()


# 170 "g_indfun.mlg"
 

let () =
  let raw_printer env sigma _ _ _ (loc,body) = Ppvernac.pr_rec_definition body in
  Pptactic.declare_extra_vernac_genarg_pprule wit_function_rec_definition_loc raw_printer



let () = Vernacextend.vernac_extend ~command:"Function"  ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Function", 
                                     Vernacextend.TyNonTerminal (Extend.TUlist1sep (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_function_rec_definition_loc), "with"), 
                                     Vernacextend.TyNil)), (let coqpp_body recsl
                                                           () ~st = let proof = (
                                                                    
# 193 "g_indfun.mlg"
         do_generate_principle false (List.map snd recsl) 
                                                                    ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun recsl
                                                           ~atts ~st
                                                           -> coqpp_body recsl
                                                           (Attributes.unsupported_attributes atts) ~st), Some 
         (fun recsl -> 
# 181 "g_indfun.mlg"
         let hard = List.exists (function
           | _,((_,(Some { CAst.v = CMeasureRec _ }
             | Some { CAst.v = CWfRec _}),_,_,_),_) -> true
           | _,((_,Some { CAst.v = CStructRec _ },_,_,_),_)
           | _,((_,None,_,_,_),_) -> false) recsl in
         match
           Vernac_classifier.classify_vernac
             (Vernacexpr.(VernacExpr([], VernacFixpoint(Decl_kinds.NoDischarge, List.map snd recsl))))
         with
         | Vernacextend.VtSideff ids, _ when hard ->
             Vernacextend.(VtStartProof (GuaranteesOpacity, ids), VtLater)
         | x -> x 
         )))]


# 196 "g_indfun.mlg"
 

let pr_fun_scheme_arg (princ_name,fun_name,s) =
  Names.Id.print princ_name ++ str " :=" ++ spc() ++ str "Induction for " ++
  Libnames.pr_qualid fun_name ++ spc() ++ str "Sort " ++
  Sorts.pr_sort_family s



let (wit_fun_scheme_arg, fun_scheme_arg) = Vernacextend.vernac_argument_extend ~name:"fun_scheme_arg" 
                                           {
                                           Vernacextend.arg_parsing = 
                                           Vernacextend.Arg_rules ([(
                                                                   Extend.Rule
                                                                   (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Aentry ident)),
                                                                    (Extend.Atoken (CLexer.terminal ":="))),
                                                                    (Extend.Atoken (CLexer.terminal "Induction"))),
                                                                    (Extend.Atoken (CLexer.terminal "for"))),
                                                                    (Extend.Aentry reference)),
                                                                    (Extend.Atoken (CLexer.terminal "Sort"))),
                                                                    (Extend.Aentry sort_family)),
                                                                   (fun s _
                                                                   fun_name _
                                                                   _ _
                                                                   princ_name
                                                                   loc -> 
                                                                   
# 207 "g_indfun.mlg"
                                                                                              (princ_name,fun_name,s) 
                                                                   )))]);
                                           Vernacextend.arg_printer = fun env sigma -> 
                                           
# 206 "g_indfun.mlg"
             pr_fun_scheme_arg 
                                           ;
                                           }
let _ = (wit_fun_scheme_arg, fun_scheme_arg)


# 210 "g_indfun.mlg"
 

let warning_error names e =
  let (e, _) = ExplainErr.process_vernac_interp_error (e, Exninfo.null) in
  match e with
    | Building_graph e ->
       let names = pr_enum Libnames.pr_qualid names in
       let error = if do_observe () then (spc () ++ CErrors.print e) else mt () in
       warn_cannot_define_graph (names,error)
    | Defining_principle e ->
       let names = pr_enum Libnames.pr_qualid names in
       let error = if do_observe () then CErrors.print e else mt () in
       warn_cannot_define_principle (names,error)
    | _ -> raise e



let () = Vernacextend.vernac_extend ~command:"NewFunctionalScheme"  ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Functional", 
                                     Vernacextend.TyTerminal ("Scheme", 
                                     Vernacextend.TyNonTerminal (Extend.TUlist1sep (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_fun_scheme_arg), "with"), 
                                     Vernacextend.TyNil))), (let coqpp_body fas
                                                            () ~st = 
                                                            let proof = (
                                                            
# 231 "g_indfun.mlg"
      fun ~pstate ->
      begin
        try
          Functional_principles_types.build_scheme fas; pstate
        with
        | Functional_principles_types.No_graph_found ->
          begin
            match fas with
            | (_,fun_name,_)::_ ->
              begin
                let pstate = make_graph ~pstate (Smartlocate.global_with_alias fun_name) in
                try Functional_principles_types.build_scheme fas; pstate
                with
                | Functional_principles_types.No_graph_found ->
                  CErrors.user_err Pp.(str "Cannot generate induction principle(s)")
                | e when CErrors.noncritical e ->
                  let names = List.map (fun (_,na,_) -> na) fas in
                  warning_error names e; pstate
              end
              | _ -> assert false (* we can only have non empty  list *)
          end
        | e when CErrors.noncritical e ->
          let names = List.map (fun (_,na,_) -> na) fas in
          warning_error names e; pstate
      end
    
                                                            ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun fas
                                                            ~atts ~st
                                                            -> coqpp_body fas
                                                            (Attributes.unsupported_attributes atts) ~st), Some 
         (fun fas -> 
# 229 "g_indfun.mlg"
        Vernacextend.(VtSideff(List.map pi1 fas), VtLater) 
         )))]

let () = Vernacextend.vernac_extend ~command:"NewFunctionalCase"  ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Functional", 
                                     Vernacextend.TyTerminal ("Case", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_fun_scheme_arg), 
                                     Vernacextend.TyNil))), (let coqpp_body fas
                                                            () ~st = 
                                                            let () = 
                                                            
# 263 "g_indfun.mlg"
       Functional_principles_types.build_case_scheme fas 
                                                             in st in fun fas
                                                            ~atts ~st
                                                            -> coqpp_body fas
                                                            (Attributes.unsupported_attributes atts) ~st), Some 
         (fun fas -> 
# 262 "g_indfun.mlg"
       Vernacextend.(VtSideff[pi1 fas], VtLater) 
         )))]

let () = Vernacextend.vernac_extend ~command:"GenerateGraph" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Generate", 
                                     Vernacextend.TyTerminal ("graph", 
                                     Vernacextend.TyTerminal ("for", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_reference), 
                                     Vernacextend.TyNil)))), (let coqpp_body c
                                                             () ~st = 
                                                             let proof = (
                                                             
# 269 "g_indfun.mlg"
    make_graph (Smartlocate.global_with_alias c) 
                                                             ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun c
                                                             ~atts ~st
                                                             -> coqpp_body c
                                                             (Attributes.unsupported_attributes atts) ~st), None))]

