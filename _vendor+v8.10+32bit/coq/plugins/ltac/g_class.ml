
# 11 "g_class.mlg"
 

open Class_tactics
open Stdarg
open Tacarg



let __coq_plugin_name = "ltac_plugin"
let _ = Mltop.add_known_module __coq_plugin_name

# 23 "g_class.mlg"
 

let set_transparency cl b =
  List.iter (fun r ->
    let gr = Smartlocate.global_with_alias r in
    let ev = Tacred.evaluable_of_global_reference (Global.env ()) gr in
      Classes.set_typeclass_transparency ev (Locality.make_section_locality None) b) cl



let () = Vernacextend.vernac_extend ~command:"Typeclasses_Unfold_Settings" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Typeclasses", 
                                     Vernacextend.TyTerminal ("Transparent", 
                                     Vernacextend.TyNonTerminal (Extend.TUlist0 (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_reference)), 
                                     Vernacextend.TyNil))), (let coqpp_body cl
                                                            () ~st = 
                                                            let () = 
                                                            
# 34 "g_class.mlg"
                                                         
    set_transparency cl true 
                                                             in st in fun cl
                                                            ~atts ~st
                                                            -> coqpp_body cl
                                                            (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"Typeclasses_Rigid_Settings" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Typeclasses", 
                                     Vernacextend.TyTerminal ("Opaque", 
                                     Vernacextend.TyNonTerminal (Extend.TUlist0 (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_reference)), 
                                     Vernacextend.TyNil))), (let coqpp_body cl
                                                            () ~st = 
                                                            let () = 
                                                            
# 39 "g_class.mlg"
                                                    
    set_transparency cl false 
                                                             in st in fun cl
                                                            ~atts ~st
                                                            -> coqpp_body cl
                                                            (Attributes.unsupported_attributes atts) ~st), None))]


# 43 "g_class.mlg"
 

let pr_debug _prc _prlc _prt b =
  if b then Pp.str "debug" else Pp.mt()



let (wit_debug, debug) = Tacentries.argument_extend ~name:"debug" {
                                                                  Tacentries.arg_parsing = 
                                                                  Vernacextend.Arg_rules (
                                                                  [(Extend.Rule
                                                                    (
                                                                    Extend.Stop,
                                                                    (fun
                                                                    loc -> 
                                                                    
# 52 "g_class.mlg"
           false 
                                                                    )));
                                                                  (Extend.Rule
                                                                   (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "debug"))),
                                                                   (fun _
                                                                   loc -> 
                                                                   
# 51 "g_class.mlg"
                   true 
                                                                   )))]);
                                                                  Tacentries.arg_tag = 
                                                                  Some
                                                                  (Geninterp.val_tag (Genarg.topwit wit_bool));
                                                                  Tacentries.arg_intern = 
                                                                  Tacentries.ArgInternWit (wit_bool);
                                                                  Tacentries.arg_subst = 
                                                                  Tacentries.ArgSubstWit (wit_bool);
                                                                  Tacentries.arg_interp = 
                                                                  Tacentries.ArgInterpWit (wit_bool);
                                                                  Tacentries.arg_printer = 
                                                                  ((fun env sigma -> 
                                                                  
# 50 "g_class.mlg"
                                                 pr_debug 
                                                                  ), (fun env sigma -> 
                                                                  
# 50 "g_class.mlg"
                                                 pr_debug 
                                                                  ), (fun env sigma -> 
                                                                  
# 50 "g_class.mlg"
                                                 pr_debug 
                                                                  ));
                                                                  }
let _ = (wit_debug, debug)


# 55 "g_class.mlg"
 

let pr_search_strategy _prc _prlc _prt = function
  | Some Dfs -> Pp.str "dfs"
  | Some Bfs -> Pp.str "bfs"
  | None -> Pp.mt ()



let (wit_eauto_search_strategy, eauto_search_strategy) = Tacentries.argument_extend ~name:"eauto_search_strategy" 
                                                         {
                                                         Tacentries.arg_parsing = 
                                                         Vernacextend.Arg_rules (
                                                         [(Extend.Rule
                                                           (Extend.Stop,
                                                           (fun loc -> 
# 67 "g_class.mlg"
           None 
                                                                    )));
                                                         (Extend.Rule
                                                          (Extend.Next 
                                                           (Extend.Stop,
                                                           (Extend.Atoken (CLexer.terminal "(dfs)"))),
                                                          (fun _ loc -> 
# 66 "g_class.mlg"
                   Some Dfs 
                                                                    )));
                                                         (Extend.Rule
                                                          (Extend.Next 
                                                           (Extend.Stop,
                                                           (Extend.Atoken (CLexer.terminal "(bfs)"))),
                                                          (fun _ loc -> 
# 65 "g_class.mlg"
                   Some Bfs 
                                                                    )))]);
                                                         Tacentries.arg_tag = 
                                                         None;
                                                         Tacentries.arg_intern = 
                                                         Tacentries.ArgInternFun (fun ist v -> (ist, v));
                                                         Tacentries.arg_subst = 
                                                         Tacentries.ArgSubstFun (fun s v -> v);
                                                         Tacentries.arg_interp = 
                                                         Tacentries.ArgInterpRet;
                                                         Tacentries.arg_printer = 
                                                         ((fun env sigma -> 
                                                         
# 64 "g_class.mlg"
                                                   pr_search_strategy 
                                                         ), (fun env sigma -> 
                                                         
# 64 "g_class.mlg"
                                                   pr_search_strategy 
                                                         ), (fun env sigma -> 
                                                         
# 64 "g_class.mlg"
                                                   pr_search_strategy 
                                                         ));
                                                         }
let _ = (wit_eauto_search_strategy, eauto_search_strategy)

let () = Vernacextend.vernac_extend ~command:"Typeclasses_Settings" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Typeclasses", 
                                     Vernacextend.TyTerminal ("eauto", 
                                     Vernacextend.TyTerminal (":=", Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_debug), 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_eauto_search_strategy), 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUopt (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_int)), 
                                                                    Vernacextend.TyNil)))))), 
         (let coqpp_body d s depth () ~st = let () = 
# 73 "g_class.mlg"
                                                                                       
     set_typeclasses_debug d;
     Option.iter set_typeclasses_strategy s;
     set_typeclasses_depth depth
   
                                             in st in fun d
         s depth ~atts ~st -> coqpp_body d s depth
         (Attributes.unsupported_attributes atts) ~st), None))]

let () = Tacentries.tactic_extend __coq_plugin_name "typeclasses_eauto" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("typeclasses", Tacentries.TyIdent ("eauto", 
                                                               Tacentries.TyIdent ("bfs", 
                                                               Tacentries.TyArg (
                                                               Extend.TUopt (
                                                               Extend.TUentry (Genarg.get_arg_tag wit_int_or_var)), 
                                                               Tacentries.TyIdent ("with", 
                                                               Tacentries.TyArg (
                                                               Extend.TUlist1 (
                                                               Extend.TUentry (Genarg.get_arg_tag wit_preident)), 
                                                               Tacentries.TyNil)))))), 
           (fun d l ist -> 
# 83 "g_class.mlg"
      typeclasses_eauto ~strategy:Bfs ~depth:d l 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("typeclasses", Tacentries.TyIdent ("eauto", 
                                                              Tacentries.TyArg (
                                                              Extend.TUopt (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_int_or_var)), 
                                                              Tacentries.TyIdent ("with", 
                                                              Tacentries.TyArg (
                                                              Extend.TUlist1 (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_preident)), 
                                                              Tacentries.TyNil))))), 
          (fun d l ist -> 
# 85 "g_class.mlg"
      typeclasses_eauto ~depth:d l 
          )));
         (Tacentries.TyML (Tacentries.TyIdent ("typeclasses", Tacentries.TyIdent ("eauto", 
                                                              Tacentries.TyArg (
                                                              Extend.TUopt (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_int_or_var)), 
                                                              Tacentries.TyNil))), 
          (fun d ist -> 
# 86 "g_class.mlg"
                                                   
     typeclasses_eauto ~only_classes:true ~depth:d [Class_tactics.typeclasses_db] 
          )))]

let () = Tacentries.tactic_extend __coq_plugin_name "head_of_constr" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("head_of_constr", Tacentries.TyArg (
                                                                  Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                  Tacentries.TyArg (
                                                                  Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                  Tacentries.TyNil))), 
           (fun h c ist -> 
# 91 "g_class.mlg"
                                               head_of_constr h c 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "not_evar" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("not_evar", Tacentries.TyArg (
                                                            Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                            Tacentries.TyNil)), 
           (fun ty ist -> 
# 95 "g_class.mlg"
                                 not_evar ty 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "is_ground" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("is_ground", Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                             Tacentries.TyNil)), 
           (fun ty ist -> 
# 99 "g_class.mlg"
                                  is_ground ty 
           )))]


# 102 "g_class.mlg"
 
let deprecated_autoapply_using =
  CWarnings.create
    ~name:"autoapply-using" ~category:"deprecated"
    (fun () -> Pp.str "The syntax [autoapply ... using] is deprecated. Use [autoapply ... with] instead.")


let () = Tacentries.tactic_extend __coq_plugin_name "autoapply" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("autoapply", Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                             Tacentries.TyIdent ("using", 
                                                             Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_preident), 
                                                             Tacentries.TyNil)))), 
           (fun c i ist -> 
# 110 "g_class.mlg"
                                                    
    deprecated_autoapply_using ();
    autoapply c i
  
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("autoapply", Tacentries.TyArg (
                                                            Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                            Tacentries.TyIdent ("with", 
                                                            Tacentries.TyArg (
                                                            Extend.TUentry (Genarg.get_arg_tag wit_preident), 
                                                            Tacentries.TyNil)))), 
          (fun c i ist -> 
# 114 "g_class.mlg"
                                                    autoapply c i 
          )))]


# 117 "g_class.mlg"
 

(** TODO: DEPRECATE *)
(* A progress test that allows to see if the evars have changed *)
open Constr
open Proofview.Notations

let rec eq_constr_mod_evars sigma x y =
  let open EConstr in
  match EConstr.kind sigma x, EConstr.kind sigma y with
  | Evar (e1, l1), Evar (e2, l2) when not (Evar.equal e1 e2) -> true
  | _, _ -> compare_constr sigma (fun x y -> eq_constr_mod_evars sigma x y) x y

let progress_evars t =
  Proofview.Goal.enter begin fun gl ->
    let concl = Proofview.Goal.concl gl in
    let check =
      Proofview.Goal.enter begin fun gl' ->
        let sigma = Tacmach.New.project gl' in
        let newconcl = Proofview.Goal.concl gl' in
        if eq_constr_mod_evars sigma concl newconcl
        then Tacticals.New.tclFAIL 0 (Pp.str"No progress made (modulo evars)")
        else Proofview.tclUNIT ()
      end
    in t <*> check
  end



let () = Tacentries.tactic_extend __coq_plugin_name "progress_evars" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("progress_evars", Tacentries.TyArg (
                                                                  Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                                                  Tacentries.TyNil)), 
           (fun t ist -> 
# 147 "g_class.mlg"
                                      progress_evars (Tacinterp.tactic_of_value ist t) 
           )))]

