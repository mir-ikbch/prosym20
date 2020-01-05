
# 11 "g_newring.mlg"
 

open Ltac_plugin
open Pp
open Util
open Newring_ast
open Newring
open Stdarg
open Tacarg
open Pcoq.Constr
open Pltac



let __coq_plugin_name = "newring_plugin"
let _ = Mltop.add_known_module __coq_plugin_name
let () = Tacentries.tactic_extend __coq_plugin_name "protect_fv" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("protect_fv", Tacentries.TyArg (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_string), 
                                                              Tacentries.TyIdent ("in", 
                                                              Tacentries.TyArg (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                              Tacentries.TyNil)))), 
           (fun map id ist -> 
# 29 "g_newring.mlg"
      protect_tac_in map id 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("protect_fv", Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_string), 
                                                             Tacentries.TyNil)), 
          (fun map ist -> 
# 31 "g_newring.mlg"
      protect_tac map 
          )))]


# 34 "g_newring.mlg"
 

open Pptactic
open Ppconstr

let pr_ring_mod env sigma = function
  | Ring_kind (Computational eq_test) -> str "decidable" ++ pr_arg (pr_constr_expr env sigma) eq_test
  | Ring_kind Abstract ->  str "abstract"
  | Ring_kind (Morphism morph) -> str "morphism" ++ pr_arg (pr_constr_expr env sigma) morph
  | Const_tac (CstTac cst_tac) -> str "constants" ++ spc () ++ str "[" ++ pr_raw_tactic env sigma cst_tac ++ str "]"
  | Const_tac (Closed l) -> str "closed" ++ spc () ++ str "[" ++ prlist_with_sep spc pr_qualid l ++ str "]"
  | Pre_tac t -> str "preprocess" ++ spc () ++ str "[" ++ pr_raw_tactic env sigma t ++ str "]"
  | Post_tac t -> str "postprocess" ++ spc () ++ str "[" ++ pr_raw_tactic env sigma t ++ str "]"
  | Setoid(sth,ext) -> str "setoid" ++ pr_arg (pr_constr_expr env sigma) sth ++ pr_arg (pr_constr_expr env sigma) ext
  | Pow_spec(Closed l,spec) -> str "power_tac" ++ pr_arg (pr_constr_expr env sigma) spec ++ spc () ++ str "[" ++ prlist_with_sep spc pr_qualid l ++ str "]"
  | Pow_spec(CstTac cst_tac,spec) -> str "power_tac" ++ pr_arg (pr_constr_expr env sigma) spec ++ spc () ++ str "[" ++ pr_raw_tactic env sigma cst_tac ++ str "]"
  | Sign_spec t -> str "sign" ++ pr_arg (pr_constr_expr env sigma) t
  | Div_spec t -> str "div" ++ pr_arg (pr_constr_expr env sigma) t



let (wit_ring_mod, ring_mod) = Vernacextend.vernac_argument_extend ~name:"ring_mod" 
                               {
                               Vernacextend.arg_parsing = Vernacextend.Arg_rules (
                                                          [(Extend.Rule
                                                            (Extend.Next 
                                                             (Extend.Next 
                                                             (Extend.Stop,
                                                             (Extend.Atoken (CLexer.terminal "div"))),
                                                             (Extend.Aentry constr)),
                                                            (fun div_spec _
                                                            loc -> 
# 70 "g_newring.mlg"
                                    Div_spec div_spec 
                                                                   )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (CLexer.terminal "power_tac"))),
                                                            (Extend.Aentry constr)),
                                                            (Extend.Atoken (CLexer.terminal "["))),
                                                            (Extend.Aentry tactic)),
                                                            (Extend.Atoken (CLexer.terminal "]"))),
                                                           (fun _ cst_tac _
                                                           pow_spec _ loc ->
                                                           
# 69 "g_newring.mlg"
             Pow_spec (CstTac cst_tac, pow_spec) 
                                                           )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (CLexer.terminal "power"))),
                                                            (Extend.Aentry constr)),
                                                            (Extend.Atoken (CLexer.terminal "["))),
                                                            (Extend.Alist1 (Extend.Aentry global))),
                                                            (Extend.Atoken (CLexer.terminal "]"))),
                                                           (fun _ l _
                                                           pow_spec _ loc ->
                                                           
# 67 "g_newring.mlg"
             Pow_spec (Closed l, pow_spec) 
                                                           )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (CLexer.terminal "sign"))),
                                                            (Extend.Aentry constr)),
                                                           (fun sign_spec _
                                                           loc -> 
# 65 "g_newring.mlg"
                                      Sign_spec sign_spec 
                                                                  )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (CLexer.terminal "setoid"))),
                                                            (Extend.Aentry constr)),
                                                            (Extend.Aentry constr)),
                                                           (fun ext sth _
                                                           loc -> 
# 64 "g_newring.mlg"
                                              Setoid(sth,ext) 
                                                                  )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (CLexer.terminal "postprocess"))),
                                                            (Extend.Atoken (CLexer.terminal "["))),
                                                            (Extend.Aentry tactic)),
                                                            (Extend.Atoken (CLexer.terminal "]"))),
                                                           (fun _ post _ _
                                                           loc -> 
# 63 "g_newring.mlg"
                                                Post_tac post 
                                                                  )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (CLexer.terminal "preprocess"))),
                                                            (Extend.Atoken (CLexer.terminal "["))),
                                                            (Extend.Aentry tactic)),
                                                            (Extend.Atoken (CLexer.terminal "]"))),
                                                           (fun _ pre _ _
                                                           loc -> 
# 62 "g_newring.mlg"
                                              Pre_tac pre 
                                                                  )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (CLexer.terminal "closed"))),
                                                            (Extend.Atoken (CLexer.terminal "["))),
                                                            (Extend.Alist1 (Extend.Aentry global))),
                                                            (Extend.Atoken (CLexer.terminal "]"))),
                                                           (fun _ l _ _
                                                           loc -> 
# 61 "g_newring.mlg"
                                                Const_tac(Closed l) 
                                                                  )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (CLexer.terminal "constants"))),
                                                            (Extend.Atoken (CLexer.terminal "["))),
                                                            (Extend.Aentry tactic)),
                                                            (Extend.Atoken (CLexer.terminal "]"))),
                                                           (fun _ cst_tac _ _
                                                           loc -> 
# 60 "g_newring.mlg"
                                                 Const_tac(CstTac cst_tac) 
                                                                  )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (CLexer.terminal "morphism"))),
                                                            (Extend.Aentry constr)),
                                                           (fun morph _
                                                           loc -> 
# 59 "g_newring.mlg"
                                      Ring_kind(Morphism morph) 
                                                                  )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (CLexer.terminal "abstract"))),
                                                           (fun _ loc -> 
# 58 "g_newring.mlg"
                        Ring_kind Abstract 
                                                                    )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (CLexer.terminal "decidable"))),
                                                            (Extend.Aentry constr)),
                                                           (fun eq_test _
                                                           loc -> 
# 57 "g_newring.mlg"
                                         Ring_kind(Computational eq_test) 
                                                                  )))]);
                               Vernacextend.arg_printer = fun env sigma -> 
                               
# 56 "g_newring.mlg"
               pr_ring_mod env sigma 
                               ;
                               }
let _ = (wit_ring_mod, ring_mod)


# 73 "g_newring.mlg"
 

let pr_ring_mods env sigma l = surround (prlist_with_sep pr_comma (pr_ring_mod env sigma) l)



let (wit_ring_mods, ring_mods) = Vernacextend.vernac_argument_extend ~name:"ring_mods" 
                                 {
                                 Vernacextend.arg_parsing = Vernacextend.Arg_rules (
                                                            [(Extend.Rule
                                                              (Extend.Next 
                                                               (Extend.Next 
                                                               (Extend.Next 
                                                               (Extend.Stop,
                                                               (Extend.Atoken (CLexer.terminal "("))),
                                                               (Extend.Alist1sep ((Extend.Aentry ring_mod), (Extend.Atoken (CLexer.terminal ","))))),
                                                               (Extend.Atoken (CLexer.terminal ")"))),
                                                              (fun _ mods _
                                                              loc -> 
                                                              
# 81 "g_newring.mlg"
                                                     mods 
                                                              )))]);
                                 Vernacextend.arg_printer = fun env sigma -> 
                                 
# 80 "g_newring.mlg"
               pr_ring_mods env sigma 
                                 ;
                                 }
let _ = (wit_ring_mods, ring_mods)

let () = Vernacextend.vernac_extend ~command:"AddSetoidRing" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Add", 
                                     Vernacextend.TyTerminal ("Ring", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                     Vernacextend.TyTerminal (":", Vernacextend.TyNonTerminal (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                   Vernacextend.TyNonTerminal (
                                                                   Extend.TUopt (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_ring_mods)), 
                                                                   Vernacextend.TyNil)))))), 
         (let coqpp_body id t l () ~st = let () = 
# 86 "g_newring.mlg"
      add_theory id t (Option.default [] l) 
                                          in st in fun id
         t l ~atts ~st -> coqpp_body id t l
         (Attributes.unsupported_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Print", 
                                    Vernacextend.TyTerminal ("Rings", 
                                    Vernacextend.TyNil)), (let coqpp_body () ~st = 
                                                          let () = 
# 87 "g_newring.mlg"
                                                                  
    print_rings ()
  
                                                           in st in fun ~atts
                                                          ~st
                                                          -> coqpp_body (Attributes.unsupported_attributes atts) ~st), Some 
         
# 87 "g_newring.mlg"
                             Vernacextend.classify_as_query 
         ))]

let () = Tacentries.tactic_extend __coq_plugin_name "ring_lookup" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("ring_lookup", Tacentries.TyArg (
                                                               Extend.TUentryl (Genarg.get_arg_tag wit_tactic, 0), 
                                                               Tacentries.TyIdent ("[", 
                                                               Tacentries.TyArg (
                                                               Extend.TUlist0 (
                                                               Extend.TUentry (Genarg.get_arg_tag wit_constr)), 
                                                               Tacentries.TyIdent ("]", 
                                                               Tacentries.TyArg (
                                                               Extend.TUlist1 (
                                                               Extend.TUentry (Genarg.get_arg_tag wit_constr)), 
                                                               Tacentries.TyNil)))))), 
           (fun f lH lrt ist -> 
# 94 "g_newring.mlg"
      let (t,lr) = List.sep_last lrt in ring_lookup f lH lr t 
           )))]


# 97 "g_newring.mlg"
 

let pr_field_mod env sigma = function
  | Ring_mod m -> pr_ring_mod env sigma m
  | Inject inj -> str "completeness" ++ pr_arg (pr_constr_expr env sigma) inj



let (wit_field_mod, field_mod) = Vernacextend.vernac_argument_extend ~name:"field_mod" 
                                 {
                                 Vernacextend.arg_parsing = Vernacextend.Arg_rules (
                                                            [(Extend.Rule
                                                              (Extend.Next 
                                                               (Extend.Next 
                                                               (Extend.Stop,
                                                               (Extend.Atoken (CLexer.terminal "completeness"))),
                                                               (Extend.Aentry constr)),
                                                              (fun inj _
                                                              loc -> 
                                                              
# 108 "g_newring.mlg"
                                        Inject inj 
                                                              )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Aentry ring_mod)),
                                                             (fun m loc -> 
# 107 "g_newring.mlg"
                         Ring_mod m 
                                                                    )))]);
                                 Vernacextend.arg_printer = fun env sigma -> 
                                 
# 106 "g_newring.mlg"
               pr_field_mod env sigma 
                                 ;
                                 }
let _ = (wit_field_mod, field_mod)


# 111 "g_newring.mlg"
 

let pr_field_mods env sigma l = surround (prlist_with_sep pr_comma (pr_field_mod env sigma) l)



let (wit_field_mods, field_mods) = Vernacextend.vernac_argument_extend ~name:"field_mods" 
                                   {
                                   Vernacextend.arg_parsing = Vernacextend.Arg_rules (
                                                              [(Extend.Rule
                                                                (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Stop,
                                                                 (Extend.Atoken (CLexer.terminal "("))),
                                                                 (Extend.Alist1sep ((Extend.Aentry field_mod), (Extend.Atoken (CLexer.terminal ","))))),
                                                                 (Extend.Atoken (CLexer.terminal ")"))),
                                                                (fun _ mods _
                                                                loc -> 
                                                                
# 119 "g_newring.mlg"
                                                      mods 
                                                                )))]);
                                   Vernacextend.arg_printer = fun env sigma -> 
                                   
# 118 "g_newring.mlg"
               pr_field_mods env sigma 
                                   ;
                                   }
let _ = (wit_field_mods, field_mods)

let () = Vernacextend.vernac_extend ~command:"AddSetoidField" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Add", 
                                     Vernacextend.TyTerminal ("Field", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                     Vernacextend.TyTerminal (":", Vernacextend.TyNonTerminal (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                                   Vernacextend.TyNonTerminal (
                                                                   Extend.TUopt (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_field_mods)), 
                                                                   Vernacextend.TyNil)))))), 
         (let coqpp_body id t l () ~st = let () = 
# 124 "g_newring.mlg"
    let l = match l with None -> [] | Some l -> l in add_field_theory id t l 
                                          in st in fun id
         t l ~atts ~st -> coqpp_body id t l
         (Attributes.unsupported_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Print", 
                                    Vernacextend.TyTerminal ("Fields", 
                                    Vernacextend.TyNil)), (let coqpp_body () ~st = 
                                                          let () = 
# 125 "g_newring.mlg"
                                                               
    print_fields ()
  
                                                           in st in fun ~atts
                                                          ~st
                                                          -> coqpp_body (Attributes.unsupported_attributes atts) ~st), Some 
         
# 125 "g_newring.mlg"
                           Vernacextend.classify_as_query
         ))]

let () = Tacentries.tactic_extend __coq_plugin_name "field_lookup" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("field_lookup", Tacentries.TyArg (
                                                                Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                                                Tacentries.TyIdent ("[", 
                                                                Tacentries.TyArg (
                                                                Extend.TUlist0 (
                                                                Extend.TUentry (Genarg.get_arg_tag wit_constr)), 
                                                                Tacentries.TyIdent ("]", 
                                                                Tacentries.TyArg (
                                                                Extend.TUlist1 (
                                                                Extend.TUentry (Genarg.get_arg_tag wit_constr)), 
                                                                Tacentries.TyNil)))))), 
           (fun f lH lt ist -> 
# 132 "g_newring.mlg"
        let (t,l) = List.sep_last lt in field_lookup f lH l t 
           )))]

