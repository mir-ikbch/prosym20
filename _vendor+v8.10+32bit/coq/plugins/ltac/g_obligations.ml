
# 15 "g_obligations.mlg"
 

open Constrexpr
open Constrexpr_ops
open Stdarg
open Tacarg
open Extraargs

let (set_default_tactic, get_default_tactic, print_default_tactic) =
  Tactic_option.declare_tactic_option "Program tactic"

let () =
  (* Delay to recover the tactic imperatively *)
  let tac = Proofview.tclBIND (Proofview.tclUNIT ()) begin fun () ->
    snd (get_default_tactic ())
  end in
  Obligations.default_tactic := tac

let with_tac f tac =
  let env = Genintern.empty_glob_sign (Global.env ()) in
  let tac = match tac with
  | None -> None
  | Some tac ->
    let tac = Genarg.in_gen (Genarg.rawwit wit_ltac) tac in
    let _, tac = Genintern.generic_intern env tac in
    Some tac
  in
  f tac

(* We define new entries for programs, with the use of this module
 * Subtac. These entries are named Subtac.<foo>
 *)

module Tactic = Pltac

open Pcoq

let sigref loc = mkRefC (Libnames.qualid_of_string ~loc "Coq.Init.Specif.sig")

type 'a withtac_argtype = (Tacexpr.raw_tactic_expr option, 'a) Genarg.abstract_argument_type

let wit_withtac : Tacexpr.raw_tactic_expr option Genarg.uniform_genarg_type =
  Genarg.create_arg "withtac"

let withtac = Pcoq.create_generic_entry Pcoq.utactic "withtac" (Genarg.rawwit wit_withtac)



let _ = let () =
        Pcoq.grammar_extend withtac None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 68 "g_obligations.mlg"
             None 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("with")))),
                             (Extend.Aentry Tactic.tactic)),
                (fun t _ loc -> 
# 67 "g_obligations.mlg"
                                       Some t 
                                ))])])
        in let () =
        Pcoq.grammar_extend Constr.closed_binder None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                        (Extend.Aentry Prim.name)),
                                                        (Extend.Atoken (Tok.PKEYWORD (":")))),
                                                        (Extend.Aentry Constr.lconstr)),
                                                        (Extend.Atoken (Tok.PKEYWORD ("|")))),
                                           (Extend.Aentry Constr.lconstr)),
                              (Extend.Atoken (Tok.PKEYWORD (")")))),
                 (fun _ c _ t _ id _ loc -> 
# 72 "g_obligations.mlg"
                                                                                
          let typ = mkAppC (sigref loc, [mkLambdaC ([id], default_binder_kind, t, c)]) in
          [CLocalAssum ([id], default_binder_kind, typ)] 
                                            ))])])
        in ()


# 79 "g_obligations.mlg"
 

open Obligations

let obligation ~pstate obl tac = Some (with_tac (fun t -> Obligations.obligation ~ontop:pstate obl t) tac)
let next_obligation ~pstate obl tac = Some (with_tac (fun t -> Obligations.next_obligation ~ontop:pstate obl t) tac)

let classify_obbl _ = Vernacextend.(VtStartProof (Doesn'tGuaranteeOpacity,[]), VtLater)



let () = Vernacextend.vernac_extend ~command:"Obligations" ~classifier:( classify_obbl ) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Obligation", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_integer), 
                                     Vernacextend.TyTerminal ("of", Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                    Vernacextend.TyTerminal (":", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_lglob), 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_withtac), 
                                                                    Vernacextend.TyNil))))))), 
         (let coqpp_body num name t tac
         () ~st = let proof = (
# 92 "g_obligations.mlg"
      obligation (num, Some name, Some t) tac 
                  ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun num
         name t tac ~atts ~st -> coqpp_body num name t tac
         (Attributes.unsupported_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Obligation", 
                                    Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_integer), 
                                    Vernacextend.TyTerminal ("of", Vernacextend.TyNonTerminal (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                   Vernacextend.TyNonTerminal (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_withtac), 
                                                                   Vernacextend.TyNil))))), 
         (let coqpp_body num name tac
         () ~st = let proof = (
# 94 "g_obligations.mlg"
      obligation (num, Some name, None) tac 
                  ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun num
         name tac ~atts ~st -> coqpp_body num name tac
         (Attributes.unsupported_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Obligation", 
                                    Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_integer), 
                                    Vernacextend.TyTerminal (":", Vernacextend.TyNonTerminal (
                                                                  Extend.TUentry (Genarg.get_arg_tag wit_lglob), 
                                                                  Vernacextend.TyNonTerminal (
                                                                  Extend.TUentry (Genarg.get_arg_tag wit_withtac), 
                                                                  Vernacextend.TyNil))))), 
         (let coqpp_body num t tac
         () ~st = let proof = (
# 96 "g_obligations.mlg"
      obligation (num, None, Some t) tac 
                  ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun num
         t tac ~atts ~st -> coqpp_body num t tac
         (Attributes.unsupported_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Obligation", 
                                    Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_integer), 
                                    Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_withtac), 
                                    Vernacextend.TyNil))), (let coqpp_body num
                                                           tac
                                                           () ~st = let proof = (
                                                                    
# 98 "g_obligations.mlg"
      obligation (num, None, None) tac 
                                                                    ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun num
                                                           tac ~atts ~st
                                                           -> coqpp_body num
                                                           tac
                                                           (Attributes.unsupported_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Next", 
                                    Vernacextend.TyTerminal ("Obligation", 
                                    Vernacextend.TyTerminal ("of", Vernacextend.TyNonTerminal (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                   Vernacextend.TyNonTerminal (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_withtac), 
                                                                   Vernacextend.TyNil))))), 
         (let coqpp_body name tac
         () ~st = let proof = (
# 100 "g_obligations.mlg"
      next_obligation (Some name) tac 
                  ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun name
         tac ~atts ~st -> coqpp_body name tac
         (Attributes.unsupported_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Next", 
                                    Vernacextend.TyTerminal ("Obligation", 
                                    Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_withtac), 
                                    Vernacextend.TyNil))), (let coqpp_body tac
                                                           () ~st = let proof = (
                                                                    
# 101 "g_obligations.mlg"
                                                       next_obligation None tac 
                                                                    ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun tac
                                                           ~atts ~st
                                                           -> coqpp_body tac
                                                           (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"Solve_Obligation" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Solve", 
                                     Vernacextend.TyTerminal ("Obligation", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_integer), 
                                     Vernacextend.TyTerminal ("of", Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                    Vernacextend.TyTerminal ("with", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                                                    Vernacextend.TyNil))))))), 
         (let coqpp_body num name t () ~st = let () = 
# 106 "g_obligations.mlg"
      try_solve_obligation num (Some name) (Some (Tacinterp.interp t)) 
                                              in st in fun num
         name t ~atts ~st -> coqpp_body num name t
         (Attributes.unsupported_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Solve", 
                                    Vernacextend.TyTerminal ("Obligation", 
                                    Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_integer), 
                                    Vernacextend.TyTerminal ("with", 
                                    Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                    Vernacextend.TyNil))))), (let coqpp_body num
                                                             t
                                                             () ~st = 
                                                             let () = 
                                                             
# 108 "g_obligations.mlg"
      try_solve_obligation num None (Some (Tacinterp.interp t)) 
                                                              in st in fun num
                                                             t ~atts ~st
                                                             -> coqpp_body num
                                                             t
                                                             (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"Solve_Obligations" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Solve", 
                                     Vernacextend.TyTerminal ("Obligations", 
                                     Vernacextend.TyTerminal ("of", Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                    Vernacextend.TyTerminal ("with", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                                                    Vernacextend.TyNil)))))), 
         (let coqpp_body name t () ~st = let () = 
# 113 "g_obligations.mlg"
      try_solve_obligations (Some name) (Some (Tacinterp.interp t)) 
                                          in st in fun name
         t ~atts ~st -> coqpp_body name t
         (Attributes.unsupported_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Solve", 
                                    Vernacextend.TyTerminal ("Obligations", 
                                    Vernacextend.TyTerminal ("with", 
                                    Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                    Vernacextend.TyNil)))), (let coqpp_body t
                                                            () ~st = 
                                                            let () = 
                                                            
# 115 "g_obligations.mlg"
      try_solve_obligations None (Some (Tacinterp.interp t)) 
                                                             in st in fun t
                                                            ~atts ~st
                                                            -> coqpp_body t
                                                            (Attributes.unsupported_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Solve", 
                                    Vernacextend.TyTerminal ("Obligations", 
                                    Vernacextend.TyNil)), (let coqpp_body () ~st = 
                                                          let () = 
# 117 "g_obligations.mlg"
      try_solve_obligations None None 
                                                           in st in fun ~atts
                                                          ~st
                                                          -> coqpp_body (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"Solve_All_Obligations" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Solve", 
                                     Vernacextend.TyTerminal ("All", 
                                     Vernacextend.TyTerminal ("Obligations", 
                                     Vernacextend.TyTerminal ("with", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                     Vernacextend.TyNil))))), (let coqpp_body t
                                                              () ~st = 
                                                              let () = 
                                                              
# 122 "g_obligations.mlg"
      solve_all_obligations (Some (Tacinterp.interp t)) 
                                                               in st in fun t
                                                              ~atts ~st
                                                              -> coqpp_body t
                                                              (Attributes.unsupported_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Solve", 
                                    Vernacextend.TyTerminal ("All", Vernacextend.TyTerminal ("Obligations", 
                                                                    Vernacextend.TyNil))), 
         (let coqpp_body () ~st = let () = 
# 124 "g_obligations.mlg"
      solve_all_obligations None 
                                   in st in fun ~atts
         ~st -> coqpp_body (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"Admit_Obligations" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Admit", 
                                     Vernacextend.TyTerminal ("Obligations", 
                                     Vernacextend.TyTerminal ("of", Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                    Vernacextend.TyNil)))), 
         (let coqpp_body name () ~st = let () = 
# 128 "g_obligations.mlg"
                                                  admit_obligations (Some name) 
                                        in st in fun name
         ~atts ~st -> coqpp_body name
         (Attributes.unsupported_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Admit", 
                                    Vernacextend.TyTerminal ("Obligations", 
                                    Vernacextend.TyNil)), (let coqpp_body () ~st = 
                                                          let () = 
# 129 "g_obligations.mlg"
                                 admit_obligations None 
                                                           in st in fun ~atts
                                                          ~st
                                                          -> coqpp_body (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"Set_Solver" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Obligation", 
                                     Vernacextend.TyTerminal ("Tactic", 
                                     Vernacextend.TyTerminal (":=", Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                                                    Vernacextend.TyNil)))), 
         (let coqpp_body t locality ~st = let () = 
# 133 "g_obligations.mlg"
                                                                                    
        set_default_tactic
          (Locality.make_section_locality locality)
          (Tacintern.glob_tactic t);
  
                                           in st in fun t
         ~atts ~st -> coqpp_body t
         (Attributes.parse Attributes.locality atts) ~st), None))]


# 140 "g_obligations.mlg"
 

open Pp



let () = Vernacextend.vernac_extend ~command:"Show_Solver" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Show", 
                                     Vernacextend.TyTerminal ("Obligation", 
                                     Vernacextend.TyTerminal ("Tactic", 
                                     Vernacextend.TyNil))), (let coqpp_body () ~st = 
                                                            let () = 
                                                            
# 147 "g_obligations.mlg"
                                       
    Feedback.msg_notice (str"Program obligation tactic is " ++ print_default_tactic ()) 
                                                             in st in fun ~atts
                                                            ~st
                                                            -> coqpp_body (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"Show_Obligations" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Obligations", 
                                     Vernacextend.TyTerminal ("of", Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                    Vernacextend.TyNil))), 
         (let coqpp_body name () ~st = let () = 
# 152 "g_obligations.mlg"
                                          show_obligations (Some name) 
                                        in st in fun name
         ~atts ~st -> coqpp_body name
         (Attributes.unsupported_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Obligations", 
                                    Vernacextend.TyNil), (let coqpp_body () ~st = 
                                                         let () = 
# 153 "g_obligations.mlg"
                         show_obligations None 
                                                          in st in fun ~atts
                                                         ~st
                                                         -> coqpp_body (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"Show_Preterm" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Preterm", 
                                     Vernacextend.TyTerminal ("of", Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                    Vernacextend.TyNil))), 
         (let coqpp_body name () ~st = let () = 
# 157 "g_obligations.mlg"
                                      Feedback.msg_notice (show_term (Some name)) 
                                        in st in fun name
         ~atts ~st -> coqpp_body name
         (Attributes.unsupported_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Preterm", 
                                    Vernacextend.TyNil), (let coqpp_body () ~st = 
                                                         let () = 
# 158 "g_obligations.mlg"
                     Feedback.msg_notice (show_term None) 
                                                          in st in fun ~atts
                                                         ~st
                                                         -> coqpp_body (Attributes.unsupported_attributes atts) ~st), None))]


# 161 "g_obligations.mlg"
 

(* Declare a printer for the content of Program tactics *)
let () =
  let printer env sigma _ _ _ = function
  | None -> mt ()
  | Some tac -> str "with" ++ spc () ++ Pptactic.pr_raw_tactic env sigma tac
  in
  Pptactic.declare_extra_vernac_genarg_pprule wit_withtac printer



