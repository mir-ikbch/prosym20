let __coq_plugin_name = "tuto1_plugin"
let _ = Mltop.add_known_module __coq_plugin_name

# 3 "g_tuto1.mlg"
 

(* If we forget this line and include our own tactic definition using
  TACTIC EXTEND, as below, then we get the strange error message
  no implementation available for Tacentries, only when compiling
  theories/Loader.v
*)
open Ltac_plugin
open Attributes
open Pp
(* This module defines the types of arguments to be used in the
   EXTEND directives below, for example the string one. *)
open Stdarg



let () = Vernacextend.vernac_extend ~command:"HelloWorld" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Hello", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_string), 
                                     Vernacextend.TyNil)), (let coqpp_body s
                                                           () ~st = let () = 
                                                                    
# 21 "g_tuto1.mlg"
    Feedback.msg_notice (strbrk "Hello " ++ str s) 
                                                                     in st in fun s
                                                           ~atts ~st
                                                           -> coqpp_body s
                                                           (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"HelloAgain" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("HelloAgain", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_reference), 
                                     Vernacextend.TyNil)), (let coqpp_body r
                                                           () ~st = let () = 
                                                                    
# 31 "g_tuto1.mlg"
    Feedback.msg_notice
      (strbrk "Hello again " ++ Ppconstr.pr_qualid r)
                                                                     in st in fun r
                                                           ~atts ~st
                                                           -> coqpp_body r
                                                           (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"TakingConstr" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Cmd1", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                     Vernacextend.TyNil)), (let coqpp_body e
                                                           () ~st = let () = 
                                                                    
# 41 "g_tuto1.mlg"
    let _ = e in Feedback.msg_notice (strbrk "Cmd1 parsed something") 
                                                                     in st in fun e
                                                           ~atts ~st
                                                           -> coqpp_body e
                                                           (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"TakingConstr2" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Cmd2", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                     Vernacextend.TyNil)), (let coqpp_body e
                                                           () ~st = let () = 
                                                                    
# 53 "g_tuto1.mlg"
    let _ = Constrintern.interp_constr
    (Global.env())
    (* Make sure you don't use Evd.empty here, as this does not
      check consistency with existing universe constraints. *)
    (Evd.from_env (Global.env())) e in
    Feedback.msg_notice (strbrk "Cmd2 parsed something legitimate") 
                                                                     in st in fun e
                                                           ~atts ~st
                                                           -> coqpp_body e
                                                           (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"TakingConstr3" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Cmd3", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                     Vernacextend.TyNil)), (let coqpp_body e
                                                           () ~st = let () = 
                                                                    
# 68 "g_tuto1.mlg"
    let _ = Constrintern.interp_constr Environ.empty_env
      Evd.empty e in
      Feedback.msg_notice
        (strbrk "Cmd3 accepted something in the empty context")
                                                                     in st in fun e
                                                           ~atts ~st
                                                           -> coqpp_body e
                                                           (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"Define1" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Cmd4", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                     Vernacextend.TyNil))), (let coqpp_body i
                                                            e
                                                            poly ~st = 
                                                            let () = 
                                                            
# 87 "g_tuto1.mlg"
    let v = Constrintern.interp_constr (Global.env())
      (Evd.from_env (Global.env())) e in
    Simple_declare.packed_declare_definition ~poly i v 
                                                             in st in fun i e
                                                            ~atts ~st
                                                            -> coqpp_body i e
                                                            (Attributes.parse polymorphic atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"Check1" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Cmd5", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                     Vernacextend.TyNil)), (let coqpp_body e
                                                           () ~st = let () = 
                                                                    
# 94 "g_tuto1.mlg"
    let v = Constrintern.interp_constr (Global.env())
      (Evd.from_env (Global.env())) e in
    let (_, ctx) = v in
    let sigma = Evd.from_ctx ctx in
    Feedback.msg_notice
    (Printer.pr_econstr_env (Global.env()) sigma
       (Simple_check.simple_check1 v)) 
                                                                     in st in fun e
                                                           ~atts ~st
                                                           -> coqpp_body e
                                                           (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"Check2" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Cmd6", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                     Vernacextend.TyNil)), (let coqpp_body e
                                                           () ~st = let () = 
                                                                    
# 105 "g_tuto1.mlg"
    let v = Constrintern.interp_constr (Global.env())
      (Evd.from_env (Global.env())) e in
    let sigma, ty = Simple_check.simple_check2 v in
    Feedback.msg_notice
      (Printer.pr_econstr_env (Global.env()) sigma ty) 
                                                                     in st in fun e
                                                           ~atts ~st
                                                           -> coqpp_body e
                                                           (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"Check1" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Cmd7", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                     Vernacextend.TyNil)), (let coqpp_body e
                                                           () ~st = let () = 
                                                                    
# 114 "g_tuto1.mlg"
    let v = Constrintern.interp_constr (Global.env())
      (Evd.from_env (Global.env())) e in
    let (a, ctx) = v in
    let sigma = Evd.from_ctx ctx in
      Feedback.msg_notice
      (Printer.pr_econstr_env (Global.env()) sigma
         (Simple_check.simple_check3 v)) 
                                                                     in st in fun e
                                                           ~atts ~st
                                                           -> coqpp_body e
                                                           (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"ExamplePrint" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Cmd8", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_reference), 
                                     Vernacextend.TyNil)), (let coqpp_body r
                                                           () ~st = let () = 
                                                                    
# 130 "g_tuto1.mlg"
    let env = Global.env() in
    let sigma = Evd.from_env env in
    Feedback.msg_notice
    (Printer.pr_econstr_env env sigma
      (EConstr.of_constr
        (Simple_print.simple_body_access (Nametab.global r)))) 
                                                                     in st in fun r
                                                           ~atts ~st
                                                           -> coqpp_body r
                                                           (Attributes.unsupported_attributes atts) ~st), None))]

let () = Tacentries.tactic_extend __coq_plugin_name "my_intro" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("my_intro", Tacentries.TyArg (
                                                            Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                            Tacentries.TyNil)), 
           (fun i ist -> 
# 140 "g_tuto1.mlg"
    Tactics.introduction i 
           )))]

let () = Vernacextend.vernac_extend ~command:"ExploreProof" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Cmd9", 
                                     Vernacextend.TyNil), (let coqpp_body () ~st = 
                                                          let proof = (
                                                          
# 149 "g_tuto1.mlg"
    fun ~pstate ->
    Option.iter (fun (pstate : Proof_global.t) ->
        let sigma, env = Pfedit.get_current_context pstate in
        let pprf = Proof.partial_proof Proof_global.(give_me_the_proof pstate) in
        Feedback.msg_notice
          (Pp.prlist_with_sep Pp.fnl (Printer.pr_econstr_env env sigma) pprf)) pstate;
    pstate 
                                                          ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun ~atts
                                                          ~st
                                                          -> coqpp_body (Attributes.unsupported_attributes atts) ~st), None))]

