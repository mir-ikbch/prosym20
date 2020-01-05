
# 11 "g_ground.mlg"
 

open Ltac_plugin
open Formula
open Sequent
open Ground
open Goptions
open Tacmach.New
open Tacticals.New
open Tacinterp
open Stdarg
open Tacarg
open Attributes
open Pcoq.Prim



let __coq_plugin_name = "ground_plugin"
let _ = Mltop.add_known_module __coq_plugin_name

# 32 "g_ground.mlg"
 

let ground_depth=ref 3

let ()=
  let gdopt=
    { optdepr=false;
      optname="Firstorder Depth";
      optkey=["Firstorder";"Depth"];
      optread=(fun ()->Some !ground_depth);
      optwrite=
   (function
        None->ground_depth:=3
      |	Some i->ground_depth:=(max i 0))}
  in
    declare_int_option gdopt


let ()=
  let congruence_depth=ref 100 in
  let gdopt=
    { optdepr=true; (* noop *)
      optname="Congruence Depth";
      optkey=["Congruence";"Depth"];
      optread=(fun ()->Some !congruence_depth);
      optwrite=
   (function
        None->congruence_depth:=0
      |	Some i->congruence_depth:=(max i 0))}
  in
    declare_int_option gdopt

let default_intuition_tac =
  let tac _ _ = Auto.h_auto None [] None in
  let name = { Tacexpr.mltac_plugin = "ground_plugin"; mltac_tactic = "auto_with"; } in
  let entry = { Tacexpr.mltac_name = name; mltac_index = 0 } in
  Tacenv.register_ml_tactic name [| tac |];
  Tacexpr.TacML (CAst.make (entry, []))

let (set_default_solver, default_solver, print_default_solver) =
  Tactic_option.declare_tactic_option ~default:default_intuition_tac "Firstorder default solver"



let () = Vernacextend.vernac_extend ~command:"Firstorder_Set_Solver" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Set", 
                                     Vernacextend.TyTerminal ("Firstorder", 
                                     Vernacextend.TyTerminal ("Solver", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                     Vernacextend.TyNil)))), (let coqpp_body t
                                                             locality ~st = 
                                                             let () = 
                                                             
# 77 "g_ground.mlg"
                                                               
      set_default_solver
        (Locality.make_section_locality locality)
        (Tacintern.glob_tactic t)
  
                                                              in st in fun t
                                                             ~atts ~st
                                                             -> coqpp_body t
                                                             (Attributes.parse locality atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"Firstorder_Print_Solver" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Print", 
                                     Vernacextend.TyTerminal ("Firstorder", 
                                     Vernacextend.TyTerminal ("Solver", 
                                     Vernacextend.TyNil))), (let coqpp_body () ~st = 
                                                            let () = 
                                                            
# 85 "g_ground.mlg"
                                        
    Feedback.msg_notice
      (Pp.(++) (Pp.str"Firstorder solver tactic is ") (print_default_solver ())) 
                                                             in st in fun ~atts
                                                            ~st
                                                            -> coqpp_body (Attributes.unsupported_attributes atts) ~st), None))]


# 90 "g_ground.mlg"
 

let gen_ground_tac flag taco ids bases =
  let backup= !qflag in
  Proofview.tclOR begin
  Proofview.Goal.enter begin fun gl ->
      qflag:=flag;
      let solver=
        match taco with
            Some tac-> tac
          | None-> snd (default_solver ()) in
      let startseq k =
        Proofview.Goal.enter begin fun gl ->
        let seq=empty_seq !ground_depth in
        let seq, sigma = extend_with_ref_list (pf_env gl) (project gl) ids seq in
        let seq, sigma = extend_with_auto_hints (pf_env gl) (project gl) bases seq in
        tclTHEN (Proofview.Unsafe.tclEVARS sigma) (k seq)
        end
      in
      let result=ground_tac solver startseq in
      qflag := backup;
      result
  end
  end
  (fun (e, info) -> qflag := backup; Proofview.tclZERO ~info e)

(* special for compatibility with Intuition

let constant str = Coqlib.get_constr str

let defined_connectives=lazy
  [[],EvalConstRef (destConst (constant "core.not.type"));
   [],EvalConstRef (destConst (constant "core.iff.type"))]

let normalize_evaluables=
  onAllHypsAndConcl
    (function
         None->unfold_in_concl (Lazy.force defined_connectives)
       | Some id->
           unfold_in_hyp (Lazy.force defined_connectives)
           (Tacexpr.InHypType id)) *)

open Ppconstr
open Printer
let pr_firstorder_using_raw _ _ _ = Pptactic.pr_auto_using pr_qualid
let pr_firstorder_using_glob _ _ _ = Pptactic.pr_auto_using (Pputils.pr_or_var (fun x -> pr_global (snd x)))
let pr_firstorder_using_typed _ _ _ = Pptactic.pr_auto_using pr_global

let warn_deprecated_syntax =
  CWarnings.create ~name:"firstorder-deprecated-syntax" ~category:"deprecated"
    (fun () -> Pp.strbrk "Deprecated syntax; use \",\" as separator")



let (wit_firstorder_using, firstorder_using) = Tacentries.argument_extend ~name:"firstorder_using" 
                                               {
                                               Tacentries.arg_parsing = 
                                               Vernacextend.Arg_rules (
                                               [(Extend.Rule (Extend.Stop,
                                                 (fun loc -> 
# 155 "g_ground.mlg"
           [] 
                                                             )));
                                               (Extend.Rule
                                                (Extend.Next (Extend.Next 
                                                             (Extend.Next 
                                                             (Extend.Next 
                                                             (Extend.Stop,
                                                             (Extend.Atoken (CLexer.terminal "using"))),
                                                             (Extend.Aentry reference)),
                                                             (Extend.Aentry reference)),
                                                             (Extend.Alist0 (Extend.Aentry reference))),
                                                (fun l b a _ loc -> 
# 151 "g_ground.mlg"
                                                              
    warn_deprecated_syntax ();
    a::b::l
  
                                                                    )));
                                               (Extend.Rule
                                                (Extend.Next (Extend.Next 
                                                             (Extend.Next 
                                                             (Extend.Next 
                                                             (Extend.Stop,
                                                             (Extend.Atoken (CLexer.terminal "using"))),
                                                             (Extend.Aentry reference)),
                                                             (Extend.Atoken (CLexer.terminal ","))),
                                                             (Extend.Alist1sep ((Extend.Aentry reference), (Extend.Atoken (CLexer.terminal ","))))),
                                                (fun l _ a _ loc -> 
# 150 "g_ground.mlg"
                                                                 a::l 
                                                                    )));
                                               (Extend.Rule
                                                (Extend.Next (Extend.Next 
                                                             (Extend.Stop,
                                                             (Extend.Atoken (CLexer.terminal "using"))),
                                                             (Extend.Aentry reference)),
                                                (fun a _ loc -> 
# 149 "g_ground.mlg"
                                [a] 
                                                                )))]);
                                               Tacentries.arg_tag = Some
                                                                    (Geninterp.Val.List 
                                                                    (Geninterp.val_tag (Genarg.topwit wit_reference)));
                                               Tacentries.arg_intern = 
                                               Tacentries.ArgInternWit (Genarg.ListArg 
                                               (wit_reference));
                                               Tacentries.arg_subst = 
                                               Tacentries.ArgSubstWit (Genarg.ListArg 
                                               (wit_reference));
                                               Tacentries.arg_interp = 
                                               Tacentries.ArgInterpWit (Genarg.ListArg 
                                               (wit_reference));
                                               Tacentries.arg_printer = 
                                               ((fun env sigma -> 
# 147 "g_ground.mlg"
                   pr_firstorder_using_raw 
                                               ), (fun env sigma -> 
# 148 "g_ground.mlg"
                    pr_firstorder_using_glob 
                                               ), (fun env sigma -> 
# 146 "g_ground.mlg"
               pr_firstorder_using_typed 
                                               ));
                                               }
let _ = (wit_firstorder_using, firstorder_using)

let () = Tacentries.tactic_extend __coq_plugin_name "firstorder" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("firstorder", Tacentries.TyArg (
                                                              Extend.TUopt (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_tactic)), 
                                                              Tacentries.TyArg (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_firstorder_using), 
                                                              Tacentries.TyNil))), 
           (fun t l ist -> 
# 160 "g_ground.mlg"
        gen_ground_tac true (Option.map (tactic_of_value ist) t) l [] 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("firstorder", Tacentries.TyArg (
                                                             Extend.TUopt (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_tactic)), 
                                                             Tacentries.TyIdent ("with", 
                                                             Tacentries.TyArg (
                                                             Extend.TUlist1 (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_preident)), 
                                                             Tacentries.TyNil)))), 
          (fun t l ist -> 
# 162 "g_ground.mlg"
        gen_ground_tac true (Option.map (tactic_of_value ist) t) [] l 
          )));
         (Tacentries.TyML (Tacentries.TyIdent ("firstorder", Tacentries.TyArg (
                                                             Extend.TUopt (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_tactic)), 
                                                             Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_firstorder_using), 
                                                             Tacentries.TyIdent ("with", 
                                                             Tacentries.TyArg (
                                                             Extend.TUlist1 (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_preident)), 
                                                             Tacentries.TyNil))))), 
          (fun t l l' ist -> 
# 165 "g_ground.mlg"
        gen_ground_tac true (Option.map (tactic_of_value ist) t) l l' 
          )))]

let () = Tacentries.tactic_extend __coq_plugin_name "gintuition" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("gintuition", Tacentries.TyArg (
                                                              Extend.TUopt (
                                                              Extend.TUentry (Genarg.get_arg_tag wit_tactic)), 
                                                              Tacentries.TyNil)), 
           (fun t ist -> 
# 170 "g_ground.mlg"
       gen_ground_tac false (Option.map (tactic_of_value ist) t) [] [] 
           )))]

