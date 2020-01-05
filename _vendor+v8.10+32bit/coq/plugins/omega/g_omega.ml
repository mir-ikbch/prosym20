let __coq_plugin_name = "omega_plugin"
let _ = Mltop.add_known_module __coq_plugin_name

# 21 "g_omega.mlg"
 

open Ltac_plugin
open Names
open Coq_omega
open Stdarg

let eval_tactic name =
  let dp = DirPath.make (List.map Id.of_string ["PreOmega"; "omega"; "Coq"]) in
  let kn = KerName.make (ModPath.MPfile dp) (Label.make name) in
  let tac = Tacenv.interp_ltac kn in
  Tacinterp.eval_tactic tac

let omega_tactic l =
  let tacs = List.map
    (function
       | "nat" -> eval_tactic "zify_nat"
       | "positive" -> eval_tactic "zify_positive"
       | "N" -> eval_tactic "zify_N"
       | "Z" -> eval_tactic "zify_op"
       | s -> CErrors.user_err Pp.(str ("No Omega knowledge base for type "^s)))
    (Util.List.sort_uniquize String.compare l)
  in
  Tacticals.New.tclTHEN
    (Tacticals.New.tclREPEAT (Tacticals.New.tclTHENLIST tacs))
    (omega_solver)



let () = Tacentries.tactic_extend __coq_plugin_name "omega" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("omega", Tacentries.TyNil), 
           (fun ist -> 
# 51 "g_omega.mlg"
                    omega_tactic [] 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "omega'" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("omega", Tacentries.TyIdent ("with", 
                                                         Tacentries.TyArg (
                                                         Extend.TUlist1 (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_ident)), 
                                                         Tacentries.TyNil))), 
           (fun l ist -> 
# 56 "g_omega.mlg"
      omega_tactic (List.map Names.Id.to_string l) 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("omega", Tacentries.TyIdent ("with", 
                                                        Tacentries.TyIdent ("*", 
                                                        Tacentries.TyNil))), 
          (fun ist -> 
# 57 "g_omega.mlg"
                              omega_tactic ["nat";"positive";"N";"Z"] 
          )))]

