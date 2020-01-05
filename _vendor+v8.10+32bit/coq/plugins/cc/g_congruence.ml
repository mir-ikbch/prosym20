
# 11 "g_congruence.mlg"
 

open Ltac_plugin
open Cctac
open Stdarg



let __coq_plugin_name = "cc_plugin"
let _ = Mltop.add_known_module __coq_plugin_name
let () = Tacentries.tactic_extend __coq_plugin_name "cc" ~level:0 [(Tacentries.TyML (
                                                                    Tacentries.TyIdent ("congruence", 
                                                                    Tacentries.TyNil), 
                                                                    (fun ist
                                                                    -> 
                                                                    
# 24 "g_congruence.mlg"
                        congruence_tac 1000 [] 
                                                                    )));
                                                                  (Tacentries.TyML (
                                                                   Tacentries.TyIdent ("congruence", 
                                                                   Tacentries.TyArg (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_integer), 
                                                                   Tacentries.TyNil)), 
                                                                   (fun n ist
                                                                   -> 
                                                                   
# 25 "g_congruence.mlg"
                                   congruence_tac n [] 
                                                                   )));
                                                                  (Tacentries.TyML (
                                                                   Tacentries.TyIdent ("congruence", 
                                                                   Tacentries.TyIdent ("with", 
                                                                   Tacentries.TyArg (
                                                                   Extend.TUlist1 (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_constr)), 
                                                                   Tacentries.TyNil))), 
                                                                   (fun l ist
                                                                   -> 
                                                                   
# 26 "g_congruence.mlg"
                                                 congruence_tac 1000 l 
                                                                   )));
                                                                  (Tacentries.TyML (
                                                                   Tacentries.TyIdent ("congruence", 
                                                                   Tacentries.TyArg (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_integer), 
                                                                   Tacentries.TyIdent ("with", 
                                                                   Tacentries.TyArg (
                                                                   Extend.TUlist1 (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_constr)), 
                                                                   Tacentries.TyNil)))), 
                                                                   (fun n l
                                                                   ist -> 
                                                                   
# 28 "g_congruence.mlg"
     congruence_tac n l 
                                                                   )))]

let () = Tacentries.tactic_extend __coq_plugin_name "f_equal" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("f_equal", Tacentries.TyNil), 
           (fun ist -> 
# 32 "g_congruence.mlg"
                     f_equal 
           )))]

