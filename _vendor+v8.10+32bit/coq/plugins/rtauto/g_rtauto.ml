
# 11 "g_rtauto.mlg"
 

open Ltac_plugin



let __coq_plugin_name = "rtauto_plugin"
let _ = Mltop.add_known_module __coq_plugin_name
let () = Tacentries.tactic_extend __coq_plugin_name "rtauto" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("rtauto", Tacentries.TyNil), 
           (fun ist -> 
# 20 "g_rtauto.mlg"
                    Refl_tauto.rtauto_tac 
           )))]

