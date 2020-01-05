
# 11 "g_btauto.mlg"
 

open Ltac_plugin



let __coq_plugin_name = "btauto_plugin"
let _ = Mltop.add_known_module __coq_plugin_name
let () = Tacentries.tactic_extend __coq_plugin_name "btauto" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("btauto", Tacentries.TyNil), 
           (fun ist -> 
# 20 "g_btauto.mlg"
                    Refl_btauto.Btauto.tac 
           )))]

