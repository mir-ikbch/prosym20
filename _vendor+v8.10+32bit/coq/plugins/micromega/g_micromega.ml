
# 19 "g_micromega.mlg"
 

open Ltac_plugin
open Stdarg
open Tacarg



let __coq_plugin_name = "micromega_plugin"
let _ = Mltop.add_known_module __coq_plugin_name
let () = Tacentries.tactic_extend __coq_plugin_name "RED" ~level:0 [(
                                                                   Tacentries.TyML (
                                                                   Tacentries.TyIdent ("myred", 
                                                                   Tacentries.TyNil), 
                                                                   (fun ist
                                                                   -> 
                                                                   
# 30 "g_micromega.mlg"
                    Tactics.red_in_concl 
                                                                   )))]

let () = Tacentries.tactic_extend __coq_plugin_name "ISGROUND" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("is_ground", Tacentries.TyArg (
                                                             Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                                             Tacentries.TyNil)), 
           (fun t ist -> 
# 34 "g_micromega.mlg"
                                 Coq_micromega.is_ground_tac t 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "PsatzZ" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("psatz_Z", Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_int_or_var), 
                                                           Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                                           Tacentries.TyNil))), 
           (fun i t ist -> 
# 39 "g_micromega.mlg"
                                              (Coq_micromega.psatz_Z i
                                               (Tacinterp.tactic_of_value ist t))
                                               
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("psatz_Z", Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                                          Tacentries.TyNil)), 
          (fun t ist -> 
# 42 "g_micromega.mlg"
                               (Coq_micromega.psatz_Z (-1)) (Tacinterp.tactic_of_value ist t) 
          )))]

let () = Tacentries.tactic_extend __coq_plugin_name "Lia" ~level:0 [(
                                                                   Tacentries.TyML (
                                                                   Tacentries.TyIdent ("xlia", 
                                                                   Tacentries.TyArg (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                                                   Tacentries.TyNil)), 
                                                                   (fun t ist
                                                                   -> 
                                                                   
# 46 "g_micromega.mlg"
                              (Coq_micromega.xlia (Tacinterp.tactic_of_value ist t)) 
                                                                   )))]

let () = Tacentries.tactic_extend __coq_plugin_name "Nia" ~level:0 [(
                                                                   Tacentries.TyML (
                                                                   Tacentries.TyIdent ("xnlia", 
                                                                   Tacentries.TyArg (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                                                   Tacentries.TyNil)), 
                                                                   (fun t ist
                                                                   -> 
                                                                   
# 50 "g_micromega.mlg"
                              (Coq_micromega.xnlia (Tacinterp.tactic_of_value ist t)) 
                                                                   )))]

let () = Tacentries.tactic_extend __coq_plugin_name "NRA" ~level:0 [(
                                                                   Tacentries.TyML (
                                                                   Tacentries.TyIdent ("xnra", 
                                                                   Tacentries.TyArg (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                                                   Tacentries.TyNil)), 
                                                                   (fun t ist
                                                                   -> 
                                                                   
# 54 "g_micromega.mlg"
                             (Coq_micromega.nra (Tacinterp.tactic_of_value ist t))
                                                                   )))]

let () = Tacentries.tactic_extend __coq_plugin_name "NQA" ~level:0 [(
                                                                   Tacentries.TyML (
                                                                   Tacentries.TyIdent ("xnqa", 
                                                                   Tacentries.TyArg (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                                                   Tacentries.TyNil)), 
                                                                   (fun t ist
                                                                   -> 
                                                                   
# 58 "g_micromega.mlg"
                             (Coq_micromega.nqa (Tacinterp.tactic_of_value ist t))
                                                                   )))]

let () = Tacentries.tactic_extend __coq_plugin_name "Sos_Z" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("sos_Z", Tacentries.TyArg (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                                         Tacentries.TyNil)), 
           (fun t ist -> 
# 64 "g_micromega.mlg"
                             (Coq_micromega.sos_Z (Tacinterp.tactic_of_value ist t)) 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "Sos_Q" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("sos_Q", Tacentries.TyArg (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                                         Tacentries.TyNil)), 
           (fun t ist -> 
# 68 "g_micromega.mlg"
                               (Coq_micromega.sos_Q (Tacinterp.tactic_of_value ist t)) 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "Sos_R" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("sos_R", Tacentries.TyArg (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                                         Tacentries.TyNil)), 
           (fun t ist -> 
# 72 "g_micromega.mlg"
                               (Coq_micromega.sos_R (Tacinterp.tactic_of_value ist t)) 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "LRA_Q" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("lra_Q", Tacentries.TyArg (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                                         Tacentries.TyNil)), 
           (fun t ist -> 
# 76 "g_micromega.mlg"
                                (Coq_micromega.lra_Q (Tacinterp.tactic_of_value ist t)) 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "LRA_R" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("lra_R", Tacentries.TyArg (
                                                         Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                                         Tacentries.TyNil)), 
           (fun t ist -> 
# 80 "g_micromega.mlg"
                               (Coq_micromega.lra_R (Tacinterp.tactic_of_value ist t)) 
           )))]

let () = Tacentries.tactic_extend __coq_plugin_name "PsatzR" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("psatz_R", Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_int_or_var), 
                                                           Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                                           Tacentries.TyNil))), 
           (fun i t ist -> 
# 84 "g_micromega.mlg"
                                               (Coq_micromega.psatz_R i (Tacinterp.tactic_of_value ist t)) 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("psatz_R", Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                                          Tacentries.TyNil)), 
          (fun t ist -> 
# 85 "g_micromega.mlg"
                                 (Coq_micromega.psatz_R (-1) (Tacinterp.tactic_of_value ist t)) 
          )))]

let () = Tacentries.tactic_extend __coq_plugin_name "PsatzQ" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("psatz_Q", Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_int_or_var), 
                                                           Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                                           Tacentries.TyNil))), 
           (fun i t ist -> 
# 89 "g_micromega.mlg"
                                              (Coq_micromega.psatz_Q i (Tacinterp.tactic_of_value ist t)) 
           )));
         (Tacentries.TyML (Tacentries.TyIdent ("psatz_Q", Tacentries.TyArg (
                                                          Extend.TUentry (Genarg.get_arg_tag wit_tactic), 
                                                          Tacentries.TyNil)), 
          (fun t ist -> 
# 90 "g_micromega.mlg"
                                (Coq_micromega.psatz_Q (-1) (Tacinterp.tactic_of_value ist t)) 
          )))]

