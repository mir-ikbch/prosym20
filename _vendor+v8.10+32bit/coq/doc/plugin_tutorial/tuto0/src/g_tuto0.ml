let __coq_plugin_name = "tuto0_plugin"
let _ = Mltop.add_known_module __coq_plugin_name

# 3 "g_tuto0.mlg"
 

open Pp
open Ltac_plugin



let () = Vernacextend.vernac_extend ~command:"HelloWorld" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("HelloWorld", 
                                     Vernacextend.TyNil), (let coqpp_body () ~st = 
                                                          let () = 
# 11 "g_tuto0.mlg"
                        Feedback.msg_notice (strbrk Tuto0_main.message) 
                                                           in st in fun ~atts
                                                          ~st
                                                          -> coqpp_body (Attributes.unsupported_attributes atts) ~st), None))]

let () = Tacentries.tactic_extend __coq_plugin_name "hello_world_tactic" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("hello_world", Tacentries.TyNil), 
           (fun ist -> 
# 16 "g_tuto0.mlg"
    let _ = Feedback.msg_notice (str Tuto0_main.message) in
    Tacticals.New.tclIDTAC 
           )))]

