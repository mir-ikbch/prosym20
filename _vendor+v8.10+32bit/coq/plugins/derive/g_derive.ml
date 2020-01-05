
# 11 "g_derive.mlg"
 

open Stdarg



let __coq_plugin_name = "derive_plugin"
let _ = Mltop.add_known_module __coq_plugin_name

# 19 "g_derive.mlg"
 

let classify_derive_command _ = Vernacextend.(VtStartProof (Doesn'tGuaranteeOpacity,[]),VtLater)



let () = Vernacextend.vernac_extend ~command:"Derive" ~classifier:( classify_derive_command ) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Derive", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                     Vernacextend.TyTerminal ("SuchThat", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_constr), 
                                     Vernacextend.TyTerminal ("As", Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                    Vernacextend.TyNil)))))), 
         (let coqpp_body f suchthat lemma
         () ~st = let proof = (
# 27 "g_derive.mlg"
    fun ~pstate -> Some Derive.(start_deriving f suchthat lemma) 
                  ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun f
         suchthat lemma ~atts ~st -> coqpp_body f suchthat lemma
         (Attributes.unsupported_attributes atts) ~st), None))]

