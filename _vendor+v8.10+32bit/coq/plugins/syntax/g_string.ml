let __coq_plugin_name = "string_notation_plugin"
let _ = Mltop.add_known_module __coq_plugin_name

# 13 "g_string.mlg"
 

open String_notation
open Names
open Stdarg



let () = Vernacextend.vernac_extend ~command:"StringNotation" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("String", 
                                     Vernacextend.TyTerminal ("Notation", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_reference), 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_reference), 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_reference), 
                                     Vernacextend.TyTerminal (":", Vernacextend.TyNonTerminal (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                   Vernacextend.TyNil))))))), 
         (let coqpp_body ty f g sc locality ~st = let () = 
# 24 "g_string.mlg"
      vernac_string_notation (Locality.make_module_locality locality) ty f g (Id.to_string sc) 
                                                   in st in fun ty
         f g sc ~atts ~st -> coqpp_body ty f g sc
         (Attributes.parse Attributes.locality atts) ~st), None))]

