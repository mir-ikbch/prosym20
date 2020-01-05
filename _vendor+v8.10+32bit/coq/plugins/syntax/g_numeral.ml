let __coq_plugin_name = "numeral_notation_plugin"
let _ = Mltop.add_known_module __coq_plugin_name

# 13 "g_numeral.mlg"
 

open Notation
open Numeral
open Pp
open Names
open Stdarg
open Pcoq.Prim

let pr_numnot_option = function
  | Nop -> mt ()
  | Warning n -> str "(warning after " ++ str n ++ str ")"
  | Abstract n -> str "(abstract after " ++ str n ++ str ")"



let (wit_numnotoption, numnotoption) = Vernacextend.vernac_argument_extend ~name:"numnotoption" 
                                       {
                                       Vernacextend.arg_parsing = Vernacextend.Arg_rules (
                                                                  [(Extend.Rule
                                                                    (
                                                                    Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "("))),
                                                                    (Extend.Atoken (CLexer.terminal "abstract"))),
                                                                    (Extend.Atoken (CLexer.terminal "after"))),
                                                                    (Extend.Aentry bigint)),
                                                                    (Extend.Atoken (CLexer.terminal ")"))),
                                                                    (fun _ n
                                                                    _ _ _
                                                                    loc -> 
                                                                    
# 33 "g_numeral.mlg"
                                                Abstract n 
                                                                    )));
                                                                  (Extend.Rule
                                                                   (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "("))),
                                                                    (Extend.Atoken (CLexer.terminal "warning"))),
                                                                    (Extend.Atoken (CLexer.terminal "after"))),
                                                                    (Extend.Aentry bigint)),
                                                                    (Extend.Atoken (CLexer.terminal ")"))),
                                                                   (fun _
                                                                   waft _ _ _
                                                                   loc -> 
                                                                   
# 32 "g_numeral.mlg"
                                                  Warning waft 
                                                                   )));
                                                                  (Extend.Rule
                                                                   (Extend.Stop,
                                                                   (fun
                                                                   loc -> 
                                                                   
# 31 "g_numeral.mlg"
           Nop 
                                                                   )))]);
                                       Vernacextend.arg_printer = fun env sigma -> 
                                       
# 30 "g_numeral.mlg"
               pr_numnot_option 
                                       ;
                                       }
let _ = (wit_numnotoption, numnotoption)

let () = Vernacextend.vernac_extend ~command:"NumeralNotation" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Numeral", 
                                     Vernacextend.TyTerminal ("Notation", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_reference), 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_reference), 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_reference), 
                                     Vernacextend.TyTerminal (":", Vernacextend.TyNonTerminal (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                                                   Vernacextend.TyNonTerminal (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_numnotoption), 
                                                                   Vernacextend.TyNil)))))))), 
         (let coqpp_body ty f g sc o locality ~st = let () = 
# 40 "g_numeral.mlg"
      vernac_numeral_notation (Locality.make_module_locality locality) ty f g (Id.to_string sc) o 
                                                     in st in fun ty
         f g sc o ~atts ~st -> coqpp_body ty f g sc o
         (Attributes.parse Attributes.locality atts) ~st), None))]

