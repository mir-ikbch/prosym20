let __coq_plugin_name = "demo_plugin"
let _ = Mltop.add_known_module __coq_plugin_name
let () = Vernacextend.vernac_extend ~command:"Cmd1" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Cmd1", 
                                     Vernacextend.TyNil), (let coqpp_body () ~st = 
                                                          let () = 
# 39 "demo.mlg"
                  () 
                                                           in st in fun ~atts
                                                          ~st
                                                          -> coqpp_body (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"Cmd2" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Cmd2", 
                                     Vernacextend.TyTerminal ("With", 
                                     Vernacextend.TyTerminal ("Some", 
                                     Vernacextend.TyTerminal ("Terminal", 
                                     Vernacextend.TyTerminal ("Parameters", 
                                     Vernacextend.TyNil))))), (let coqpp_body () ~st = 
                                                              let () = 
                                                              
# 106 "demo.mlg"
                                                        () 
                                                               in st in fun ~atts
                                                              ~st
                                                              -> coqpp_body (Attributes.unsupported_attributes atts) ~st), None))]


# 122 "demo.mlg"
 

open Stdarg



let () = Vernacextend.vernac_extend ~command:"Cmd3" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Cmd3", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_int), 
                                     Vernacextend.TyNil)), (let coqpp_body i
                                                           () ~st = let () = 
                                                                    
# 129 "demo.mlg"
                         () 
                                                                     in st in fun i
                                                           ~atts ~st
                                                           -> coqpp_body i
                                                           (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"Cmd4" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Cmd4", 
                                     Vernacextend.TyNonTerminal (Extend.TUlist0 (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_int)), 
                                     Vernacextend.TyNil)), (let coqpp_body l
                                                           () ~st = let () = 
                                                                    
# 186 "demo.mlg"
                              () 
                                                                     in st in fun l
                                                           ~atts ~st
                                                           -> coqpp_body l
                                                           (Attributes.unsupported_attributes atts) ~st), None))]


# 218 "demo.mlg"
 

open Ltac_plugin




# 237 "demo.mlg"
 

type type_5 = Foo_5 | Bar_5




# 256 "demo.mlg"
 

open Pp



let (wit_custom5, custom5) = Vernacextend.vernac_argument_extend ~name:"custom5" 
                             {
                             Vernacextend.arg_parsing = Vernacextend.Arg_rules (
                                                        [(Extend.Rule
                                                          (Extend.Next 
                                                           (Extend.Stop,
                                                           (Extend.Atoken (CLexer.terminal "Bar_5"))),
                                                          (fun _ loc -> 
# 264 "demo.mlg"
                   Bar_5 
                                                                    )));
                                                        (Extend.Rule
                                                         (Extend.Next 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (CLexer.terminal "Foo_5"))),
                                                         (fun _ loc -> 
# 263 "demo.mlg"
                   Foo_5 
                                                                    )))]);
                             Vernacextend.arg_printer = fun env sigma -> 
                             fun _ -> Pp.str "missing printer";
                             }
let _ = (wit_custom5, custom5)

let () = Vernacextend.vernac_extend ~command:"Cmd5" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Cmd5", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_custom5), 
                                     Vernacextend.TyNil)), (let coqpp_body x
                                                           () ~st = let () = 
                                                                    
# 284 "demo.mlg"
                             () 
                                                                     in st in fun x
                                                           ~atts ~st
                                                           -> coqpp_body x
                                                           (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"Cmd6" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Cmd6", 
                                     Vernacextend.TyNil), (let coqpp_body () ~st = 
                                                          let () = 
# 299 "demo.mlg"
                  Feedback.msg_notice (Pp.str "Everything is awesome!") 
                                                           in st in fun ~atts
                                                          ~st
                                                          -> coqpp_body (Attributes.unsupported_attributes atts) ~st), None))]


# 325 "demo.mlg"
 

open Summary.Local




# 341 "demo.mlg"
 

let counter = ref ~name:"counter" 0



let () = Vernacextend.vernac_extend ~command:"Cmd7" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Cmd7", 
                                     Vernacextend.TyNil), (let coqpp_body () ~st = 
                                                          let () = 
# 348 "demo.mlg"
                  counter := succ !counter;
                  Feedback.msg_notice (Pp.str "counter = " ++ Pp.str (string_of_int (!counter))) 
                                                           in st in fun ~atts
                                                          ~st
                                                          -> coqpp_body (Attributes.unsupported_attributes atts) ~st), None))]

let () = Tacentries.tactic_extend __coq_plugin_name "tactic1" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("tactic1", Tacentries.TyNil), 
           (fun ist -> 
# 353 "demo.mlg"
                     Proofview.tclUNIT () 
           )))]


# 358 "demo.mlg"
 

type custom = Foo_2 | Bar_2

let pr_custom _ _ _ = function
  | Foo_2 -> Pp.str "Foo_2"
  | Bar_2 -> Pp.str "Bar_2"



let (wit_custom2, custom2) = Tacentries.argument_extend ~name:"custom2" 
                             {
                             Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                      [(Extend.Rule
                                                        (Extend.Next 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (CLexer.terminal "Bar_2"))),
                                                        (fun _ loc -> 
# 370 "demo.mlg"
                   Bar_2 
                                                                    )));
                                                      (Extend.Rule
                                                       (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "Foo_2"))),
                                                       (fun _ loc -> 
# 369 "demo.mlg"
                   Foo_2 
                                                                    )))]);
                             Tacentries.arg_tag = None;
                             Tacentries.arg_intern = Tacentries.ArgInternFun (fun ist v -> (ist, v));
                             Tacentries.arg_subst = Tacentries.ArgSubstFun (fun s v -> v);
                             Tacentries.arg_interp = Tacentries.ArgInterpRet;
                             Tacentries.arg_printer = ((fun env sigma -> 
                                                      
# 368 "demo.mlg"
                                     pr_custom 
                                                      ), (fun env sigma -> 
                                                      
# 368 "demo.mlg"
                                     pr_custom 
                                                      ), (fun env sigma -> 
                                                      
# 368 "demo.mlg"
                                     pr_custom 
                                                      ));
                             }
let _ = (wit_custom2, custom2)

let () = Tacentries.tactic_extend __coq_plugin_name "tactic2" ~level:0 
         [(Tacentries.TyML (Tacentries.TyIdent ("tactic2", Tacentries.TyArg (
                                                           Extend.TUentry (Genarg.get_arg_tag wit_custom2), 
                                                           Tacentries.TyNil)), 
           (fun x ist -> 
# 374 "demo.mlg"
                                Proofview.tclUNIT () 
           )))]

