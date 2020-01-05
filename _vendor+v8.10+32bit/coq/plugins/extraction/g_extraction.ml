
# 11 "g_extraction.mlg"
 

open Pcoq.Prim



let __coq_plugin_name = "extraction_plugin"
let _ = Mltop.add_known_module __coq_plugin_name

# 19 "g_extraction.mlg"
 

(* ML names *)

open Ltac_plugin
open Stdarg
open Pp
open Names
open Table
open Extract_env

let pr_mlname _ _ _ s = spc () ++ qs s



let (wit_mlname, mlname) = Tacentries.argument_extend ~name:"mlname" 
                           {
                           Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                    [(Extend.Rule
                                                      (Extend.Next (Extend.Stop,
                                                                   (Extend.Aentry string)),
                                                      (fun s loc -> 
# 38 "g_extraction.mlg"
                     s 
                                                                    )));
                                                    (Extend.Rule
                                                     (Extend.Next (Extend.Stop,
                                                                  (Extend.Aentry preident)),
                                                     (fun id loc -> 
# 37 "g_extraction.mlg"
                        id 
                                                                    )))]);
                           Tacentries.arg_tag = Some
                                                (Geninterp.val_tag (Genarg.topwit wit_string));
                           Tacentries.arg_intern = Tacentries.ArgInternWit (wit_string);
                           Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_string);
                           Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_string);
                           Tacentries.arg_printer = ((fun env sigma -> 
                                                    
# 36 "g_extraction.mlg"
               pr_mlname 
                                                    ), (fun env sigma -> 
                                                    
# 36 "g_extraction.mlg"
               pr_mlname 
                                                    ), (fun env sigma -> 
                                                    
# 36 "g_extraction.mlg"
               pr_mlname 
                                                    ));
                           }
let _ = (wit_mlname, mlname)


# 41 "g_extraction.mlg"
 

let pr_int_or_id _ _ _ = function
  | ArgInt i -> int i
  | ArgId id -> Id.print id



let (wit_int_or_id, int_or_id) = Tacentries.argument_extend ~name:"int_or_id" 
                                 {
                                 Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                          [(Extend.Rule
                                                            (Extend.Next 
                                                             (Extend.Stop,
                                                             (Extend.Aentry integer)),
                                                            (fun i loc -> 
# 52 "g_extraction.mlg"
                      ArgInt i 
                                                                    )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Aentry preident)),
                                                           (fun id loc -> 
# 51 "g_extraction.mlg"
                        ArgId (Id.of_string id) 
                                                                    )))]);
                                 Tacentries.arg_tag = None;
                                 Tacentries.arg_intern = Tacentries.ArgInternFun (fun ist v -> (ist, v));
                                 Tacentries.arg_subst = Tacentries.ArgSubstFun (fun s v -> v);
                                 Tacentries.arg_interp = Tacentries.ArgInterpRet;
                                 Tacentries.arg_printer = ((fun env sigma -> 
                                                          
# 50 "g_extraction.mlg"
               pr_int_or_id 
                                                          ), (fun env sigma -> 
                                                          
# 50 "g_extraction.mlg"
               pr_int_or_id 
                                                          ), (fun env sigma -> 
                                                          
# 50 "g_extraction.mlg"
               pr_int_or_id 
                                                          ));
                                 }
let _ = (wit_int_or_id, int_or_id)


# 55 "g_extraction.mlg"
 

let pr_language = function
  | Ocaml -> str "OCaml"
  | Haskell -> str "Haskell"
  | Scheme -> str "Scheme"
  | JSON -> str "JSON"

let warn_deprecated_ocaml_spelling =
  CWarnings.create ~name:"deprecated-ocaml-spelling" ~category:"deprecated"
    (fun () ->
      strbrk ("The spelling \"OCaml\" should be used instead of \"Ocaml\"."))



let (wit_language, language) = Vernacextend.vernac_argument_extend ~name:"language" 
                               {
                               Vernacextend.arg_parsing = Vernacextend.Arg_rules (
                                                          [(Extend.Rule
                                                            (Extend.Next 
                                                             (Extend.Stop,
                                                             (Extend.Atoken (CLexer.terminal "JSON"))),
                                                            (fun _ loc -> 
# 76 "g_extraction.mlg"
                  JSON 
                                                                    )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (CLexer.terminal "Scheme"))),
                                                           (fun _ loc -> 
# 75 "g_extraction.mlg"
                    Scheme 
                                                                    )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (CLexer.terminal "Haskell"))),
                                                           (fun _ loc -> 
# 74 "g_extraction.mlg"
                     Haskell 
                                                                    )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (CLexer.terminal "OCaml"))),
                                                           (fun _ loc -> 
# 73 "g_extraction.mlg"
                   Ocaml 
                                                                    )));
                                                          (Extend.Rule
                                                           (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (CLexer.terminal "Ocaml"))),
                                                           (fun _ loc -> 
# 72 "g_extraction.mlg"
                   let _ = warn_deprecated_ocaml_spelling () in Ocaml 
                                                                    )))]);
                               Vernacextend.arg_printer = fun env sigma -> 
                               
# 71 "g_extraction.mlg"
             pr_language 
                               ;
                               }
let _ = (wit_language, language)

let () = Vernacextend.vernac_extend ~command:"Extraction" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Extraction", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_global), 
                                     Vernacextend.TyNil)), (let coqpp_body x
                                                           () ~st = let () = 
                                                                    
# 83 "g_extraction.mlg"
                                  simple_extraction x 
                                                                     in st in fun x
                                                           ~atts ~st
                                                           -> coqpp_body x
                                                           (Attributes.unsupported_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Recursive", 
                                    Vernacextend.TyTerminal ("Extraction", 
                                    Vernacextend.TyNonTerminal (Extend.TUlist1 (
                                                                Extend.TUentry (Genarg.get_arg_tag wit_global)), 
                                    Vernacextend.TyNil))), (let coqpp_body l
                                                           () ~st = let () = 
                                                                    
# 84 "g_extraction.mlg"
                                                      full_extraction None l 
                                                                     in st in fun l
                                                           ~atts ~st
                                                           -> coqpp_body l
                                                           (Attributes.unsupported_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Extraction", 
                                    Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_string), 
                                    Vernacextend.TyNonTerminal (Extend.TUlist1 (
                                                                Extend.TUentry (Genarg.get_arg_tag wit_global)), 
                                    Vernacextend.TyNil))), (let coqpp_body f
                                                           l
                                                           () ~st = let () = 
                                                                    
# 88 "g_extraction.mlg"
       full_extraction (Some f) l 
                                                                     in st in fun f
                                                           l ~atts ~st
                                                           -> coqpp_body f l
                                                           (Attributes.unsupported_attributes atts) ~st), None));
         (Vernacextend.TyML (false, Vernacextend.TyTerminal ("Extraction", 
                                    Vernacextend.TyTerminal ("TestCompile", 
                                    Vernacextend.TyNonTerminal (Extend.TUlist1 (
                                                                Extend.TUentry (Genarg.get_arg_tag wit_global)), 
                                    Vernacextend.TyNil))), (let coqpp_body l
                                                           () ~st = let () = 
                                                                    
# 92 "g_extraction.mlg"
       extract_and_compile l 
                                                                     in st in fun l
                                                           ~atts ~st
                                                           -> coqpp_body l
                                                           (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"SeparateExtraction" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Separate", 
                                     Vernacextend.TyTerminal ("Extraction", 
                                     Vernacextend.TyNonTerminal (Extend.TUlist1 (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_global)), 
                                     Vernacextend.TyNil))), (let coqpp_body l
                                                            () ~st = 
                                                            let () = 
                                                            
# 98 "g_extraction.mlg"
       separate_extraction l 
                                                             in st in fun l
                                                            ~atts ~st
                                                            -> coqpp_body l
                                                            (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"ExtractionLibrary" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Extraction", 
                                     Vernacextend.TyTerminal ("Library", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                     Vernacextend.TyNil))), (let coqpp_body m
                                                            () ~st = 
                                                            let () = 
                                                            
# 104 "g_extraction.mlg"
       extraction_library false m 
                                                             in st in fun m
                                                            ~atts ~st
                                                            -> coqpp_body m
                                                            (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"RecursiveExtractionLibrary" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Recursive", 
                                     Vernacextend.TyTerminal ("Extraction", 
                                     Vernacextend.TyTerminal ("Library", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_ident), 
                                     Vernacextend.TyNil)))), (let coqpp_body m
                                                             () ~st = 
                                                             let () = 
                                                             
# 109 "g_extraction.mlg"
       extraction_library true m 
                                                              in st in fun m
                                                             ~atts ~st
                                                             -> coqpp_body m
                                                             (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"ExtractionLanguage" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Extraction", 
                                     Vernacextend.TyTerminal ("Language", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_language), 
                                     Vernacextend.TyNil))), (let coqpp_body l
                                                            () ~st = 
                                                            let () = 
                                                            
# 115 "g_extraction.mlg"
       extraction_language l 
                                                             in st in fun l
                                                            ~atts ~st
                                                            -> coqpp_body l
                                                            (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"ExtractionInline" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Extraction", 
                                     Vernacextend.TyTerminal ("Inline", 
                                     Vernacextend.TyNonTerminal (Extend.TUlist1 (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_global)), 
                                     Vernacextend.TyNil))), (let coqpp_body l
                                                            () ~st = 
                                                            let () = 
                                                            
# 121 "g_extraction.mlg"
       extraction_inline true l 
                                                             in st in fun l
                                                            ~atts ~st
                                                            -> coqpp_body l
                                                            (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"ExtractionNoInline" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Extraction", 
                                     Vernacextend.TyTerminal ("NoInline", 
                                     Vernacextend.TyNonTerminal (Extend.TUlist1 (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_global)), 
                                     Vernacextend.TyNil))), (let coqpp_body l
                                                            () ~st = 
                                                            let () = 
                                                            
# 126 "g_extraction.mlg"
       extraction_inline false l 
                                                             in st in fun l
                                                            ~atts ~st
                                                            -> coqpp_body l
                                                            (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"PrintExtractionInline" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Print", 
                                     Vernacextend.TyTerminal ("Extraction", 
                                     Vernacextend.TyTerminal ("Inline", 
                                     Vernacextend.TyNil))), (let coqpp_body () ~st = 
                                                            let () = 
                                                            
# 131 "g_extraction.mlg"
      Feedback.msg_notice (print_extraction_inline ()) 
                                                             in st in fun ~atts
                                                            ~st
                                                            -> coqpp_body (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"ResetExtractionInline" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Reset", 
                                     Vernacextend.TyTerminal ("Extraction", 
                                     Vernacextend.TyTerminal ("Inline", 
                                     Vernacextend.TyNil))), (let coqpp_body () ~st = 
                                                            let () = 
                                                            
# 136 "g_extraction.mlg"
       reset_extraction_inline () 
                                                             in st in fun ~atts
                                                            ~st
                                                            -> coqpp_body (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"ExtractionImplicit" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Extraction", 
                                     Vernacextend.TyTerminal ("Implicit", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_global), 
                                     Vernacextend.TyTerminal ("[", Vernacextend.TyNonTerminal (
                                                                   Extend.TUlist0 (
                                                                   Extend.TUentry (Genarg.get_arg_tag wit_int_or_id)), 
                                                                   Vernacextend.TyTerminal ("]", 
                                                                   Vernacextend.TyNil)))))), 
         (let coqpp_body r l () ~st = let () = 
# 142 "g_extraction.mlg"
       extraction_implicit r l 
                                       in st in fun r
         l ~atts ~st -> coqpp_body r l
         (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"ExtractionBlacklist" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Extraction", 
                                     Vernacextend.TyTerminal ("Blacklist", 
                                     Vernacextend.TyNonTerminal (Extend.TUlist1 (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_ident)), 
                                     Vernacextend.TyNil))), (let coqpp_body l
                                                            () ~st = 
                                                            let () = 
                                                            
# 148 "g_extraction.mlg"
       extraction_blacklist l 
                                                             in st in fun l
                                                            ~atts ~st
                                                            -> coqpp_body l
                                                            (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"PrintExtractionBlacklist" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Print", 
                                     Vernacextend.TyTerminal ("Extraction", 
                                     Vernacextend.TyTerminal ("Blacklist", 
                                     Vernacextend.TyNil))), (let coqpp_body () ~st = 
                                                            let () = 
                                                            
# 153 "g_extraction.mlg"
       Feedback.msg_notice (print_extraction_blacklist ()) 
                                                             in st in fun ~atts
                                                            ~st
                                                            -> coqpp_body (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"ResetExtractionBlacklist" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Reset", 
                                     Vernacextend.TyTerminal ("Extraction", 
                                     Vernacextend.TyTerminal ("Blacklist", 
                                     Vernacextend.TyNil))), (let coqpp_body () ~st = 
                                                            let () = 
                                                            
# 158 "g_extraction.mlg"
       reset_extraction_blacklist () 
                                                             in st in fun ~atts
                                                            ~st
                                                            -> coqpp_body (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"ExtractionConstant" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Extract", 
                                     Vernacextend.TyTerminal ("Constant", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_global), 
                                     Vernacextend.TyNonTerminal (Extend.TUlist0 (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_string)), 
                                     Vernacextend.TyTerminal ("=>", Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_mlname), 
                                                                    Vernacextend.TyNil)))))), 
         (let coqpp_body x idl y () ~st = let () = 
# 165 "g_extraction.mlg"
       extract_constant_inline false x idl y 
                                           in st in fun x
         idl y ~atts ~st -> coqpp_body x idl y
         (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"ExtractionInlinedConstant" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Extract", 
                                     Vernacextend.TyTerminal ("Inlined", 
                                     Vernacextend.TyTerminal ("Constant", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_global), 
                                     Vernacextend.TyTerminal ("=>", Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_mlname), 
                                                                    Vernacextend.TyNil)))))), 
         (let coqpp_body x y () ~st = let () = 
# 170 "g_extraction.mlg"
       extract_constant_inline true x [] y 
                                       in st in fun x
         y ~atts ~st -> coqpp_body x y
         (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"ExtractionInductive" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Extract", 
                                     Vernacextend.TyTerminal ("Inductive", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_global), 
                                     Vernacextend.TyTerminal ("=>", Vernacextend.TyNonTerminal (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_mlname), 
                                                                    Vernacextend.TyTerminal ("[", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUlist0 (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_mlname)), 
                                                                    Vernacextend.TyTerminal ("]", 
                                                                    Vernacextend.TyNonTerminal (
                                                                    Extend.TUopt (
                                                                    Extend.TUentry (Genarg.get_arg_tag wit_string)), 
                                                                    Vernacextend.TyNil))))))))), 
         (let coqpp_body x id idl o () ~st = let () = 
# 176 "g_extraction.mlg"
       extract_inductive x id idl o 
                                              in st in fun x
         id idl o ~atts ~st -> coqpp_body x id idl o
         (Attributes.unsupported_attributes atts) ~st), None))]

let () = Vernacextend.vernac_extend ~command:"ShowExtraction" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Show", 
                                     Vernacextend.TyTerminal ("Extraction", 
                                     Vernacextend.TyNil)), (let coqpp_body () ~st = 
                                                           let proof = (
                                                           
# 182 "g_extraction.mlg"
       fun ~pstate -> let () = show_extraction ~pstate in pstate 
                                                           ) ~pstate:st.Vernacstate.proof in { st with Vernacstate.proof } in fun ~atts
                                                           ~st
                                                           -> coqpp_body (Attributes.unsupported_attributes atts) ~st), None))]

