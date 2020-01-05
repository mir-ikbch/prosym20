
# 11 "extraargs.mlg"
 

open Pp
open Stdarg
open Tacarg
open Pcoq.Prim
open Pcoq.Constr
open Names
open Tacmach
open Tacexpr
open Taccoerce
open Tacinterp
open Locus

(** Adding scopes for generic arguments not defined through ARGUMENT EXTEND *)

let create_generic_quotation name e wit =
  let inject (loc, v) = Tacexpr.TacGeneric (Genarg.in_gen (Genarg.rawwit wit) v) in
  Tacentries.create_ltac_quotation name inject (e, None)

let () = create_generic_quotation "integer" Pcoq.Prim.integer Stdarg.wit_int
let () = create_generic_quotation "string" Pcoq.Prim.string Stdarg.wit_string

let () = create_generic_quotation "ident" Pcoq.Prim.ident Stdarg.wit_ident
let () = create_generic_quotation "reference" Pcoq.Prim.reference Stdarg.wit_ref
let () = create_generic_quotation "uconstr" Pcoq.Constr.lconstr Stdarg.wit_uconstr
let () = create_generic_quotation "constr" Pcoq.Constr.lconstr Stdarg.wit_constr
let () = create_generic_quotation "ipattern" Pltac.simple_intropattern wit_simple_intropattern
let () = create_generic_quotation "open_constr" Pcoq.Constr.lconstr Stdarg.wit_open_constr
let () =
  let inject (loc, v) = Tacexpr.Tacexp v in
  Tacentries.create_ltac_quotation "ltac" inject (Pltac.tactic_expr, Some 5)

(** Backward-compatible tactic notation entry names *)

let () =
  let register name entry = Tacentries.register_tactic_notation_entry name entry in
  register "hyp" wit_var;
  register "simple_intropattern" wit_simple_intropattern;
  register "integer" wit_integer;
  register "reference" wit_ref;
  ()

(* Rewriting orientation *)

let _ =
  Mltop.declare_cache_obj
    (fun () -> Metasyntax.add_token_obj "<-";
               Metasyntax.add_token_obj "->")
    "ltac_plugin"

let pr_orient _prc _prlc _prt = function
  | true -> Pp.mt ()
  | false -> Pp.str " <-"



let (wit_orient, orient) = Tacentries.argument_extend ~name:"orient" 
                           {
                           Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                    [(Extend.Rule
                                                      (Extend.Stop,
                                                      (fun loc -> 
# 71 "extraargs.mlg"
           true 
                                                                  )));
                                                    (Extend.Rule
                                                     (Extend.Next (Extend.Stop,
                                                                  (Extend.Atoken (CLexer.terminal "<-"))),
                                                     (fun _ loc -> 
# 70 "extraargs.mlg"
                false 
                                                                   )));
                                                    (Extend.Rule
                                                     (Extend.Next (Extend.Stop,
                                                                  (Extend.Atoken (CLexer.terminal "->"))),
                                                     (fun _ loc -> 
# 69 "extraargs.mlg"
                true 
                                                                   )))]);
                           Tacentries.arg_tag = Some
                                                (Geninterp.val_tag (Genarg.topwit wit_bool));
                           Tacentries.arg_intern = Tacentries.ArgInternWit (wit_bool);
                           Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_bool);
                           Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_bool);
                           Tacentries.arg_printer = ((fun env sigma -> 
                                                    
# 68 "extraargs.mlg"
                                                  pr_orient 
                                                    ), (fun env sigma -> 
                                                    
# 68 "extraargs.mlg"
                                                  pr_orient 
                                                    ), (fun env sigma -> 
                                                    
# 68 "extraargs.mlg"
                                                  pr_orient 
                                                    ));
                           }
let _ = (wit_orient, orient)


# 74 "extraargs.mlg"
 

let pr_int _ _ _ i = Pp.int i

let _natural = Pcoq.Prim.natural



let (wit_natural, natural) = Tacentries.argument_extend ~name:"natural" 
                             {
                             Tacentries.arg_parsing = Vernacextend.Arg_alias (_natural);
                             Tacentries.arg_tag = Some
                                                  (Geninterp.val_tag (Genarg.topwit wit_int));
                             Tacentries.arg_intern = Tacentries.ArgInternWit (wit_int);
                             Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_int);
                             Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_int);
                             Tacentries.arg_printer = ((fun env sigma -> 
                                                      
# 82 "extraargs.mlg"
                                                  pr_int 
                                                      ), (fun env sigma -> 
                                                      
# 82 "extraargs.mlg"
                                                  pr_int 
                                                      ), (fun env sigma -> 
                                                      
# 82 "extraargs.mlg"
                                                  pr_int 
                                                      ));
                             }
let _ = (wit_natural, natural)


# 86 "extraargs.mlg"
 

let pr_orient = pr_orient () () ()

let pr_int_list = Pp.pr_sequence Pp.int
let pr_int_list_full _prc _prlc _prt l = pr_int_list l

let pr_occurrences _prc _prlc _prt l =
  match l with
    | ArgArg x -> pr_int_list x
    | ArgVar { CAst.loc = loc; v=id } -> Id.print id

let occurrences_of = function
  | [] -> NoOccurrences
  | n::_ as nl when n < 0 -> AllOccurrencesBut (List.map abs nl)
  | nl ->
      if List.exists (fun n -> n < 0) nl then
        CErrors.user_err Pp.(str "Illegal negative occurrence number.");
      OnlyOccurrences nl

let coerce_to_int v = match Value.to_int v with
  | None -> raise (CannotCoerceTo "an integer")
  | Some n -> n

let int_list_of_VList v = match Value.to_list v with
| Some l -> List.map (fun n -> coerce_to_int n) l
| _ -> raise (CannotCoerceTo "an integer")

let interp_occs ist gl l =
  match l with
    | ArgArg x -> x
    | ArgVar ({ CAst.v = id } as locid) ->
        (try int_list_of_VList (Id.Map.find id ist.lfun)
          with Not_found | CannotCoerceTo _ -> [interp_int ist locid])
let interp_occs ist gl l =
  Tacmach.project gl , interp_occs ist gl l

let glob_occs ist l = l

let subst_occs evm l = l



let (wit_occurrences, occurrences) = Tacentries.argument_extend ~name:"occurrences" 
                                     {
                                     Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                              [(Extend.Rule
                                                                (Extend.Next 
                                                                 (Extend.Stop,
                                                                 (Extend.Aentry var)),
                                                                (fun id
                                                                loc -> 
                                                                
# 141 "extraargs.mlg"
                   ArgVar id 
                                                                )));
                                                              (Extend.Rule
                                                               (Extend.Next 
                                                                (Extend.Stop,
                                                                (Extend.Alist1 (Extend.Aentry integer))),
                                                               (fun l loc ->
                                                               
# 140 "extraargs.mlg"
                              ArgArg l 
                                                               )))]);
                                     Tacentries.arg_tag = Some
                                                          (Geninterp.Val.List 
                                                          (Geninterp.val_tag (Genarg.topwit wit_int)));
                                     Tacentries.arg_intern = Tacentries.ArgInternFun ((fun f ist v -> (ist, f ist v)) (
                                                             
# 134 "extraargs.mlg"
                  glob_occs 
                                                             ));
                                     Tacentries.arg_subst = Tacentries.ArgSubstFun (
                                                            
# 135 "extraargs.mlg"
                   subst_occs 
                                                            );
                                     Tacentries.arg_interp = Tacentries.ArgInterpLegacy (
                                                             
# 133 "extraargs.mlg"
                   interp_occs 
                                                             );
                                     Tacentries.arg_printer = ((fun env sigma -> 
                                                              
# 137 "extraargs.mlg"
                   pr_occurrences 
                                                              ), (fun env sigma -> 
                                                              
# 138 "extraargs.mlg"
                    pr_occurrences 
                                                              ), (fun env sigma -> 
                                                              
# 131 "extraargs.mlg"
               pr_int_list_full 
                                                              ));
                                     }
let _ = (wit_occurrences, occurrences)


# 144 "extraargs.mlg"
 

let pr_occurrences = pr_occurrences () () ()

let pr_gen env sigma prc _prlc _prtac x = prc env sigma x

let pr_globc env sigma _prc _prlc _prtac (_,glob) =
  Printer.pr_glob_constr_env env glob

let interp_glob ist gl (t,_) = Tacmach.project gl , (ist,t)

let glob_glob = Tacintern.intern_constr

let pr_lconstr env sigma _ prc _ c = prc env sigma c

let subst_glob = Tacsubst.subst_glob_constr_and_expr



let (wit_glob, glob) = Tacentries.argument_extend ~name:"glob" {
                                                               Tacentries.arg_parsing = 
                                                               Vernacextend.Arg_alias (constr);
                                                               Tacentries.arg_tag = 
                                                               None;
                                                               Tacentries.arg_intern = 
                                                               Tacentries.ArgInternFun ((fun f ist v -> (ist, f ist v)) (
                                                               
# 167 "extraargs.mlg"
                     glob_glob 
                                                               ));
                                                               Tacentries.arg_subst = 
                                                               Tacentries.ArgSubstFun (
                                                               
# 168 "extraargs.mlg"
                      subst_glob 
                                                               );
                                                               Tacentries.arg_interp = 
                                                               Tacentries.ArgInterpLegacy (
                                                               
# 166 "extraargs.mlg"
                      interp_glob 
                                                               );
                                                               Tacentries.arg_printer = 
                                                               ((fun env sigma -> 
                                                               
# 170 "extraargs.mlg"
                      pr_gen env sigma 
                                                               ), (fun env sigma -> 
                                                               
# 171 "extraargs.mlg"
                       pr_gen env sigma 
                                                               ), (fun env sigma -> 
                                                               
# 164 "extraargs.mlg"
                 pr_globc env sigma 
                                                               ));
                                                               }
let _ = (wit_glob, glob)


# 175 "extraargs.mlg"
 

let l_constr = Pcoq.Constr.lconstr



let (wit_lconstr, lconstr) = Tacentries.argument_extend ~name:"lconstr" 
                             {
                             Tacentries.arg_parsing = Vernacextend.Arg_alias (l_constr);
                             Tacentries.arg_tag = Some
                                                  (Geninterp.val_tag (Genarg.topwit wit_constr));
                             Tacentries.arg_intern = Tacentries.ArgInternWit (wit_constr);
                             Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_constr);
                             Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_constr);
                             Tacentries.arg_printer = ((fun env sigma -> 
                                                      
# 183 "extraargs.mlg"
                 pr_lconstr env sigma 
                                                      ), (fun env sigma -> 
                                                      
# 183 "extraargs.mlg"
                 pr_lconstr env sigma 
                                                      ), (fun env sigma -> 
                                                      
# 183 "extraargs.mlg"
                 pr_lconstr env sigma 
                                                      ));
                             }
let _ = (wit_lconstr, lconstr)

let (wit_lglob, lglob) = Tacentries.argument_extend ~name:"lglob" {
                                                                  Tacentries.arg_parsing = 
                                                                  Vernacextend.Arg_alias (lconstr);
                                                                  Tacentries.arg_tag = 
                                                                  Some
                                                                  (Geninterp.val_tag (Genarg.topwit wit_glob));
                                                                  Tacentries.arg_intern = 
                                                                  Tacentries.ArgInternFun ((fun f ist v -> (ist, f ist v)) (
                                                                  
# 192 "extraargs.mlg"
                     glob_glob 
                                                                  ));
                                                                  Tacentries.arg_subst = 
                                                                  Tacentries.ArgSubstFun (
                                                                  
# 193 "extraargs.mlg"
                      subst_glob 
                                                                  );
                                                                  Tacentries.arg_interp = 
                                                                  Tacentries.ArgInterpLegacy (
                                                                  
# 191 "extraargs.mlg"
                      interp_glob 
                                                                  );
                                                                  Tacentries.arg_printer = 
                                                                  ((fun env sigma -> 
                                                                  
# 195 "extraargs.mlg"
                      pr_gen env sigma 
                                                                  ), (fun env sigma -> 
                                                                  
# 196 "extraargs.mlg"
                       pr_gen env sigma 
                                                                  ), (fun env sigma -> 
                                                                  
# 189 "extraargs.mlg"
                 pr_globc env sigma 
                                                                  ));
                                                                  }
let _ = (wit_lglob, lglob)


# 200 "extraargs.mlg"
 

let interp_casted_constr ist gl c =
  interp_constr_gen (Pretyping.OfType (pf_concl gl)) ist (pf_env gl) (project gl) c



let (wit_casted_constr, casted_constr) = Tacentries.argument_extend ~name:"casted_constr" 
                                         {
                                         Tacentries.arg_parsing = Vernacextend.Arg_alias (constr);
                                         Tacentries.arg_tag = Some
                                                              (Geninterp.val_tag (Genarg.topwit wit_constr));
                                         Tacentries.arg_intern = Tacentries.ArgInternWit (wit_constr);
                                         Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_constr);
                                         Tacentries.arg_interp = Tacentries.ArgInterpLegacy (
                                                                 
# 210 "extraargs.mlg"
                   interp_casted_constr 
                                                                 );
                                         Tacentries.arg_printer = ((fun env sigma -> 
                                                                  
# 209 "extraargs.mlg"
               pr_gen env sigma 
                                                                  ), (fun env sigma -> 
                                                                  
# 209 "extraargs.mlg"
               pr_gen env sigma 
                                                                  ), (fun env sigma -> 
                                                                  
# 209 "extraargs.mlg"
               pr_gen env sigma 
                                                                  ));
                                         }
let _ = (wit_casted_constr, casted_constr)


# 214 "extraargs.mlg"
 

type 'id gen_place= ('id * hyp_location_flag,unit) location

type loc_place = lident gen_place
type place = Id.t gen_place

let pr_gen_place pr_id = function
    ConclLocation () -> Pp.mt ()
  | HypLocation (id,InHyp) -> str "in " ++ pr_id id
  | HypLocation (id,InHypTypeOnly) ->
      str "in (type of " ++ pr_id id ++ str ")"
  | HypLocation (id,InHypValueOnly) ->
      str "in (value of " ++ pr_id id ++ str ")"

let pr_loc_place _ _ _ = pr_gen_place (fun { CAst.v = id } -> Id.print id)
let pr_place _ _ _ = pr_gen_place Id.print
let pr_hloc = pr_loc_place () () ()

let intern_place ist = function
    ConclLocation () -> ConclLocation ()
  | HypLocation (id,hl) -> HypLocation (Tacintern.intern_hyp ist id,hl)

let interp_place ist env sigma = function
    ConclLocation () -> ConclLocation ()
  | HypLocation (id,hl) -> HypLocation (Tacinterp.interp_hyp ist env sigma id,hl)

let interp_place ist gl p =
  Tacmach.project gl , interp_place ist (Tacmach.pf_env gl) (Tacmach.project gl) p

let subst_place subst pl = pl

let warn_deprecated_instantiate_syntax =
  CWarnings.create ~name:"deprecated-instantiate-syntax" ~category:"deprecated"
         (fun (v,v',id) ->
           let s = Id.to_string id in
           Pp.strbrk
             ("Syntax \"in (" ^ v ^ " of " ^ s ^ ")\" is deprecated; use \"in (" ^ v' ^ " of " ^ s ^ ")\".")
         )



let (wit_hloc, hloc) = Tacentries.argument_extend ~name:"hloc" {
                                                               Tacentries.arg_parsing = 
                                                               Vernacextend.Arg_rules (
                                                               [(Extend.Rule
                                                                 (Extend.Next 
                                                                  (Extend.Next 
                                                                  (Extend.Next 
                                                                  (Extend.Next 
                                                                  (Extend.Next 
                                                                  (Extend.Next 
                                                                  (Extend.Stop,
                                                                  (Extend.Atoken (CLexer.terminal "in"))),
                                                                  (Extend.Atoken (CLexer.terminal "("))),
                                                                  (Extend.Atoken (CLexer.terminal "value"))),
                                                                  (Extend.Atoken (CLexer.terminal "of"))),
                                                                  (Extend.Aentry ident)),
                                                                  (Extend.Atoken (CLexer.terminal ")"))),
                                                                 (fun _ id _
                                                                 _ _ _ loc ->
                                                                 
# 278 "extraargs.mlg"
      HypLocation ((CAst.make id),InHypValueOnly) 
                                                                 )));
                                                               (Extend.Rule
                                                                (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Stop,
                                                                 (Extend.Atoken (CLexer.terminal "in"))),
                                                                 (Extend.Atoken (CLexer.terminal "("))),
                                                                 (Extend.Atoken (CLexer.terminal "type"))),
                                                                 (Extend.Atoken (CLexer.terminal "of"))),
                                                                 (Extend.Aentry ident)),
                                                                 (Extend.Atoken (CLexer.terminal ")"))),
                                                                (fun _ id _ _
                                                                _ _ loc -> 
                                                                
# 276 "extraargs.mlg"
      HypLocation ((CAst.make id),InHypTypeOnly) 
                                                                )));
                                                               (Extend.Rule
                                                                (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Stop,
                                                                 (Extend.Atoken (CLexer.terminal "in"))),
                                                                 (Extend.Atoken (CLexer.terminal "("))),
                                                                 (Extend.Atoken (CLexer.terminal "Value"))),
                                                                 (Extend.Atoken (CLexer.terminal "of"))),
                                                                 (Extend.Aentry ident)),
                                                                 (Extend.Atoken (CLexer.terminal ")"))),
                                                                (fun _ id _ _
                                                                _ _ loc -> 
                                                                
# 273 "extraargs.mlg"
      warn_deprecated_instantiate_syntax ("Value","value",id);
      HypLocation ((CAst.make id),InHypValueOnly) 
                                                                )));
                                                               (Extend.Rule
                                                                (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Stop,
                                                                 (Extend.Atoken (CLexer.terminal "in"))),
                                                                 (Extend.Atoken (CLexer.terminal "("))),
                                                                 (Extend.Atoken (CLexer.terminal "Type"))),
                                                                 (Extend.Atoken (CLexer.terminal "of"))),
                                                                 (Extend.Aentry ident)),
                                                                 (Extend.Atoken (CLexer.terminal ")"))),
                                                                (fun _ id _ _
                                                                _ _ loc -> 
                                                                
# 270 "extraargs.mlg"
      warn_deprecated_instantiate_syntax ("Type","type",id);
      HypLocation ((CAst.make id),InHypTypeOnly) 
                                                                )));
                                                               (Extend.Rule
                                                                (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Stop,
                                                                 (Extend.Atoken (CLexer.terminal "in"))),
                                                                 (Extend.Aentry ident)),
                                                                (fun id _
                                                                loc -> 
                                                                
# 268 "extraargs.mlg"
      HypLocation ((CAst.make id),InHyp) 
                                                                )));
                                                               (Extend.Rule
                                                                (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Next 
                                                                 (Extend.Stop,
                                                                 (Extend.Atoken (CLexer.terminal "in"))),
                                                                 (Extend.Atoken (CLexer.terminal "|-"))),
                                                                 (Extend.Atoken (CLexer.terminal "*"))),
                                                                (fun _ _ _
                                                                loc -> 
                                                                
# 266 "extraargs.mlg"
      ConclLocation () 
                                                                )));
                                                               (Extend.Rule
                                                                (Extend.Stop,
                                                                (fun loc -> 
# 264 "extraargs.mlg"
      ConclLocation () 
                                                                    )))]);
                                                               Tacentries.arg_tag = 
                                                               None;
                                                               Tacentries.arg_intern = 
                                                               Tacentries.ArgInternFun ((fun f ist v -> (ist, f ist v)) (
                                                               
# 259 "extraargs.mlg"
                    intern_place 
                                                               ));
                                                               Tacentries.arg_subst = 
                                                               Tacentries.ArgSubstFun (
                                                               
# 260 "extraargs.mlg"
                     subst_place 
                                                               );
                                                               Tacentries.arg_interp = 
                                                               Tacentries.ArgInterpLegacy (
                                                               
# 258 "extraargs.mlg"
                     interp_place 
                                                               );
                                                               Tacentries.arg_printer = 
                                                               ((fun env sigma -> 
                                                               
# 261 "extraargs.mlg"
                     pr_loc_place 
                                                               ), (fun env sigma -> 
                                                               
# 262 "extraargs.mlg"
                      pr_loc_place 
                                                               ), (fun env sigma -> 
                                                               
# 257 "extraargs.mlg"
                 pr_place 
                                                               ));
                                                               }
let _ = (wit_hloc, hloc)


# 282 "extraargs.mlg"
 

let pr_rename _ _ _ (n, m) = Id.print n ++ str " into " ++ Id.print m



let (wit_rename, rename) = Tacentries.argument_extend ~name:"rename" 
                           {
                           Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                    [(Extend.Rule
                                                      (Extend.Next (Extend.Next 
                                                                   (Extend.Next 
                                                                   (Extend.Stop,
                                                                   (Extend.Aentry ident)),
                                                                   (Extend.Atoken (CLexer.terminal "into"))),
                                                                   (Extend.Aentry ident)),
                                                      (fun m _ n loc -> 
# 291 "extraargs.mlg"
                                    (n, m) 
                                                                    )))]);
                           Tacentries.arg_tag = Some
                                                (Geninterp.Val.Pair (
                                                (Geninterp.val_tag (Genarg.topwit wit_ident)), 
                                                (Geninterp.val_tag (Genarg.topwit wit_ident))));
                           Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.PairArg (
                                                   (wit_ident), (wit_ident)));
                           Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.PairArg (
                                                  (wit_ident), (wit_ident)));
                           Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.PairArg (
                                                   (wit_ident), (wit_ident)));
                           Tacentries.arg_printer = ((fun env sigma -> 
                                                    
# 290 "extraargs.mlg"
               pr_rename 
                                                    ), (fun env sigma -> 
                                                    
# 290 "extraargs.mlg"
               pr_rename 
                                                    ), (fun env sigma -> 
                                                    
# 290 "extraargs.mlg"
               pr_rename 
                                                    ));
                           }
let _ = (wit_rename, rename)


# 296 "extraargs.mlg"
 

let pr_by_arg_tac env sigma _prc _prlc prtac opt_c =
  match opt_c with
    | None -> mt ()
    | Some t -> hov 2 (str "by" ++ spc () ++ prtac env sigma (3,Notation_gram.E) t)



let (wit_by_arg_tac, by_arg_tac) = Tacentries.argument_extend ~name:"by_arg_tac" 
                                   {
                                   Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                            [(Extend.Rule
                                                              (Extend.Stop,
                                                              (fun loc -> 
# 309 "extraargs.mlg"
           None 
                                                                    )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Atoken (CLexer.terminal "by"))),
                                                              (Extend.Aentryl (Pltac.tactic_expr, "3"))),
                                                             (fun c _ loc ->
                                                             
# 308 "extraargs.mlg"
                           Some c 
                                                             )))]);
                                   Tacentries.arg_tag = Some
                                                        (Geninterp.Val.Opt 
                                                        (Geninterp.val_tag (Genarg.topwit wit_tactic)));
                                   Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.OptArg 
                                                           (wit_tactic));
                                   Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.OptArg 
                                                          (wit_tactic));
                                   Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.OptArg 
                                                           (wit_tactic));
                                   Tacentries.arg_printer = ((fun env sigma -> 
                                                            
# 307 "extraargs.mlg"
               pr_by_arg_tac env sigma 
                                                            ), (fun env sigma -> 
                                                            
# 307 "extraargs.mlg"
               pr_by_arg_tac env sigma 
                                                            ), (fun env sigma -> 
                                                            
# 307 "extraargs.mlg"
               pr_by_arg_tac env sigma 
                                                            ));
                                   }
let _ = (wit_by_arg_tac, by_arg_tac)


# 312 "extraargs.mlg"
 

let pr_by_arg_tac env sigma prtac opt_c = pr_by_arg_tac env sigma () () prtac opt_c

let pr_in_clause _ _ _ cl = Pptactic.pr_in_clause Pputils.pr_lident cl
let pr_in_top_clause _ _ _ cl = Pptactic.pr_in_clause Id.print cl
let in_clause' = Pltac.in_clause



let (wit_in_clause, in_clause) = Tacentries.argument_extend ~name:"in_clause" 
                                 {
                                 Tacentries.arg_parsing = Vernacextend.Arg_alias (in_clause');
                                 Tacentries.arg_tag = Some
                                                      (Geninterp.val_tag (Genarg.topwit wit_clause_dft_concl));
                                 Tacentries.arg_intern = Tacentries.ArgInternWit (wit_clause_dft_concl);
                                 Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_clause_dft_concl);
                                 Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_clause_dft_concl);
                                 Tacentries.arg_printer = ((fun env sigma -> 
                                                          
# 325 "extraargs.mlg"
                   pr_in_clause 
                                                          ), (fun env sigma -> 
                                                          
# 326 "extraargs.mlg"
                    pr_in_clause 
                                                          ), (fun env sigma -> 
                                                          
# 324 "extraargs.mlg"
               pr_in_top_clause 
                                                          ));
                                 }
let _ = (wit_in_clause, in_clause)


# 330 "extraargs.mlg"
 

let local_test_lpar_id_colon =
  let err () = raise Stream.Failure in
  Pcoq.Entry.of_parser "lpar_id_colon"
    (fun strm ->
      match Util.stream_nth 0 strm with
        | Tok.KEYWORD "(" ->
            (match Util.stream_nth 1 strm with
              | Tok.IDENT _ ->
                  (match Util.stream_nth 2 strm with
                    | Tok.KEYWORD ":" -> ()
                    | _ -> err ())
              | _ -> err ())
        | _ -> err ())

let pr_lpar_id_colon _ _ _ _ = mt ()



let (wit_test_lpar_id_colon, test_lpar_id_colon) = Tacentries.argument_extend ~name:"test_lpar_id_colon" 
                                                   {
                                                   Tacentries.arg_parsing = 
                                                   Vernacextend.Arg_rules (
                                                   [(Extend.Rule
                                                     (Extend.Next (Extend.Stop,
                                                                  (Extend.Aentry local_test_lpar_id_colon)),
                                                     (fun x loc -> 
# 351 "extraargs.mlg"
                                       () 
                                                                   )))]);
                                                   Tacentries.arg_tag = 
                                                   Some
                                                   (Geninterp.val_tag (Genarg.topwit wit_unit));
                                                   Tacentries.arg_intern = 
                                                   Tacentries.ArgInternWit (wit_unit);
                                                   Tacentries.arg_subst = 
                                                   Tacentries.ArgSubstWit (wit_unit);
                                                   Tacentries.arg_interp = 
                                                   Tacentries.ArgInterpWit (wit_unit);
                                                   Tacentries.arg_printer = 
                                                   ((fun env sigma -> 
                                                   
# 350 "extraargs.mlg"
                                                              pr_lpar_id_colon 
                                                   ), (fun env sigma -> 
                                                   
# 350 "extraargs.mlg"
                                                              pr_lpar_id_colon 
                                                   ), (fun env sigma -> 
                                                   
# 350 "extraargs.mlg"
                                                              pr_lpar_id_colon 
                                                   ));
                                                   }
let _ = (wit_test_lpar_id_colon, test_lpar_id_colon)

