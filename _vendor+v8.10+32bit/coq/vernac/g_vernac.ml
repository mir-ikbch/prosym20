
# 11 "g_vernac.mlg"
 

open Pp
open CErrors
open Util
open Names
open Glob_term
open Vernacexpr
open Impargs
open Constrexpr
open Constrexpr_ops
open Extend
open Decl_kinds
open Declaremods
open Declarations
open Namegen
open Tok (* necessary for camlp5 *)

open Pcoq
open Pcoq.Prim
open Pcoq.Constr
open Pcoq.Module
open Pvernac.Vernac_
open Attributes

let vernac_kw = [ ";"; ","; ">->"; ":<"; "<:"; "where"; "at" ]
let _ = List.iter CLexer.add_keyword vernac_kw

(* Rem: do not join the different GEXTEND into one, it breaks native *)
(* compilation on PowerPC and Sun architectures *)

let query_command = Entry.create "vernac:query_command"

let subprf = Entry.create "vernac:subprf"

let class_rawexpr = Entry.create "vernac:class_rawexpr"
let thm_token = Entry.create "vernac:thm_token"
let def_body = Entry.create "vernac:def_body"
let decl_notation = Entry.create "vernac:decl_notation"
let record_field = Entry.create "vernac:record_field"
let of_type_with_opt_coercion = Entry.create "vernac:of_type_with_opt_coercion"
let instance_name = Entry.create "vernac:instance_name"
let section_subset_expr = Entry.create "vernac:section_subset_expr"

let make_bullet s =
  let open Proof_bullet in
  let n = String.length s in
  match s.[0] with
  | '-' -> Dash n
  | '+' -> Plus n
  | '*' -> Star n
  | _ -> assert false

let parse_compat_version = let open Flags in function
  | "8.10" -> Current
  | "8.9" -> V8_9
  | "8.8" -> V8_8
  | ("8.7" | "8.6" | "8.5" | "8.4" | "8.3" | "8.2" | "8.1" | "8.0") as s ->
    CErrors.user_err ~hdr:"get_compat_version"
      Pp.(str "Compatibility with version " ++ str s ++ str " not supported.")
  | s ->
    CErrors.user_err ~hdr:"get_compat_version"
      Pp.(str "Unknown compatibility version \"" ++ str s ++ str "\".")



let _ = let decorated_vernac = Pcoq.Entry.create "decorated_vernac"
        and quoted_attributes = Pcoq.Entry.create "quoted_attributes"
        and attribute_list = Pcoq.Entry.create "attribute_list"
        and attribute = Pcoq.Entry.create "attribute"
        and attribute_value = Pcoq.Entry.create "attribute_value"
        and vernac = Pcoq.Entry.create "vernac"
        and vernac_poly = Pcoq.Entry.create "vernac_poly"
        and vernac_aux = Pcoq.Entry.create "vernac_aux"
        and located_vernac = Pcoq.Entry.create "located_vernac"
        in
        let () =
        Pcoq.grammar_extend vernac_control None
        (Some
        (Gramlib.Gramext.First), [(None, None,
                                  [Extend.Rule
                                   (Extend.Next (Extend.Stop,
                                                (Extend.Aentry decorated_vernac)),
                                   (fun v loc -> 
# 84 "g_vernac.mlg"
                                  let (f, v) = v in VernacExpr(f, v) 
                                                 ));
                                  Extend.Rule
                                  (Extend.Next (Extend.Next (Extend.Stop,
                                                            (Extend.Atoken (Tok.PIDENT (Some
                                                            ("Fail"))))),
                                               (Extend.Aentry located_vernac)),
                                  (fun v _ loc -> 
# 83 "g_vernac.mlg"
                                              VernacFail v 
                                                  ));
                                  Extend.Rule
                                  (Extend.Next (Extend.Next (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (Tok.PIDENT (Some
                                                            ("Timeout"))))),
                                                            (Extend.Aentry natural)),
                                               (Extend.Aentry located_vernac)),
                                  (fun v n _ loc -> 
# 82 "g_vernac.mlg"
                                                              VernacTimeout(n,v) 
                                                    ));
                                  Extend.Rule
                                  (Extend.Next (Extend.Next (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (Tok.PIDENT (Some
                                                            ("Redirect"))))),
                                                            (Extend.Aentry ne_string)),
                                               (Extend.Aentry located_vernac)),
                                  (fun c s _ loc -> 
# 81 "g_vernac.mlg"
                                                                 VernacRedirect (s, c) 
                                                    ));
                                  Extend.Rule
                                  (Extend.Next (Extend.Next (Extend.Stop,
                                                            (Extend.Atoken (Tok.PIDENT (Some
                                                            ("Time"))))),
                                               (Extend.Aentry located_vernac)),
                                  (fun c _ loc -> 
# 80 "g_vernac.mlg"
                                              VernacTime (false,c) 
                                                  ))])])
        in let () =
        Pcoq.grammar_extend decorated_vernac None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Alist0 (Extend.Aentry quoted_attributes))),
                              (Extend.Aentry vernac)),
                 (fun fv a loc -> 
# 89 "g_vernac.mlg"
          let (f, v) = fv in (List.append (List.flatten a) f, v) 
                                  ))])])
        in let () =
        Pcoq.grammar_extend quoted_attributes None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("#[")))),
                                           (Extend.Aentry attribute_list)),
                              (Extend.Atoken (Tok.PKEYWORD ("]")))),
                 (fun _ a _ loc -> 
# 92 "g_vernac.mlg"
                                             a 
                                   ))])])
        in let () =
        Pcoq.grammar_extend attribute_list None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Alist0sep ((Extend.Aentry attribute), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                 (fun a loc -> 
# 96 "g_vernac.mlg"
                                         a 
                               ))])])
        in let () =
        Pcoq.grammar_extend attribute None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry ident)),
                              (Extend.Aentry attribute_value)),
                 (fun v k loc -> 
# 100 "g_vernac.mlg"
                                             Names.Id.to_string k, v 
                                 ))])])
        in let () =
        Pcoq.grammar_extend attribute_value None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 106 "g_vernac.mlg"
             VernacFlagEmpty 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                          (Extend.Aentry attribute_list)),
                             (Extend.Atoken (Tok.PKEYWORD (")")))),
                (fun _ a _ loc -> 
# 105 "g_vernac.mlg"
                                            VernacFlagList a 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("=")))),
                             (Extend.Aentry string)),
                (fun v _ loc -> 
# 104 "g_vernac.mlg"
                              VernacFlagLeaf v 
                                ))])])
        in let () =
        Pcoq.grammar_extend vernac None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry vernac_poly)),
                 (fun v loc -> 
# 113 "g_vernac.mlg"
                             v 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Global"))))),
                             (Extend.Aentry vernac_poly)),
                (fun v _ loc -> 
# 111 "g_vernac.mlg"
                                             let (f, v) = v in (("global", VernacFlagEmpty) :: f, v) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Local"))))),
                             (Extend.Aentry vernac_poly)),
                (fun v _ loc -> 
# 110 "g_vernac.mlg"
                                            let (f, v) = v in (("local", VernacFlagEmpty) :: f, v) 
                                ))])])
        in let () =
        Pcoq.grammar_extend vernac_poly None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry vernac_aux)),
                 (fun v loc -> 
# 121 "g_vernac.mlg"
                            v 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Monomorphic"))))),
                             (Extend.Aentry vernac_aux)),
                (fun v _ loc -> 
# 120 "g_vernac.mlg"
          let (f, v) = v in (Attributes.vernac_monomorphic_flag :: f, v) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Polymorphic"))))),
                             (Extend.Aentry vernac_aux)),
                (fun v _ loc -> 
# 118 "g_vernac.mlg"
          let (f, v) = v in (Attributes.vernac_polymorphic_flag :: f, v) 
                                ))])])
        in let () =
        Pcoq.grammar_extend vernac_aux None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry subprf)),
                 (fun c loc -> 
# 133 "g_vernac.mlg"
                        ([], c) 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry syntax)),
                             (Extend.Atoken (Tok.PKEYWORD (".")))),
                (fun _ c loc -> 
# 132 "g_vernac.mlg"
                             ([], c) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry command)),
                             (Extend.Atoken (Tok.PKEYWORD (".")))),
                (fun _ c loc -> 
# 131 "g_vernac.mlg"
                              ([], c) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry gallina_ext)),
                             (Extend.Atoken (Tok.PKEYWORD (".")))),
                (fun _ g loc -> 
# 130 "g_vernac.mlg"
                                  ([], g) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry gallina)),
                             (Extend.Atoken (Tok.PKEYWORD (".")))),
                (fun _ g loc -> 
# 129 "g_vernac.mlg"
                              ([], g) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Program"))))),
                                          (Extend.Aentry gallina_ext)),
                             (Extend.Atoken (Tok.PKEYWORD (".")))),
                (fun _ g _ loc -> 
# 128 "g_vernac.mlg"
                                                   (["program", VernacFlagEmpty], g) 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Program"))))),
                                          (Extend.Aentry gallina)),
                             (Extend.Atoken (Tok.PKEYWORD (".")))),
                (fun _ g _ loc -> 
# 127 "g_vernac.mlg"
                                               (["program", VernacFlagEmpty], g) 
                                  ))])])
        in let () =
        Pcoq.grammar_extend vernac_aux None
        (Some
        (Gramlib.Gramext.Last), [(None, None,
                                 [Extend.Rule
                                  (Extend.Next (Extend.Stop,
                                               (Extend.Aentry command_entry)),
                                  (fun prfcom loc -> 
# 137 "g_vernac.mlg"
                                    ([], prfcom) 
                                                     ))])])
        in let () =
        Pcoq.grammar_extend noedit_mode None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry query_command)),
                 (fun c loc -> 
# 140 "g_vernac.mlg"
                               c None 
                               ))])])
        in let () =
        Pcoq.grammar_extend subprf None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Atoken (Tok.PKEYWORD ("}")))),
                 (fun _ loc -> 
# 146 "g_vernac.mlg"
               VernacEndSubproof 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("{")))),
                (fun _ loc -> 
# 145 "g_vernac.mlg"
               VernacSubproof None 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PBULLET (None)))),
                (fun s loc -> 
# 144 "g_vernac.mlg"
                      VernacBullet (make_bullet s) 
                              ))])])
        in let () =
        Pcoq.grammar_extend located_vernac None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry vernac_control)),
                 (fun v loc -> 
# 151 "g_vernac.mlg"
                                CAst.make ~loc v 
                               ))])])
        in ()


# 155 "g_vernac.mlg"
 

let warn_plural_command =
  CWarnings.create ~name:"plural-command" ~category:"pedantic" ~default:CWarnings.Disabled
         (fun kwd -> strbrk (Printf.sprintf "Command \"%s\" expects more than one assumption." kwd))

let test_plural_form loc kwd = function
  | [(_,([_],_))] ->
     warn_plural_command ~loc kwd
  | _ -> ()

let test_plural_form_types loc kwd = function
  | [([_],_)] ->
     warn_plural_command ~loc kwd
  | _ -> ()

let lname_of_lident : lident -> lname =
  CAst.map (fun s -> Name s)

let name_of_ident_decl : ident_decl -> name_decl =
  on_fst lname_of_lident



let _ = let register_token = Pcoq.Entry.create "register_token"
        and register_type_token = Pcoq.Entry.create "register_type_token"
        and register_prim_token = Pcoq.Entry.create "register_prim_token"
        and def_token = Pcoq.Entry.create "def_token"
        and assumption_token = Pcoq.Entry.create "assumption_token"
        and assumptions_token = Pcoq.Entry.create "assumptions_token"
        and inline = Pcoq.Entry.create "inline"
        and univ_constraint = Pcoq.Entry.create "univ_constraint"
        and finite_token = Pcoq.Entry.create "finite_token"
        and cumulativity_token = Pcoq.Entry.create "cumulativity_token"
        and private_token = Pcoq.Entry.create "private_token"
        and reduce = Pcoq.Entry.create "reduce"
        and one_decl_notation = Pcoq.Entry.create "one_decl_notation"
        and decl_sep = Pcoq.Entry.create "decl_sep"
        and opt_constructors_or_fields =
          Pcoq.Entry.create "opt_constructors_or_fields"
        and inductive_definition = Pcoq.Entry.create "inductive_definition"
        and constructor_list_or_record_decl =
          Pcoq.Entry.create "constructor_list_or_record_decl"
        and opt_coercion = Pcoq.Entry.create "opt_coercion"
        and corec_definition = Pcoq.Entry.create "corec_definition"
        and type_cstr = Pcoq.Entry.create "type_cstr"
        and scheme = Pcoq.Entry.create "scheme"
        and scheme_kind = Pcoq.Entry.create "scheme_kind"
        and record_fields = Pcoq.Entry.create "record_fields"
        and record_binder_body = Pcoq.Entry.create "record_binder_body"
        and record_binder = Pcoq.Entry.create "record_binder"
        and assum_list = Pcoq.Entry.create "assum_list"
        and assum_coe = Pcoq.Entry.create "assum_coe"
        and simple_assum_coe = Pcoq.Entry.create "simple_assum_coe"
        and constructor_type = Pcoq.Entry.create "constructor_type"
        and constructor = Pcoq.Entry.create "constructor"
        in
        let () =
        Pcoq.grammar_extend gallina None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PIDENT (Some
                                           ("Constraint"))))),
                              (Extend.Alist1sep ((Extend.Aentry univ_constraint), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                 (fun l _ loc -> 
# 226 "g_vernac.mlg"
                                                                   VernacConstraint l 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Universes"))))),
                             (Extend.Alist1 (Extend.Aentry identref))),
                (fun l _ loc -> 
# 225 "g_vernac.mlg"
                                                   VernacUniverse l 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Universe"))))),
                             (Extend.Alist1 (Extend.Aentry identref))),
                (fun l _ loc -> 
# 224 "g_vernac.mlg"
                                                  VernacUniverse l 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Primitive"))))),
                                                                    (Extend.Aentry identref)),
                                                       (Extend.Aopt (Extend.Arules 
                                                       [Extend.Rules 
                                                       (Extend.NextNoRec 
                                                       (Extend.NextNoRec 
                                                       (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD (":")))),
                                                       (Extend.Aentry lconstr)),
                                                       (fun typ _ loc -> 
                                                       
# 222 "g_vernac.mlg"
                                                                                 typ 
                                                       ))]))),
                                          (Extend.Atoken (Tok.PKEYWORD (":=")))),
                             (Extend.Aentry register_token)),
                (fun r _ typopt id _ loc -> 
# 223 "g_vernac.mlg"
            VernacPrimitive(id, r, typopt) 
                                            ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Register"))))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Inline"))))),
                             (Extend.Aentry global)),
                (fun g _ _ loc -> 
# 221 "g_vernac.mlg"
            VernacRegister(g, RegisterInline) 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Register"))))),
                                                       (Extend.Aentry global)),
                                          (Extend.Atoken (Tok.PKEYWORD ("as")))),
                             (Extend.Aentry qualid)),
                (fun quid _ g _ loc -> 
# 219 "g_vernac.mlg"
            VernacRegister(g, RegisterCoqlib quid) 
                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Combined"))))),
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Scheme"))))),
                                                       (Extend.Aentry identref)),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("from"))))),
                             (Extend.Alist1sep ((Extend.Aentry identref), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                (fun l _ id _ _ loc -> 
# 217 "g_vernac.mlg"
                                              VernacCombinedScheme (id, l) 
                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Scheme"))))),
                             (Extend.Alist1sep ((Extend.Aentry scheme), (Extend.Atoken (Tok.PKEYWORD ("with")))))),
                (fun l _ loc -> 
# 215 "g_vernac.mlg"
                                                         VernacScheme l 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Let"))))),
                                          (Extend.Atoken (Tok.PKEYWORD ("CoFixpoint")))),
                             (Extend.Alist1sep ((Extend.Aentry corec_definition), (Extend.Atoken (Tok.PKEYWORD ("with")))))),
                (fun corecs _ _ loc -> 
# 214 "g_vernac.mlg"
            VernacCoFixpoint (DoDischarge, corecs) 
                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("CoFixpoint")))),
                             (Extend.Alist1sep ((Extend.Aentry corec_definition), (Extend.Atoken (Tok.PKEYWORD ("with")))))),
                (fun corecs _ loc -> 
# 212 "g_vernac.mlg"
            VernacCoFixpoint (NoDischarge, corecs) 
                                     ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Let"))))),
                                          (Extend.Atoken (Tok.PKEYWORD ("Fixpoint")))),
                             (Extend.Alist1sep ((Extend.Aentry rec_definition), (Extend.Atoken (Tok.PKEYWORD ("with")))))),
                (fun recs _ _ loc -> 
# 210 "g_vernac.mlg"
            VernacFixpoint (DoDischarge, recs) 
                                     ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("Fixpoint")))),
                             (Extend.Alist1sep ((Extend.Aentry rec_definition), (Extend.Atoken (Tok.PKEYWORD ("with")))))),
                (fun recs _ loc -> 
# 208 "g_vernac.mlg"
            VernacFixpoint (NoDischarge, recs) 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Aopt (Extend.Aentry cumulativity_token))),
                                                       (Extend.Aentry private_token)),
                                          (Extend.Aentry finite_token)),
                             (Extend.Alist1sep ((Extend.Aentry inductive_definition), (Extend.Atoken (Tok.PKEYWORD ("with")))))),
                (fun indl f priv cum loc -> 
# 204 "g_vernac.mlg"
            let (k,f) = f in
          let indl=List.map (fun ((a,b,c,d),e) -> ((a,b,c,k,d),e)) indl in
          VernacInductive (cum, priv,f,indl) 
                                            ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Let"))))),
                                          (Extend.Aentry identref)),
                             (Extend.Aentry def_body)),
                (fun b id _ loc -> 
# 200 "g_vernac.mlg"
            VernacDefinition ((DoDischarge, Let), (lname_of_lident id, None), b) 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry def_token)),
                                          (Extend.Aentry ident_decl)),
                             (Extend.Aentry def_body)),
                (fun b id d loc -> 
# 198 "g_vernac.mlg"
            VernacDefinition (d, name_of_ident_decl id, b) 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry assumptions_token)),
                                          (Extend.Aentry inline)),
                             (Extend.Aentry assum_list)),
                (fun bl nl tk loc -> 
# 194 "g_vernac.mlg"
            let (kwd,stre) = tk in
            test_plural_form loc kwd bl;
            VernacAssumption (stre, nl, bl) 
                                     ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry assumption_token)),
                                          (Extend.Aentry inline)),
                             (Extend.Aentry assum_list)),
                (fun bl nl stre loc -> 
# 192 "g_vernac.mlg"
            VernacAssumption (stre, nl, bl) 
                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Aentry thm_token)),
                                                                    (Extend.Aentry ident_decl)),
                                                                    (Extend.Aentry binders)),
                                                       (Extend.Atoken (Tok.PKEYWORD (":")))),
                                          (Extend.Aentry lconstr)),
                             (Extend.Alist0 (Extend.Arules [Extend.Rules 
                                                           (Extend.NextNoRec 
                                                           (Extend.NextNoRec 
                                                           (Extend.NextNoRec 
                                                           (Extend.NextNoRec 
                                                           (Extend.NextNoRec 
                                                           (Extend.Stop,
                                                           (Extend.Atoken (Tok.PKEYWORD ("with")))),
                                                           (Extend.Aentry ident_decl)),
                                                           (Extend.Aentry binders)),
                                                           (Extend.Atoken (Tok.PKEYWORD (":")))),
                                                           (Extend.Aentry lconstr)),
                                                           (fun c _ bl id _
                                                           loc -> 
# 189 "g_vernac.mlg"
            (id,(bl,c)) 
                                                                  ))]))),
                (fun l c _ bl id thm loc -> 
# 190 "g_vernac.mlg"
            VernacStartTheoremProof (thm, (id,(bl,c))::l) 
                                            ))])])
        in let () =
        Pcoq.grammar_extend register_token None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Aentry register_type_token)),
                 (fun r loc -> 
# 232 "g_vernac.mlg"
                                      CPrimitives.OT_type r 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Aentry register_prim_token)),
                (fun r loc -> 
# 231 "g_vernac.mlg"
                                      CPrimitives.OT_op r 
                              ))])])
        in let () =
        Pcoq.grammar_extend register_type_token None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Atoken (Tok.PKEYWORD ("#int63_type")))),
                 (fun _ loc -> 
# 236 "g_vernac.mlg"
                            CPrimitives.PT_int63 
                               ))])])
        in let () =
        Pcoq.grammar_extend register_prim_token None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Atoken (Tok.PKEYWORD ("#int63_compare")))),
                 (fun _ loc -> 
# 263 "g_vernac.mlg"
                               CPrimitives.Int63compare 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("#int63_le")))),
                (fun _ loc -> 
# 262 "g_vernac.mlg"
                          CPrimitives.Int63le 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("#int63_lt")))),
                (fun _ loc -> 
# 261 "g_vernac.mlg"
                          CPrimitives.Int63lt 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("#int63_eq")))),
                (fun _ loc -> 
# 260 "g_vernac.mlg"
                          CPrimitives.Int63eq 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("#int63_addmuldiv")))),
                (fun _ loc -> 
# 259 "g_vernac.mlg"
                                 CPrimitives.Int63addMulDiv 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("#int63_div21")))),
                (fun _ loc -> 
# 258 "g_vernac.mlg"
                             CPrimitives.Int63div21 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("#int63_diveucl")))),
                (fun _ loc -> 
# 257 "g_vernac.mlg"
                               CPrimitives.Int63diveucl 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("#int63_mulc")))),
                (fun _ loc -> 
# 256 "g_vernac.mlg"
                            CPrimitives.Int63mulc 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("#int63_subcarryc")))),
                (fun _ loc -> 
# 255 "g_vernac.mlg"
                                 CPrimitives.Int63subCarryC 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("#int63_addcarryc")))),
                (fun _ loc -> 
# 254 "g_vernac.mlg"
                                 CPrimitives.Int63addCarryC 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("#int63_subc")))),
                (fun _ loc -> 
# 253 "g_vernac.mlg"
                            CPrimitives.Int63subc 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("#int63_addc")))),
                (fun _ loc -> 
# 252 "g_vernac.mlg"
                            CPrimitives.Int63addc 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("#int63_lxor")))),
                (fun _ loc -> 
# 251 "g_vernac.mlg"
                            CPrimitives.Int63lxor 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("#int63_lor")))),
                (fun _ loc -> 
# 250 "g_vernac.mlg"
                           CPrimitives.Int63lor 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("#int63_land")))),
                (fun _ loc -> 
# 249 "g_vernac.mlg"
                            CPrimitives.Int63land 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("#int63_lsl")))),
                (fun _ loc -> 
# 248 "g_vernac.mlg"
                           CPrimitives.Int63lsl 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("#int63_lsr")))),
                (fun _ loc -> 
# 247 "g_vernac.mlg"
                           CPrimitives.Int63lsr 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("#int63_mod")))),
                (fun _ loc -> 
# 246 "g_vernac.mlg"
                           CPrimitives.Int63mod 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("#int63_div")))),
                (fun _ loc -> 
# 245 "g_vernac.mlg"
                           CPrimitives.Int63div 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("#int63_mul")))),
                (fun _ loc -> 
# 244 "g_vernac.mlg"
                           CPrimitives.Int63mul 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("#int63_sub")))),
                (fun _ loc -> 
# 243 "g_vernac.mlg"
                           CPrimitives.Int63sub 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("#int63_add")))),
                (fun _ loc -> 
# 242 "g_vernac.mlg"
                           CPrimitives.Int63add 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("#int63_tail0")))),
                (fun _ loc -> 
# 241 "g_vernac.mlg"
                             CPrimitives.Int63tail0 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("#int63_head0")))),
                (fun _ loc -> 
# 240 "g_vernac.mlg"
                             CPrimitives.Int63head0 
                              ))])])
        in let () =
        Pcoq.grammar_extend thm_token None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                              ("Property"))))),
                 (fun _ loc -> 
# 274 "g_vernac.mlg"
                              Property 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Proposition"))))),
                (fun _ loc -> 
# 273 "g_vernac.mlg"
                                 Proposition 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Corollary"))))),
                (fun _ loc -> 
# 272 "g_vernac.mlg"
                               Corollary 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Remark"))))),
                (fun _ loc -> 
# 271 "g_vernac.mlg"
                            Remark 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Fact"))))),
                (fun _ loc -> 
# 270 "g_vernac.mlg"
                          Fact 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Lemma"))))),
                (fun _ loc -> 
# 269 "g_vernac.mlg"
                           Lemma 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("Theorem")))),
                (fun _ loc -> 
# 268 "g_vernac.mlg"
                       Theorem 
                              ))])])
        in let () =
        Pcoq.grammar_extend def_token None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                              ("SubClass"))))),
                 (fun _ loc -> 
# 279 "g_vernac.mlg"
                              (NoDischarge,SubClass) 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Example"))))),
                (fun _ loc -> 
# 278 "g_vernac.mlg"
                             (NoDischarge,Example) 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("Definition")))),
                (fun _ loc -> 
# 277 "g_vernac.mlg"
                          (NoDischarge,Definition) 
                              ))])])
        in let () =
        Pcoq.grammar_extend assumption_token None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                              ("Conjecture"))))),
                 (fun _ loc -> 
# 286 "g_vernac.mlg"
                                (NoDischarge, Conjectural) 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("Parameter")))),
                (fun _ loc -> 
# 285 "g_vernac.mlg"
                         (NoDischarge, Definitional) 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("Axiom")))),
                (fun _ loc -> 
# 284 "g_vernac.mlg"
                     (NoDischarge, Logical) 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("Variable")))),
                (fun _ loc -> 
# 283 "g_vernac.mlg"
                        (DoDischarge, Definitional) 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("Hypothesis")))),
                (fun _ loc -> 
# 282 "g_vernac.mlg"
                          (DoDischarge, Logical) 
                              ))])])
        in let () =
        Pcoq.grammar_extend assumptions_token None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                              ("Conjectures"))))),
                 (fun _ loc -> 
# 293 "g_vernac.mlg"
                                 ("Conjectures", (NoDischarge, Conjectural)) 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Parameters"))))),
                (fun _ loc -> 
# 292 "g_vernac.mlg"
                                ("Parameters", (NoDischarge, Definitional)) 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Axioms"))))),
                (fun _ loc -> 
# 291 "g_vernac.mlg"
                            ("Axioms", (NoDischarge, Logical)) 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Variables"))))),
                (fun _ loc -> 
# 290 "g_vernac.mlg"
                               ("Variables", (DoDischarge, Definitional)) 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Hypotheses"))))),
                (fun _ loc -> 
# 289 "g_vernac.mlg"
                                ("Hypotheses", (DoDischarge, Logical)) 
                              ))])])
        in let () =
        Pcoq.grammar_extend inline None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 298 "g_vernac.mlg"
             NoInline 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Inline"))))),
                (fun _ loc -> 
# 297 "g_vernac.mlg"
                            DefaultInline 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Inline"))))),
                                                       (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                          (Extend.Aentry natural)),
                             (Extend.Atoken (Tok.PKEYWORD (")")))),
                (fun _ i _ _ loc -> 
# 296 "g_vernac.mlg"
                                                   InlineAt i 
                                    ))])])
        in let () =
        Pcoq.grammar_extend univ_constraint None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Aentry universe_level)),
                                           (Extend.Arules [Extend.Rules 
                                                          (Extend.NextNoRec 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (Tok.PKEYWORD ("<=")))),
                                                          (fun _ loc -> 
                                                          
# 301 "g_vernac.mlg"
                                                                                        Univ.Le 
                                                          ));
                                                          Extend.Rules 
                                                          (Extend.NextNoRec 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (Tok.PKEYWORD ("=")))),
                                                          (fun _ loc -> 
                                                          
# 301 "g_vernac.mlg"
                                                                  Univ.Eq 
                                                          ));
                                                          Extend.Rules 
                                                          (Extend.NextNoRec 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (Tok.PKEYWORD ("<")))),
                                                          (fun _ loc -> 
                                                          
# 301 "g_vernac.mlg"
                                             Univ.Lt 
                                                          ))])),
                              (Extend.Aentry universe_level)),
                 (fun r ord l loc -> 
# 302 "g_vernac.mlg"
                                (l, ord, r) 
                                     ))])])
        in let () =
        Pcoq.grammar_extend univ_decl None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("@{")))),
                                                        (Extend.Alist0 (Extend.Aentry identref))),
                                           (Extend.Arules [Extend.Rules 
                                                          (Extend.Stop, (fun
                                                          loc -> 
# 305 "g_vernac.mlg"
                                                                   false 
                                                                 ));
                                                          Extend.Rules 
                                                          (Extend.NextNoRec 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (Tok.PKEYWORD ("+")))),
                                                          (fun _ loc -> 
                                                          
# 305 "g_vernac.mlg"
                                                     true 
                                                          ))])),
                              (Extend.Arules [Extend.Rules (Extend.NextNoRec 
                                                           (Extend.Stop,
                                                           (Extend.Arules 
                                                           [Extend.Rules 
                                                           (Extend.NextNoRec 
                                                           (Extend.Stop,
                                                           (Extend.Atoken (Tok.PKEYWORD ("|}")))),
                                                           (fun _ loc -> 
                                                           
# 308 "g_vernac.mlg"
                                                    false 
                                                           ));
                                                           Extend.Rules 
                                                           (Extend.NextNoRec 
                                                           (Extend.Stop,
                                                           (Extend.Atoken (Tok.PKEYWORD ("}")))),
                                                           (fun _ loc -> 
                                                           
# 308 "g_vernac.mlg"
                                 true 
                                                           ))])), (fun ext
                                                           loc -> 
# 308 "g_vernac.mlg"
                                                                   ([], ext) 
                                                                  ));
                                             Extend.Rules (Extend.NextNoRec 
                                                          (Extend.NextNoRec 
                                                          (Extend.NextNoRec 
                                                          (Extend.NextNoRec 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (Tok.PKEYWORD ("|")))),
                                                          (Extend.Alist0sep ((Extend.Aentry univ_constraint), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                                                          (Extend.Arules 
                                                          [Extend.Rules 
                                                          (Extend.Stop, (fun
                                                          loc -> 
# 307 "g_vernac.mlg"
                                               false 
                                                                 ));
                                                          Extend.Rules 
                                                          (Extend.NextNoRec 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (Tok.PKEYWORD ("+")))),
                                                          (fun _ loc -> 
                                                          
# 307 "g_vernac.mlg"
                                 true 
                                                          ))])),
                                                          (Extend.Atoken (Tok.PKEYWORD ("}")))),
                                                          (fun _ ext l' _
                                                          loc -> 
# 307 "g_vernac.mlg"
                                                                   (l',ext) 
                                                                 ))])),
                 (fun cs ext l _ loc -> 
# 310 "g_vernac.mlg"
           let open UState in
         { univdecl_instance = l;
           univdecl_extensible_instance = ext;
           univdecl_constraints = fst cs;
           univdecl_extensible_constraints = snd cs } 
                                        ))])])
        in let () =
        Pcoq.grammar_extend ident_decl None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry identref)),
                              (Extend.Aopt (Extend.Aentry univ_decl))),
                 (fun l i loc -> 
# 318 "g_vernac.mlg"
                                             (i, l) 
                                 ))])])
        in let () =
        Pcoq.grammar_extend finite_token None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                              ("Class"))))),
                 (fun _ loc -> 
# 327 "g_vernac.mlg"
                           (Class true,BiFinite) 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Structure"))))),
                (fun _ loc -> 
# 326 "g_vernac.mlg"
                               (Structure,BiFinite) 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Record"))))),
                (fun _ loc -> 
# 325 "g_vernac.mlg"
                            (Record,BiFinite) 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Variant"))))),
                (fun _ loc -> 
# 324 "g_vernac.mlg"
                             (Variant,BiFinite) 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("CoInductive"))))),
                (fun _ loc -> 
# 323 "g_vernac.mlg"
                                 (CoInductive,CoFinite) 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Inductive"))))),
                (fun _ loc -> 
# 322 "g_vernac.mlg"
                               (Inductive_kw,Finite) 
                              ))])])
        in let () =
        Pcoq.grammar_extend cumulativity_token None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                              ("NonCumulative"))))),
                 (fun _ loc -> 
# 331 "g_vernac.mlg"
                                   VernacNonCumulative 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Cumulative"))))),
                (fun _ loc -> 
# 330 "g_vernac.mlg"
                                VernacCumulative 
                              ))])])
        in let () =
        Pcoq.grammar_extend private_token None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 334 "g_vernac.mlg"
                                           false 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Private"))))),
                (fun _ loc -> 
# 334 "g_vernac.mlg"
                             true 
                              ))])])
        in let () =
        Pcoq.grammar_extend def_body None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Aentry binders)),
                                           (Extend.Atoken (Tok.PKEYWORD (":")))),
                              (Extend.Aentry lconstr)),
                 (fun t _ bl loc -> 
# 359 "g_vernac.mlg"
          ProveBody (bl, t) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Aentry binders)),
                                                                    (Extend.Atoken (Tok.PKEYWORD (":")))),
                                                                    (Extend.Aentry lconstr)),
                                                       (Extend.Atoken (Tok.PKEYWORD (":=")))),
                                          (Extend.Aentry reduce)),
                             (Extend.Aentry lconstr)),
                (fun c red _ t _ bl loc -> 
# 349 "g_vernac.mlg"
          let ((bl, c), tyo) =
          if List.exists (function CLocalPattern _ -> true | _ -> false) bl
          then
            (* FIXME: "red" will be applied to types in bl and Cast with remain *)
            let c = CAst.make ~loc @@ CCast (c, CastConv t) in
            (([],mkLambdaCN ~loc bl c), None)
          else ((bl, c), Some t)
        in
        DefineBody (bl, red, c, tyo) 
                                           ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Aentry binders)),
                                                       (Extend.Atoken (Tok.PKEYWORD (":=")))),
                                          (Extend.Aentry reduce)),
                             (Extend.Aentry lconstr)),
                (fun c red _ bl loc -> 
# 339 "g_vernac.mlg"
        if List.exists (function CLocalPattern _ -> true | _ -> false) bl
      then
        (* FIXME: "red" will be applied to types in bl and Cast with remain *)
        let c = mkLambdaCN ~loc bl c in
        DefineBody ([], red, c, None)
      else
        (match c with
        | { CAst.v = CCast(c, CastConv t) } -> DefineBody (bl, red, c, Some t)
        | _ -> DefineBody (bl, red, c, None)) 
                                       ))])])
        in let () =
        Pcoq.grammar_extend reduce None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 363 "g_vernac.mlg"
             None 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Eval"))))),
                                          (Extend.Aentry red_expr)),
                             (Extend.Atoken (Tok.PKEYWORD ("in")))),
                (fun _ r _ loc -> 
# 362 "g_vernac.mlg"
                                              Some r 
                                  ))])])
        in let () =
        Pcoq.grammar_extend one_decl_notation None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Aentry ne_lstring)),
                                                        (Extend.Atoken (Tok.PKEYWORD (":=")))),
                                           (Extend.Aentry constr)),
                              (Extend.Aopt (Extend.Arules [Extend.Rules 
                                                          (Extend.NextNoRec 
                                                          (Extend.NextNoRec 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (Tok.PKEYWORD (":")))),
                                                          (Extend.Atoken (Tok.PIDENT (None)))),
                                                          (fun sc _ loc -> 
                                                          
# 367 "g_vernac.mlg"
                                           sc 
                                                          ))]))),
                 (fun scopt c _ ntn loc -> 
# 367 "g_vernac.mlg"
                                                       (ntn,c,scopt) 
                                           ))])])
        in let () =
        Pcoq.grammar_extend decl_sep None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                              ("and"))))),
                 (fun _ loc -> 
# 370 "g_vernac.mlg"
                         () 
                               ))])])
        in let () =
        Pcoq.grammar_extend decl_notation None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 374 "g_vernac.mlg"
           [] 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("where")))),
                             (Extend.Alist1sep ((Extend.Aentry one_decl_notation), (Extend.Aentry decl_sep)))),
                (fun l _ loc -> 
# 373 "g_vernac.mlg"
                                                               l 
                                ))])])
        in let () =
        Pcoq.grammar_extend opt_constructors_or_fields None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 379 "g_vernac.mlg"
             RecordDecl (None, []) 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD (":=")))),
                             (Extend.Aentry constructor_list_or_record_decl)),
                (fun lc _ loc -> 
# 378 "g_vernac.mlg"
                                                        lc 
                                 ))])])
        in let () =
        Pcoq.grammar_extend inductive_definition None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Aentry opt_coercion)),
                                                        (Extend.Aentry ident_decl)),
                                                        (Extend.Aentry binders)),
                                                        (Extend.Aopt (Extend.Arules 
                                                        [Extend.Rules 
                                                        (Extend.NextNoRec 
                                                        (Extend.NextNoRec 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD (":")))),
                                                        (Extend.Aentry lconstr)),
                                                        (fun c _ loc -> 
                                                        
# 383 "g_vernac.mlg"
                                        c 
                                                        ))]))),
                                           (Extend.Aentry opt_constructors_or_fields)),
                              (Extend.Aentry decl_notation)),
                 (fun ntn lc c indpar id oc loc -> 
# 385 "g_vernac.mlg"
             (((oc,id),indpar,c,lc),ntn) 
                                                   ))])])
        in let () =
        Pcoq.grammar_extend constructor_list_or_record_decl None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 395 "g_vernac.mlg"
              Constructors [] 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD ("{")))),
                                          (Extend.Aentry record_fields)),
                             (Extend.Atoken (Tok.PKEYWORD ("}")))),
                (fun _ fs _ loc -> 
# 394 "g_vernac.mlg"
                                         RecordDecl (None,fs) 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Aentry identref)),
                                                       (Extend.Atoken (Tok.PKEYWORD ("{")))),
                                          (Extend.Aentry record_fields)),
                             (Extend.Atoken (Tok.PKEYWORD ("}")))),
                (fun _ fs _ cstr loc -> 
# 393 "g_vernac.mlg"
            RecordDecl (Some cstr,fs) 
                                        ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry identref)),
                             (Extend.Aentry constructor_type)),
                (fun c id loc -> 
# 391 "g_vernac.mlg"
                                                  Constructors [ c id ] 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Aentry identref)),
                                                       (Extend.Aentry constructor_type)),
                                          (Extend.Atoken (Tok.PKEYWORD ("|")))),
                             (Extend.Alist0sep ((Extend.Aentry constructor), (Extend.Atoken (Tok.PKEYWORD ("|")))))),
                (fun l _ c id loc -> 
# 390 "g_vernac.mlg"
            Constructors ((c id)::l) 
                                     ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("|")))),
                             (Extend.Alist1sep ((Extend.Aentry constructor), (Extend.Atoken (Tok.PKEYWORD ("|")))))),
                (fun l _ loc -> 
# 388 "g_vernac.mlg"
                                                Constructors l 
                                ))])])
        in let () =
        Pcoq.grammar_extend opt_coercion None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 404 "g_vernac.mlg"
              false 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD (">")))),
                (fun _ loc -> 
# 403 "g_vernac.mlg"
                 true 
                              ))])])
        in let () =
        Pcoq.grammar_extend rec_definition None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Aentry ident_decl)),
                                                        (Extend.Aentry binders_fixannot)),
                                                        (Extend.Aentry type_cstr)),
                                           (Extend.Aopt (Extend.Arules 
                                           [Extend.Rules (Extend.NextNoRec 
                                                         (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (Tok.PKEYWORD (":=")))),
                                                         (Extend.Aentry lconstr)),
                                                         (fun def _ loc -> 
                                                         
# 411 "g_vernac.mlg"
                                            def 
                                                         ))]))),
                              (Extend.Aentry decl_notation)),
                 (fun ntn def ty bl id loc -> 
# 412 "g_vernac.mlg"
            let bl, annot = bl in ((id,annot,bl,ty,def),ntn) 
                                              ))])])
        in let () =
        Pcoq.grammar_extend corec_definition None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Aentry ident_decl)),
                                                        (Extend.Aentry binders)),
                                                        (Extend.Aentry type_cstr)),
                                           (Extend.Aopt (Extend.Arules 
                                           [Extend.Rules (Extend.NextNoRec 
                                                         (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (Tok.PKEYWORD (":=")))),
                                                         (Extend.Aentry lconstr)),
                                                         (fun def _ loc -> 
                                                         
# 416 "g_vernac.mlg"
                                            def 
                                                         ))]))),
                              (Extend.Aentry decl_notation)),
                 (fun ntn def ty bl id loc -> 
# 417 "g_vernac.mlg"
            ((id,bl,ty,def),ntn) 
                                              ))])])
        in let () =
        Pcoq.grammar_extend type_cstr None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 421 "g_vernac.mlg"
             CAst.make ~loc @@ CHole (None, IntroAnonymous, None) 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD (":")))),
                             (Extend.Aentry lconstr)),
                (fun c _ loc -> 
# 420 "g_vernac.mlg"
                            c 
                                ))])])
        in let () =
        Pcoq.grammar_extend scheme None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Aentry identref)),
                                           (Extend.Atoken (Tok.PKEYWORD (":=")))),
                              (Extend.Aentry scheme_kind)),
                 (fun kind _ id loc -> 
# 426 "g_vernac.mlg"
                                                     (Some id,kind) 
                                       ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry scheme_kind)),
                (fun kind loc -> 
# 425 "g_vernac.mlg"
                                (None,kind) 
                                 ))])])
        in let () =
        Pcoq.grammar_extend scheme_kind None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Atoken (Tok.PIDENT (Some
                                                        ("Equality"))))),
                                           (Extend.Atoken (Tok.PKEYWORD ("for")))),
                              (Extend.Aentry smart_global)),
                 (fun ind _ _ loc -> 
# 437 "g_vernac.mlg"
                                                          EqualityScheme(ind) 
                                     ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Case"))))),
                                                                    (Extend.Atoken (Tok.PKEYWORD ("for")))),
                                                       (Extend.Aentry smart_global)),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Sort"))))),
                             (Extend.Aentry sort_family)),
                (fun s _ ind _ _ loc -> 
# 436 "g_vernac.mlg"
                                            CaseScheme(false,ind,s) 
                                        ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Elimination"))))),
                                                                    (Extend.Atoken (Tok.PKEYWORD ("for")))),
                                                       (Extend.Aentry smart_global)),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Sort"))))),
                             (Extend.Aentry sort_family)),
                (fun s _ ind _ _ loc -> 
# 434 "g_vernac.mlg"
                                            CaseScheme(true,ind,s) 
                                        ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Minimality"))))),
                                                                    (Extend.Atoken (Tok.PKEYWORD ("for")))),
                                                       (Extend.Aentry smart_global)),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Sort"))))),
                             (Extend.Aentry sort_family)),
                (fun s _ ind _ _ loc -> 
# 432 "g_vernac.mlg"
                                            InductionScheme(false,ind,s) 
                                        ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Induction"))))),
                                                                    (Extend.Atoken (Tok.PKEYWORD ("for")))),
                                                       (Extend.Aentry smart_global)),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Sort"))))),
                             (Extend.Aentry sort_family)),
                (fun s _ ind _ _ loc -> 
# 430 "g_vernac.mlg"
                                            InductionScheme(true,ind,s) 
                                        ))])])
        in let () =
        Pcoq.grammar_extend record_field None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Aentry record_binder)),
                                           (Extend.Aopt (Extend.Arules 
                                           [Extend.Rules (Extend.NextNoRec 
                                                         (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (Tok.PKEYWORD ("|")))),
                                                         (Extend.Aentry natural)),
                                                         (fun n _ loc -> 
                                                         
# 453 "g_vernac.mlg"
                                                            n 
                                                         ))]))),
                              (Extend.Aentry decl_notation)),
                 (fun ntn pri bd loc -> 
# 454 "g_vernac.mlg"
                               (bd,pri),ntn 
                                        ))])])
        in let () =
        Pcoq.grammar_extend record_fields None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 460 "g_vernac.mlg"
             [] 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry record_field)),
                (fun f loc -> 
# 459 "g_vernac.mlg"
                              [f] 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry record_field)),
                             (Extend.Atoken (Tok.PKEYWORD (";")))),
                (fun _ f loc -> 
# 458 "g_vernac.mlg"
                                   [f] 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry record_field)),
                                          (Extend.Atoken (Tok.PKEYWORD (";")))),
                             (Extend.Aentry record_fields)),
                (fun fs _ f loc -> 
# 457 "g_vernac.mlg"
                                                       f :: fs 
                                   ))])])
        in let () =
        Pcoq.grammar_extend record_binder_body None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Aentry binders)),
                                           (Extend.Atoken (Tok.PKEYWORD (":=")))),
                              (Extend.Aentry lconstr)),
                 (fun b _ l loc -> 
# 469 "g_vernac.mlg"
                                            fun id ->
         match b.CAst.v with
         | CCast(b', (CastConv t|CastVM t|CastNative t)) ->
             (None,DefExpr(id,mkLambdaCN ~loc l b',Some (mkProdCN ~loc l t)))
         | _ ->
             (None,DefExpr(id,mkLambdaCN ~loc l b,None)) 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Aentry binders)),
                                                                    (Extend.Aentry of_type_with_opt_coercion)),
                                                       (Extend.Aentry lconstr)),
                                          (Extend.Atoken (Tok.PKEYWORD (":=")))),
                             (Extend.Aentry lconstr)),
                (fun b _ t oc l loc -> 
# 467 "g_vernac.mlg"
                                             fun id ->
           (oc,DefExpr (id,mkLambdaCN ~loc l b,Some (mkProdCN ~loc l t))) 
                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry binders)),
                                          (Extend.Aentry of_type_with_opt_coercion)),
                             (Extend.Aentry lconstr)),
                (fun t oc l loc -> 
# 465 "g_vernac.mlg"
                          fun id -> (oc,AssumExpr (id,mkProdCN ~loc l t)) 
                                   ))])])
        in let () =
        Pcoq.grammar_extend record_binder None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry name)),
                              (Extend.Aentry record_binder_body)),
                 (fun f id loc -> 
# 478 "g_vernac.mlg"
                                               f id 
                                  ));
                Extend.Rule (Extend.Next (Extend.Stop, (Extend.Aentry name)),
                (fun id loc -> 
# 477 "g_vernac.mlg"
                       (None,AssumExpr(id, CAst.make ~loc @@ CHole (None, IntroAnonymous, None))) 
                               ))])])
        in let () =
        Pcoq.grammar_extend assum_list None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry simple_assum_coe)),
                 (fun b loc -> 
# 481 "g_vernac.mlg"
                                                                   [b] 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Alist1 (Extend.Aentry assum_coe))),
                (fun bl loc -> 
# 481 "g_vernac.mlg"
                                  bl 
                               ))])])
        in let () =
        Pcoq.grammar_extend assum_coe None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                           (Extend.Aentry simple_assum_coe)),
                              (Extend.Atoken (Tok.PKEYWORD (")")))),
                 (fun _ a _ loc -> 
# 484 "g_vernac.mlg"
                                            a 
                                   ))])])
        in let () =
        Pcoq.grammar_extend simple_assum_coe None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Alist1 (Extend.Aentry ident_decl))),
                                           (Extend.Aentry of_type_with_opt_coercion)),
                              (Extend.Aentry lconstr)),
                 (fun c oc idl loc -> 
# 488 "g_vernac.mlg"
          (not (Option.is_empty oc),(idl,c)) 
                                      ))])])
        in let () =
        Pcoq.grammar_extend constructor_type None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry binders)),
                              (Extend.Arules [Extend.Rules (Extend.Stop, (fun
                                                           loc -> 
# 496 "g_vernac.mlg"
                   fun l id -> (false,(id,mkProdCN ~loc l (CAst.make ~loc @@ CHole (None, IntroAnonymous, None)))) 
                                                                  ));
                                             Extend.Rules (Extend.NextNoRec 
                                                          (Extend.NextNoRec 
                                                          (Extend.Stop,
                                                          (Extend.Aentry of_type_with_opt_coercion)),
                                                          (Extend.Aentry lconstr)),
                                                          (fun c coe loc ->
                                                          
# 494 "g_vernac.mlg"
                      fun l id -> (not (Option.is_empty coe),(id,mkProdCN ~loc l c)) 
                                                          ))])),
                 (fun t l loc -> 
# 497 "g_vernac.mlg"
              t l 
                                 ))])])
        in let () =
        Pcoq.grammar_extend constructor None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry identref)),
                              (Extend.Aentry constructor_type)),
                 (fun c id loc -> 
# 502 "g_vernac.mlg"
                                               c id 
                                  ))])])
        in let () =
        Pcoq.grammar_extend of_type_with_opt_coercion None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Atoken (Tok.PKEYWORD (":")))),
                 (fun _ loc -> 
# 510 "g_vernac.mlg"
                   None 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD (":")))),
                             (Extend.Atoken (Tok.PKEYWORD (">")))),
                (fun _ _ loc -> 
# 509 "g_vernac.mlg"
                        Some true 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD (":")))),
                                          (Extend.Atoken (Tok.PKEYWORD (">")))),
                             (Extend.Atoken (Tok.PKEYWORD (">")))),
                (fun _ _ _ loc -> 
# 508 "g_vernac.mlg"
                             Some false 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD (":>")))),
                (fun _ loc -> 
# 507 "g_vernac.mlg"
                    Some true 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD (":>")))),
                             (Extend.Atoken (Tok.PKEYWORD (">")))),
                (fun _ _ loc -> 
# 506 "g_vernac.mlg"
                         Some false 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD (":>>")))),
                (fun _ loc -> 
# 505 "g_vernac.mlg"
                   Some false 
                              ))])])
        in ()


# 514 "g_vernac.mlg"
 

let only_starredidentrefs =
  Pcoq.Entry.of_parser "test_only_starredidentrefs"
    (fun strm ->
      let rec aux n =
      match Util.stream_nth n strm with
        | KEYWORD "." -> ()
        | KEYWORD ")" -> ()
        | (IDENT _ | KEYWORD "Type" | KEYWORD "*") -> aux (n+1)
        | _ -> raise Stream.Failure in
      aux 0)
let starredidentreflist_to_expr l =
  match l with
  | [] -> SsEmpty
  | x :: xs -> List.fold_right (fun i acc -> SsUnion(i,acc)) xs x

let warn_deprecated_include_type =
  CWarnings.create ~name:"deprecated-include-type" ~category:"deprecated"
         (fun () -> strbrk "Include Type is deprecated; use Include instead")



let _ = let export_token = Pcoq.Entry.create "export_token"
        and ext_module_type = Pcoq.Entry.create "ext_module_type"
        and ext_module_expr = Pcoq.Entry.create "ext_module_expr"
        and check_module_type = Pcoq.Entry.create "check_module_type"
        and check_module_types = Pcoq.Entry.create "check_module_types"
        and of_module_type = Pcoq.Entry.create "of_module_type"
        and is_module_type = Pcoq.Entry.create "is_module_type"
        and is_module_expr = Pcoq.Entry.create "is_module_expr"
        and functor_app_annot = Pcoq.Entry.create "functor_app_annot"
        and module_expr_inl = Pcoq.Entry.create "module_expr_inl"
        and module_type_inl = Pcoq.Entry.create "module_type_inl"
        and module_binder = Pcoq.Entry.create "module_binder"
        and module_expr_atom = Pcoq.Entry.create "module_expr_atom"
        and with_declaration = Pcoq.Entry.create "with_declaration"
        and starredidentref = Pcoq.Entry.create "starredidentref"
        and ssexpr = Pcoq.Entry.create "ssexpr"
        in
        let () =
        Pcoq.grammar_extend gallina_ext None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PIDENT (Some
                                                        ("Include"))))),
                                                        (Extend.Atoken (Tok.PKEYWORD ("Type")))),
                                           (Extend.Aentry module_type_inl)),
                              (Extend.Alist0 (Extend.Aentry ext_module_type))),
                 (fun l e _ _ loc -> 
# 576 "g_vernac.mlg"
           warn_deprecated_include_type ~loc ();
        VernacInclude(e::l) 
                                     ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Include"))))),
                                          (Extend.Aentry module_type_inl)),
                             (Extend.Alist0 (Extend.Aentry ext_module_expr))),
                (fun l e _ loc -> 
# 574 "g_vernac.mlg"
            VernacInclude(e::l) 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Export"))))),
                             (Extend.Alist1 (Extend.Aentry global))),
                (fun qidl _ loc -> 
# 572 "g_vernac.mlg"
                                                 VernacImport (true,qidl) 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Import"))))),
                             (Extend.Alist1 (Extend.Aentry global))),
                (fun qidl _ loc -> 
# 571 "g_vernac.mlg"
                                                 VernacImport (false,qidl) 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("From"))))),
                                                                    (Extend.Aentry global)),
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Require"))))),
                                          (Extend.Aentry export_token)),
                             (Extend.Alist1 (Extend.Aentry global))),
                (fun qidl export _ ns _ loc -> 
# 570 "g_vernac.mlg"
          VernacRequire (Some ns, export, qidl) 
                                               ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Require"))))),
                                          (Extend.Aentry export_token)),
                             (Extend.Alist1 (Extend.Aentry global))),
                (fun qidl export _ loc -> 
# 567 "g_vernac.mlg"
            VernacRequire (None, export, qidl) 
                                          ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Collection"))))),
                                                       (Extend.Aentry identref)),
                                          (Extend.Atoken (Tok.PKEYWORD (":=")))),
                             (Extend.Aentry section_subset_expr)),
                (fun expr _ id _ loc -> 
# 563 "g_vernac.mlg"
            VernacNameSectionHypSet (id, expr) 
                                        ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("End"))))),
                             (Extend.Aentry identref)),
                (fun id _ loc -> 
# 559 "g_vernac.mlg"
                                        VernacEndSegment id 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Chapter"))))),
                             (Extend.Aentry identref)),
                (fun id _ loc -> 
# 556 "g_vernac.mlg"
                                            VernacBeginSection id 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Section"))))),
                             (Extend.Aentry identref)),
                (fun id _ loc -> 
# 555 "g_vernac.mlg"
                                            VernacBeginSection id 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Declare"))))),
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Module"))))),
                                                                    (Extend.Aentry export_token)),
                                                                    (Extend.Aentry identref)),
                                                       (Extend.Alist0 (Extend.Aentry module_binder))),
                                          (Extend.Atoken (Tok.PKEYWORD (":")))),
                             (Extend.Aentry module_type_inl)),
                (fun mty _ bl id export _ _ loc -> 
# 553 "g_vernac.mlg"
            VernacDeclareModule (export, id, bl, mty) 
                                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Module"))))),
                                                                    (Extend.Atoken (Tok.PKEYWORD ("Type")))),
                                                                    (Extend.Aentry identref)),
                                                       (Extend.Alist0 (Extend.Aentry module_binder))),
                                          (Extend.Aentry check_module_types)),
                             (Extend.Aentry is_module_type)),
                (fun body sign bl id _ _ loc -> 
# 550 "g_vernac.mlg"
            VernacDeclareModuleType (id, bl, sign, body) 
                                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Module"))))),
                                                                    (Extend.Aentry export_token)),
                                                                    (Extend.Aentry identref)),
                                                       (Extend.Alist0 (Extend.Aentry module_binder))),
                                          (Extend.Aentry of_module_type)),
                             (Extend.Aentry is_module_expr)),
                (fun body sign bl id export _ loc -> 
# 546 "g_vernac.mlg"
            VernacDefineModule (export, id, bl, sign, body) 
                                                     ))])])
        in let () =
        Pcoq.grammar_extend export_token None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 582 "g_vernac.mlg"
              None 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Export"))))),
                (fun _ loc -> 
# 581 "g_vernac.mlg"
                            Some true 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Import"))))),
                (fun _ loc -> 
# 580 "g_vernac.mlg"
                            Some false 
                              ))])])
        in let () =
        Pcoq.grammar_extend ext_module_type None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PKEYWORD ("<+")))),
                              (Extend.Aentry module_type_inl)),
                 (fun mty _ loc -> 
# 585 "g_vernac.mlg"
                                         mty 
                                   ))])])
        in let () =
        Pcoq.grammar_extend ext_module_expr None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PKEYWORD ("<+")))),
                              (Extend.Aentry module_expr_inl)),
                 (fun mexpr _ loc -> 
# 588 "g_vernac.mlg"
                                           mexpr 
                                     ))])])
        in let () =
        Pcoq.grammar_extend check_module_type None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PKEYWORD ("<:")))),
                              (Extend.Aentry module_type_inl)),
                 (fun mty _ loc -> 
# 591 "g_vernac.mlg"
                                         mty 
                                   ))])])
        in let () =
        Pcoq.grammar_extend check_module_types None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Alist0 (Extend.Aentry check_module_type))),
                 (fun mtys loc -> 
# 594 "g_vernac.mlg"
                                            mtys 
                                  ))])])
        in let () =
        Pcoq.grammar_extend of_module_type None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Aentry check_module_types)),
                 (fun mtys loc -> 
# 598 "g_vernac.mlg"
                                       Check mtys 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD (":")))),
                             (Extend.Aentry module_type_inl)),
                (fun mty _ loc -> 
# 597 "g_vernac.mlg"
                                        Enforce mty 
                                  ))])])
        in let () =
        Pcoq.grammar_extend is_module_type None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 602 "g_vernac.mlg"
             [] 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD (":=")))),
                                          (Extend.Aentry module_type_inl)),
                             (Extend.Alist0 (Extend.Aentry ext_module_type))),
                (fun l mty _ loc -> 
# 601 "g_vernac.mlg"
                                                                     (mty::l) 
                                    ))])])
        in let () =
        Pcoq.grammar_extend is_module_expr None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 606 "g_vernac.mlg"
             [] 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD (":=")))),
                                          (Extend.Aentry module_expr_inl)),
                             (Extend.Alist0 (Extend.Aentry ext_module_expr))),
                (fun l mexpr _ loc -> 
# 605 "g_vernac.mlg"
                                                                      (mexpr::l) 
                                      ))])])
        in let () =
        Pcoq.grammar_extend functor_app_annot None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 612 "g_vernac.mlg"
             DefaultInline 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("[")))),
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("no"))))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("inline"))))),
                             (Extend.Atoken (Tok.PKEYWORD ("]")))),
                (fun _ _ _ _ loc -> 
# 611 "g_vernac.mlg"
                                                  NoInline 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("[")))),
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("inline"))))),
                                                                    (Extend.Atoken (Tok.PKEYWORD ("at")))),
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("level"))))),
                                          (Extend.Aentry natural)),
                             (Extend.Atoken (Tok.PKEYWORD ("]")))),
                (fun _ i _ _ _ _ loc -> 
# 610 "g_vernac.mlg"
          InlineAt i 
                                        ))])])
        in let () =
        Pcoq.grammar_extend module_expr_inl None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry module_expr)),
                              (Extend.Aentry functor_app_annot)),
                 (fun a me loc -> 
# 617 "g_vernac.mlg"
                                                     (me,a) 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("!")))),
                             (Extend.Aentry module_expr)),
                (fun me _ loc -> 
# 616 "g_vernac.mlg"
                                   (me,NoInline) 
                                 ))])])
        in let () =
        Pcoq.grammar_extend module_type_inl None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry module_type)),
                              (Extend.Aentry functor_app_annot)),
                 (fun a me loc -> 
# 621 "g_vernac.mlg"
                                                     (me,a) 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("!")))),
                             (Extend.Aentry module_type)),
                (fun me _ loc -> 
# 620 "g_vernac.mlg"
                                   (me,NoInline) 
                                 ))])])
        in let () =
        Pcoq.grammar_extend module_binder None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                        (Extend.Aentry export_token)),
                                                        (Extend.Alist1 (Extend.Aentry identref))),
                                                        (Extend.Atoken (Tok.PKEYWORD (":")))),
                                           (Extend.Aentry module_type_inl)),
                              (Extend.Atoken (Tok.PKEYWORD (")")))),
                 (fun _ mty _ idl export _ loc -> 
# 626 "g_vernac.mlg"
                                         (export,idl,mty) 
                                                  ))])])
        in let () =
        Pcoq.grammar_extend module_expr None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry module_expr)),
                              (Extend.Aentry module_expr_atom)),
                 (fun me2 me1 loc -> 
# 631 "g_vernac.mlg"
                                                       CAst.make ~loc @@ CMapply (me1,me2) 
                                     ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry module_expr_atom)),
                (fun me loc -> 
# 630 "g_vernac.mlg"
                                   me 
                               ))])])
        in let () =
        Pcoq.grammar_extend module_expr_atom None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                           (Extend.Aentry module_expr)),
                              (Extend.Atoken (Tok.PKEYWORD (")")))),
                 (fun _ me _ loc -> 
# 635 "g_vernac.mlg"
                                                                                            me 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry qualid)),
                (fun qid loc -> 
# 635 "g_vernac.mlg"
                          CAst.make ~loc @@ CMident qid 
                                ))])])
        in let () =
        Pcoq.grammar_extend with_declaration None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PIDENT (Some
                                                        ("Module"))))),
                                                        (Extend.Aentry fullyqualid)),
                                           (Extend.Atoken (Tok.PKEYWORD (":=")))),
                              (Extend.Aentry qualid)),
                 (fun qid _ fqid _ loc -> 
# 641 "g_vernac.mlg"
            CWith_Module (fqid,qid) 
                                          ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("Definition")))),
                                                                    (Extend.Aentry fullyqualid)),
                                                       (Extend.Aopt (Extend.Aentry univ_decl))),
                                          (Extend.Atoken (Tok.PKEYWORD (":=")))),
                             (Extend.Aentry Constr.lconstr)),
                (fun c _ udecl fqid _ loc -> 
# 639 "g_vernac.mlg"
            CWith_Definition (fqid,udecl,c) 
                                             ))])])
        in let () =
        Pcoq.grammar_extend module_type None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Aentry module_type)),
                                           (Extend.Atoken (Tok.PKEYWORD ("with")))),
                              (Extend.Aentry with_declaration)),
                 (fun decl _ mty loc -> 
# 650 "g_vernac.mlg"
            CAst.make ~loc @@ CMwith (mty,decl) 
                                        ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry module_type)),
                             (Extend.Aentry module_expr_atom)),
                (fun me mty loc -> 
# 648 "g_vernac.mlg"
            CAst.make ~loc @@ CMapply (mty,me) 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                          (Extend.Aentry module_type)),
                             (Extend.Atoken (Tok.PKEYWORD (")")))),
                (fun _ mt _ loc -> 
# 646 "g_vernac.mlg"
                                        mt 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry qualid)),
                (fun qid loc -> 
# 645 "g_vernac.mlg"
                          CAst.make ~loc @@ CMident qid 
                                ))])])
        in let () =
        Pcoq.grammar_extend section_subset_expr None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry ssexpr)),
                 (fun e loc -> 
# 657 "g_vernac.mlg"
                        e 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry only_starredidentrefs)),
                             (Extend.Alist0 (Extend.Aentry starredidentref))),
                (fun l _ loc -> 
# 656 "g_vernac.mlg"
            starredidentreflist_to_expr l 
                                ))])])
        in let () =
        Pcoq.grammar_extend starredidentref None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PKEYWORD ("Type")))),
                              (Extend.Atoken (Tok.PKEYWORD ("*")))),
                 (fun _ _ loc -> 
# 663 "g_vernac.mlg"
                         SsFwdClose SsType 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("Type")))),
                (fun _ loc -> 
# 662 "g_vernac.mlg"
                    SsType 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry identref)),
                             (Extend.Atoken (Tok.PKEYWORD ("*")))),
                (fun _ i loc -> 
# 661 "g_vernac.mlg"
                               SsFwdClose(SsSingl i) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry identref)),
                (fun i loc -> 
# 660 "g_vernac.mlg"
                          SsSingl i 
                              ))])])
        in let () =
        Pcoq.grammar_extend ssexpr None
        (None, [(Some ("35"), None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PKEYWORD ("-")))),
                              (Extend.Aentry ssexpr)),
                 (fun e _ loc -> 
# 667 "g_vernac.mlg"
                             SsCompl e 
                                 ))]);
               (Some ("50"), None,
               [Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry ssexpr)),
                                          (Extend.Atoken (Tok.PKEYWORD ("+")))),
                             (Extend.Aentry ssexpr)),
                (fun e2 _ e1 loc -> 
# 670 "g_vernac.mlg"
                                          SsUnion(e1,e2) 
                                    ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                      (Extend.Aentry ssexpr)),
                                         (Extend.Atoken (Tok.PKEYWORD ("-")))),
                            (Extend.Aentry ssexpr)),
               (fun e2 _ e1 loc -> 
# 669 "g_vernac.mlg"
                                          SsSubstr(e1,e2) 
                                   ))]);
               (Some ("0"), None,
               [Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                       (Extend.Aentry ssexpr)),
                                          (Extend.Atoken (Tok.PKEYWORD (")")))),
                             (Extend.Atoken (Tok.PKEYWORD ("*")))),
                (fun _ _ e _ loc -> 
# 678 "g_vernac.mlg"
                                       SsFwdClose e 
                                    ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                      (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                         (Extend.Aentry ssexpr)),
                            (Extend.Atoken (Tok.PKEYWORD (")")))),
               (fun _ e _ loc -> 
# 677 "g_vernac.mlg"
                                 e 
                                 ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                   (Extend.Stop,
                                                                   (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                                   (Extend.Aentry only_starredidentrefs)),
                                                      (Extend.Alist0 (Extend.Aentry starredidentref))),
                                         (Extend.Atoken (Tok.PKEYWORD (")")))),
                            (Extend.Atoken (Tok.PKEYWORD ("*")))),
               (fun _ _ l _ _ loc -> 
# 676 "g_vernac.mlg"
            SsFwdClose(starredidentreflist_to_expr l) 
                                     ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                   (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                      (Extend.Aentry only_starredidentrefs)),
                                         (Extend.Alist0 (Extend.Aentry starredidentref))),
                            (Extend.Atoken (Tok.PKEYWORD (")")))),
               (fun _ l _ _ loc -> 
# 674 "g_vernac.mlg"
            starredidentreflist_to_expr l 
                                   ));
               Extend.Rule
               (Extend.Next (Extend.Stop, (Extend.Aentry starredidentref)),
               (fun i loc -> 
# 672 "g_vernac.mlg"
                                 i 
                             ))])])
        in ()

let _ = let arguments_modifier = Pcoq.Entry.create "arguments_modifier"
        and scope = Pcoq.Entry.create "scope"
        and argument_spec = Pcoq.Entry.create "argument_spec"
        and argument_spec_block = Pcoq.Entry.create "argument_spec_block"
        and more_implicits_block = Pcoq.Entry.create "more_implicits_block"
        and strategy_level = Pcoq.Entry.create "strategy_level"
        and reserv_list = Pcoq.Entry.create "reserv_list"
        and reserv_tuple = Pcoq.Entry.create "reserv_tuple"
        and simple_reserv = Pcoq.Entry.create "simple_reserv"
        in
        let () =
        Pcoq.grammar_extend gallina_ext None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PIDENT (Some
                                           ("Generalizable"))))),
                              (Extend.Arules [Extend.Rules (Extend.NextNoRec 
                                                           (Extend.NextNoRec 
                                                           (Extend.Stop,
                                                           (Extend.Arules 
                                                           [Extend.Rules 
                                                           (Extend.NextNoRec 
                                                           (Extend.Stop,
                                                           (Extend.Atoken (Tok.PIDENT (Some
                                                           ("Variables"))))),
                                                           (fun _ loc -> 
                                                           
# 776 "g_vernac.mlg"
                                                              () 
                                                           ));
                                                           Extend.Rules 
                                                           (Extend.NextNoRec 
                                                           (Extend.Stop,
                                                           (Extend.Atoken (Tok.PKEYWORD ("Variable")))),
                                                           (fun _ loc -> 
                                                           
# 776 "g_vernac.mlg"
                                () 
                                                           ))])),
                                                           (Extend.Alist1 (Extend.Aentry identref))),
                                                           (fun idl _ loc ->
                                                           
# 777 "g_vernac.mlg"
                                            Some idl 
                                                           ));
                                             Extend.Rules (Extend.NextNoRec 
                                                          (Extend.NextNoRec 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (Tok.PIDENT (Some
                                                          ("No"))))),
                                                          (Extend.Atoken (Tok.PIDENT (Some
                                                          ("Variables"))))),
                                                          (fun _ _ loc -> 
                                                          
# 775 "g_vernac.mlg"
                                                  None 
                                                          ));
                                             Extend.Rules (Extend.NextNoRec 
                                                          (Extend.NextNoRec 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (Tok.PIDENT (Some
                                                          ("All"))))),
                                                          (Extend.Atoken (Tok.PIDENT (Some
                                                          ("Variables"))))),
                                                          (fun _ _ loc -> 
                                                          
# 774 "g_vernac.mlg"
                                                      Some [] 
                                                          ))])),
                 (fun gen _ loc -> 
# 778 "g_vernac.mlg"
               VernacGeneralizable gen 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Implicit"))))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Types"))))),
                             (Extend.Aentry reserv_list)),
                (fun bl _ _ loc -> 
# 770 "g_vernac.mlg"
            test_plural_form_types loc "Implicit Types" bl;
           VernacReserve bl 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Implicit"))))),
                                          (Extend.Atoken (Tok.PKEYWORD ("Type")))),
                             (Extend.Aentry reserv_list)),
                (fun bl _ _ loc -> 
# 767 "g_vernac.mlg"
             VernacReserve bl 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Arguments"))))),
                                                                    (Extend.Aentry smart_global)),
                                                       (Extend.Alist0 (Extend.Aentry argument_spec_block))),
                                          (Extend.Aopt (Extend.Arules 
                                          [Extend.Rules (Extend.NextNoRec 
                                                        (Extend.NextNoRec 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD (",")))),
                                                        (Extend.Alist1sep ((Extend.Arules 
                                                        [Extend.Rules 
                                                        (Extend.NextNoRec 
                                                        (Extend.Stop,
                                                        (Extend.Alist0 (Extend.Aentry more_implicits_block))),
                                                        (fun impl loc -> 
                                                        
# 747 "g_vernac.mlg"
                                                     List.flatten impl 
                                                        ))]), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                                                        (fun impl _ loc -> 
                                                        
# 748 "g_vernac.mlg"
                         impl 
                                                        ))]))),
                             (Extend.Aopt (Extend.Arules [Extend.Rules 
                                                         (Extend.NextNoRec 
                                                         (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (Tok.PKEYWORD (":")))),
                                                         (Extend.Alist1sep ((Extend.Aentry arguments_modifier), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                                                         (fun l _ loc -> 
                                                         
# 750 "g_vernac.mlg"
                                                                    l 
                                                         ))]))),
                (fun mods more_implicits args qid _ loc -> 
# 751 "g_vernac.mlg"
           let mods = match mods with None -> [] | Some l -> List.flatten l in
         let slash_position = ref None in
         let rec parse_args i = function
           | [] -> []
           | `Id x :: args -> x :: parse_args (i+1) args
           | `Slash :: args ->
              if Option.is_empty !slash_position then
                (slash_position := Some i; parse_args i args)
              else
                user_err Pp.(str "The \"/\" modifier can occur only once")
         in
         let args = parse_args 0 (List.flatten args) in
         let more_implicits = Option.default [] more_implicits in
         VernacArguments (qid, args, more_implicits, !slash_position, mods) 
                                                           ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Existing"))))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Class"))))),
                             (Extend.Aentry global)),
                (fun is _ _ loc -> 
# 740 "g_vernac.mlg"
                                                          VernacExistingClass is 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Existing"))))),
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Instances"))))),
                                          (Extend.Alist1 (Extend.Aentry global))),
                             (Extend.Aopt (Extend.Arules [Extend.Rules 
                                                         (Extend.NextNoRec 
                                                         (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (Tok.PKEYWORD ("|")))),
                                                         (Extend.Aentry natural)),
                                                         (fun i _ loc -> 
                                                         
# 735 "g_vernac.mlg"
                                          i 
                                                         ))]))),
                (fun pri ids _ _ loc -> 
# 736 "g_vernac.mlg"
           let info = { Typeclasses.hint_priority = pri; hint_pattern = None } in
         let insts = List.map (fun i -> (i, info)) ids in
          VernacExistingInstance insts 
                                        ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Existing"))))),
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Instance"))))),
                                          (Extend.Aentry global)),
                             (Extend.Aentry hint_info)),
                (fun info id _ _ loc -> 
# 732 "g_vernac.mlg"
            VernacExistingInstance [id, info] 
                                        ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Instance"))))),
                                                                    (Extend.Aentry instance_name)),
                                                                    (Extend.Atoken (Tok.PKEYWORD (":")))),
                                                                    (Extend.Arules 
                                                                    [Extend.Rules 
                                                                    (Extend.Stop,
                                                                    (fun
                                                                    loc -> 
                                                                    
# 724 "g_vernac.mlg"
                                                        Decl_kinds.Explicit 
                                                                    ));
                                                                    Extend.Rules 
                                                                    (Extend.NextNoRec 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("!")))),
                                                                    (fun _
                                                                    loc -> 
                                                                    
# 724 "g_vernac.mlg"
                           Decl_kinds.Implicit 
                                                                    ))])),
                                                       (Extend.Aentryl (operconstr, "200"))),
                                          (Extend.Aentry hint_info)),
                             (Extend.Arules [Extend.Rules (Extend.Stop, (fun
                                                          loc -> 
# 727 "g_vernac.mlg"
                                                            None 
                                                                 ));
                                            Extend.Rules (Extend.NextNoRec 
                                                         (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (Tok.PKEYWORD (":=")))),
                                                         (Extend.Aentry lconstr)),
                                                         (fun c _ loc -> 
                                                         
# 727 "g_vernac.mlg"
                                    Some (false,c) 
                                                         ));
                                            Extend.Rules (Extend.NextNoRec 
                                                         (Extend.NextNoRec 
                                                         (Extend.NextNoRec 
                                                         (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (Tok.PKEYWORD (":=")))),
                                                         (Extend.Atoken (Tok.PKEYWORD ("{")))),
                                                         (Extend.Aentry record_declaration)),
                                                         (Extend.Atoken (Tok.PKEYWORD ("}")))),
                                                         (fun _ r _ _ loc ->
                                                         
# 726 "g_vernac.mlg"
                                                               Some (true,r) 
                                                         ))])),
                (fun props info t expl _ namesup _ loc -> 
# 728 "g_vernac.mlg"
             VernacInstance (snd namesup,(fst namesup,expl,t),props,info) 
                                                          ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Context"))))),
                             (Extend.Alist1 (Extend.Aentry binder))),
                (fun c _ loc -> 
# 721 "g_vernac.mlg"
            VernacContext (List.flatten c) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Coercion"))))),
                                                                    (Extend.Aentry by_notation)),
                                                                    (Extend.Atoken (Tok.PKEYWORD (":")))),
                                                       (Extend.Aentry class_rawexpr)),
                                          (Extend.Atoken (Tok.PKEYWORD (">->")))),
                             (Extend.Aentry class_rawexpr)),
                (fun t _ s _ ntn _ loc -> 
# 718 "g_vernac.mlg"
            VernacCoercion (CAst.make ~loc @@ ByNotation ntn, s, t) 
                                          ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Coercion"))))),
                                                                    (Extend.Aentry global)),
                                                                    (Extend.Atoken (Tok.PKEYWORD (":")))),
                                                       (Extend.Aentry class_rawexpr)),
                                          (Extend.Atoken (Tok.PKEYWORD (">->")))),
                             (Extend.Aentry class_rawexpr)),
                (fun t _ s _ qid _ loc -> 
# 715 "g_vernac.mlg"
            VernacCoercion (CAst.make ~loc @@ AN qid, s, t) 
                                          ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Identity"))))),
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Coercion"))))),
                                                                    (Extend.Aentry identref)),
                                                                    (Extend.Atoken (Tok.PKEYWORD (":")))),
                                                       (Extend.Aentry class_rawexpr)),
                                          (Extend.Atoken (Tok.PKEYWORD (">->")))),
                             (Extend.Aentry class_rawexpr)),
                (fun t _ s _ f _ _ loc -> 
# 712 "g_vernac.mlg"
             VernacIdentityCoercion (f, s, t) 
                                          ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Coercion"))))),
                                                       (Extend.Aentry global)),
                                          (Extend.Aopt (Extend.Aentry univ_decl))),
                             (Extend.Aentry def_body)),
                (fun d u qid _ loc -> 
# 708 "g_vernac.mlg"
            let s = coerce_reference_to_id qid in
          VernacDefinition ((NoDischarge,Coercion),((CAst.make (Name s)),u),d) 
                                      ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Canonical"))))),
                                          (Extend.Aopt (Extend.Arules 
                                          [Extend.Rules (Extend.NextNoRec 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PIDENT (Some
                                                        ("Structure"))))),
                                                        (fun _ loc -> 
                                                        
# 703 "g_vernac.mlg"
                                                       ()
                                                        ))]))),
                             (Extend.Aentry by_notation)),
                (fun ntn _ _ loc -> 
# 704 "g_vernac.mlg"
            VernacCanonical CAst.(make ~loc @@ ByNotation ntn) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Canonical"))))),
                                                       (Extend.Aopt (Extend.Arules 
                                                       [Extend.Rules 
                                                       (Extend.NextNoRec 
                                                       (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Structure"))))),
                                                       (fun _ loc -> 
                                                       
# 696 "g_vernac.mlg"
                                                       ()
                                                       ))]))),
                                          (Extend.Aentry global)),
                             (Extend.Aopt (Extend.Arules [Extend.Rules 
                                                         (Extend.NextNoRec 
                                                         (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Aopt (Extend.Aentry univ_decl))),
                                                         (Extend.Aentry def_body)),
                                                         (fun d u loc -> 
                                                         
# 696 "g_vernac.mlg"
                                                                                                                            (u,d) 
                                                         ))]))),
                (fun ud qid _ _ loc -> 
# 697 "g_vernac.mlg"
            match ud with
           | None ->
             VernacCanonical CAst.(make ~loc @@ AN qid)
           | Some (u,d) ->
             let s = coerce_reference_to_id qid in
             VernacDefinition ((NoDischarge,CanonicalStructure),((CAst.make (Name s)),u),d) 
                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Strategy"))))),
                             (Extend.Alist1 (Extend.Arules [Extend.Rules 
                                                           (Extend.NextNoRec 
                                                           (Extend.NextNoRec 
                                                           (Extend.NextNoRec 
                                                           (Extend.NextNoRec 
                                                           (Extend.Stop,
                                                           (Extend.Aentry strategy_level)),
                                                           (Extend.Atoken (Tok.PKEYWORD ("[")))),
                                                           (Extend.Alist1 (Extend.Aentry smart_global))),
                                                           (Extend.Atoken (Tok.PKEYWORD ("]")))),
                                                           (fun _ q _ v
                                                           loc -> 
# 693 "g_vernac.mlg"
                                                                        (v,q) 
                                                                  ))]))),
                (fun l _ loc -> 
# 694 "g_vernac.mlg"
              VernacSetStrategy l 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Opaque"))))),
                             (Extend.Alist1 (Extend.Aentry smart_global))),
                (fun l _ loc -> 
# 691 "g_vernac.mlg"
            VernacSetOpacity (Conv_oracle.Opaque, l) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Transparent"))))),
                             (Extend.Alist1 (Extend.Aentry smart_global))),
                (fun l _ loc -> 
# 689 "g_vernac.mlg"
            VernacSetOpacity (Conv_oracle.transparent, l) 
                                ))])])
        in let () =
        Pcoq.grammar_extend arguments_modifier None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PIDENT (Some
                                                        ("clear"))))),
                                                        (Extend.Atoken (Tok.PIDENT (Some
                                                        ("implicits"))))),
                                           (Extend.Atoken (Tok.PIDENT (Some
                                           ("and"))))),
                              (Extend.Atoken (Tok.PIDENT (Some ("scopes"))))),
                 (fun _ _ _ _ loc -> 
# 792 "g_vernac.mlg"
            [`ClearImplicits; `ClearScopes] 
                                     ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("clear"))))),
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("scopes"))))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("and"))))),
                             (Extend.Atoken (Tok.PIDENT (Some
                             ("implicits"))))),
                (fun _ _ _ _ loc -> 
# 790 "g_vernac.mlg"
            [`ClearImplicits; `ClearScopes] 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("extra"))))),
                             (Extend.Atoken (Tok.PIDENT (Some ("scopes"))))),
                (fun _ _ loc -> 
# 788 "g_vernac.mlg"
                                           [`ExtraScopes] 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("assert"))))),
                (fun _ loc -> 
# 787 "g_vernac.mlg"
                            [`Assert] 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("rename"))))),
                (fun _ loc -> 
# 786 "g_vernac.mlg"
                            [`Rename] 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("clear"))))),
                             (Extend.Atoken (Tok.PIDENT (Some ("scopes"))))),
                (fun _ _ loc -> 
# 785 "g_vernac.mlg"
                                           [`ClearScopes] 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("clear"))))),
                             (Extend.Atoken (Tok.PIDENT (Some
                             ("implicits"))))),
                (fun _ _ loc -> 
# 784 "g_vernac.mlg"
                                              [`ClearImplicits] 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("default"))))),
                             (Extend.Atoken (Tok.PIDENT (Some
                             ("implicits"))))),
                (fun _ _ loc -> 
# 783 "g_vernac.mlg"
                                                [`DefaultImplicits] 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("simpl"))))),
                             (Extend.Atoken (Tok.PIDENT (Some ("never"))))),
                (fun _ _ loc -> 
# 782 "g_vernac.mlg"
                                          [`ReductionNeverUnfold] 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("simpl"))))),
                             (Extend.Atoken (Tok.PIDENT (Some ("nomatch"))))),
                (fun _ _ loc -> 
# 781 "g_vernac.mlg"
                                            [`ReductionDontExposeCase] 
                                ))])])
        in let () =
        Pcoq.grammar_extend scope None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PKEYWORD ("%")))),
                              (Extend.Atoken (Tok.PIDENT (None)))),
                 (fun key _ loc -> 
# 796 "g_vernac.mlg"
                              key 
                                   ))])])
        in let () =
        Pcoq.grammar_extend argument_spec None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Aopt (Extend.Atoken (Tok.PKEYWORD ("!"))))),
                                           (Extend.Aentry name)),
                              (Extend.Aopt (Extend.Aentry scope))),
                 (fun s id b loc -> 
# 800 "g_vernac.mlg"
         id.CAst.v, not (Option.is_empty b), Option.map (fun x -> CAst.make ~loc x) s 
                                    ))])])
        in let () =
        Pcoq.grammar_extend argument_spec_block None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("{")))),
                                                        (Extend.Alist1 (Extend.Aentry argument_spec))),
                                           (Extend.Atoken (Tok.PKEYWORD ("}")))),
                              (Extend.Aopt (Extend.Aentry scope))),
                 (fun sc _ items _ loc -> 
# 828 "g_vernac.mlg"
         let f x = match sc, x with
         | None, x -> x | x, None -> Option.map (fun y -> CAst.make ~loc y) x
         | Some _, Some _ -> user_err Pp.(str "scope declared twice") in
       List.map (fun (name,recarg_like,notation_scope) ->
           `Id { name=name; recarg_like=recarg_like;
                 notation_scope=f notation_scope;
                 implicit_status = MaximallyImplicit}) items 
                                          ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("[")))),
                                                       (Extend.Alist1 (Extend.Aentry argument_spec))),
                                          (Extend.Atoken (Tok.PKEYWORD ("]")))),
                             (Extend.Aopt (Extend.Aentry scope))),
                (fun sc _ items _ loc -> 
# 820 "g_vernac.mlg"
         let f x = match sc, x with
         | None, x -> x | x, None -> Option.map (fun y -> CAst.make ~loc y) x
         | Some _, Some _ -> user_err Pp.(str "scope declared twice") in
       List.map (fun (name,recarg_like,notation_scope) ->
           `Id { name=name; recarg_like=recarg_like;
                 notation_scope=f notation_scope;
                 implicit_status = Implicit}) items 
                                         ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                       (Extend.Alist1 (Extend.Aentry argument_spec))),
                                          (Extend.Atoken (Tok.PKEYWORD (")")))),
                             (Extend.Aopt (Extend.Aentry scope))),
                (fun sc _ items _ loc -> 
# 812 "g_vernac.mlg"
         let f x = match sc, x with
         | None, x -> x | x, None -> Option.map (fun y -> CAst.make ~loc y) x
         | Some _, Some _ -> user_err Pp.(str "scope declared twice") in
       List.map (fun (name,recarg_like,notation_scope) ->
           `Id { name=name; recarg_like=recarg_like;
                 notation_scope=f notation_scope;
                 implicit_status = NotImplicit}) items 
                                         ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("/")))),
                (fun _ loc -> 
# 810 "g_vernac.mlg"
               [`Slash] 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry argument_spec)),
                (fun item loc -> 
# 806 "g_vernac.mlg"
        let name, recarg_like, notation_scope = item in
      [`Id { name=name; recarg_like=recarg_like;
             notation_scope=notation_scope;
             implicit_status = NotImplicit}] 
                                 ))])])
        in let () =
        Pcoq.grammar_extend more_implicits_block None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("{")))),
                                           (Extend.Alist1 (Extend.Aentry name))),
                              (Extend.Atoken (Tok.PKEYWORD ("}")))),
                 (fun _ items _ loc -> 
# 843 "g_vernac.mlg"
         List.map (fun name -> (name.CAst.v, MaximallyImplicit)) items 
                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD ("[")))),
                                          (Extend.Alist1 (Extend.Aentry name))),
                             (Extend.Atoken (Tok.PKEYWORD ("]")))),
                (fun _ items _ loc -> 
# 841 "g_vernac.mlg"
         List.map (fun name -> (name.CAst.v, Impargs.Implicit)) items 
                                      ));
                Extend.Rule (Extend.Next (Extend.Stop, (Extend.Aentry name)),
                (fun name loc -> 
# 839 "g_vernac.mlg"
                       [(name.CAst.v, NotImplicit)] 
                                 ))])])
        in let () =
        Pcoq.grammar_extend strategy_level None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                              ("transparent"))))),
                 (fun _ loc -> 
# 850 "g_vernac.mlg"
                                 Conv_oracle.transparent 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry integer)),
                (fun n loc -> 
# 849 "g_vernac.mlg"
                       Conv_oracle.Level n 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("opaque"))))),
                (fun _ loc -> 
# 848 "g_vernac.mlg"
                            Conv_oracle.Opaque 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("expand"))))),
                (fun _ loc -> 
# 847 "g_vernac.mlg"
                            Conv_oracle.Expand 
                              ))])])
        in let () =
        Pcoq.grammar_extend instance_name None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 855 "g_vernac.mlg"
             ((CAst.make ~loc Anonymous), None), []  
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry ident_decl)),
                             (Extend.Aentry binders)),
                (fun bl name loc -> 
# 854 "g_vernac.mlg"
            (CAst.map (fun id -> Name id) (fst name), snd name), bl 
                                    ))])])
        in let () =
        Pcoq.grammar_extend hint_info None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 860 "g_vernac.mlg"
             { Typeclasses.hint_priority = None; hint_pattern = None } 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD ("|")))),
                                          (Extend.Aopt (Extend.Aentry natural))),
                             (Extend.Aopt (Extend.Aentry constr_pattern))),
                (fun pat i _ loc -> 
# 859 "g_vernac.mlg"
           { Typeclasses.hint_priority = i; hint_pattern = pat } 
                                    ))])])
        in let () =
        Pcoq.grammar_extend reserv_list None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry simple_reserv)),
                 (fun b loc -> 
# 863 "g_vernac.mlg"
                                                                   [b] 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Alist1 (Extend.Aentry reserv_tuple))),
                (fun bl loc -> 
# 863 "g_vernac.mlg"
                                     bl 
                               ))])])
        in let () =
        Pcoq.grammar_extend reserv_tuple None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                           (Extend.Aentry simple_reserv)),
                              (Extend.Atoken (Tok.PKEYWORD (")")))),
                 (fun _ a _ loc -> 
# 866 "g_vernac.mlg"
                                         a 
                                   ))])])
        in let () =
        Pcoq.grammar_extend simple_reserv None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Alist1 (Extend.Aentry identref))),
                                           (Extend.Atoken (Tok.PKEYWORD (":")))),
                              (Extend.Aentry lconstr)),
                 (fun c _ idl loc -> 
# 869 "g_vernac.mlg"
                                                    (idl,c) 
                                     ))])])
        in ()

let _ = let printable = Pcoq.Entry.create "printable"
        and printunivs_subgraph = Pcoq.Entry.create "printunivs_subgraph"
        and locatable = Pcoq.Entry.create "locatable"
        and option_setting = Pcoq.Entry.create "option_setting"
        and option_ref_value = Pcoq.Entry.create "option_ref_value"
        and option_table = Pcoq.Entry.create "option_table"
        and as_dirpath = Pcoq.Entry.create "as_dirpath"
        and ne_in_or_out_modules = Pcoq.Entry.create "ne_in_or_out_modules"
        and in_or_out_modules = Pcoq.Entry.create "in_or_out_modules"
        and comment = Pcoq.Entry.create "comment"
        and positive_search_mark = Pcoq.Entry.create "positive_search_mark"
        and scope = Pcoq.Entry.create "scope"
        and searchabout_query = Pcoq.Entry.create "searchabout_query"
        and searchabout_queries = Pcoq.Entry.create "searchabout_queries"
        and univ_name_list = Pcoq.Entry.create "univ_name_list"
        in
        let () =
        Pcoq.grammar_extend gallina_ext None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Atoken (Tok.PIDENT (Some
                                                        ("Export"))))),
                                           (Extend.Atoken (Tok.PIDENT (Some
                                           ("Unset"))))),
                              (Extend.Aentry option_table)),
                 (fun table _ _ loc -> 
# 881 "g_vernac.mlg"
            VernacSetOption (true, table, OptionUnset) 
                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Export"))))),
                                                       (Extend.Atoken (Tok.PKEYWORD ("Set")))),
                                          (Extend.Aentry option_table)),
                             (Extend.Aentry option_setting)),
                (fun v table _ _ loc -> 
# 879 "g_vernac.mlg"
          VernacSetOption (true, table, v) 
                                        ))])])
        in let () =
        Pcoq.grammar_extend command None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Atoken (Tok.PIDENT (Some
                                                        ("Remove"))))),
                                           (Extend.Atoken (Tok.PIDENT (None)))),
                              (Extend.Alist1 (Extend.Aentry option_ref_value))),
                 (fun v table _ loc -> 
# 971 "g_vernac.mlg"
            VernacRemoveOption ([table], v) 
                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Remove"))))),
                                                       (Extend.Atoken (Tok.PIDENT (None)))),
                                          (Extend.Atoken (Tok.PIDENT (None)))),
                             (Extend.Alist1 (Extend.Aentry option_ref_value))),
                (fun v field table _ loc -> 
# 969 "g_vernac.mlg"
             VernacRemoveOption ([table;field], v) 
                                            ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Test"))))),
                             (Extend.Aentry option_table)),
                (fun table _ loc -> 
# 966 "g_vernac.mlg"
            VernacPrintOption table 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Test"))))),
                                                       (Extend.Aentry option_table)),
                                          (Extend.Atoken (Tok.PKEYWORD ("for")))),
                             (Extend.Alist1 (Extend.Aentry option_ref_value))),
                (fun v _ table _ loc -> 
# 964 "g_vernac.mlg"
             VernacMemOption (table, v) 
                                        ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Add"))))),
                                          (Extend.Atoken (Tok.PIDENT (None)))),
                             (Extend.Alist1 (Extend.Aentry option_ref_value))),
                (fun v table _ loc -> 
# 961 "g_vernac.mlg"
            VernacAddOption ([table], v) 
                                      ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Add"))))),
                                                       (Extend.Atoken (Tok.PIDENT (None)))),
                                          (Extend.Atoken (Tok.PIDENT (None)))),
                             (Extend.Alist1 (Extend.Aentry option_ref_value))),
                (fun v field table _ loc -> 
# 955 "g_vernac.mlg"
             VernacAddOption ([table;field], v) 
                                            ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Print"))))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Table"))))),
                             (Extend.Aentry option_table)),
                (fun table _ _ loc -> 
# 952 "g_vernac.mlg"
            VernacPrintOption table 
                                      ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Unset"))))),
                             (Extend.Aentry option_table)),
                (fun table _ loc -> 
# 949 "g_vernac.mlg"
            VernacSetOption (false, table, OptionUnset) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD ("Set")))),
                                          (Extend.Aentry option_table)),
                             (Extend.Aentry option_setting)),
                (fun v table _ loc -> 
# 947 "g_vernac.mlg"
            VernacSetOption (false, table, v) 
                                      ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Add"))))),
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Rec"))))),
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("ML"))))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Path"))))),
                             (Extend.Aentry ne_string)),
                (fun dir _ _ _ _ loc -> 
# 943 "g_vernac.mlg"
            VernacAddMLPath (true, dir) 
                                        ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Add"))))),
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("ML"))))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Path"))))),
                             (Extend.Aentry ne_string)),
                (fun dir _ _ _ loc -> 
# 941 "g_vernac.mlg"
            VernacAddMLPath (false, dir) 
                                      ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Inspect"))))),
                             (Extend.Aentry natural)),
                (fun n _ loc -> 
# 938 "g_vernac.mlg"
                                          VernacPrint (PrintInspect n) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Print"))))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Namespace"))))),
                             (Extend.Aentry dirpath)),
                (fun ns _ _ loc -> 
# 937 "g_vernac.mlg"
            VernacPrint (PrintNamespace ns) 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Print"))))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Module"))))),
                             (Extend.Aentry global)),
                (fun qid _ _ loc -> 
# 935 "g_vernac.mlg"
            VernacPrint (PrintModule qid) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Print"))))),
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Module"))))),
                                          (Extend.Atoken (Tok.PKEYWORD ("Type")))),
                             (Extend.Aentry global)),
                (fun qid _ _ _ loc -> 
# 933 "g_vernac.mlg"
            VernacPrint (PrintModuleType qid) 
                                      ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Print"))))),
                                          (Extend.Aentry smart_global)),
                             (Extend.Aopt (Extend.Aentry univ_name_list))),
                (fun l qid _ loc -> 
# 931 "g_vernac.mlg"
                                                                       VernacPrint (PrintName (qid,l)) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Print"))))),
                             (Extend.Aentry printable)),
                (fun p _ loc -> 
# 930 "g_vernac.mlg"
                                          VernacPrint p 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("Type")))),
                             (Extend.Aentry lconstr)),
                (fun c _ loc -> 
# 927 "g_vernac.mlg"
                                 VernacGlobalCheck c 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("DelPath"))))),
                             (Extend.Aentry ne_string)),
                (fun dir _ loc -> 
# 924 "g_vernac.mlg"
            VernacRemoveLoadPath dir 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("AddRecPath"))))),
                                                       (Extend.Aentry ne_string)),
                                          (Extend.Atoken (Tok.PKEYWORD ("as")))),
                             (Extend.Aentry as_dirpath)),
                (fun alias _ dir _ loc -> 
# 922 "g_vernac.mlg"
            VernacAddLoadPath (true, dir, alias) 
                                          ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("AddPath"))))),
                                                       (Extend.Aentry ne_string)),
                                          (Extend.Atoken (Tok.PKEYWORD ("as")))),
                             (Extend.Aentry as_dirpath)),
                (fun alias _ dir _ loc -> 
# 920 "g_vernac.mlg"
            VernacAddLoadPath (false, dir, alias) 
                                          ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Remove"))))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("LoadPath"))))),
                             (Extend.Aentry ne_string)),
                (fun dir _ _ loc -> 
# 916 "g_vernac.mlg"
            VernacRemoveLoadPath dir 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Add"))))),
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Rec"))))),
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("LoadPath"))))),
                                          (Extend.Aentry ne_string)),
                             (Extend.Aentry as_dirpath)),
                (fun alias dir _ _ _ loc -> 
# 914 "g_vernac.mlg"
                                  VernacAddLoadPath (true, dir, alias) 
                                            ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Add"))))),
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("LoadPath"))))),
                                          (Extend.Aentry ne_string)),
                             (Extend.Aentry as_dirpath)),
                (fun alias dir _ _ loc -> 
# 912 "g_vernac.mlg"
            VernacAddLoadPath (false, dir, alias) 
                                          ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Locate"))))),
                             (Extend.Aentry locatable)),
                (fun l _ loc -> 
# 908 "g_vernac.mlg"
                                           VernacLocate l 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Declare"))))),
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("ML"))))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Module"))))),
                             (Extend.Alist1 (Extend.Aentry ne_string))),
                (fun l _ _ _ loc -> 
# 906 "g_vernac.mlg"
            VernacDeclareMLModule l 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Load"))))),
                                          (Extend.Arules [Extend.Rules 
                                                         (Extend.Stop, (fun
                                                         loc -> 
# 902 "g_vernac.mlg"
                                                                       false 
                                                                ));
                                                         Extend.Rules 
                                                         (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (Tok.PIDENT (Some
                                                         ("Verbose"))))),
                                                         (fun _ loc -> 
                                                         
# 902 "g_vernac.mlg"
                                                         true 
                                                         ))])),
                             (Extend.Arules [Extend.Rules (Extend.NextNoRec 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (Tok.PIDENT (None)))),
                                                          (fun s loc -> 
                                                          
# 903 "g_vernac.mlg"
                                                      s 
                                                          ));
                                            Extend.Rules (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Aentry ne_string)),
                                                         (fun s loc -> 
                                                         
# 903 "g_vernac.mlg"
                                 s 
                                                         ))])),
                (fun s verbosely _ loc -> 
# 904 "g_vernac.mlg"
            VernacLoad (verbosely, s) 
                                          ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Cd"))))),
                             (Extend.Aentry ne_string)),
                (fun dir _ loc -> 
# 900 "g_vernac.mlg"
                                         VernacChdir (Some dir) 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Cd"))))),
                (fun _ loc -> 
# 899 "g_vernac.mlg"
                        VernacChdir None 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Pwd"))))),
                (fun _ loc -> 
# 898 "g_vernac.mlg"
                         VernacChdir None 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Declare"))))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Scope"))))),
                             (Extend.Atoken (Tok.PIDENT (None)))),
                (fun sc _ _ loc -> 
# 895 "g_vernac.mlg"
            VernacDeclareScope sc 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Declare"))))),
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Instance"))))),
                                                                    (Extend.Aentry ident_decl)),
                                                                    (Extend.Aentry binders)),
                                                                    (Extend.Atoken (Tok.PKEYWORD (":")))),
                                                       (Extend.Arules 
                                                       [Extend.Rules 
                                                       (Extend.Stop, (fun
                                                       loc -> 
# 889 "g_vernac.mlg"
                                                        Decl_kinds.Explicit 
                                                              ));
                                                       Extend.Rules (Extend.NextNoRec 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("!")))),
                                                                    (fun _
                                                                    loc -> 
                                                                    
# 889 "g_vernac.mlg"
                           Decl_kinds.Implicit 
                                                                    ))])),
                                          (Extend.Aentryl (operconstr, "200"))),
                             (Extend.Aentry hint_info)),
                (fun info t expl _ bl id _ _ loc -> 
# 891 "g_vernac.mlg"
             VernacDeclareInstance (bl, (id, expl, t), info) 
                                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Comments"))))),
                             (Extend.Alist0 (Extend.Aentry comment))),
                (fun l _ loc -> 
# 885 "g_vernac.mlg"
                                                 VernacComments l 
                                ))])])
        in let () =
        Pcoq.grammar_extend query_command None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PIDENT (Some
                                                        ("SearchAbout"))))),
                                                        (Extend.Atoken (Tok.PKEYWORD ("[")))),
                                                        (Extend.Alist1 (Extend.Aentry searchabout_query))),
                                                        (Extend.Atoken (Tok.PKEYWORD ("]")))),
                                           (Extend.Aentry in_or_out_modules)),
                              (Extend.Atoken (Tok.PKEYWORD (".")))),
                 (fun _ l _ sl _ _ loc -> 
# 997 "g_vernac.mlg"
           fun g -> VernacSearch (SearchAbout sl,g, l) 
                                          ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("SearchAbout"))))),
                                                       (Extend.Aentry searchabout_query)),
                                          (Extend.Aentry searchabout_queries)),
                             (Extend.Atoken (Tok.PKEYWORD (".")))),
                (fun _ l s _ loc -> 
# 993 "g_vernac.mlg"
            fun g -> let (sl,m) = l in VernacSearch (SearchAbout (s::sl),g, m) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Search"))))),
                                                       (Extend.Aentry searchabout_query)),
                                          (Extend.Aentry searchabout_queries)),
                             (Extend.Atoken (Tok.PKEYWORD (".")))),
                (fun _ l s _ loc -> 
# 990 "g_vernac.mlg"
            let (sl,m) = l in fun g -> VernacSearch (SearchAbout (s::sl),g, m) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("SearchRewrite"))))),
                                                       (Extend.Aentry constr_pattern)),
                                          (Extend.Aentry in_or_out_modules)),
                             (Extend.Atoken (Tok.PKEYWORD (".")))),
                (fun _ l c _ loc -> 
# 988 "g_vernac.mlg"
            fun g -> VernacSearch (SearchRewrite c,g, l) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("SearchPattern"))))),
                                                       (Extend.Aentry constr_pattern)),
                                          (Extend.Aentry in_or_out_modules)),
                             (Extend.Atoken (Tok.PKEYWORD (".")))),
                (fun _ l c _ loc -> 
# 986 "g_vernac.mlg"
            fun g -> VernacSearch (SearchPattern c,g, l) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("SearchHead"))))),
                                                       (Extend.Aentry constr_pattern)),
                                          (Extend.Aentry in_or_out_modules)),
                             (Extend.Atoken (Tok.PKEYWORD (".")))),
                (fun _ l c _ loc -> 
# 984 "g_vernac.mlg"
            fun g -> VernacSearch (SearchHead c,g, l) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("About"))))),
                                                       (Extend.Aentry smart_global)),
                                          (Extend.Aopt (Extend.Aentry univ_name_list))),
                             (Extend.Atoken (Tok.PKEYWORD (".")))),
                (fun _ l qid _ loc -> 
# 982 "g_vernac.mlg"
           fun g -> VernacPrint (PrintAbout (qid,l,g)) 
                                      ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Check"))))),
                                          (Extend.Aentry lconstr)),
                             (Extend.Atoken (Tok.PKEYWORD (".")))),
                (fun _ c _ loc -> 
# 979 "g_vernac.mlg"
           fun g -> VernacCheckMayEval (None, g, c) 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Compute"))))),
                                          (Extend.Aentry lconstr)),
                             (Extend.Atoken (Tok.PKEYWORD (".")))),
                (fun _ c _ loc -> 
# 977 "g_vernac.mlg"
            fun g -> VernacCheckMayEval (Some (Genredexpr.CbvVm None), g, c) 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Eval"))))),
                                                                    (Extend.Aentry red_expr)),
                                                       (Extend.Atoken (Tok.PKEYWORD ("in")))),
                                          (Extend.Aentry lconstr)),
                             (Extend.Atoken (Tok.PKEYWORD (".")))),
                (fun _ c _ r _ loc -> 
# 975 "g_vernac.mlg"
            fun g -> VernacCheckMayEval (Some r, g, c) 
                                      ))])])
        in let () =
        Pcoq.grammar_extend printable None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                              ("Registered"))))),
                 (fun _ loc -> 
# 1045 "g_vernac.mlg"
                                PrintRegistered 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Strategies"))))),
                (fun _ loc -> 
# 1044 "g_vernac.mlg"
                                PrintStrategy None 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Strategy"))))),
                             (Extend.Aentry smart_global)),
                (fun qid _ loc -> 
# 1043 "g_vernac.mlg"
                                                  PrintStrategy (Some qid) 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("All"))))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Dependencies"))))),
                             (Extend.Aentry smart_global)),
                (fun qid _ _ loc -> 
# 1042 "g_vernac.mlg"
                                                                   PrintAssumptions (true, true, qid) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Transparent"))))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Dependencies"))))),
                             (Extend.Aentry smart_global)),
                (fun qid _ _ loc -> 
# 1041 "g_vernac.mlg"
                                                                           PrintAssumptions (false, true, qid) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Opaque"))))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Dependencies"))))),
                             (Extend.Aentry smart_global)),
                (fun qid _ _ loc -> 
# 1040 "g_vernac.mlg"
                                                                      PrintAssumptions (true, false, qid) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Assumptions"))))),
                             (Extend.Aentry smart_global)),
                (fun qid _ loc -> 
# 1039 "g_vernac.mlg"
                                                     PrintAssumptions (false, false, qid) 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Arules 
                                                                    [Extend.Rules 
                                                                    (Extend.Stop,
                                                                    (fun
                                                                    loc -> 
                                                                    
# 1036 "g_vernac.mlg"
                                                false 
                                                                    ));
                                                                    Extend.Rules 
                                                                    (Extend.NextNoRec 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Sorted"))))),
                                                                    (fun _
                                                                    loc -> 
                                                                    
# 1036 "g_vernac.mlg"
                                  true 
                                                                    ))])),
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Universes"))))),
                                          (Extend.Aopt (Extend.Aentry printunivs_subgraph))),
                             (Extend.Aopt (Extend.Aentry ne_string))),
                (fun fopt g _ b loc -> 
# 1038 "g_vernac.mlg"
          PrintUniverses (b, g, fopt) 
                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Implicit"))))),
                             (Extend.Aentry smart_global)),
                (fun qid _ loc -> 
# 1035 "g_vernac.mlg"
                                                  PrintImplicit qid 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Visibility"))))),
                             (Extend.Aopt (Extend.Atoken (Tok.PIDENT (None))))),
                (fun s _ loc -> 
# 1034 "g_vernac.mlg"
                                               PrintVisibility s 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Scope"))))),
                             (Extend.Atoken (Tok.PIDENT (None)))),
                (fun s _ loc -> 
# 1033 "g_vernac.mlg"
                                      PrintScope s 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Scopes"))))),
                (fun _ loc -> 
# 1032 "g_vernac.mlg"
                            PrintScopes 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("HintDb"))))),
                             (Extend.Atoken (Tok.PIDENT (None)))),
                (fun s _ loc -> 
# 1031 "g_vernac.mlg"
                                       PrintHintDbName s 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Hint"))))),
                             (Extend.Atoken (Tok.PKEYWORD ("*")))),
                (fun _ _ loc -> 
# 1030 "g_vernac.mlg"
                               PrintHintDb 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Hint"))))),
                             (Extend.Aentry smart_global)),
                (fun qid _ loc -> 
# 1029 "g_vernac.mlg"
                                              PrintHint qid 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Hint"))))),
                (fun _ loc -> 
# 1028 "g_vernac.mlg"
                          PrintHintGoal 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Options"))))),
                (fun _ loc -> 
# 1027 "g_vernac.mlg"
                             PrintTables (* A Synonymous to Tables *) 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Tables"))))),
                (fun _ loc -> 
# 1026 "g_vernac.mlg"
                            PrintTables 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Canonical"))))),
                             (Extend.Atoken (Tok.PIDENT (Some
                             ("Projections"))))),
                (fun _ _ loc -> 
# 1025 "g_vernac.mlg"
                                                    PrintCanonicalConversions 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Coercion"))))),
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Paths"))))),
                                          (Extend.Aentry class_rawexpr)),
                             (Extend.Aentry class_rawexpr)),
                (fun t s _ _ loc -> 
# 1024 "g_vernac.mlg"
              PrintCoercionPaths (s,t) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Coercions"))))),
                (fun _ loc -> 
# 1022 "g_vernac.mlg"
                               PrintCoercions 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Instances"))))),
                             (Extend.Aentry smart_global)),
                (fun qid _ loc -> 
# 1021 "g_vernac.mlg"
                                                   PrintInstances qid 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("TypeClasses"))))),
                (fun _ loc -> 
# 1020 "g_vernac.mlg"
                                 PrintTypeClasses 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Classes"))))),
                (fun _ loc -> 
# 1019 "g_vernac.mlg"
                              PrintClasses 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Graph"))))),
                (fun _ loc -> 
# 1018 "g_vernac.mlg"
                           PrintGraph 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Debug"))))),
                             (Extend.Atoken (Tok.PIDENT (Some ("GC"))))),
                (fun _ _ loc -> 
# 1017 "g_vernac.mlg"
                                       PrintDebugGC 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("ML"))))),
                             (Extend.Atoken (Tok.PIDENT (Some ("Modules"))))),
                (fun _ _ loc -> 
# 1016 "g_vernac.mlg"
                                         PrintMLModules 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("ML"))))),
                             (Extend.Atoken (Tok.PIDENT (Some ("Path"))))),
                (fun _ _ loc -> 
# 1015 "g_vernac.mlg"
                                      PrintMLLoadPath 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Libraries"))))),
                (fun _ loc -> 
# 1013 "g_vernac.mlg"
                               PrintModules 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Modules"))))),
                (fun _ loc -> 
# 1012 "g_vernac.mlg"
            user_err Pp.(str "Print Modules is obsolete; use Print Libraries instead") 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("LoadPath"))))),
                             (Extend.Aopt (Extend.Aentry dirpath))),
                (fun dir _ loc -> 
# 1010 "g_vernac.mlg"
                                                 PrintLoadPath dir 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Custom"))))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Grammar"))))),
                             (Extend.Atoken (Tok.PIDENT (None)))),
                (fun ent _ _ loc -> 
# 1009 "g_vernac.mlg"
            PrintCustomGrammar ent 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Grammar"))))),
                             (Extend.Atoken (Tok.PIDENT (None)))),
                (fun ent _ loc -> 
# 1006 "g_vernac.mlg"
            PrintGrammar ent 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Section"))))),
                             (Extend.Aentry global)),
                (fun s _ loc -> 
# 1003 "g_vernac.mlg"
                                         PrintSectionContext s 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("All"))))),
                (fun _ loc -> 
# 1002 "g_vernac.mlg"
                         PrintFullContext 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Term"))))),
                                          (Extend.Aentry smart_global)),
                             (Extend.Aopt (Extend.Aentry univ_name_list))),
                (fun l qid _ loc -> 
# 1001 "g_vernac.mlg"
                                                                      PrintName (qid,l) 
                                    ))])])
        in let () =
        Pcoq.grammar_extend printunivs_subgraph None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PIDENT (Some
                                                        ("Subgraph"))))),
                                                        (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                           (Extend.Alist0 (Extend.Aentry reference))),
                              (Extend.Atoken (Tok.PKEYWORD (")")))),
                 (fun _ l _ _ loc -> 
# 1049 "g_vernac.mlg"
                                                             l 
                                     ))])])
        in let () =
        Pcoq.grammar_extend class_rawexpr None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry smart_global)),
                 (fun qid loc -> 
# 1054 "g_vernac.mlg"
                                RefClass qid 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Sortclass"))))),
                (fun _ loc -> 
# 1053 "g_vernac.mlg"
                               SortClass 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Funclass"))))),
                (fun _ loc -> 
# 1052 "g_vernac.mlg"
                              FunClass 
                              ))])])
        in let () =
        Pcoq.grammar_extend locatable None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PIDENT (Some
                                           ("Module"))))),
                              (Extend.Aentry global)),
                 (fun qid _ loc -> 
# 1061 "g_vernac.mlg"
                                          LocateModule qid 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Library"))))),
                             (Extend.Aentry global)),
                (fun qid _ loc -> 
# 1060 "g_vernac.mlg"
                                           LocateLibrary qid 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("File"))))),
                             (Extend.Aentry ne_string)),
                (fun f _ loc -> 
# 1059 "g_vernac.mlg"
                                         LocateFile f 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Term"))))),
                             (Extend.Aentry smart_global)),
                (fun qid _ loc -> 
# 1058 "g_vernac.mlg"
                                              LocateTerm qid 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry smart_global)),
                (fun qid loc -> 
# 1057 "g_vernac.mlg"
                                LocateAny qid 
                                ))])])
        in let () =
        Pcoq.grammar_extend option_setting None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Atoken (Tok.PSTRING (None)))),
                 (fun s loc -> 
# 1066 "g_vernac.mlg"
                            OptionSetString s 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry integer)),
                (fun n loc -> 
# 1065 "g_vernac.mlg"
                            OptionSetInt n 
                              ));
                Extend.Rule (Extend.Stop, (fun loc -> 
# 1064 "g_vernac.mlg"
             OptionSetTrue 
                                                      ))])])
        in let () =
        Pcoq.grammar_extend option_ref_value None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Atoken (Tok.PSTRING (None)))),
                 (fun s loc -> 
# 1070 "g_vernac.mlg"
                           StringRefValue s 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry global)),
                (fun id loc -> 
# 1069 "g_vernac.mlg"
                           QualidRefValue id 
                               ))])])
        in let () =
        Pcoq.grammar_extend option_table None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Alist1 (Extend.Arules [Extend.Rules 
                                                            (Extend.NextNoRec 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (Tok.PIDENT (None)))),
                                                            (fun x loc -> 
                                                            
# 1073 "g_vernac.mlg"
                                    x 
                                                            ))]))),
                 (fun fl loc -> 
# 1073 "g_vernac.mlg"
                                               fl 
                                ))])])
        in let () =
        Pcoq.grammar_extend as_dirpath None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Aopt (Extend.Arules [Extend.Rules 
                                                          (Extend.NextNoRec 
                                                          (Extend.NextNoRec 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (Tok.PKEYWORD ("as")))),
                                                          (Extend.Aentry dirpath)),
                                                          (fun d _ loc -> 
                                                          
# 1076 "g_vernac.mlg"
                                         d 
                                                          ))]))),
                 (fun d loc -> 
# 1076 "g_vernac.mlg"
                                                    d 
                               ))])])
        in let () =
        Pcoq.grammar_extend ne_in_or_out_modules None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PIDENT (Some
                                           ("outside"))))),
                              (Extend.Alist1 (Extend.Aentry global))),
                 (fun l _ loc -> 
# 1080 "g_vernac.mlg"
                                               SearchOutside l 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("inside"))))),
                             (Extend.Alist1 (Extend.Aentry global))),
                (fun l _ loc -> 
# 1079 "g_vernac.mlg"
                                              SearchInside l 
                                ))])])
        in let () =
        Pcoq.grammar_extend in_or_out_modules None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 1084 "g_vernac.mlg"
             SearchOutside [] 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Aentry ne_in_or_out_modules)),
                (fun m loc -> 
# 1083 "g_vernac.mlg"
                                      m 
                              ))])])
        in let () =
        Pcoq.grammar_extend comment None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry natural)),
                 (fun n loc -> 
# 1089 "g_vernac.mlg"
                         CommentInt n 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PSTRING (None)))),
                (fun s loc -> 
# 1088 "g_vernac.mlg"
                        CommentString s 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry constr)),
                (fun c loc -> 
# 1087 "g_vernac.mlg"
                        CommentConstr c 
                              ))])])
        in let () =
        Pcoq.grammar_extend positive_search_mark None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 1092 "g_vernac.mlg"
                                true 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("-")))),
                (fun _ loc -> 
# 1092 "g_vernac.mlg"
                 false 
                              ))])])
        in let () =
        Pcoq.grammar_extend scope None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PKEYWORD ("%")))),
                              (Extend.Atoken (Tok.PIDENT (None)))),
                 (fun key _ loc -> 
# 1095 "g_vernac.mlg"
                              key 
                                   ))])])
        in let () =
        Pcoq.grammar_extend searchabout_query None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry positive_search_mark)),
                              (Extend.Aentry constr_pattern)),
                 (fun p b loc -> 
# 1101 "g_vernac.mlg"
              (b, SearchSubPattern p) 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry positive_search_mark)),
                                          (Extend.Aentry ne_string)),
                             (Extend.Aopt (Extend.Aentry scope))),
                (fun sc s b loc -> 
# 1099 "g_vernac.mlg"
              (b, SearchString (s,sc)) 
                                   ))])])
        in let () =
        Pcoq.grammar_extend searchabout_queries None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 1108 "g_vernac.mlg"
             ([],SearchOutside []) 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry searchabout_query)),
                             (Extend.Aentry searchabout_queries)),
                (fun l s loc -> 
# 1107 "g_vernac.mlg"
          let (sl,m) = l in (s::sl,m) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Aentry ne_in_or_out_modules)),
                (fun m loc -> 
# 1105 "g_vernac.mlg"
                                      ([],m) 
                              ))])])
        in let () =
        Pcoq.grammar_extend univ_name_list None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("@{")))),
                                           (Extend.Alist0 (Extend.Aentry name))),
                              (Extend.Atoken (Tok.PKEYWORD ("}")))),
                 (fun _ l _ loc -> 
# 1112 "g_vernac.mlg"
                                         l 
                                   ))])])
        in ()

let _ = let () =
        Pcoq.grammar_extend command None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PIDENT (Some
                                                        ("Declare"))))),
                                                        (Extend.Atoken (Tok.PIDENT (Some
                                                        ("Custom"))))),
                                           (Extend.Atoken (Tok.PIDENT (Some
                                           ("Entry"))))),
                              (Extend.Atoken (Tok.PIDENT (None)))),
                 (fun s _ _ _ loc -> 
# 1150 "g_vernac.mlg"
             VernacDeclareCustomEntry s 
                                     ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Declare"))))),
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Reduction"))))),
                                                       (Extend.Atoken (Tok.PIDENT (None)))),
                                          (Extend.Atoken (Tok.PKEYWORD (":=")))),
                             (Extend.Aentry red_expr)),
                (fun r _ s _ _ loc -> 
# 1145 "g_vernac.mlg"
             VernacDeclareReduction (s,r) 
                                      ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Debug"))))),
                             (Extend.Atoken (Tok.PIDENT (Some ("Off"))))),
                (fun _ _ loc -> 
# 1139 "g_vernac.mlg"
            VernacSetOption (false, ["Ltac";"Debug"], OptionUnset) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Debug"))))),
                             (Extend.Atoken (Tok.PIDENT (Some ("On"))))),
                (fun _ _ loc -> 
# 1136 "g_vernac.mlg"
            VernacSetOption (false, ["Ltac";"Debug"], OptionSetTrue) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("BackTo"))))),
                             (Extend.Aentry natural)),
                (fun n _ loc -> 
# 1132 "g_vernac.mlg"
                                         VernacBackTo n 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Back"))))),
                             (Extend.Aentry natural)),
                (fun n _ loc -> 
# 1131 "g_vernac.mlg"
                                       VernacBack n 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Back"))))),
                (fun _ loc -> 
# 1130 "g_vernac.mlg"
                          VernacBack 1 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Reset"))))),
                             (Extend.Aentry identref)),
                (fun id _ loc -> 
# 1129 "g_vernac.mlg"
                                          VernacResetName id 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Reset"))))),
                             (Extend.Atoken (Tok.PIDENT (Some ("Initial"))))),
                (fun _ _ loc -> 
# 1128 "g_vernac.mlg"
                                            VernacResetInitial 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Restore"))))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("State"))))),
                             (Extend.Aentry ne_string)),
                (fun s _ _ loc -> 
# 1125 "g_vernac.mlg"
                                                           VernacRestoreState s 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Restore"))))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("State"))))),
                             (Extend.Atoken (Tok.PIDENT (None)))),
                (fun s _ _ loc -> 
# 1124 "g_vernac.mlg"
                                                       VernacRestoreState s 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Write"))))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("State"))))),
                             (Extend.Aentry ne_string)),
                (fun s _ _ loc -> 
# 1123 "g_vernac.mlg"
                                                         VernacWriteState s 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Write"))))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("State"))))),
                             (Extend.Atoken (Tok.PIDENT (None)))),
                (fun s _ _ loc -> 
# 1122 "g_vernac.mlg"
                                                     VernacWriteState s 
                                  ))])])
        in ()

let _ = let only_parsing = Pcoq.Entry.create "only_parsing"
        and level = Pcoq.Entry.create "level"
        and syntax_modifier = Pcoq.Entry.create "syntax_modifier"
        and syntax_extension_type = Pcoq.Entry.create "syntax_extension_type"
        and at_level = Pcoq.Entry.create "at_level"
        and constr_as_binder_kind = Pcoq.Entry.create "constr_as_binder_kind"
        in
        let () =
        Pcoq.grammar_extend syntax None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PIDENT (Some
                                                        ("Reserved"))))),
                                                        (Extend.Atoken (Tok.PIDENT (Some
                                                        ("Notation"))))),
                                           (Extend.Aentry ne_lstring)),
                              (Extend.Arules [Extend.Rules (Extend.Stop, (fun
                                                           loc -> 
# 1198 "g_vernac.mlg"
                                                                           [] 
                                                                  ));
                                             Extend.Rules (Extend.NextNoRec 
                                                          (Extend.NextNoRec 
                                                          (Extend.NextNoRec 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                          (Extend.Alist1sep ((Extend.Aentry syntax_modifier), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                                                          (Extend.Atoken (Tok.PKEYWORD (")")))),
                                                          (fun _ l _ loc ->
                                                          
# 1198 "g_vernac.mlg"
                                                                l 
                                                          ))])),
                 (fun l s _ _ loc -> 
# 1199 "g_vernac.mlg"
              VernacSyntaxExtension (false, (s,l)) 
                                     ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Reserved"))))),
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Infix"))))),
                                          (Extend.Aentry ne_lstring)),
                             (Extend.Arules [Extend.Rules (Extend.Stop, (fun
                                                          loc -> 
# 1192 "g_vernac.mlg"
                                                                           [] 
                                                                 ));
                                            Extend.Rules (Extend.NextNoRec 
                                                         (Extend.NextNoRec 
                                                         (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                         (Extend.Alist1sep ((Extend.Aentry syntax_modifier), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                                                         (Extend.Atoken (Tok.PKEYWORD (")")))),
                                                         (fun _ l _ loc -> 
                                                         
# 1192 "g_vernac.mlg"
                                                                l 
                                                         ))])),
                (fun l s _ _ loc -> 
# 1193 "g_vernac.mlg"
             let s = CAst.map (fun s -> "x '"^s^"' y") s in
           VernacSyntaxExtension (true,(s,l)) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Format"))))),
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Notation"))))),
                                                       (Extend.Atoken (Tok.PSTRING (None)))),
                                          (Extend.Atoken (Tok.PSTRING (None)))),
                             (Extend.Atoken (Tok.PSTRING (None)))),
                (fun fmt s n _ _ loc -> 
# 1189 "g_vernac.mlg"
             VernacNotationAddFormat (n,s,fmt) 
                                        ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Notation"))))),
                                                                    (Extend.Aentry lstring)),
                                                                    (Extend.Atoken (Tok.PKEYWORD (":=")))),
                                                       (Extend.Aentry constr)),
                                          (Extend.Arules [Extend.Rules 
                                                         (Extend.Stop, (fun
                                                         loc -> 
# 1185 "g_vernac.mlg"
                                                                              [] 
                                                                ));
                                                         Extend.Rules 
                                                         (Extend.NextNoRec 
                                                         (Extend.NextNoRec 
                                                         (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                         (Extend.Alist1sep ((Extend.Aentry syntax_modifier), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                                                         (Extend.Atoken (Tok.PKEYWORD (")")))),
                                                         (fun _ l _ loc -> 
                                                         
# 1185 "g_vernac.mlg"
                                                                   l 
                                                         ))])),
                             (Extend.Aopt (Extend.Arules [Extend.Rules 
                                                         (Extend.NextNoRec 
                                                         (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (Tok.PKEYWORD (":")))),
                                                         (Extend.Atoken (Tok.PIDENT (None)))),
                                                         (fun sc _ loc -> 
                                                         
# 1186 "g_vernac.mlg"
                                         sc 
                                                         ))]))),
                (fun sc modl c _ s _ loc -> 
# 1187 "g_vernac.mlg"
             VernacNotation (c,(s,modl),sc) 
                                            ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Notation"))))),
                                                                    (Extend.Aentry identref)),
                                                                    (Extend.Alist0 (Extend.Aentry ident))),
                                                       (Extend.Atoken (Tok.PKEYWORD (":=")))),
                                          (Extend.Aentry constr)),
                             (Extend.Aentry only_parsing)),
                (fun b c _ idl id _ loc -> 
# 1181 "g_vernac.mlg"
             VernacSyntacticDefinition
             (id,(idl,c),b) 
                                           ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Infix"))))),
                                                                    (Extend.Aentry ne_lstring)),
                                                                    (Extend.Atoken (Tok.PKEYWORD (":=")))),
                                                       (Extend.Aentry constr)),
                                          (Extend.Arules [Extend.Rules 
                                                         (Extend.Stop, (fun
                                                         loc -> 
# 1176 "g_vernac.mlg"
                                                                              [] 
                                                                ));
                                                         Extend.Rules 
                                                         (Extend.NextNoRec 
                                                         (Extend.NextNoRec 
                                                         (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                         (Extend.Alist1sep ((Extend.Aentry syntax_modifier), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                                                         (Extend.Atoken (Tok.PKEYWORD (")")))),
                                                         (fun _ l _ loc -> 
                                                         
# 1176 "g_vernac.mlg"
                                                                   l 
                                                         ))])),
                             (Extend.Aopt (Extend.Arules [Extend.Rules 
                                                         (Extend.NextNoRec 
                                                         (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (Tok.PKEYWORD (":")))),
                                                         (Extend.Atoken (Tok.PIDENT (None)))),
                                                         (fun sc _ loc -> 
                                                         
# 1177 "g_vernac.mlg"
                                         sc 
                                                         ))]))),
                (fun sc modl p _ op _ loc -> 
# 1178 "g_vernac.mlg"
           VernacInfix ((op,modl),p,sc) 
                                             ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Bind"))))),
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Scope"))))),
                                                       (Extend.Atoken (Tok.PIDENT (None)))),
                                          (Extend.Atoken (Tok.PKEYWORD ("with")))),
                             (Extend.Alist1 (Extend.Aentry class_rawexpr))),
                (fun refl _ sc _ _ loc -> 
# 1173 "g_vernac.mlg"
                                       VernacBindScope (sc,refl) 
                                          ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Undelimit"))))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Scope"))))),
                             (Extend.Atoken (Tok.PIDENT (None)))),
                (fun sc _ _ loc -> 
# 1170 "g_vernac.mlg"
           VernacDelimiters (sc, None) 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Delimit"))))),
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Scope"))))),
                                                       (Extend.Atoken (Tok.PIDENT (None)))),
                                          (Extend.Atoken (Tok.PKEYWORD ("with")))),
                             (Extend.Atoken (Tok.PIDENT (None)))),
                (fun key _ sc _ _ loc -> 
# 1168 "g_vernac.mlg"
           VernacDelimiters (sc, Some key) 
                                         ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Close"))))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Scope"))))),
                             (Extend.Atoken (Tok.PIDENT (None)))),
                (fun sc _ _ loc -> 
# 1165 "g_vernac.mlg"
           VernacOpenCloseScope (false,sc) 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Open"))))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Scope"))))),
                             (Extend.Atoken (Tok.PIDENT (None)))),
                (fun sc _ _ loc -> 
# 1162 "g_vernac.mlg"
           VernacOpenCloseScope (true,sc) 
                                   ))])])
        in let () =
        Pcoq.grammar_extend only_parsing None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 1210 "g_vernac.mlg"
             None 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("compat"))))),
                                          (Extend.Atoken (Tok.PSTRING (None)))),
                             (Extend.Atoken (Tok.PKEYWORD (")")))),
                (fun _ s _ _ loc -> 
# 1209 "g_vernac.mlg"
           Some (parse_compat_version s) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("only"))))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("parsing"))))),
                             (Extend.Atoken (Tok.PKEYWORD (")")))),
                (fun _ _ _ _ loc -> 
# 1207 "g_vernac.mlg"
           Some Flags.Current 
                                    ))])])
        in let () =
        Pcoq.grammar_extend level None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PIDENT (Some
                                           ("next"))))),
                              (Extend.Atoken (Tok.PIDENT (Some ("level"))))),
                 (fun _ _ loc -> 
# 1214 "g_vernac.mlg"
                                         NextLevel 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("level"))))),
                             (Extend.Aentry natural)),
                (fun n _ loc -> 
# 1213 "g_vernac.mlg"
                                        NumLevel n 
                                ))])])
        in let () =
        Pcoq.grammar_extend syntax_modifier None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PIDENT (None)))),
                              (Extend.Aentry syntax_extension_type)),
                 (fun typ x loc -> 
# 1239 "g_vernac.mlg"
                                                    SetEntryType (x,typ) 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (None)))),
                             (Extend.Aentry constr_as_binder_kind)),
                (fun b x loc -> 
# 1238 "g_vernac.mlg"
                                                  SetItemLevel ([x],Some b,None) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (None)))),
                                                       (Extend.Atoken (Tok.PKEYWORD ("at")))),
                                          (Extend.Aentry level)),
                             (Extend.Aentry constr_as_binder_kind)),
                (fun b lev _ x loc -> 
# 1237 "g_vernac.mlg"
          SetItemLevel ([x],Some b,Some lev) 
                                      ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (None)))),
                                          (Extend.Atoken (Tok.PKEYWORD ("at")))),
                             (Extend.Aentry level)),
                (fun lev _ x loc -> 
# 1235 "g_vernac.mlg"
                                          SetItemLevel ([x],None,Some lev) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (None)))),
                                                                    (Extend.Atoken (Tok.PKEYWORD (",")))),
                                                       (Extend.Alist1sep ((Extend.Arules 
                                                       [Extend.Rules 
                                                       (Extend.NextNoRec 
                                                       (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (None)))),
                                                       (fun id loc -> 
                                                       
# 1233 "g_vernac.mlg"
                                                   id 
                                                       ))]), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                                          (Extend.Atoken (Tok.PKEYWORD ("at")))),
                             (Extend.Aentry level)),
                (fun lev _ l _ x loc -> 
# 1234 "g_vernac.mlg"
                         SetItemLevel (x::l,None,Some lev) 
                                        ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("format"))))),
                                          (Extend.Arules [Extend.Rules 
                                                         (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (Tok.PSTRING (None)))),
                                                         (fun s loc -> 
                                                         
# 1228 "g_vernac.mlg"
                                              CAst.make ~loc s 
                                                         ))])),
                             (Extend.Aopt (Extend.Arules [Extend.Rules 
                                                         (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (Tok.PSTRING (None)))),
                                                         (fun s loc -> 
                                                         
# 1229 "g_vernac.mlg"
                                                  CAst.make ~loc s 
                                                         ))]))),
                (fun s2 s1 _ loc -> 
# 1230 "g_vernac.mlg"
            begin match s1, s2 with
          | { CAst.v = k }, Some s -> SetFormat(k,s)
          | s, None -> SetFormat ("text",s) end 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("compat"))))),
                             (Extend.Atoken (Tok.PSTRING (None)))),
                (fun s _ loc -> 
# 1227 "g_vernac.mlg"
          SetCompatVersion (parse_compat_version s) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("only"))))),
                             (Extend.Atoken (Tok.PIDENT (Some ("parsing"))))),
                (fun _ _ loc -> 
# 1225 "g_vernac.mlg"
                                           SetOnlyParsing 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("only"))))),
                             (Extend.Atoken (Tok.PIDENT (Some
                             ("printing"))))),
                (fun _ _ loc -> 
# 1224 "g_vernac.mlg"
                                            SetOnlyPrinting 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("no"))))),
                             (Extend.Atoken (Tok.PIDENT (Some
                             ("associativity"))))),
                (fun _ _ loc -> 
# 1223 "g_vernac.mlg"
                                               SetAssoc Gramlib.Gramext.NonA 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("right"))))),
                             (Extend.Atoken (Tok.PIDENT (Some
                             ("associativity"))))),
                (fun _ _ loc -> 
# 1222 "g_vernac.mlg"
                                                  SetAssoc Gramlib.Gramext.RightA 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("left"))))),
                             (Extend.Atoken (Tok.PIDENT (Some
                             ("associativity"))))),
                (fun _ _ loc -> 
# 1221 "g_vernac.mlg"
                                                 SetAssoc Gramlib.Gramext.LeftA 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("in")))),
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("custom"))))),
                                                                    (Extend.Atoken (Tok.PIDENT (None)))),
                                                       (Extend.Atoken (Tok.PKEYWORD ("at")))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("level"))))),
                             (Extend.Aentry natural)),
                (fun n _ _ x _ _ loc -> 
# 1220 "g_vernac.mlg"
           SetCustomEntry (x,Some n) 
                                        ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD ("in")))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("custom"))))),
                             (Extend.Atoken (Tok.PIDENT (None)))),
                (fun x _ _ loc -> 
# 1218 "g_vernac.mlg"
                                             SetCustomEntry (x,None) 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD ("at")))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("level"))))),
                             (Extend.Aentry natural)),
                (fun n _ _ loc -> 
# 1217 "g_vernac.mlg"
                                              SetLevel n 
                                  ))])])
        in let () =
        Pcoq.grammar_extend syntax_extension_type None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PIDENT (Some
                                                        ("custom"))))),
                                                        (Extend.Atoken (Tok.PIDENT (None)))),
                                           (Extend.Aopt (Extend.Aentry at_level))),
                              (Extend.Aopt (Extend.Aentry constr_as_binder_kind))),
                 (fun b n x _ loc -> 
# 1254 "g_vernac.mlg"
             ETConstr (InCustomEntry x,b,n) 
                                     ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("closed"))))),
                             (Extend.Atoken (Tok.PIDENT (Some ("binder"))))),
                (fun _ _ loc -> 
# 1252 "g_vernac.mlg"
                                            ETBinder false 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("strict"))))),
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("pattern"))))),
                                                       (Extend.Atoken (Tok.PKEYWORD ("at")))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("level"))))),
                             (Extend.Aentry natural)),
                (fun n _ _ _ _ loc -> 
# 1251 "g_vernac.mlg"
                                                                               ETPattern (true,Some n) 
                                      ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("strict"))))),
                             (Extend.Atoken (Tok.PIDENT (Some ("pattern"))))),
                (fun _ _ loc -> 
# 1250 "g_vernac.mlg"
                                             ETPattern (true,None) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("pattern"))))),
                                                       (Extend.Atoken (Tok.PKEYWORD ("at")))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("level"))))),
                             (Extend.Aentry natural)),
                (fun n _ _ _ loc -> 
# 1249 "g_vernac.mlg"
                                                               ETPattern (false,Some n) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("pattern"))))),
                (fun _ loc -> 
# 1248 "g_vernac.mlg"
                             ETPattern (false,None) 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("constr"))))),
                                          (Extend.Aopt (Extend.Aentry at_level))),
                             (Extend.Aopt (Extend.Aentry constr_as_binder_kind))),
                (fun b n _ loc -> 
# 1247 "g_vernac.mlg"
                                                                             ETConstr (InConstrEntry,b,n) 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("constr"))))),
                (fun _ loc -> 
# 1246 "g_vernac.mlg"
                            ETConstr (InConstrEntry,None,None) 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("binder"))))),
                (fun _ loc -> 
# 1245 "g_vernac.mlg"
                            ETBinder true 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("bigint"))))),
                (fun _ loc -> 
# 1244 "g_vernac.mlg"
                            ETBigint 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("global"))))),
                (fun _ loc -> 
# 1243 "g_vernac.mlg"
                                                           ETGlobal 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("ident"))))),
                (fun _ loc -> 
# 1243 "g_vernac.mlg"
                           ETIdent 
                              ))])])
        in let () =
        Pcoq.grammar_extend at_level None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PKEYWORD ("at")))),
                              (Extend.Aentry level)),
                 (fun n _ loc -> 
# 1258 "g_vernac.mlg"
                             n 
                                 ))])])
        in let () =
        Pcoq.grammar_extend constr_as_binder_kind None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("as")))),
                                           (Extend.Atoken (Tok.PIDENT (Some
                                           ("strict"))))),
                              (Extend.Atoken (Tok.PIDENT (Some
                              ("pattern"))))),
                 (fun _ _ _ loc -> 
# 1263 "g_vernac.mlg"
                                                   Notation_term.AsStrictPattern 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("as")))),
                             (Extend.Atoken (Tok.PIDENT (Some ("pattern"))))),
                (fun _ _ loc -> 
# 1262 "g_vernac.mlg"
                                   Notation_term.AsIdentOrPattern 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("as")))),
                             (Extend.Atoken (Tok.PIDENT (Some ("ident"))))),
                (fun _ _ loc -> 
# 1261 "g_vernac.mlg"
                                 Notation_term.AsIdent 
                                ))])])
        in ()

