
# 11 "g_constr.mlg"
 

open Names
open Constr
open Libnames
open Glob_term
open Constrexpr
open Constrexpr_ops
open Util
open Tok
open Namegen
open Decl_kinds

open Pcoq
open Pcoq.Prim
open Pcoq.Constr

(* TODO: avoid this redefinition without an extra dep to Notation_ops *)
let ldots_var = Id.of_string ".."

let constr_kw =
  [ "forall"; "fun"; "match"; "fix"; "cofix"; "with"; "in"; "for";
    "end"; "as"; "let"; "if"; "then"; "else"; "return";
    "SProp"; "Prop"; "Set"; "Type"; ".("; "_"; "..";
    "`{"; "`("; "{|"; "|}" ]

let _ = List.iter CLexer.add_keyword constr_kw

let mk_cast = function
    (c,(_,None)) -> c
  | (c,(_,Some ty)) ->
    let loc = Loc.merge_opt (constr_loc c) (constr_loc ty)
    in CAst.make ?loc @@ CCast(c, CastConv ty)

let binder_of_name expl { CAst.loc = loc; v = na } =
  CLocalAssum ([CAst.make ?loc na], Default expl,
    CAst.make ?loc @@ CHole (Some (Evar_kinds.BinderType na), IntroAnonymous, None))

let binders_of_names l =
  List.map (binder_of_name Explicit) l

let mk_fixb (id,bl,ann,body,(loc,tyc)) : fix_expr =
  let ty = match tyc with
      Some ty -> ty
    | None    -> CAst.make @@ CHole (None, IntroAnonymous, None) in
  (id,ann,bl,ty,body)

let mk_cofixb (id,bl,ann,body,(loc,tyc)) : cofix_expr =
  Option.iter (fun { CAst.loc = aloc } ->
    CErrors.user_err ?loc:aloc
      ~hdr:"Constr:mk_cofixb"
      (Pp.str"Annotation forbidden in cofix expression.")) ann;
  let ty = match tyc with
      Some ty -> ty
    | None -> CAst.make @@ CHole (None, IntroAnonymous, None) in
  (id,bl,ty,body)

let mk_fix(loc,kw,id,dcls) =
  if kw then
    let fb : fix_expr list = List.map mk_fixb dcls in
    CAst.make ~loc @@ CFix(id,fb)
  else
    let fb : cofix_expr list = List.map mk_cofixb dcls in
    CAst.make ~loc @@ CCoFix(id,fb)

let mk_single_fix (loc,kw,dcl) =
  let (id,_,_,_,_) = dcl in mk_fix(loc,kw,id,[dcl])

let err () = raise Stream.Failure

(* Hack to parse "(x:=t)" as an explicit argument without conflicts with the *)
(* admissible notation "(x t)" *)
let lpar_id_coloneq =
  Pcoq.Entry.of_parser "test_lpar_id_coloneq"
    (fun strm ->
      match stream_nth 0 strm with
        | KEYWORD "(" ->
            (match stream_nth 1 strm with
              | IDENT s ->
                  (match stream_nth 2 strm with
                    | KEYWORD ":=" ->
                        stream_njunk 3 strm;
                        Names.Id.of_string s
                    | _ -> err ())
              | _ -> err ())
        | _ -> err ())

let impl_ident_head =
  Pcoq.Entry.of_parser "impl_ident_head"
    (fun strm ->
      match stream_nth 0 strm with
        | KEYWORD "{" ->
            (match stream_nth 1 strm with
              | IDENT ("wf"|"struct"|"measure") -> err ()
              | IDENT s ->
                  stream_njunk 2 strm;
                  Names.Id.of_string s
              | _ -> err ())
        | _ -> err ())

let name_colon =
  Pcoq.Entry.of_parser "name_colon"
    (fun strm ->
      match stream_nth 0 strm with
        | IDENT s ->
            (match stream_nth 1 strm with
              | KEYWORD ":" ->
                  stream_njunk 2 strm;
                  Name (Names.Id.of_string s)
              | _ -> err ())
        | KEYWORD "_" ->
          (match stream_nth 1 strm with
              | KEYWORD ":" ->
                  stream_njunk 2 strm;
                  Anonymous
              | _ -> err ())
        | _ -> err ())

let aliasvar = function { CAst.v = CPatAlias (_, na) } -> Some na | _ -> None



let _ = let universe_expr = Pcoq.Entry.create "universe_expr"
        and universe = Pcoq.Entry.create "universe"
        and record_fields = Pcoq.Entry.create "record_fields"
        and record_field_declaration =
          Pcoq.Entry.create "record_field_declaration"
        and atomic_constr = Pcoq.Entry.create "atomic_constr"
        and inst = Pcoq.Entry.create "inst"
        and evar_instance = Pcoq.Entry.create "evar_instance"
        and instance = Pcoq.Entry.create "instance"
        and fix_constr = Pcoq.Entry.create "fix_constr"
        and single_fix = Pcoq.Entry.create "single_fix"
        and fix_kw = Pcoq.Entry.create "fix_kw"
        and fix_decl = Pcoq.Entry.create "fix_decl"
        and match_constr = Pcoq.Entry.create "match_constr"
        and case_item = Pcoq.Entry.create "case_item"
        and case_type = Pcoq.Entry.create "case_type"
        and return_type = Pcoq.Entry.create "return_type"
        and branches = Pcoq.Entry.create "branches"
        and mult_pattern = Pcoq.Entry.create "mult_pattern"
        and eqn = Pcoq.Entry.create "eqn"
        and record_pattern = Pcoq.Entry.create "record_pattern"
        and record_patterns = Pcoq.Entry.create "record_patterns"
        and impl_ident_tail = Pcoq.Entry.create "impl_ident_tail"
        and fixannot = Pcoq.Entry.create "fixannot"
        and impl_name_head = Pcoq.Entry.create "impl_name_head"
        and type_cstr = Pcoq.Entry.create "type_cstr"
        in
        let () =
        Pcoq.grammar_extend Constr.ident None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry Prim.ident)),
                 (fun id loc -> 
# 139 "g_constr.mlg"
                             id 
                                ))])])
        in let () =
        Pcoq.grammar_extend Prim.name None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Atoken (Tok.PKEYWORD ("_")))),
                 (fun _ loc -> 
# 142 "g_constr.mlg"
                 CAst.make ~loc Anonymous 
                               ))])])
        in let () =
        Pcoq.grammar_extend global None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry Prim.reference)),
                 (fun r loc -> 
# 145 "g_constr.mlg"
                                r 
                               ))])])
        in let () =
        Pcoq.grammar_extend constr_pattern None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry constr)),
                 (fun c loc -> 
# 148 "g_constr.mlg"
                        c 
                               ))])])
        in let () =
        Pcoq.grammar_extend lconstr_pattern None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry lconstr)),
                 (fun c loc -> 
# 151 "g_constr.mlg"
                         c 
                               ))])])
        in let () =
        Pcoq.grammar_extend sort None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("Type")))),
                                                        (Extend.Atoken (Tok.PKEYWORD ("@{")))),
                                           (Extend.Aentry universe)),
                              (Extend.Atoken (Tok.PKEYWORD ("}")))),
                 (fun _ u _ _ loc -> 
# 158 "g_constr.mlg"
                                             GType u 
                                     ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("Type")))),
                (fun _ loc -> 
# 157 "g_constr.mlg"
                    GType [] 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("SProp")))),
                (fun _ loc -> 
# 156 "g_constr.mlg"
                     GSProp 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("Prop")))),
                (fun _ loc -> 
# 155 "g_constr.mlg"
                    GProp 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("Set")))),
                (fun _ loc -> 
# 154 "g_constr.mlg"
                    GSet 
                              ))])])
        in let () =
        Pcoq.grammar_extend sort_family None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Atoken (Tok.PKEYWORD ("Type")))),
                 (fun _ loc -> 
# 165 "g_constr.mlg"
                    Sorts.InType 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("SProp")))),
                (fun _ loc -> 
# 164 "g_constr.mlg"
                     Sorts.InSProp 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("Prop")))),
                (fun _ loc -> 
# 163 "g_constr.mlg"
                    Sorts.InProp 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("Set")))),
                (fun _ loc -> 
# 162 "g_constr.mlg"
                    Sorts.InSet 
                              ))])])
        in let () =
        Pcoq.grammar_extend universe_expr None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Atoken (Tok.PKEYWORD ("_")))),
                 (fun _ loc -> 
# 171 "g_constr.mlg"
                 None 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry global)),
                (fun id loc -> 
# 170 "g_constr.mlg"
                         Some (id,0) 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry global)),
                                          (Extend.Atoken (Tok.PKEYWORD ("+")))),
                             (Extend.Aentry natural)),
                (fun n _ id loc -> 
# 169 "g_constr.mlg"
                                           Some (id,n) 
                                   ))])])
        in let () =
        Pcoq.grammar_extend universe None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry universe_expr)),
                 (fun u loc -> 
# 176 "g_constr.mlg"
                               [u] 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("max"))))),
                                                       (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                          (Extend.Alist1sep ((Extend.Aentry universe_expr), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                             (Extend.Atoken (Tok.PKEYWORD (")")))),
                (fun _ ids _ _ loc -> 
# 175 "g_constr.mlg"
                                                                      ids 
                                      ))])])
        in let () =
        Pcoq.grammar_extend lconstr None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Aentryl (operconstr, "200"))),
                 (fun c loc -> 
# 180 "g_constr.mlg"
                                        c 
                               ))])])
        in let () =
        Pcoq.grammar_extend constr None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("@")))),
                                           (Extend.Aentry global)),
                              (Extend.Aentry instance)),
                 (fun i f _ loc -> 
# 184 "g_constr.mlg"
                                         CAst.make ~loc @@ CAppExpl((None,f,i),[]) 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Aentryl (operconstr, "8"))),
                (fun c loc -> 
# 183 "g_constr.mlg"
                                      c 
                              ))])])
        in let () =
        Pcoq.grammar_extend operconstr None
        (None, [(Some ("200"), Some (Gramlib.Gramext.RightA),
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry binder_constr)),
                 (fun c loc -> 
# 188 "g_constr.mlg"
                               c 
                               ))]);
               (Some ("100"), Some (Gramlib.Gramext.RightA),
               [Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry operconstr)),
                             (Extend.Atoken (Tok.PKEYWORD (":>")))),
                (fun _ c1 loc -> 
# 203 "g_constr.mlg"
                   CAst.make ~loc @@ CCast(c1, CastCoerce) 
                                 ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                      (Extend.Aentry operconstr)),
                                         (Extend.Atoken (Tok.PKEYWORD (":")))),
                            Extend.Aself),
               (fun c2 _ c1 loc -> 
# 201 "g_constr.mlg"
                   CAst.make ~loc @@ CCast(c1, CastConv c2) 
                                   ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                      (Extend.Aentry operconstr)),
                                         (Extend.Atoken (Tok.PKEYWORD (":")))),
                            (Extend.Aentry binder_constr)),
               (fun c2 _ c1 loc -> 
# 199 "g_constr.mlg"
                   CAst.make ~loc @@ CCast(c1, CastConv c2) 
                                   ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                      (Extend.Aentry operconstr)),
                                         (Extend.Atoken (Tok.PKEYWORD ("<<:")))),
                            Extend.Aself),
               (fun c2 _ c1 loc -> 
# 197 "g_constr.mlg"
                   CAst.make ~loc @@ CCast(c1, CastNative c2) 
                                   ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                      (Extend.Aentry operconstr)),
                                         (Extend.Atoken (Tok.PKEYWORD ("<<:")))),
                            (Extend.Aentry binder_constr)),
               (fun c2 _ c1 loc -> 
# 195 "g_constr.mlg"
                   CAst.make ~loc @@ CCast(c1, CastNative c2) 
                                   ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                      (Extend.Aentry operconstr)),
                                         (Extend.Atoken (Tok.PKEYWORD ("<:")))),
                            Extend.Aself),
               (fun c2 _ c1 loc -> 
# 193 "g_constr.mlg"
                   CAst.make ~loc @@ CCast(c1, CastVM c2) 
                                   ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                      (Extend.Aentry operconstr)),
                                         (Extend.Atoken (Tok.PKEYWORD ("<:")))),
                            (Extend.Aentry binder_constr)),
               (fun c2 _ c1 loc -> 
# 191 "g_constr.mlg"
                   CAst.make ~loc @@ CCast(c1, CastVM c2) 
                                   ))]);
               (Some ("99"), Some (Gramlib.Gramext.RightA), []);
               (Some ("90"), Some (Gramlib.Gramext.RightA), []);
               (Some ("10"), Some (Gramlib.Gramext.LeftA),
               [Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD ("@")))),
                                          (Extend.Aentry pattern_identref)),
                             (Extend.Alist1 (Extend.Aentry identref))),
                (fun args lid _ loc -> 
# 210 "g_constr.mlg"
          let { CAst.loc = locid; v = id } = lid in
          let args = List.map (fun x -> CAst.make @@ CRef (qualid_of_ident ?loc:x.CAst.loc x.CAst.v, None), None) args in
          CAst.make ~loc @@ CApp((None, CAst.make ?loc:locid @@ CPatVar id),args) 
                                       ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                   (Extend.Atoken (Tok.PKEYWORD ("@")))),
                                                      (Extend.Aentry global)),
                                         (Extend.Aentry instance)),
                            (Extend.Alist0 Extend.Anext)),
               (fun args i f _ loc -> 
# 208 "g_constr.mlg"
                                                          CAst.make ~loc @@ CAppExpl((None,f,i),args) 
                                      ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Stop,
                                         (Extend.Aentry operconstr)),
                            (Extend.Alist1 (Extend.Aentry appl_arg))),
               (fun args f loc -> 
# 207 "g_constr.mlg"
                                               CAst.make ~loc @@ CApp((None,f),args) 
                                  ))]);
               (Some ("9"), None,
               [Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD ("..")))),
                                          (Extend.Aentryl (operconstr, "0"))),
                             (Extend.Atoken (Tok.PKEYWORD ("..")))),
                (fun _ c _ loc -> 
# 215 "g_constr.mlg"
            CAst.make ~loc @@ CAppExpl ((None, (qualid_of_ident ~loc ldots_var), None),[c]) 
                                  ))]);
               (Some ("8"), None, []);
               (Some ("1"), Some (Gramlib.Gramext.LeftA),
               [Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry operconstr)),
                                          (Extend.Atoken (Tok.PKEYWORD ("%")))),
                             (Extend.Atoken (Tok.PIDENT (None)))),
                (fun key _ c loc -> 
# 223 "g_constr.mlg"
                                          CAst.make ~loc @@ CDelimiters (key,c) 
                                    ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                   (Extend.Next 
                                                                   (Extend.Stop,
                                                                   (Extend.Aentry operconstr)),
                                                                   (Extend.Atoken (Tok.PKEYWORD (".(")))),
                                                                   (Extend.Atoken (Tok.PKEYWORD ("@")))),
                                                      (Extend.Aentry global)),
                                         (Extend.Alist0 (Extend.Aentryl (operconstr, "9")))),
                            (Extend.Atoken (Tok.PKEYWORD (")")))),
               (fun _ args f _ _ c loc -> 
# 222 "g_constr.mlg"
          CAst.make ~loc @@ CAppExpl((Some (List.length args+1),f,None),args@[c]) 
                                          ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                   (Extend.Stop,
                                                                   (Extend.Aentry operconstr)),
                                                                   (Extend.Atoken (Tok.PKEYWORD (".(")))),
                                                      (Extend.Aentry global)),
                                         (Extend.Alist0 (Extend.Aentry appl_arg))),
                            (Extend.Atoken (Tok.PKEYWORD (")")))),
               (fun _ args f _ c loc -> 
# 219 "g_constr.mlg"
          CAst.make ~loc @@ CApp((Some (List.length args+1), CAst.make @@ CRef (f,None)),args@[c,None]) 
                                        ))]);
               (Some ("0"), None,
               [Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD ("`(")))),
                                          (Extend.Aentryl (operconstr, "200"))),
                             (Extend.Atoken (Tok.PKEYWORD (")")))),
                (fun _ c _ loc -> 
# 238 "g_constr.mlg"
            CAst.make ~loc @@ CGeneralization (Explicit, None, c) 
                                  ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                      (Extend.Atoken (Tok.PKEYWORD ("`{")))),
                                         (Extend.Aentryl (operconstr, "200"))),
                            (Extend.Atoken (Tok.PKEYWORD ("}")))),
               (fun _ c _ loc -> 
# 236 "g_constr.mlg"
            CAst.make ~loc @@ CGeneralization (Implicit, None, c) 
                                 ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                      (Extend.Atoken (Tok.PKEYWORD ("{")))),
                                         (Extend.Aentry binder_constr)),
                            (Extend.Atoken (Tok.PKEYWORD ("}")))),
               (fun _ c _ loc -> 
# 234 "g_constr.mlg"
            CAst.make ~loc @@ CNotation((InConstrEntrySomeLevel,"{ _ }"),([c],[],[],[])) 
                                 ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                      (Extend.Atoken (Tok.PKEYWORD ("{|")))),
                                         (Extend.Aentry record_declaration)),
                            (Extend.Atoken (Tok.PKEYWORD ("|}")))),
               (fun _ c _ loc -> 
# 232 "g_constr.mlg"
                                                c 
                                 ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                      (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                         (Extend.Aentryl (operconstr, "200"))),
                            (Extend.Atoken (Tok.PKEYWORD (")")))),
               (fun _ c _ loc -> 
# 228 "g_constr.mlg"
          (match c.CAst.v with
            | CPrim (Numeral (SPlus,n)) ->
                CAst.make ~loc @@ CNotation((InConstrEntrySomeLevel,"( _ )"),([c],[],[],[]))
            | _ -> c) 
                                 ));
               Extend.Rule
               (Extend.Next (Extend.Stop, (Extend.Aentry match_constr)),
               (fun c loc -> 
# 226 "g_constr.mlg"
                            c 
                             ));
               Extend.Rule
               (Extend.Next (Extend.Stop, (Extend.Aentry atomic_constr)),
               (fun c loc -> 
# 225 "g_constr.mlg"
                             c 
                             ))])])
        in let () =
        Pcoq.grammar_extend record_declaration None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry record_fields)),
                 (fun fs loc -> 
# 242 "g_constr.mlg"
                                CAst.make ~loc @@ CRecord fs 
                                ))])])
        in let () =
        Pcoq.grammar_extend record_fields None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 248 "g_constr.mlg"
             [] 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Aentry record_field_declaration)),
                (fun f loc -> 
# 247 "g_constr.mlg"
                                          [f] 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry record_field_declaration)),
                                          (Extend.Atoken (Tok.PKEYWORD (";")))),
                             (Extend.Aentry record_fields)),
                (fun fs _ f loc -> 
# 246 "g_constr.mlg"
                                                                   f :: fs 
                                   ))])])
        in let () =
        Pcoq.grammar_extend record_field_declaration None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Aentry global)),
                                                        (Extend.Aentry binders)),
                                           (Extend.Atoken (Tok.PKEYWORD (":=")))),
                              (Extend.Aentry lconstr)),
                 (fun c _ bl id loc -> 
# 254 "g_constr.mlg"
        (id, mkLambdaCN ~loc bl c) 
                                       ))])])
        in let () =
        Pcoq.grammar_extend binder_constr None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry fix_constr)),
                 (fun c loc -> 
# 298 "g_constr.mlg"
                          c 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("if")))),
                                                                    (Extend.Aentryl (operconstr, "200"))),
                                                                    (Extend.Aentry return_type)),
                                                                    (Extend.Atoken (Tok.PKEYWORD ("then")))),
                                                       (Extend.Aentryl (operconstr, "200"))),
                                          (Extend.Atoken (Tok.PKEYWORD ("else")))),
                             (Extend.Aentryl (operconstr, "200"))),
                (fun b2 _ b1 _ po c _ loc -> 
# 297 "g_constr.mlg"
            CAst.make ~loc @@ CIf (c, po, b1, b2) 
                                             ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("let")))),
                                                                    (Extend.Atoken (Tok.PKEYWORD ("'")))),
                                                                    (Extend.Aentry pattern)),
                                                                    (Extend.Atoken (Tok.PKEYWORD ("in")))),
                                                                    (Extend.Aentryl (pattern, "200"))),
                                                                    (Extend.Atoken (Tok.PKEYWORD (":=")))),
                                                                    (Extend.Aentryl (operconstr, "200"))),
                                                       (Extend.Aentry case_type)),
                                          (Extend.Atoken (Tok.PKEYWORD ("in")))),
                             (Extend.Aentryl (operconstr, "200"))),
                (fun c2 _ rt c1 _ t _ p _ _ loc -> 
# 292 "g_constr.mlg"
              CAst.make ~loc @@
            CCases (LetPatternStyle, Some rt, [c1, aliasvar p, Some t], [CAst.make ~loc ([[p]], c2)]) 
                                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("let")))),
                                                                    (Extend.Atoken (Tok.PKEYWORD ("'")))),
                                                                    (Extend.Aentry pattern)),
                                                                    (Extend.Atoken (Tok.PKEYWORD (":=")))),
                                                                    (Extend.Aentryl (operconstr, "200"))),
                                                       (Extend.Aentry case_type)),
                                          (Extend.Atoken (Tok.PKEYWORD ("in")))),
                             (Extend.Aentryl (operconstr, "200"))),
                (fun c2 _ rt c1 _ p _ _ loc -> 
# 286 "g_constr.mlg"
              CAst.make ~loc @@
            CCases (LetPatternStyle, Some rt, [c1, aliasvar p, None], [CAst.make ~loc ([[p]], c2)]) 
                                               ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("let")))),
                                                                    (Extend.Atoken (Tok.PKEYWORD ("'")))),
                                                                    (Extend.Aentry pattern)),
                                                                    (Extend.Atoken (Tok.PKEYWORD (":=")))),
                                                       (Extend.Aentryl (operconstr, "200"))),
                                          (Extend.Atoken (Tok.PKEYWORD ("in")))),
                             (Extend.Aentryl (operconstr, "200"))),
                (fun c2 _ c1 _ p _ _ loc -> 
# 282 "g_constr.mlg"
              CAst.make ~loc @@
            CCases (LetPatternStyle, None,    [c1, None, None],       [CAst.make ~loc ([[p]], c2)]) 
                                            ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("let")))),
                                                                    (Extend.Arules 
                                                                    [Extend.Rules 
                                                                    (Extend.NextNoRec 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("()")))),
                                                                    (fun _
                                                                    loc -> 
                                                                    
# 275 "g_constr.mlg"
                                                                         [] 
                                                                    ));
                                                                    Extend.Rules 
                                                                    (Extend.NextNoRec 
                                                                    (Extend.NextNoRec 
                                                                    (Extend.NextNoRec 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                                    (Extend.Alist0sep ((Extend.Aentry name), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                                                                    (Extend.Atoken (Tok.PKEYWORD (")")))),
                                                                    (fun _ l
                                                                    _ loc ->
                                                                    
# 275 "g_constr.mlg"
                                                         l 
                                                                    ))])),
                                                                    (Extend.Aentry return_type)),
                                                                    (Extend.Atoken (Tok.PKEYWORD (":=")))),
                                                       (Extend.Aentryl (operconstr, "200"))),
                                          (Extend.Atoken (Tok.PKEYWORD ("in")))),
                             (Extend.Aentryl (operconstr, "200"))),
                (fun c2 _ c1 _ po lb _ loc -> 
# 279 "g_constr.mlg"
            CAst.make ~loc @@ CLetTuple (lb,po,c1,c2) 
                                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("let")))),
                                                       (Extend.Aentry single_fix)),
                                          (Extend.Atoken (Tok.PKEYWORD ("in")))),
                             (Extend.Aentryl (operconstr, "200"))),
                (fun c _ fx _ loc -> 
# 269 "g_constr.mlg"
            let fixp = mk_single_fix fx in
          let { CAst.loc = li; v = id } = match fixp.CAst.v with
              CFix(id,_) -> id
            | CCoFix(id,_) -> id
            | _ -> assert false in
          CAst.make ~loc @@ CLetIn( CAst.make ?loc:li @@ Name id,fixp,None,c) 
                                     ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("let")))),
                                                                    (Extend.Aentry name)),
                                                                    (Extend.Aentry binders)),
                                                                    (Extend.Aentry type_cstr)),
                                                                    (Extend.Atoken (Tok.PKEYWORD (":=")))),
                                                       (Extend.Aentryl (operconstr, "200"))),
                                          (Extend.Atoken (Tok.PKEYWORD ("in")))),
                             (Extend.Aentryl (operconstr, "200"))),
                (fun c2 _ c1 _ ty bl id _ loc -> 
# 263 "g_constr.mlg"
          let ty,c1 = match ty, c1 with
          | (_,None), { CAst.v = CCast(c, CastConv t) } -> (Loc.tag ?loc:(constr_loc t) @@ Some t), c (* Tolerance, see G_vernac.def_body *)
          | _, _ -> ty, c1 in
          CAst.make ~loc @@ CLetIn(id,mkLambdaCN ?loc:(constr_loc c1) bl c1,
                 Option.map (mkProdCN ?loc:(fst ty) bl) (snd ty), c2) 
                                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("fun")))),
                                                       (Extend.Aentry open_binders)),
                                          (Extend.Atoken (Tok.PKEYWORD ("=>")))),
                             (Extend.Aentryl (operconstr, "200"))),
                (fun c _ bl _ loc -> 
# 260 "g_constr.mlg"
            mkLambdaCN ~loc bl c 
                                     ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("forall")))),
                                                       (Extend.Aentry open_binders)),
                                          (Extend.Atoken (Tok.PKEYWORD (",")))),
                             (Extend.Aentryl (operconstr, "200"))),
                (fun c _ bl _ loc -> 
# 258 "g_constr.mlg"
            mkProdCN ~loc bl c 
                                     ))])])
        in let () =
        Pcoq.grammar_extend appl_arg None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Aentryl (operconstr, "9"))),
                 (fun c loc -> 
# 303 "g_constr.mlg"
                                    (c,None) 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry lpar_id_coloneq)),
                                          (Extend.Aentry lconstr)),
                             (Extend.Atoken (Tok.PKEYWORD (")")))),
                (fun _ c id loc -> 
# 302 "g_constr.mlg"
            (c,Some (CAst.make ~loc @@ ExplByName id)) 
                                   ))])])
        in let () =
        Pcoq.grammar_extend atomic_constr None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry pattern_ident)),
                              (Extend.Aentry evar_instance)),
                 (fun inst id loc -> 
# 313 "g_constr.mlg"
                                                    CAst.make ~loc @@ CEvar(id,inst) 
                                     ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("?")))),
                                                       (Extend.Atoken (Tok.PKEYWORD ("[")))),
                                          (Extend.Aentry pattern_ident)),
                             (Extend.Atoken (Tok.PKEYWORD ("]")))),
                (fun _ id _ _ loc -> 
# 312 "g_constr.mlg"
                                              CAst.make ~loc @@  CHole (None, IntroFresh id, None) 
                                     ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("?")))),
                                                       (Extend.Atoken (Tok.PKEYWORD ("[")))),
                                          (Extend.Aentry ident)),
                             (Extend.Atoken (Tok.PKEYWORD ("]")))),
                (fun _ id _ _ loc -> 
# 311 "g_constr.mlg"
                                      CAst.make ~loc @@  CHole (None, IntroIdentifier id, None) 
                                     ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("_")))),
                (fun _ loc -> 
# 310 "g_constr.mlg"
                      CAst.make ~loc @@ CHole (None, IntroAnonymous, None) 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry string)),
                (fun s loc -> 
# 309 "g_constr.mlg"
                      CAst.make ~loc @@ CPrim (String s) 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PNUMERAL None))),
                (fun n loc -> 
# 308 "g_constr.mlg"
                      CAst.make ~loc @@ CPrim (Numeral (SPlus,n)) 
                              ));
                Extend.Rule (Extend.Next (Extend.Stop, (Extend.Aentry sort)),
                (fun s loc -> 
# 307 "g_constr.mlg"
                      CAst.make ~loc @@  CSort s 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry global)),
                             (Extend.Aentry instance)),
                (fun i g loc -> 
# 306 "g_constr.mlg"
                                  CAst.make ~loc @@ CRef (g,i) 
                                ))])])
        in let () =
        Pcoq.grammar_extend inst None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Aentry ident)),
                                           (Extend.Atoken (Tok.PKEYWORD (":=")))),
                              (Extend.Aentry lconstr)),
                 (fun c _ id loc -> 
# 316 "g_constr.mlg"
                                           (id,c) 
                                    ))])])
        in let () =
        Pcoq.grammar_extend evar_instance None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 320 "g_constr.mlg"
             [] 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD ("@{")))),
                                          (Extend.Alist1sep ((Extend.Aentry inst), (Extend.Atoken (Tok.PKEYWORD (";")))))),
                             (Extend.Atoken (Tok.PKEYWORD ("}")))),
                (fun _ l _ loc -> 
# 319 "g_constr.mlg"
                                               l 
                                  ))])])
        in let () =
        Pcoq.grammar_extend instance None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 324 "g_constr.mlg"
             None 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD ("@{")))),
                                          (Extend.Alist0 (Extend.Aentry universe_level))),
                             (Extend.Atoken (Tok.PKEYWORD ("}")))),
                (fun _ l _ loc -> 
# 323 "g_constr.mlg"
                                                 Some l 
                                  ))])])
        in let () =
        Pcoq.grammar_extend universe_level None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry global)),
                 (fun id loc -> 
# 332 "g_constr.mlg"
                         GType (UNamed id) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("_")))),
                (fun _ loc -> 
# 331 "g_constr.mlg"
                 GType UAnonymous 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("Type")))),
                (fun _ loc -> 
# 330 "g_constr.mlg"
                    GType UUnknown 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("Prop")))),
                (fun _ loc -> 
# 329 "g_constr.mlg"
                    GProp 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("Set")))),
                (fun _ loc -> 
# 327 "g_constr.mlg"
                   GSet 
                              ))])])
        in let () =
        Pcoq.grammar_extend fix_constr None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Aentry single_fix)),
                                                        (Extend.Atoken (Tok.PKEYWORD ("with")))),
                                                        (Extend.Alist1sep ((Extend.Aentry fix_decl), (Extend.Atoken (Tok.PKEYWORD ("with")))))),
                                           (Extend.Atoken (Tok.PKEYWORD ("for")))),
                              (Extend.Aentry identref)),
                 (fun id _ dcls _ f loc -> 
# 339 "g_constr.mlg"
            let (_,kw,dcl1) = f in
          mk_fix(loc,kw,id,dcl1::dcls) 
                                           ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry single_fix)),
                (fun fx1 loc -> 
# 336 "g_constr.mlg"
                            mk_single_fix fx1 
                                ))])])
        in let () =
        Pcoq.grammar_extend single_fix None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry fix_kw)),
                              (Extend.Aentry fix_decl)),
                 (fun dcl kw loc -> 
# 344 "g_constr.mlg"
                                     (loc,kw,dcl) 
                                    ))])])
        in let () =
        Pcoq.grammar_extend fix_kw None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Atoken (Tok.PKEYWORD ("cofix")))),
                 (fun _ loc -> 
# 348 "g_constr.mlg"
                     false 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("fix")))),
                (fun _ loc -> 
# 347 "g_constr.mlg"
                   true 
                              ))])])
        in let () =
        Pcoq.grammar_extend fix_decl None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Aentry identref)),
                                                        (Extend.Aentry binders_fixannot)),
                                                        (Extend.Aentry type_cstr)),
                                           (Extend.Atoken (Tok.PKEYWORD (":=")))),
                              (Extend.Aentryl (operconstr, "200"))),
                 (fun c _ ty bl id loc -> 
# 353 "g_constr.mlg"
            (id,fst bl,snd bl,c,ty) 
                                          ))])])
        in let () =
        Pcoq.grammar_extend match_constr None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("match")))),
                                                        (Extend.Alist1sep ((Extend.Aentry case_item), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                                                        (Extend.Aopt (Extend.Aentry case_type))),
                                                        (Extend.Atoken (Tok.PKEYWORD ("with")))),
                                           (Extend.Aentry branches)),
                              (Extend.Atoken (Tok.PKEYWORD ("end")))),
                 (fun _ br _ ty ci _ loc -> 
# 357 "g_constr.mlg"
                                CAst.make ~loc @@ CCases(RegularStyle,ty,ci,br) 
                                            ))])])
        in let () =
        Pcoq.grammar_extend case_item None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Aentryl (operconstr, "100"))),
                                           (Extend.Aopt (Extend.Arules 
                                           [Extend.Rules (Extend.NextNoRec 
                                                         (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (Tok.PKEYWORD ("as")))),
                                                         (Extend.Aentry name)),
                                                         (fun id _ loc -> 
                                                         
# 361 "g_constr.mlg"
                                      id 
                                                         ))]))),
                              (Extend.Aopt (Extend.Arules [Extend.Rules 
                                                          (Extend.NextNoRec 
                                                          (Extend.NextNoRec 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (Tok.PKEYWORD ("in")))),
                                                          (Extend.Aentry pattern)),
                                                          (fun t _ loc -> 
                                                          
# 362 "g_constr.mlg"
                                       t 
                                                          ))]))),
                 (fun ty ona c loc -> 
# 363 "g_constr.mlg"
             (c,ona,ty) 
                                      ))])])
        in let () =
        Pcoq.grammar_extend case_type None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PKEYWORD ("return")))),
                              (Extend.Aentryl (operconstr, "100"))),
                 (fun ty _ loc -> 
# 366 "g_constr.mlg"
                                                   ty 
                                  ))])])
        in let () =
        Pcoq.grammar_extend return_type None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Aopt (Extend.Arules [Extend.Rules 
                                                          (Extend.NextNoRec 
                                                          (Extend.NextNoRec 
                                                          (Extend.Stop,
                                                          (Extend.Aopt (Extend.Arules 
                                                          [Extend.Rules 
                                                          (Extend.NextNoRec 
                                                          (Extend.NextNoRec 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (Tok.PKEYWORD ("as")))),
                                                          (Extend.Aentry name)),
                                                          (fun na _ loc -> 
                                                          
# 369 "g_constr.mlg"
                                              na 
                                                          ))]))),
                                                          (Extend.Aentry case_type)),
                                                          (fun ty na loc ->
                                                          
# 370 "g_constr.mlg"
                                      (na,ty) 
                                                          ))]))),
                 (fun a loc -> 
# 371 "g_constr.mlg"
          match a with
          | None -> None, None
          | Some (na,t) -> (na, Some t) 
                               ))])])
        in let () =
        Pcoq.grammar_extend branches None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aopt (Extend.Atoken (Tok.PKEYWORD ("|"))))),
                              (Extend.Alist0sep ((Extend.Aentry eqn), (Extend.Atoken (Tok.PKEYWORD ("|")))))),
                 (fun br _ loc -> 
# 377 "g_constr.mlg"
                                          br 
                                  ))])])
        in let () =
        Pcoq.grammar_extend mult_pattern None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Alist1sep ((Extend.Aentryl (pattern, "99")), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                 (fun pl loc -> 
# 380 "g_constr.mlg"
                                                   pl 
                                ))])])
        in let () =
        Pcoq.grammar_extend eqn None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Alist1sep ((Extend.Aentry mult_pattern), (Extend.Atoken (Tok.PKEYWORD ("|")))))),
                                           (Extend.Atoken (Tok.PKEYWORD ("=>")))),
                              (Extend.Aentry lconstr)),
                 (fun rhs _ pll loc -> 
# 384 "g_constr.mlg"
                                 (CAst.make ~loc (pll,rhs)) 
                                       ))])])
        in let () =
        Pcoq.grammar_extend record_pattern None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Aentry global)),
                                           (Extend.Atoken (Tok.PKEYWORD (":=")))),
                              (Extend.Aentry pattern)),
                 (fun pat _ id loc -> 
# 387 "g_constr.mlg"
                                              (id, pat) 
                                      ))])])
        in let () =
        Pcoq.grammar_extend record_patterns None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 393 "g_constr.mlg"
             [] 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry record_pattern)),
                (fun p loc -> 
# 392 "g_constr.mlg"
                               [p] 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry record_pattern)),
                             (Extend.Atoken (Tok.PKEYWORD (";")))),
                (fun _ p loc -> 
# 391 "g_constr.mlg"
                                     [p] 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry record_pattern)),
                                          (Extend.Atoken (Tok.PKEYWORD (";")))),
                             (Extend.Aentry record_patterns)),
                (fun ps _ p loc -> 
# 390 "g_constr.mlg"
                                                           p :: ps 
                                   ))])])
        in let () =
        Pcoq.grammar_extend pattern None
        (None, [(Some ("200"), Some (Gramlib.Gramext.RightA), []);
               (Some ("100"), Some (Gramlib.Gramext.RightA),
               [Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry pattern)),
                                          (Extend.Atoken (Tok.PKEYWORD ("|")))),
                             (Extend.Alist1sep ((Extend.Aentry pattern), (Extend.Atoken (Tok.PKEYWORD ("|")))))),
                (fun pl _ p loc -> 
# 399 "g_constr.mlg"
                                                          CAst.make ~loc @@ CPatOr (p::pl) 
                                   ))]);
               (Some ("99"), Some (Gramlib.Gramext.RightA), []);
               (Some ("90"), Some (Gramlib.Gramext.RightA), []);
               (Some ("10"), Some (Gramlib.Gramext.LeftA),
               [Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD ("@")))),
                                          (Extend.Aentry Prim.reference)),
                             (Extend.Alist0 Extend.Anext)),
                (fun lp r _ loc -> 
# 407 "g_constr.mlg"
          CAst.make ~loc @@ CPatCstr (r, Some lp, []) 
                                   ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Stop,
                                         (Extend.Aentry pattern)),
                            (Extend.Alist1 Extend.Anext)),
               (fun lp p loc -> 
# 405 "g_constr.mlg"
                                          mkAppPattern ~loc p lp 
                                ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                      (Extend.Aentry pattern)),
                                         (Extend.Atoken (Tok.PKEYWORD ("as")))),
                            (Extend.Aentry name)),
               (fun na _ p loc -> 
# 404 "g_constr.mlg"
          CAst.make ~loc @@ CPatAlias (p, na) 
                                  ))]);
               (Some ("1"), Some (Gramlib.Gramext.LeftA),
               [Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry pattern)),
                                          (Extend.Atoken (Tok.PKEYWORD ("%")))),
                             (Extend.Atoken (Tok.PIDENT (None)))),
                (fun key _ c loc -> 
# 409 "g_constr.mlg"
                                         CAst.make ~loc @@ CPatDelimiters (key,c) 
                                    ))]);
               (Some ("0"), None,
               [Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry string)),
                (fun s loc -> 
# 428 "g_constr.mlg"
                        CAst.make ~loc @@ CPatPrim (String s) 
                              ));
               Extend.Rule
               (Extend.Next (Extend.Stop,
                            (Extend.Atoken (Tok.PNUMERAL None))),
               (fun n loc -> 
# 427 "g_constr.mlg"
                        CAst.make ~loc @@ CPatPrim (Numeral (SPlus,n)) 
                             ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                   (Extend.Stop,
                                                                   (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                                   (Extend.Aentryl (pattern, "200"))),
                                                      (Extend.Atoken (Tok.PKEYWORD (":")))),
                                         (Extend.Aentry lconstr)),
                            (Extend.Atoken (Tok.PKEYWORD (")")))),
               (fun _ ty _ p _ loc -> 
# 420 "g_constr.mlg"
            let p =
            match p with
            | { CAst.v = CPatPrim (Numeral (SPlus,n)) } ->
                 CAst.make ~loc @@ CPatNotation((InConstrEntrySomeLevel,"( _ )"),([p],[]),[])
            | _ -> p
          in
          CAst.make ~loc @@ CPatCast (p, ty) 
                                      ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                      (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                         (Extend.Aentryl (pattern, "200"))),
                            (Extend.Atoken (Tok.PKEYWORD (")")))),
               (fun _ p _ loc -> 
# 415 "g_constr.mlg"
            (match p.CAst.v with
            | CPatPrim (Numeral (SPlus,n)) ->
                 CAst.make ~loc @@ CPatNotation((InConstrEntrySomeLevel,"( _ )"),([p],[]),[])
            | _ -> p) 
                                 ));
               Extend.Rule
               (Extend.Next (Extend.Stop,
                            (Extend.Atoken (Tok.PKEYWORD ("_")))),
               (fun _ loc -> 
# 413 "g_constr.mlg"
                 CAst.make ~loc @@ CPatAtom None 
                             ));
               Extend.Rule
               (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                      (Extend.Atoken (Tok.PKEYWORD ("{|")))),
                                         (Extend.Aentry record_patterns)),
                            (Extend.Atoken (Tok.PKEYWORD ("|}")))),
               (fun _ pat _ loc -> 
# 412 "g_constr.mlg"
                                               CAst.make ~loc @@ CPatRecord pat 
                                   ));
               Extend.Rule
               (Extend.Next (Extend.Stop, (Extend.Aentry Prim.reference)),
               (fun r loc -> 
# 411 "g_constr.mlg"
                                               CAst.make ~loc @@ CPatAtom (Some r) 
                             ))])])
        in let () =
        Pcoq.grammar_extend impl_ident_tail None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD (":")))),
                                           (Extend.Aentry lconstr)),
                              (Extend.Atoken (Tok.PKEYWORD ("}")))),
                 (fun _ c _ loc -> 
# 439 "g_constr.mlg"
          (fun na -> CLocalAssum ([na],Default Implicit,c)) 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Alist1 (Extend.Aentry name))),
                             (Extend.Atoken (Tok.PKEYWORD ("}")))),
                (fun _ nal loc -> 
# 435 "g_constr.mlg"
          (fun na -> CLocalAssum (na::nal,Default Implicit,
                                CAst.make ?loc:(Loc.merge_opt na.CAst.loc (Some loc)) @@
                                CHole (Some (Evar_kinds.BinderType na.CAst.v), IntroAnonymous, None))) 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Alist1 (Extend.Aentry name))),
                                                       (Extend.Atoken (Tok.PKEYWORD (":")))),
                                          (Extend.Aentry lconstr)),
                             (Extend.Atoken (Tok.PKEYWORD ("}")))),
                (fun _ c _ nal loc -> 
# 433 "g_constr.mlg"
          (fun na -> CLocalAssum (na::nal,Default Implicit,c)) 
                                      ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("}")))),
                (fun _ loc -> 
# 431 "g_constr.mlg"
                 binder_of_name Implicit 
                              ))])])
        in let () =
        Pcoq.grammar_extend fixannot None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("{")))),
                                                        (Extend.Atoken (Tok.PIDENT (Some
                                                        ("measure"))))),
                                                        (Extend.Aentry constr)),
                                                        (Extend.Aopt (Extend.Aentry identref))),
                                           (Extend.Aopt (Extend.Aentry constr))),
                              (Extend.Atoken (Tok.PKEYWORD ("}")))),
                 (fun _ rel id m _ _ loc -> 
# 446 "g_constr.mlg"
                                 CAst.make ~loc @@ CMeasureRec (id,m,rel) 
                                            ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("{")))),
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("wf"))))),
                                                       (Extend.Aentry constr)),
                                          (Extend.Aentry identref)),
                             (Extend.Atoken (Tok.PKEYWORD ("}")))),
                (fun _ id rel _ _ loc -> 
# 444 "g_constr.mlg"
                                                         CAst.make ~loc @@ CWfRec(id,rel) 
                                         ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("{")))),
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("struct"))))),
                                          (Extend.Aentry identref)),
                             (Extend.Atoken (Tok.PKEYWORD ("}")))),
                (fun _ id _ _ loc -> 
# 443 "g_constr.mlg"
                                                   CAst.make ~loc @@ CStructRec id 
                                     ))])])
        in let () =
        Pcoq.grammar_extend impl_name_head None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry impl_ident_head)),
                 (fun id loc -> 
# 450 "g_constr.mlg"
                                  CAst.make ~loc @@ Name id 
                                ))])])
        in let () =
        Pcoq.grammar_extend binders_fixannot None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 457 "g_constr.mlg"
             [], None 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry binder)),
                             (Extend.Aentry binders_fixannot)),
                (fun bl b loc -> 
# 456 "g_constr.mlg"
                                               b @ fst bl, snd bl 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry fixannot)),
                (fun f loc -> 
# 455 "g_constr.mlg"
                          [], Some f 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry impl_name_head)),
                                          (Extend.Aentry impl_ident_tail)),
                             (Extend.Aentry binders_fixannot)),
                (fun bl assum na loc -> 
# 454 "g_constr.mlg"
            (assum na :: fst bl), snd bl 
                                        ))])])
        in let () =
        Pcoq.grammar_extend open_binders None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry closed_binder)),
                              (Extend.Aentry binders)),
                 (fun bl' bl loc -> 
# 473 "g_constr.mlg"
            bl@bl' 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry name)),
                                          (Extend.Atoken (Tok.PKEYWORD ("..")))),
                             (Extend.Aentry name)),
                (fun id2 _ id1 loc -> 
# 470 "g_constr.mlg"
            [CLocalAssum ([id1;(CAst.make ~loc (Name ldots_var));id2],
                          Default Explicit, CAst.make ~loc @@ CHole (None, IntroAnonymous, None))] 
                                      ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry name)),
                                          (Extend.Alist0 (Extend.Aentry name))),
                             (Extend.Aentry binders)),
                (fun bl idl id loc -> 
# 468 "g_constr.mlg"
            binders_of_names (id::idl) @ bl 
                                      ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Aentry name)),
                                                       (Extend.Alist0 (Extend.Aentry name))),
                                          (Extend.Atoken (Tok.PKEYWORD (":")))),
                             (Extend.Aentry lconstr)),
                (fun c _ idl id loc -> 
# 465 "g_constr.mlg"
            [CLocalAssum (id::idl,Default Explicit,c)]
        (* binders factorized with open binder *) 
                                       ))])])
        in let () =
        Pcoq.grammar_extend binders None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Alist0 (Extend.Aentry binder))),
                 (fun l loc -> 
# 477 "g_constr.mlg"
                              List.flatten l 
                               ))])])
        in let () =
        Pcoq.grammar_extend binder None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry closed_binder)),
                 (fun bl loc -> 
# 481 "g_constr.mlg"
                                bl 
                                ));
                Extend.Rule (Extend.Next (Extend.Stop, (Extend.Aentry name)),
                (fun id loc -> 
# 480 "g_constr.mlg"
                       [CLocalAssum ([id],Default Explicit, CAst.make ~loc @@ CHole (None, IntroAnonymous, None))] 
                               ))])])
        in let () =
        Pcoq.grammar_extend closed_binder None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PKEYWORD ("'")))),
                              (Extend.Aentryl (pattern, "0"))),
                 (fun p _ loc -> 
# 507 "g_constr.mlg"
            let (p, ty) =
            match p.CAst.v with
            | CPatCast (p, ty) -> (p, Some ty)
            | _ -> (p, None)
          in
          [CLocalPattern (CAst.make ~loc (p, ty))] 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD ("`{")))),
                                          (Extend.Alist1sep ((Extend.Aentry typeclass_constraint), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                             (Extend.Atoken (Tok.PKEYWORD ("}")))),
                (fun _ tc _ loc -> 
# 505 "g_constr.mlg"
            List.map (fun (n, b, t) -> CLocalAssum ([n], Generalized (Implicit, b), t)) tc 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD ("`(")))),
                                          (Extend.Alist1sep ((Extend.Aentry typeclass_constraint), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                             (Extend.Atoken (Tok.PKEYWORD (")")))),
                (fun _ tc _ loc -> 
# 503 "g_constr.mlg"
            List.map (fun (n, b, t) -> CLocalAssum ([n], Generalized (Explicit, b), t)) tc 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("{")))),
                                                       (Extend.Aentry name)),
                                          (Extend.Alist1 (Extend.Aentry name))),
                             (Extend.Atoken (Tok.PKEYWORD ("}")))),
                (fun _ idl id _ loc -> 
# 501 "g_constr.mlg"
            List.map (fun id -> CLocalAssum ([id],Default Implicit, CAst.make ~loc @@ CHole (None, IntroAnonymous, None))) (id::idl) 
                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("{")))),
                                                                    (Extend.Aentry name)),
                                                       (Extend.Atoken (Tok.PKEYWORD (":")))),
                                          (Extend.Aentry lconstr)),
                             (Extend.Atoken (Tok.PKEYWORD ("}")))),
                (fun _ c _ id _ loc -> 
# 499 "g_constr.mlg"
            [CLocalAssum ([id],Default Implicit,c)] 
                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("{")))),
                                                                    (Extend.Aentry name)),
                                                                    (Extend.Alist1 (Extend.Aentry name))),
                                                       (Extend.Atoken (Tok.PKEYWORD (":")))),
                                          (Extend.Aentry lconstr)),
                             (Extend.Atoken (Tok.PKEYWORD ("}")))),
                (fun _ c _ idl id _ loc -> 
# 497 "g_constr.mlg"
            [CLocalAssum (id::idl,Default Implicit,c)] 
                                           ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD ("{")))),
                                          (Extend.Aentry name)),
                             (Extend.Atoken (Tok.PKEYWORD ("}")))),
                (fun _ id _ loc -> 
# 495 "g_constr.mlg"
            [CLocalAssum ([id],Default Implicit, CAst.make ~loc @@ CHole (None, IntroAnonymous, None))] 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                                    (Extend.Aentry name)),
                                                                    (Extend.Atoken (Tok.PKEYWORD (":")))),
                                                                    (Extend.Aentry lconstr)),
                                                       (Extend.Atoken (Tok.PKEYWORD (":=")))),
                                          (Extend.Aentry lconstr)),
                             (Extend.Atoken (Tok.PKEYWORD (")")))),
                (fun _ c _ t _ id _ loc -> 
# 493 "g_constr.mlg"
            [CLocalDef (id,c,Some t)] 
                                           ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                                    (Extend.Aentry name)),
                                                       (Extend.Atoken (Tok.PKEYWORD (":=")))),
                                          (Extend.Aentry lconstr)),
                             (Extend.Atoken (Tok.PKEYWORD (")")))),
                (fun _ c _ id _ loc -> 
# 489 "g_constr.mlg"
            (match c.CAst.v with
          | CCast(c, CastConv t) -> [CLocalDef (id,c,Some t)]
          | _ -> [CLocalDef (id,c,None)]) 
                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                                    (Extend.Aentry name)),
                                                       (Extend.Atoken (Tok.PKEYWORD (":")))),
                                          (Extend.Aentry lconstr)),
                             (Extend.Atoken (Tok.PKEYWORD (")")))),
                (fun _ c _ id _ loc -> 
# 487 "g_constr.mlg"
            [CLocalAssum ([id],Default Explicit,c)] 
                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                                    (Extend.Aentry name)),
                                                                    (Extend.Alist1 (Extend.Aentry name))),
                                                       (Extend.Atoken (Tok.PKEYWORD (":")))),
                                          (Extend.Aentry lconstr)),
                             (Extend.Atoken (Tok.PKEYWORD (")")))),
                (fun _ c _ idl id _ loc -> 
# 485 "g_constr.mlg"
            [CLocalAssum (id::idl,Default Explicit,c)] 
                                           ))])])
        in let () =
        Pcoq.grammar_extend typeclass_constraint None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Aentryl (operconstr, "200"))),
                 (fun c loc -> 
# 522 "g_constr.mlg"
            (CAst.make ~loc Anonymous), false, c 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry name_colon)),
                                          (Extend.Arules [Extend.Rules 
                                                         (Extend.Stop, (fun
                                                         loc -> 
# 519 "g_constr.mlg"
                                                         false 
                                                                ));
                                                         Extend.Rules 
                                                         (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (Tok.PKEYWORD ("!")))),
                                                         (fun _ loc -> 
                                                         
# 519 "g_constr.mlg"
                                           true 
                                                         ))])),
                             (Extend.Aentryl (operconstr, "200"))),
                (fun c expl iid loc -> 
# 520 "g_constr.mlg"
            (CAst.make ~loc iid), expl, c 
                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("{")))),
                                                                    (Extend.Aentry name)),
                                                                    (Extend.Atoken (Tok.PKEYWORD ("}")))),
                                                       (Extend.Atoken (Tok.PKEYWORD (":")))),
                                          (Extend.Arules [Extend.Rules 
                                                         (Extend.Stop, (fun
                                                         loc -> 
# 517 "g_constr.mlg"
                                                                   false 
                                                                ));
                                                         Extend.Rules 
                                                         (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (Tok.PKEYWORD ("!")))),
                                                         (fun _ loc -> 
                                                         
# 517 "g_constr.mlg"
                                                     true 
                                                         ))])),
                             (Extend.Aentryl (operconstr, "200"))),
                (fun c expl _ _ id _ loc -> 
# 518 "g_constr.mlg"
            id, expl, c 
                                            ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("!")))),
                             (Extend.Aentryl (operconstr, "200"))),
                (fun c _ loc -> 
# 516 "g_constr.mlg"
                                              (CAst.make ~loc Anonymous), true, c 
                                ))])])
        in let () =
        Pcoq.grammar_extend type_cstr None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Aopt (Extend.Arules [Extend.Rules 
                                                          (Extend.NextNoRec 
                                                          (Extend.NextNoRec 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (Tok.PKEYWORD (":")))),
                                                          (Extend.Aentry lconstr)),
                                                          (fun c _ loc -> 
                                                          
# 527 "g_constr.mlg"
                                   c 
                                                          ))]))),
                 (fun c loc -> 
# 527 "g_constr.mlg"
                                              Loc.tag ~loc c 
                               ))])])
        in ()

