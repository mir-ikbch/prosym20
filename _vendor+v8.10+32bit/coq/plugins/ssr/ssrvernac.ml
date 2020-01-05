
# 13 "ssrvernac.mlg"
 

open Names
module CoqConstr = Constr
open CoqConstr
open Termops
open Constrexpr
open Constrexpr_ops
open Pcoq
open Pcoq.Prim
open Pcoq.Constr
open Pvernac.Vernac_
open Ltac_plugin
open Notation_ops
open Notation_term
open Glob_term
open Stdarg
open Decl_kinds
open Pp
open Ppconstr
open Printer
open Util
open Extraargs
open Evar_kinds
open Ssrprinters
open Ssrcommon
open Ssrparser



let __coq_plugin_name = "ssreflect_plugin"
let _ = Mltop.add_known_module __coq_plugin_name

# 45 "ssrvernac.mlg"
 

(* Defining grammar rules with "xx" in it automatically declares keywords too,
 * we thus save the lexer to restore it at the end of the file *)
let frozen_lexer = CLexer.get_keyword_state () ;;

(* global syntactic changes and vernacular commands *)

(** Alternative notations for "match" and anonymous arguments. *)(* ************)

(* Syntax:                                                        *)
(*  if <term> is <pattern> then ... else ...                      *)
(*  if <term> is <pattern> [in ..] return ... then ... else ...   *)
(*  let: <pattern> := <term> in ...                               *)
(*  let: <pattern> [in ...] := <term> return ... in ...           *)
(* The scope of a top-level 'as' in the pattern extends over the  *)
(* 'return' type (dependent if/let).                              *)
(* Note that the optional "in ..." appears next to the <pattern>  *)
(* rather than the <term> in then "let:" syntax. The alternative  *)
(* would lead to ambiguities in, e.g.,                            *)
(* let: p1 := (*v---INNER LET:---v *)                             *)
(*   let: p2 := let: p3 := e3 in k return t in k2 in k1 return t' *)
(* in b       (*^--ALTERNATIVE INNER LET--------^ *)              *)

(* Caveat : There is no pretty-printing support, since this would *)
(* require a modification to the Coq kernel (adding a new match   *)
(* display style -- why aren't these strings?); also, the v8.1    *)
(* pretty-printer only allows extension hooks for printing        *)
(* integer or string literals.                                    *)
(*   Also note that in the v8 grammar "is" needs to be a keyword; *)
(* as this can't be done from an ML extension file, the new       *)
(* syntax will only work when ssreflect.v is imported.            *)

let no_ct = None, None and no_rt = None
let aliasvar = function
  | [[{ CAst.v = CPatAlias (_, na); loc }]] -> Some na
  | _ -> None
let mk_cnotype mp = aliasvar mp, None
let mk_ctype mp t = aliasvar mp, Some t
let mk_rtype t = Some t
let mk_dthen ?loc (mp, ct, rt) c = (CAst.make ?loc (mp, c)), ct, rt
let mk_let ?loc rt ct mp c1 =
  CAst.make ?loc @@ CCases (LetPatternStyle, rt, ct, [CAst.make ?loc (mp, c1)])
let mk_pat c (na, t) = (c, na, t)



let _ = let ssr_rtype = Pcoq.Entry.create "ssr_rtype"
        and ssr_mpat = Pcoq.Entry.create "ssr_mpat"
        and ssr_dpat = Pcoq.Entry.create "ssr_dpat"
        and ssr_dthen = Pcoq.Entry.create "ssr_dthen"
        and ssr_elsepat = Pcoq.Entry.create "ssr_elsepat"
        and ssr_else = Pcoq.Entry.create "ssr_else"
        in
        let () =
        Pcoq.grammar_extend ssr_rtype None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PKEYWORD ("return")))),
                              (Extend.Aentryl (operconstr, "100"))),
                 (fun t _ loc -> 
# 94 "ssrvernac.mlg"
                                                          mk_rtype t 
                                 ))])])
        in let () =
        Pcoq.grammar_extend ssr_mpat None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry pattern)),
                 (fun p loc -> 
# 95 "ssrvernac.mlg"
                                [[p]] 
                               ))])])
        in let () =
        Pcoq.grammar_extend ssr_dpat None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry ssr_mpat)),
                 (fun mp loc -> 
# 99 "ssrvernac.mlg"
                         mp, no_ct, no_rt 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry ssr_mpat)),
                             (Extend.Aentry ssr_rtype)),
                (fun rt mp loc -> 
# 98 "ssrvernac.mlg"
                                         mp, mk_cnotype mp, rt 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Aentry ssr_mpat)),
                                                       (Extend.Atoken (Tok.PKEYWORD ("in")))),
                                          (Extend.Aentry pattern)),
                             (Extend.Aentry ssr_rtype)),
                (fun rt t _ mp loc -> 
# 97 "ssrvernac.mlg"
                                                            mp, mk_ctype mp t, rt 
                                      ))])])
        in let () =
        Pcoq.grammar_extend ssr_dthen None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Aentry ssr_dpat)),
                                           (Extend.Atoken (Tok.PKEYWORD ("then")))),
                              (Extend.Aentry lconstr)),
                 (fun c _ dp loc -> 
# 101 "ssrvernac.mlg"
                                                        mk_dthen ~loc dp c 
                                    ))])])
        in let () =
        Pcoq.grammar_extend ssr_elsepat None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Atoken (Tok.PKEYWORD ("else")))),
                 (fun _ loc -> 
# 102 "ssrvernac.mlg"
                              [[CAst.make ~loc @@ CPatAtom None]] 
                               ))])])
        in let () =
        Pcoq.grammar_extend ssr_else None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry ssr_elsepat)),
                              (Extend.Aentry lconstr)),
                 (fun c mp loc -> 
# 103 "ssrvernac.mlg"
                                                  CAst.make ~loc (mp, c) 
                                  ))])])
        in let () =
        Pcoq.grammar_extend binder_constr None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("let")))),
                                                        (Extend.Atoken (Tok.PKEYWORD (":")))),
                                                        (Extend.Aentry ssr_mpat)),
                                                        (Extend.Atoken (Tok.PKEYWORD ("in")))),
                                                        (Extend.Aentry pattern)),
                                                        (Extend.Atoken (Tok.PKEYWORD (":=")))),
                                                        (Extend.Aentry lconstr)),
                                                        (Extend.Aentry ssr_rtype)),
                                           (Extend.Atoken (Tok.PKEYWORD ("in")))),
                              (Extend.Aentry lconstr)),
                 (fun c1 _ rt c _ t _ mp _ _ loc -> 
# 121 "ssrvernac.mlg"
        mk_let ~loc rt [mk_pat c (mk_ctype mp t)] mp c1 
                                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("let")))),
                                                                    (Extend.Atoken (Tok.PKEYWORD (":")))),
                                                                    (Extend.Aentry ssr_mpat)),
                                                                    (Extend.Atoken (Tok.PKEYWORD (":=")))),
                                                                    (Extend.Aentry lconstr)),
                                                       (Extend.Aentry ssr_rtype)),
                                          (Extend.Atoken (Tok.PKEYWORD ("in")))),
                             (Extend.Aentry lconstr)),
                (fun c1 _ rt c _ mp _ _ loc -> 
# 118 "ssrvernac.mlg"
        mk_let ~loc rt [mk_pat c (mk_cnotype mp)] mp c1 
                                               ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("let")))),
                                                                    (Extend.Atoken (Tok.PKEYWORD (":")))),
                                                                    (Extend.Aentry ssr_mpat)),
                                                                    (Extend.Atoken (Tok.PKEYWORD (":=")))),
                                                       (Extend.Aentry lconstr)),
                                          (Extend.Atoken (Tok.PKEYWORD ("in")))),
                             (Extend.Aentry lconstr)),
                (fun c1 _ c _ mp _ _ loc -> 
# 115 "ssrvernac.mlg"
        mk_let ~loc no_rt [mk_pat c no_ct] mp c1 
                                            ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("if")))),
                                                                    (Extend.Aentryl (operconstr, "200"))),
                                                       (Extend.Atoken (Tok.PKEYWORD ("isn't")))),
                                          (Extend.Aentry ssr_dthen)),
                             (Extend.Aentry ssr_else)),
                (fun b2 db1 _ c _ loc -> 
# 108 "ssrvernac.mlg"
        let b1, ct, rt = db1 in
      let b1, b2 = let open CAst in
        let {loc=l1; v=(p1, r1)}, {loc=l2; v=(p2, r2)} = b1, b2 in
        (make ?loc:l1 (p1, r2), make ?loc:l2 (p2, r1))
      in
      CAst.make ~loc @@ CCases (MatchStyle, rt, [mk_pat c ct], [b1; b2]) 
                                         ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("if")))),
                                                                    (Extend.Aentryl (operconstr, "200"))),
                                                       (Extend.Atoken (Tok.PKEYWORD ("is")))),
                                          (Extend.Aentry ssr_dthen)),
                             (Extend.Aentry ssr_else)),
                (fun b2 db1 _ c _ loc -> 
# 106 "ssrvernac.mlg"
        let b1, ct, rt = db1 in CAst.make ~loc @@ CCases (MatchStyle, rt, [mk_pat c ct], [b1; b2]) 
                                         ))])])
        in ()

let _ = let () =
        Pcoq.grammar_extend closed_binder None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Arules [Extend.Rules 
                                                          (Extend.NextNoRec 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (Tok.PKEYWORD ("&")))),
                                                          (fun _ loc -> 
                                                          
# 128 "ssrvernac.mlg"
                                 () 
                                                          ));
                                                          Extend.Rules 
                                                          (Extend.NextNoRec 
                                                          (Extend.Stop,
                                                          (Extend.Atoken (Tok.PKEYWORD ("of")))),
                                                          (fun _ loc -> 
                                                          
# 128 "ssrvernac.mlg"
                 () 
                                                          ))])),
                              (Extend.Aentryl (operconstr, "99"))),
                 (fun c _ loc -> 
# 129 "ssrvernac.mlg"
        [CLocalAssum ([CAst.make ~loc Anonymous], Default Explicit, c)] 
                                 ))])])
        in ()


# 147 "ssrvernac.mlg"
 

let declare_one_prenex_implicit locality f =
  let fref =
    try Smartlocate.global_with_alias f
    with _ -> errorstrm (pr_qualid f ++ str " is not declared") in
  let rec loop = function
  | a :: args' when Impargs.is_status_implicit a ->
    Impargs.MaximallyImplicit :: loop args'
  | args' when List.exists Impargs.is_status_implicit args' ->
      errorstrm (str "Expected prenex implicits for " ++ pr_qualid f)
  | _ -> [] in
  let impls =
    match Impargs.implicits_of_global fref  with
    | [cond,impls] -> impls
    | [] -> errorstrm (str "Expected some implicits for " ++ pr_qualid f)
    | _ -> errorstrm (str "Multiple implicits not supported") in
  match loop impls  with
  | [] ->
    errorstrm (str "Expected some implicits for " ++ pr_qualid f)
  | impls ->
    Impargs.set_implicits locality fref [impls]



let () = Vernacextend.vernac_extend ~command:"Ssrpreneximplicits" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Prenex", 
                                     Vernacextend.TyTerminal ("Implicits", 
                                     Vernacextend.TyNonTerminal (Extend.TUlist1 (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_global)), 
                                     Vernacextend.TyNil))), (let coqpp_body fl
                                                            locality ~st = 
                                                            let () = 
                                                            
# 174 "ssrvernac.mlg"
      
         let locality = Locality.make_section_locality locality in
         List.iter (declare_one_prenex_implicit locality) fl;
     
                                                             in st in fun fl
                                                            ~atts ~st
                                                            -> coqpp_body fl
                                                            (Attributes.parse Attributes.locality atts) ~st), None))]

let _ = let () =
        Pcoq.grammar_extend gallina_ext None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Atoken (Tok.PIDENT (Some
                                                        ("Import"))))),
                                           (Extend.Atoken (Tok.PIDENT (Some
                                           ("Prenex"))))),
                              (Extend.Atoken (Tok.PIDENT (Some
                              ("Implicits"))))),
                 (fun _ _ _ loc -> 
# 186 "ssrvernac.mlg"
        Vernacexpr.VernacSetOption (false, ["Printing"; "Implicit"; "Defensive"], Vernacexpr.OptionUnset) 
                                   ))])])
        in ()


# 195 "ssrvernac.mlg"
 

type raw_glob_search_about_item =
  | RGlobSearchSubPattern of constr_expr
  | RGlobSearchString of Loc.t * string * string option

let pr_search_item env sigma = function
  | RGlobSearchString (_,s,_) -> str s
  | RGlobSearchSubPattern p -> pr_constr_expr env sigma p

let wit_ssr_searchitem = add_genarg "ssr_searchitem" pr_search_item

let pr_ssr_search_item env sigma _ _ _ = pr_search_item env sigma

(* Workaround the notation API that can only print notations *)

let is_ident s = try CLexer.check_ident s; true with _ -> false

let is_ident_part s = is_ident ("H" ^ s)

let interp_search_notation ?loc tag okey =
  let err msg = CErrors.user_err ?loc ~hdr:"interp_search_notation" msg in
  let mk_pntn s for_key =
    let n = String.length s in
    let s' = Bytes.make (n + 2) ' ' in
    let rec loop i i' =
      if i >= n then s', i' - 2 else if s.[i] = ' ' then loop (i + 1) i' else
      let j = try String.index_from s (i + 1) ' ' with _ -> n in
      let m = j - i in
      if s.[i] = '\'' && i < j - 2 && s.[j - 1] = '\'' then
        (String.blit s (i + 1) s' i' (m - 2); loop (j + 1) (i' + m - 1))
      else if for_key && is_ident (String.sub s i m) then
         (Bytes.set s' i' '_'; loop (j + 1) (i' + 2))
      else (String.blit s i s' i' m; loop (j + 1) (i' + m + 1)) in
    loop 0 1 in
  let trim_ntn (pntn, m) = (InConstrEntrySomeLevel,Bytes.sub_string pntn 1 (max 0 m)) in
  let pr_ntn ntn = str "(" ++ Notation.pr_notation ntn ++ str ")" in
  let pr_and_list pr = function
    | [x] -> pr x
    | x :: lx -> pr_list pr_comma pr lx ++ pr_comma () ++ str "and " ++ pr x
    | [] -> mt () in
  let pr_sc sc = str (if sc = "" then "independently" else sc) in
  let pr_scs = function
    | [""] -> pr_sc ""
    | scs -> str "in " ++ pr_and_list pr_sc scs in
  let generator, pr_tag_sc =
    let ign _ = mt () in match okey with
  | Some key ->
    let sc = Notation.find_delimiters_scope ?loc key in
    let pr_sc s_in = str s_in ++ spc() ++ str sc ++ pr_comma() in
    Notation.pr_scope ign sc, pr_sc
  | None -> Notation.pr_scopes ign, ign in
  let qtag s_in = pr_tag_sc s_in ++ qstring tag ++ spc()in
  let ptag, ttag =
    let ptag, m = mk_pntn tag false in
    if m <= 0 then err (str "empty notation fragment");
    ptag, trim_ntn (ptag, m) in
  let last = ref "" and last_sc = ref "" in
  let scs = ref [] and ntns = ref [] in
  let push_sc sc = match !scs with
  | "" :: scs' ->  scs := "" :: sc :: scs'
  | scs' -> scs := sc :: scs' in
  let get s _ _ = match !last with
  | "Scope " -> last_sc := s; last := ""
  | "Lonely notation" -> last_sc := ""; last := ""
  | "\"" ->
      let pntn, m = mk_pntn s true in
      if String.string_contains ~where:(Bytes.to_string pntn) ~what:(Bytes.to_string ptag) then begin
        let ntn = trim_ntn (pntn, m) in
        match !ntns with
        | [] -> ntns := [ntn]; scs := [!last_sc]
        | ntn' :: _ when ntn' = ntn -> push_sc !last_sc
        | _ when ntn = ttag -> ntns := ntn :: !ntns; scs := [!last_sc]
        | _ :: ntns' when List.mem ntn ntns' -> ()
        | ntn' :: ntns' -> ntns := ntn' :: ntn :: ntns'
      end;
      last := ""
  | _ -> last := s in
  pp_with (Format.make_formatter get (fun _ -> ())) generator;
  let ntn = match !ntns with
  | [] ->
    err (hov 0 (qtag "in" ++ str "does not occur in any notation"))
  | ntn :: ntns' when ntn = ttag ->
    if ntns' <> [] then begin
      let pr_ntns' = pr_and_list pr_ntn ntns' in
      Feedback.msg_warning (hov 4 (qtag "In" ++ str "also occurs in " ++ pr_ntns'))
    end; ntn
  | [ntn] ->
    Feedback.msg_notice (hov 4 (qtag "In" ++ str "is part of notation " ++ pr_ntn ntn)); ntn
  | ntns' ->
    let e = str "occurs in" ++ spc() ++ pr_and_list pr_ntn ntns' in
    err (hov 4 (str "ambiguous: " ++ qtag "in" ++ e)) in
  let (nvars, body), ((_, pat), osc) = match !scs with
  | [sc] -> Notation.interp_notation ?loc ntn (None, [sc])
  | scs' ->
    try Notation.interp_notation ?loc ntn (None, []) with _ ->
    let e = pr_ntn ntn ++ spc() ++ str "is defined " ++ pr_scs scs' in
    err (hov 4 (str "ambiguous: " ++ pr_tag_sc "in" ++ e)) in
  let sc = Option.default "" osc in
  let _ =
    let m_sc =
      if osc <> None then str "In " ++ str sc ++ pr_comma() else mt() in
    let ntn_pat = trim_ntn (mk_pntn pat false) in
    let rbody = glob_constr_of_notation_constr ?loc body in
    let m_body = hov 0 (Constrextern.without_symbols prl_glob_constr rbody) in
    let m = m_sc ++ pr_ntn ntn_pat ++ spc () ++ str "denotes " ++ m_body in
    Feedback.msg_notice (hov 0 m) in
  if List.length !scs > 1 then
    let scs' = List.remove (=) sc !scs in
    let w = pr_ntn ntn ++ str " is also defined " ++ pr_scs scs' in
    Feedback.msg_warning (hov 4 w)
  else if String.string_contains ~where:(snd ntn) ~what:" .. " then
    err (pr_ntn ntn ++ str " is an n-ary notation");
  let nvars = List.filter (fun (_,(_,typ)) -> typ = NtnTypeConstr) nvars in
  let rec sub () = function
  | NVar x when List.mem_assoc x nvars -> DAst.make ?loc @@ GPatVar (FirstOrderPatVar x)
  | c ->
    glob_constr_of_notation_constr_with_binders ?loc (fun _ x -> (), None, x) sub () c in
  let _, npat = Patternops.pattern_of_glob_constr (sub () body) in
  Search.GlobSearchSubPattern npat



let (wit_ssr_search_item, ssr_search_item) = Tacentries.argument_extend ~name:"ssr_search_item" 
                                             {
                                             Tacentries.arg_parsing = 
                                             Vernacextend.Arg_rules (
                                             [(Extend.Rule
                                               (Extend.Next (Extend.Stop,
                                                            (Extend.Aentry constr_pattern)),
                                               (fun p loc -> 
# 322 "ssrvernac.mlg"
                                RGlobSearchSubPattern p  
                                                             )));
                                             (Extend.Rule
                                              (Extend.Next (Extend.Next 
                                                           (Extend.Next 
                                                           (Extend.Stop,
                                                           (Extend.Aentry string)),
                                                           (Extend.Atoken (CLexer.terminal "%"))),
                                                           (Extend.Aentry preident)),
                                              (fun key _ s loc -> 
# 321 "ssrvernac.mlg"
                                          RGlobSearchString (loc,s,Some key)  
                                                                  )));
                                             (Extend.Rule
                                              (Extend.Next (Extend.Stop,
                                                           (Extend.Aentry string)),
                                              (fun s loc -> 
# 320 "ssrvernac.mlg"
                        RGlobSearchString (loc,s,None)  
                                                            )))]);
                                             Tacentries.arg_tag = Some
                                                                  (Geninterp.val_tag (Genarg.topwit wit_ssr_searchitem));
                                             Tacentries.arg_intern = 
                                             Tacentries.ArgInternWit (wit_ssr_searchitem);
                                             Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ssr_searchitem);
                                             Tacentries.arg_interp = 
                                             Tacentries.ArgInterpWit (wit_ssr_searchitem);
                                             Tacentries.arg_printer = 
                                             ((fun env sigma -> 
# 319 "ssrvernac.mlg"
               pr_ssr_search_item env sigma 
                                             ), (fun env sigma -> 
# 319 "ssrvernac.mlg"
               pr_ssr_search_item env sigma 
                                             ), (fun env sigma -> 
# 319 "ssrvernac.mlg"
               pr_ssr_search_item env sigma 
                                             ));
                                             }
let _ = (wit_ssr_search_item, ssr_search_item)


# 325 "ssrvernac.mlg"
 

let pr_ssr_search_arg env sigma _ _ _ =
  let pr_item (b, p) = str (if b then "-" else "") ++ pr_search_item env sigma p in
  pr_list spc pr_item



let (wit_ssr_search_arg, ssr_search_arg) = Tacentries.argument_extend ~name:"ssr_search_arg" 
                                           {
                                           Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                                    [(
                                                                    Extend.Rule
                                                                    (
                                                                    Extend.Stop,
                                                                    (fun
                                                                    loc -> 
                                                                    
# 337 "ssrvernac.mlg"
              []  
                                                                    )));
                                                                    (
                                                                    Extend.Rule
                                                                    (
                                                                    Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Aentry ssr_search_item)),
                                                                    Extend.Aself),
                                                                    (fun a p
                                                                    loc -> 
                                                                    
# 336 "ssrvernac.mlg"
                                                   (true, p) :: a  
                                                                    )));
                                                                    (
                                                                    Extend.Rule
                                                                    (
                                                                    Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (CLexer.terminal "-"))),
                                                                    (Extend.Aentry ssr_search_item)),
                                                                    Extend.Aself),
                                                                    (fun a p
                                                                    _ loc ->
                                                                    
# 335 "ssrvernac.mlg"
                                                       (false, p) :: a  
                                                                    )))]);
                                           Tacentries.arg_tag = Some
                                                                (Geninterp.Val.List 
                                                                (Geninterp.Val.Pair (
                                                                (Geninterp.val_tag (Genarg.topwit wit_bool)), 
                                                                (Geninterp.val_tag (Genarg.topwit wit_ssr_searchitem)))));
                                           Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.ListArg 
                                                                   (Genarg.PairArg (
                                                                   (wit_bool), 
                                                                   (wit_ssr_searchitem))));
                                           Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.ListArg 
                                                                  (Genarg.PairArg (
                                                                  (wit_bool), 
                                                                  (wit_ssr_searchitem))));
                                           Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.ListArg 
                                                                   (Genarg.PairArg (
                                                                   (wit_bool), 
                                                                   (wit_ssr_searchitem))));
                                           Tacentries.arg_printer = ((fun env sigma -> 
                                                                    
# 334 "ssrvernac.mlg"
               pr_ssr_search_arg env sigma 
                                                                    ), (fun env sigma -> 
                                                                    
# 334 "ssrvernac.mlg"
               pr_ssr_search_arg env sigma 
                                                                    ), (fun env sigma -> 
                                                                    
# 334 "ssrvernac.mlg"
               pr_ssr_search_arg env sigma 
                                                                    ));
                                           }
let _ = (wit_ssr_search_arg, ssr_search_arg)


# 340 "ssrvernac.mlg"
 

(* Main type conclusion pattern filter *)

let rec splay_search_pattern na = function
  | Pattern.PApp (fp, args) -> splay_search_pattern (na + Array.length args) fp
  | Pattern.PLetIn (_, _, _, bp) -> splay_search_pattern na bp
  | Pattern.PRef hr -> hr, na
  | _ -> CErrors.user_err (Pp.str "no head constant in head search pattern")

let push_rels_assum l e =
  let l = List.map (fun (n,t) -> n, EConstr.Unsafe.to_constr t) l in
  push_rels_assum l e

let coerce_search_pattern_to_sort hpat =
  let env = Global.env () in
  let sigma = Evd.(from_env env) in
  let mkPApp fp n_imps args =
    let args' = Array.append (Array.make n_imps (Pattern.PMeta None)) args in
    Pattern.PApp (fp, args') in
  let hr, na = splay_search_pattern 0 hpat in
  let dc, ht =
    let hr, _ = Typeops.type_of_global_in_context env hr (* FIXME *) in
    Reductionops.splay_prod env sigma (EConstr.of_constr hr) in
  let np = List.length dc in
  if np < na then CErrors.user_err (Pp.str "too many arguments in head search pattern") else
  let hpat' = if np = na then hpat else mkPApp hpat (np - na) [||] in
  let warn () =
    Feedback.msg_warning (str "Listing only lemmas with conclusion matching " ++
      pr_constr_pattern_env env sigma hpat') in
  if EConstr.isSort sigma ht then begin warn (); true, hpat' end else
  let filter_head, coe_path =
    try
      let _, cp =
        Classops.lookup_path_to_sort_from (push_rels_assum dc env) sigma ht in
      warn ();
      true, cp
    with _ -> false, [] in
  let coerce hp coe_index =
    let coe_ref = coe_index.Classops.coe_value in
    try
      let n_imps = Option.get (Classops.hide_coercion coe_ref) in
      mkPApp (Pattern.PRef coe_ref) n_imps [|hp|]
    with Not_found | Option.IsNone ->
    errorstrm (str "need explicit coercion " ++ pr_global coe_ref ++ spc ()
            ++ str "to interpret head search pattern as type") in
  filter_head, List.fold_left coerce hpat' coe_path

let interp_head_pat hpat =
  let filter_head, p = coerce_search_pattern_to_sort hpat in
  let rec loop c = match CoqConstr.kind c with
  | Cast (c', _, _) -> loop c'
  | Prod (_, _, c') -> loop c'
  | LetIn (_, _, _, c') -> loop c'
  | _ ->
    let env = Global.env () in
    let sigma = Evd.from_env env in
    Constr_matching.is_matching env sigma p (EConstr.of_constr c) in
  filter_head, loop

let all_true _ = true

let rec interp_search_about args accu = match args with
| [] -> accu
| (flag, arg) :: rem ->
  fun gr env typ ->
    let ans = Search.search_about_filter arg gr env typ in
    (if flag then ans else not ans) && interp_search_about rem accu gr env typ

let interp_search_arg arg =
  let arg = List.map (fun (x,arg) -> x, match arg with
  | RGlobSearchString (loc,s,key) ->
      if is_ident_part s then Search.GlobSearchString s else
      interp_search_notation ~loc s key
  | RGlobSearchSubPattern p ->
      try
        let env = Global.env () in
        let _, p = Constrintern.intern_constr_pattern env (Evd.from_env env) p in
        Search.GlobSearchSubPattern p
      with e -> let e = CErrors.push e in iraise (ExplainErr.process_vernac_interp_error e)) arg in
  let hpat, a1 = match arg with
  | (_, Search.GlobSearchSubPattern (Pattern.PMeta _)) :: a' -> all_true, a'
  | (true, Search.GlobSearchSubPattern p) :: a' ->
     let filter_head, p = interp_head_pat p in
     if filter_head then p, a' else all_true, arg
  | _ -> all_true, arg in
  let is_string =
    function (_, Search.GlobSearchString _) -> true | _ -> false in
  let a2, a3 = List.partition is_string a1 in
  interp_search_about (a2 @ a3) (fun gr env typ -> hpat typ)

(* Module path postfilter *)

let pr_modloc (b, m) = if b then str "-" ++ pr_qualid m else pr_qualid m

let wit_ssrmodloc = add_genarg "ssrmodloc" (fun env sigma -> pr_modloc)

let pr_ssr_modlocs _ _ _ ml =
  if ml = [] then str "" else spc () ++ str "in " ++ pr_list spc pr_modloc ml



let (wit_ssr_modlocs, ssr_modlocs) = Tacentries.argument_extend ~name:"ssr_modlocs" 
                                     {
                                     Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                              [(Extend.Rule
                                                                (Extend.Stop,
                                                                (fun loc -> 
# 443 "ssrvernac.mlg"
              []  
                                                                    )))]);
                                     Tacentries.arg_tag = Some
                                                          (Geninterp.Val.List 
                                                          (Geninterp.val_tag (Genarg.topwit wit_ssrmodloc)));
                                     Tacentries.arg_intern = Tacentries.ArgInternWit (Genarg.ListArg 
                                                             (wit_ssrmodloc));
                                     Tacentries.arg_subst = Tacentries.ArgSubstWit (Genarg.ListArg 
                                                            (wit_ssrmodloc));
                                     Tacentries.arg_interp = Tacentries.ArgInterpWit (Genarg.ListArg 
                                                             (wit_ssrmodloc));
                                     Tacentries.arg_printer = ((fun env sigma -> 
                                                              
# 442 "ssrvernac.mlg"
                                                                 pr_ssr_modlocs 
                                                              ), (fun env sigma -> 
                                                              
# 442 "ssrvernac.mlg"
                                                                 pr_ssr_modlocs 
                                                              ), (fun env sigma -> 
                                                              
# 442 "ssrvernac.mlg"
                                                                 pr_ssr_modlocs 
                                                              ));
                                     }
let _ = (wit_ssr_modlocs, ssr_modlocs)

let _ = let modloc = Pcoq.Entry.create "modloc"
        in
        let () =
        Pcoq.grammar_extend modloc None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry global)),
                 (fun m loc -> 
# 448 "ssrvernac.mlg"
                                                              false, m 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("-")))),
                             (Extend.Aentry global)),
                (fun m _ loc -> 
# 448 "ssrvernac.mlg"
                                  true, m 
                                ))])])
        in let () =
        Pcoq.grammar_extend ssr_modlocs None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PKEYWORD ("in")))),
                              (Extend.Alist1 (Extend.Aentry modloc))),
                 (fun ml _ loc -> 
# 449 "ssrvernac.mlg"
                                               ml 
                                  ))])])
        in ()


# 452 "ssrvernac.mlg"
 

let interp_modloc mr =
  let interp_mod (_, qid) =
    try Nametab.full_name_module qid with Not_found ->
    CErrors.user_err ?loc:qid.CAst.loc (str "No Module " ++ pr_qualid qid) in
  let mr_out, mr_in = List.partition fst mr in
  let interp_bmod b = function
  | [] -> fun _ _ _ -> true
  | rmods -> Search.module_filter (List.map interp_mod rmods, b) in
  let is_in = interp_bmod false mr_in and is_out = interp_bmod true mr_out in
  fun gr env typ -> is_in gr env typ && is_out gr env typ

(* The unified, extended vernacular "Search" command *)

let ssrdisplaysearch gr env t =
  let pr_res = pr_global gr ++ str ":" ++ spc () ++ pr_lconstr_env env Evd.empty t in
  Feedback.msg_notice (hov 2 pr_res ++ fnl ())



let () = Vernacextend.vernac_extend ~command:"SsrSearchPattern" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Search", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_ssr_search_arg), 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_ssr_modlocs), 
                                     Vernacextend.TyNil))), (let coqpp_body a
                                                            mr
                                                            () ~st = 
                                                            let () = 
                                                            
# 475 "ssrvernac.mlg"
    let hpat = interp_search_arg a in
    let in_mod = interp_modloc mr in
    let post_filter gr env typ = in_mod gr env typ && hpat gr env typ in
    let display gr env typ =
      if post_filter gr env typ then ssrdisplaysearch gr env typ
    in
    Search.generic_search None display 
                                                             in st in fun a
                                                            mr ~atts ~st
                                                            -> coqpp_body a
                                                            mr
                                                            (Attributes.unsupported_attributes atts) ~st), None))]


# 492 "ssrvernac.mlg"
 

let pr_raw_ssrhintref env sigma prc _ _ = let open CAst in function
  | { v = CAppExpl ((None, r,x), args) } when isCHoles args ->
    prc env sigma (CAst.make @@ CRef (r,x)) ++ str "|" ++ int (List.length args)
  | { v = CApp ((_, { v = CRef _ }), _) } as c -> prc env sigma c
  | { v = CApp ((_, c), args) } when isCxHoles args ->
    prc env sigma c ++ str "|" ++ int (List.length args)
  | c -> prc env sigma c

let pr_rawhintref env sigma c =
  match DAst.get c with
  | GApp (f, args) when isRHoles args ->
    pr_glob_constr_env env f ++ str "|" ++ int (List.length args)
  | _ -> pr_glob_constr_env env c

let pr_glob_ssrhintref env sigma _ _ _ (c, _) = pr_rawhintref env sigma c

let pr_ssrhintref env sigma prc _ _ = prc env sigma

let mkhintref ?loc c n = match c.CAst.v with
  | CRef (r,x) -> CAst.make ?loc @@ CAppExpl ((None, r, x), mkCHoles ?loc n)
  | _ -> mkAppC (c, mkCHoles ?loc n)



let (wit_ssrhintref, ssrhintref) = Tacentries.argument_extend ~name:"ssrhintref" 
                                   {
                                   Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                            [(Extend.Rule
                                                              (Extend.Next 
                                                               (Extend.Next 
                                                               (Extend.Next 
                                                               (Extend.Stop,
                                                               (Extend.Aentry constr)),
                                                               (Extend.Atoken (CLexer.terminal "|"))),
                                                               (Extend.Aentry natural)),
                                                              (fun n _ c
                                                              loc -> 
                                                              
# 524 "ssrvernac.mlg"
                                       mkhintref ~loc c n  
                                                              )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Aentry constr)),
                                                             (fun c loc -> 
# 523 "ssrvernac.mlg"
                        c  
                                                                    )))]);
                                   Tacentries.arg_tag = Some
                                                        (Geninterp.val_tag (Genarg.topwit wit_constr));
                                   Tacentries.arg_intern = Tacentries.ArgInternWit (wit_constr);
                                   Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_constr);
                                   Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_constr);
                                   Tacentries.arg_printer = ((fun env sigma -> 
                                                            
# 521 "ssrvernac.mlg"
                   pr_raw_ssrhintref env sigma 
                                                            ), (fun env sigma -> 
                                                            
# 522 "ssrvernac.mlg"
                    pr_glob_ssrhintref env sigma 
                                                            ), (fun env sigma -> 
                                                            
# 520 "ssrvernac.mlg"
               pr_ssrhintref env sigma 
                                                            ));
                                   }
let _ = (wit_ssrhintref, ssrhintref)


# 527 "ssrvernac.mlg"
 

(* View purpose *)

let pr_viewpos = function
  | Some Ssrview.AdaptorDb.Forward -> str " for move/"
  | Some Ssrview.AdaptorDb.Backward -> str " for apply/"
  | Some Ssrview.AdaptorDb.Equivalence -> str " for apply//"
  | None -> mt ()

let pr_ssrviewpos _ _ _ = pr_viewpos



let (wit_ssrviewpos, ssrviewpos) = Tacentries.argument_extend ~name:"ssrviewpos" 
                                   {
                                   Tacentries.arg_parsing = Vernacextend.Arg_rules (
                                                            [(Extend.Rule
                                                              (Extend.Stop,
                                                              (fun loc -> 
# 546 "ssrvernac.mlg"
              None  
                                                                    )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Atoken (CLexer.terminal "for"))),
                                                              (Extend.Atoken (CLexer.terminal "apply"))),
                                                              (Extend.Atoken (CLexer.terminal "//"))),
                                                             (fun _ _ _
                                                             loc -> 
# 545 "ssrvernac.mlg"
                                 Some Ssrview.AdaptorDb.Equivalence  
                                                                    )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Atoken (CLexer.terminal "for"))),
                                                              (Extend.Atoken (CLexer.terminal "apply"))),
                                                              (Extend.Atoken (CLexer.terminal "/"))),
                                                              (Extend.Atoken (CLexer.terminal "/"))),
                                                             (fun _ _ _ _
                                                             loc -> 
# 544 "ssrvernac.mlg"
                                    Some Ssrview.AdaptorDb.Equivalence  
                                                                    )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Atoken (CLexer.terminal "for"))),
                                                              (Extend.Atoken (CLexer.terminal "apply"))),
                                                              (Extend.Atoken (CLexer.terminal "/"))),
                                                             (fun _ _ _
                                                             loc -> 
# 543 "ssrvernac.mlg"
                                Some Ssrview.AdaptorDb.Backward  
                                                                    )));
                                                            (Extend.Rule
                                                             (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Next 
                                                              (Extend.Stop,
                                                              (Extend.Atoken (CLexer.terminal "for"))),
                                                              (Extend.Atoken (CLexer.terminal "move"))),
                                                              (Extend.Atoken (CLexer.terminal "/"))),
                                                             (fun _ _ _
                                                             loc -> 
# 542 "ssrvernac.mlg"
                               Some Ssrview.AdaptorDb.Forward  
                                                                    )))]);
                                   Tacentries.arg_tag = None;
                                   Tacentries.arg_intern = Tacentries.ArgInternFun (fun ist v -> (ist, v));
                                   Tacentries.arg_subst = Tacentries.ArgSubstFun (fun s v -> v);
                                   Tacentries.arg_interp = Tacentries.ArgInterpRet;
                                   Tacentries.arg_printer = ((fun env sigma -> 
                                                            
# 541 "ssrvernac.mlg"
                                        pr_ssrviewpos 
                                                            ), (fun env sigma -> 
                                                            
# 541 "ssrvernac.mlg"
                                        pr_ssrviewpos 
                                                            ), (fun env sigma -> 
                                                            
# 541 "ssrvernac.mlg"
                                        pr_ssrviewpos 
                                                            ));
                                   }
let _ = (wit_ssrviewpos, ssrviewpos)


# 549 "ssrvernac.mlg"
 

let pr_ssrviewposspc _ _ _ i = pr_viewpos i ++ spc ()



let (wit_ssrviewposspc, ssrviewposspc) = Tacentries.argument_extend ~name:"ssrviewposspc" 
                                         {
                                         Tacentries.arg_parsing = Vernacextend.Arg_alias (ssrviewpos);
                                         Tacentries.arg_tag = Some
                                                              (Geninterp.val_tag (Genarg.topwit wit_ssrviewpos));
                                         Tacentries.arg_intern = Tacentries.ArgInternWit (wit_ssrviewpos);
                                         Tacentries.arg_subst = Tacentries.ArgSubstWit (wit_ssrviewpos);
                                         Tacentries.arg_interp = Tacentries.ArgInterpWit (wit_ssrviewpos);
                                         Tacentries.arg_printer = ((fun env sigma -> 
                                                                  
# 555 "ssrvernac.mlg"
                                                               pr_ssrviewposspc 
                                                                  ), (fun env sigma -> 
                                                                  
# 555 "ssrvernac.mlg"
                                                               pr_ssrviewposspc 
                                                                  ), (fun env sigma -> 
                                                                  
# 555 "ssrvernac.mlg"
                                                               pr_ssrviewposspc 
                                                                  ));
                                         }
let _ = (wit_ssrviewposspc, ssrviewposspc)


# 559 "ssrvernac.mlg"
 

let print_view_hints env sigma kind l =
  let pp_viewname = str "Hint View" ++ pr_viewpos (Some kind) ++ str " " in
  let pp_hints = pr_list spc (pr_rawhintref env sigma) l in
  Feedback.msg_notice  (pp_viewname ++ hov 0 pp_hints ++ Pp.cut ())



let () = Vernacextend.vernac_extend ~command:"PrintView" ~classifier:(fun _ -> Vernacextend.classify_as_query) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Print", 
                                     Vernacextend.TyTerminal ("Hint", 
                                     Vernacextend.TyTerminal ("View", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_ssrviewpos), 
                                     Vernacextend.TyNil)))), (let coqpp_body i
                                                             () ~st = 
                                                             let () = 
                                                             
# 570 "ssrvernac.mlg"
   
    let env = Global.env () in
    let sigma = Evd.from_env env in
    (match i with
    | Some k ->
      print_view_hints env sigma k (Ssrview.AdaptorDb.get k)
    | None ->
        List.iter (fun k -> print_view_hints env sigma k (Ssrview.AdaptorDb.get k))
          [ Ssrview.AdaptorDb.Forward;
            Ssrview.AdaptorDb.Backward;
            Ssrview.AdaptorDb.Equivalence ])
  
                                                              in st in fun i
                                                             ~atts ~st
                                                             -> coqpp_body i
                                                             (Attributes.unsupported_attributes atts) ~st), None))]


# 584 "ssrvernac.mlg"
 

let glob_view_hints lvh =
  List.map (Constrintern.intern_constr (Global.env ()) (Evd.from_env (Global.env ()))) lvh



let () = Vernacextend.vernac_extend ~command:"HintView" ~classifier:(fun _ -> Vernacextend.classify_as_sideeff) ?entry:None 
         [(Vernacextend.TyML (false, Vernacextend.TyTerminal ("Hint", 
                                     Vernacextend.TyTerminal ("View", 
                                     Vernacextend.TyNonTerminal (Extend.TUentry (Genarg.get_arg_tag wit_ssrviewposspc), 
                                     Vernacextend.TyNonTerminal (Extend.TUlist1 (
                                                                 Extend.TUentry (Genarg.get_arg_tag wit_ssrhintref)), 
                                     Vernacextend.TyNil)))), (let coqpp_body n
                                                             lvh
                                                             () ~st = 
                                                             let () = 
                                                             
# 593 "ssrvernac.mlg"
       let hints = glob_view_hints lvh in
       match n with
       | None ->
          Ssrview.AdaptorDb.declare Ssrview.AdaptorDb.Forward hints;
          Ssrview.AdaptorDb.declare Ssrview.AdaptorDb.Backward hints
       | Some k ->
          Ssrview.AdaptorDb.declare k hints 
                                                              in st in fun n
                                                             lvh ~atts ~st
                                                             -> coqpp_body n
                                                             lvh
                                                             (Attributes.unsupported_attributes atts) ~st), None))]


# 616 "ssrvernac.mlg"
 

open Pltac



let _ = let () =
        Pcoq.grammar_extend hypident None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                        (Extend.Atoken (Tok.PIDENT (Some
                                                        ("value"))))),
                                                        (Extend.Atoken (Tok.PKEYWORD ("of")))),
                                           (Extend.Aentry Prim.identref)),
                              (Extend.Atoken (Tok.PKEYWORD (")")))),
                 (fun _ id _ _ _ loc -> 
# 626 "ssrvernac.mlg"
                                                           id, Locus.InHypValueOnly 
                                        ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("type"))))),
                                                       (Extend.Atoken (Tok.PKEYWORD ("of")))),
                                          (Extend.Aentry Prim.identref)),
                             (Extend.Atoken (Tok.PKEYWORD (")")))),
                (fun _ id _ _ _ loc -> 
# 625 "ssrvernac.mlg"
                                                          id, Locus.InHypTypeOnly 
                                       ))])])
        in ()

let _ = let () =
        Pcoq.grammar_extend hloc None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("in")))),
                                                        (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                        (Extend.Atoken (Tok.PIDENT (Some
                                                        ("Value"))))),
                                                        (Extend.Atoken (Tok.PKEYWORD ("of")))),
                                           (Extend.Aentry ident)),
                              (Extend.Atoken (Tok.PKEYWORD (")")))),
                 (fun _ id _ _ _ _ loc -> 
# 636 "ssrvernac.mlg"
      Tacexpr.HypLocation (CAst.make id, Locus.InHypValueOnly) 
                                          ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("in")))),
                                                                    (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                                    (Extend.Atoken (Tok.PKEYWORD ("Type")))),
                                                       (Extend.Atoken (Tok.PKEYWORD ("of")))),
                                          (Extend.Aentry ident)),
                             (Extend.Atoken (Tok.PKEYWORD (")")))),
                (fun _ id _ _ _ _ loc -> 
# 634 "ssrvernac.mlg"
      Tacexpr.HypLocation (CAst.make id, Locus.InHypTypeOnly) 
                                         ))])])
        in ()

let _ = let () =
        Pcoq.grammar_extend constr_eval None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Atoken (Tok.PIDENT (Some
                                                        ("type"))))),
                                           (Extend.Atoken (Tok.PKEYWORD ("of")))),
                              (Extend.Aentry Constr.constr)),
                 (fun c _ _ loc -> 
# 643 "ssrvernac.mlg"
                                                 Genredexpr.ConstrTypeOf c 
                                   ))])])
        in ()


# 651 "ssrvernac.mlg"
 

let () = CLexer.set_keyword_state frozen_lexer ;;



