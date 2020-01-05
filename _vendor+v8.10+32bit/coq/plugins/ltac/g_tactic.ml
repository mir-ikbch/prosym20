
# 11 "g_tactic.mlg"
 

open Pp
open CErrors
open Util
open Names
open Namegen
open Tacexpr
open Genredexpr
open Constrexpr
open Libnames
open Tok
open Tactypes
open Tactics
open Inv
open Locus
open Decl_kinds

open Pcoq


let all_with delta = Redops.make_red_flag [FBeta;FMatch;FFix;FCofix;FZeta;delta]

let tactic_kw = [ "->"; "<-" ; "by" ]
let _ = List.iter CLexer.add_keyword tactic_kw

let err () = raise Stream.Failure

(* Hack to parse "(x:=t)" as an explicit argument without conflicts with the *)
(* admissible notation "(x t)" *)
let test_lpar_id_coloneq =
  Pcoq.Entry.of_parser "lpar_id_coloneq"
    (fun strm ->
      match stream_nth 0 strm with
        | KEYWORD "(" ->
            (match stream_nth 1 strm with
              | IDENT _ ->
                  (match stream_nth 2 strm with
                    | KEYWORD ":=" -> ()
                    | _ -> err ())
              | _ -> err ())
        | _ -> err ())

(* Hack to recognize "(x)" *)
let test_lpar_id_rpar =
  Pcoq.Entry.of_parser "lpar_id_coloneq"
    (fun strm ->
      match stream_nth 0 strm with
        | KEYWORD "(" ->
            (match stream_nth 1 strm with
              | IDENT _ ->
                  (match stream_nth 2 strm with
                    | KEYWORD ")" -> ()
                    | _ -> err ())
              | _ -> err ())
        | _ -> err ())

(* idem for (x:=t) and (1:=t) *)
let test_lpar_idnum_coloneq =
  Pcoq.Entry.of_parser "test_lpar_idnum_coloneq"
    (fun strm ->
      match stream_nth 0 strm with
        | KEYWORD "(" ->
            (match stream_nth 1 strm with
              | IDENT _ | NUMERAL _ ->
                  (match stream_nth 2 strm with
                    | KEYWORD ":=" -> ()
                    | _ -> err ())
              | _ -> err ())
        | _ -> err ())

(* idem for (x:t) *)
open Extraargs

(* idem for (x1..xn:t) [n^2 complexity but exceptional use] *)
let check_for_coloneq =
  Pcoq.Entry.of_parser "lpar_id_colon"
    (fun strm ->
      let rec skip_to_rpar p n =
        match List.last (Stream.npeek n strm) with
        | KEYWORD "(" -> skip_to_rpar (p+1) (n+1)
        | KEYWORD ")" -> if Int.equal p 0 then n+1 else skip_to_rpar (p-1) (n+1)
        | KEYWORD "." -> err ()
        | _ -> skip_to_rpar p (n+1) in
      let rec skip_names n =
        match List.last (Stream.npeek n strm) with
        | IDENT _ | KEYWORD "_" -> skip_names (n+1)
        | KEYWORD ":" -> skip_to_rpar 0 (n+1) (* skip a constr *)
        | _ -> err () in
      let rec skip_binders n =
        match List.last (Stream.npeek n strm) with
        | KEYWORD "(" -> skip_binders (skip_names (n+1))
        | IDENT _ | KEYWORD "_" -> skip_binders (n+1)
        | KEYWORD ":=" -> ()
        | _ -> err () in
      match stream_nth 0 strm with
      | KEYWORD "(" -> skip_binders 2
      | _ -> err ())

let lookup_at_as_comma =
  Pcoq.Entry.of_parser "lookup_at_as_comma"
    (fun strm ->
      match stream_nth 0 strm with
        | KEYWORD (","|"at"|"as") -> ()
        | _ -> err ())

open Constr
open Prim
open Pltac

let mk_fix_tac (loc,id,bl,ann,ty) =
  let n =
    match bl,ann with
        [([_],_,_)], None -> 1
      | _, Some x ->
          let ids = List.map (fun x -> x.CAst.v) (List.flatten (List.map (fun (nal,_,_) -> nal) bl)) in
          (try List.index Names.Name.equal x.CAst.v ids
          with Not_found -> user_err Pp.(str "No such fix variable."))
      | _ -> user_err Pp.(str "Cannot guess decreasing argument of fix.") in
  let bl = List.map (fun (nal,bk,t) -> CLocalAssum (nal,bk,t)) bl in
  (id,n, CAst.make ~loc @@ CProdN(bl,ty))

let mk_cofix_tac (loc,id,bl,ann,ty) =
  let _ = Option.map (fun { CAst.loc = aloc } ->
    user_err ?loc:aloc
      ~hdr:"Constr:mk_cofix_tac"
      (Pp.str"Annotation forbidden in cofix expression.")) ann in
  let bl = List.map (fun (nal,bk,t) -> CLocalAssum (nal,bk,t)) bl in
  (id,CAst.make ~loc @@ CProdN(bl,ty))

(* Functions overloaded by quotifier *)
let destruction_arg_of_constr (c,lbind as clbind) = match lbind with
  | NoBindings ->
    begin
      try ElimOnIdent (CAst.make ?loc:(Constrexpr_ops.constr_loc c) (Constrexpr_ops.coerce_to_id c).CAst.v)
      with e when CErrors.noncritical e -> ElimOnConstr clbind
    end
  | _ -> ElimOnConstr clbind

let mkNumeral n =
  Numeral ((if 0<=n then SPlus else SMinus),NumTok.int (string_of_int (abs n)))

let mkTacCase with_evar = function
  | [(clear,ElimOnConstr cl),(None,None),None],None ->
      TacCase (with_evar,(clear,cl))
  (* Reinterpret numbers as a notation for terms *)
  | [(clear,ElimOnAnonHyp n),(None,None),None],None ->
      TacCase (with_evar,
        (clear,(CAst.make @@ CPrim (mkNumeral n),
         NoBindings)))
  (* Reinterpret ident as notations for variables in the context *)
  (* because we don't know if they are quantified or not *)
  | [(clear,ElimOnIdent id),(None,None),None],None ->
      TacCase (with_evar,(clear,(CAst.make @@ CRef (qualid_of_ident ?loc:id.CAst.loc id.CAst.v,None),NoBindings)))
  | ic ->
      if List.exists (function ((_, ElimOnAnonHyp _),_,_) -> true | _ -> false) (fst ic)
      then
        user_err Pp.(str "Use of numbers as direct arguments of 'case' is not supported.");
      TacInductionDestruct (false,with_evar,ic)

let rec mkCLambdaN_simple_loc ?loc bll c =
  match bll with
  | ({CAst.loc = loc1}::_ as idl,bk,t) :: bll ->
      CAst.make ?loc @@ CLambdaN ([CLocalAssum (idl,bk,t)],mkCLambdaN_simple_loc ?loc:(Loc.merge_opt loc1 loc) bll c)
  | ([],_,_) :: bll -> mkCLambdaN_simple_loc ?loc bll c
  | [] -> c

let mkCLambdaN_simple bl c = match bl with
  | [] -> c
  | h :: _ ->
    let loc = Loc.merge_opt (List.hd (pi1 h)).CAst.loc (Constrexpr_ops.constr_loc c) in
    mkCLambdaN_simple_loc ?loc bl c

let loc_of_ne_list l = Loc.merge_opt (List.hd l).CAst.loc (List.last l).CAst.loc

let map_int_or_var f = function
  | ArgArg x -> ArgArg (f x)
  | ArgVar _ as y -> y

let all_concl_occs_clause = { onhyps=Some[]; concl_occs=AllOccurrences }

let merge_occurrences loc cl = function
  | None ->
      if Locusops.clause_with_generic_occurrences cl then (None, cl)
      else
        user_err ~loc  (str "Found an \"at\" clause without \"with\" clause.")
  | Some (occs, p) ->
    let ans = match occs with
    | AllOccurrences -> cl
    | _ ->
      begin match cl with
      | { onhyps = Some []; concl_occs = AllOccurrences } ->
        { onhyps = Some []; concl_occs = occs }
      | { onhyps = Some [(AllOccurrences, id), l]; concl_occs = NoOccurrences } ->
        { cl with onhyps = Some [(occs, id), l] }
      | _ ->
        if Locusops.clause_with_generic_occurrences cl then
          user_err ~loc  (str "Unable to interpret the \"at\" clause; move it in the \"in\" clause.")
        else
          user_err ~loc  (str "Cannot use clause \"at\" twice.")
      end
    in
    (Some p, ans)

let warn_deprecated_eqn_syntax =
  CWarnings.create ~name:"deprecated-eqn-syntax" ~category:"deprecated"
         (fun arg -> strbrk (Printf.sprintf "Syntax \"_eqn:%s\" is deprecated. Please use \"eqn:%s\" instead." arg arg))

(* Auxiliary grammar rules *)

open Pvernac.Vernac_



let _ = let nat_or_var = Pcoq.Entry.create "nat_or_var"
        and id_or_meta = Pcoq.Entry.create "id_or_meta"
        and constr_with_bindings_arg =
          Pcoq.Entry.create "constr_with_bindings_arg"
        and conversion = Pcoq.Entry.create "conversion"
        and occs_nums = Pcoq.Entry.create "occs_nums"
        and occs = Pcoq.Entry.create "occs"
        and pattern_occ = Pcoq.Entry.create "pattern_occ"
        and ref_or_pattern_occ = Pcoq.Entry.create "ref_or_pattern_occ"
        and unfold_occ = Pcoq.Entry.create "unfold_occ"
        and intropatterns = Pcoq.Entry.create "intropatterns"
        and ne_intropatterns = Pcoq.Entry.create "ne_intropatterns"
        and or_and_intropattern = Pcoq.Entry.create "or_and_intropattern"
        and equality_intropattern = Pcoq.Entry.create "equality_intropattern"
        and naming_intropattern = Pcoq.Entry.create "naming_intropattern"
        and intropattern = Pcoq.Entry.create "intropattern"
        and simple_intropattern_closed =
          Pcoq.Entry.create "simple_intropattern_closed"
        and simple_binding = Pcoq.Entry.create "simple_binding"
        and with_bindings = Pcoq.Entry.create "with_bindings"
        and red_flags = Pcoq.Entry.create "red_flags"
        and delta_flag = Pcoq.Entry.create "delta_flag"
        and strategy_flag = Pcoq.Entry.create "strategy_flag"
        and hypident_occ = Pcoq.Entry.create "hypident_occ"
        and clause_dft_all = Pcoq.Entry.create "clause_dft_all"
        and opt_clause = Pcoq.Entry.create "opt_clause"
        and concl_occ = Pcoq.Entry.create "concl_occ"
        and in_hyp_list = Pcoq.Entry.create "in_hyp_list"
        and in_hyp_as = Pcoq.Entry.create "in_hyp_as"
        and orient = Pcoq.Entry.create "orient"
        and simple_binder = Pcoq.Entry.create "simple_binder"
        and fixdecl = Pcoq.Entry.create "fixdecl"
        and fixannot = Pcoq.Entry.create "fixannot"
        and cofixdecl = Pcoq.Entry.create "cofixdecl"
        and bindings_with_parameters =
          Pcoq.Entry.create "bindings_with_parameters"
        and eliminator = Pcoq.Entry.create "eliminator"
        and as_ipat = Pcoq.Entry.create "as_ipat"
        and or_and_intropattern_loc =
          Pcoq.Entry.create "or_and_intropattern_loc"
        and as_or_and_ipat = Pcoq.Entry.create "as_or_and_ipat"
        and eqn_ipat = Pcoq.Entry.create "eqn_ipat"
        and as_name = Pcoq.Entry.create "as_name"
        and by_tactic = Pcoq.Entry.create "by_tactic"
        and rewriter = Pcoq.Entry.create "rewriter"
        and oriented_rewriter = Pcoq.Entry.create "oriented_rewriter"
        and induction_clause = Pcoq.Entry.create "induction_clause"
        and induction_clause_list = Pcoq.Entry.create "induction_clause_list"
        in
        let () =
        Pcoq.grammar_extend int_or_var None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry identref)),
                 (fun id loc -> 
# 232 "g_tactic.mlg"
                           ArgVar id 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry integer)),
                (fun n loc -> 
# 231 "g_tactic.mlg"
                          ArgArg n 
                              ))])])
        in let () =
        Pcoq.grammar_extend nat_or_var None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry identref)),
                 (fun id loc -> 
# 236 "g_tactic.mlg"
                           ArgVar id 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry natural)),
                (fun n loc -> 
# 235 "g_tactic.mlg"
                          ArgArg n 
                              ))])])
        in let () =
        Pcoq.grammar_extend id_or_meta None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry identref)),
                 (fun id loc -> 
# 240 "g_tactic.mlg"
                           id 
                                ))])])
        in let () =
        Pcoq.grammar_extend open_constr None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry constr)),
                 (fun c loc -> 
# 243 "g_tactic.mlg"
                        c 
                               ))])])
        in let () =
        Pcoq.grammar_extend uconstr None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry constr)),
                 (fun c loc -> 
# 246 "g_tactic.mlg"
                        c 
                               ))])])
        in let () =
        Pcoq.grammar_extend destruction_arg None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Aentry constr_with_bindings_arg)),
                 (fun c loc -> 
# 252 "g_tactic.mlg"
                                          on_snd destruction_arg_of_constr c 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry test_lpar_id_rpar)),
                             (Extend.Aentry constr_with_bindings)),
                (fun c _ loc -> 
# 251 "g_tactic.mlg"
          (Some false,destruction_arg_of_constr c) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry natural)),
                (fun n loc -> 
# 249 "g_tactic.mlg"
                         (None,ElimOnAnonHyp n) 
                              ))])])
        in let () =
        Pcoq.grammar_extend constr_with_bindings_arg None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Aentry constr_with_bindings)),
                 (fun c loc -> 
# 257 "g_tactic.mlg"
                                      (None,c) 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD (">")))),
                             (Extend.Aentry constr_with_bindings)),
                (fun c _ loc -> 
# 256 "g_tactic.mlg"
                                           (Some true,c) 
                                ))])])
        in let () =
        Pcoq.grammar_extend quantified_hypothesis None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry natural)),
                 (fun n loc -> 
# 261 "g_tactic.mlg"
                         AnonHyp n 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry ident)),
                (fun id loc -> 
# 260 "g_tactic.mlg"
                        NamedHyp id 
                               ))])])
        in let () =
        Pcoq.grammar_extend conversion None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Aentry constr)),
                                                        (Extend.Atoken (Tok.PKEYWORD ("at")))),
                                                        (Extend.Aentry occs_nums)),
                                           (Extend.Atoken (Tok.PKEYWORD ("with")))),
                              (Extend.Aentry constr)),
                 (fun c2 _ occs _ c1 loc -> 
# 267 "g_tactic.mlg"
            (Some (occs,c1), c2) 
                                            ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry constr)),
                                          (Extend.Atoken (Tok.PKEYWORD ("with")))),
                             (Extend.Aentry constr)),
                (fun c2 _ c1 loc -> 
# 265 "g_tactic.mlg"
                                              (Some (AllOccurrences,c1),c2) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry constr)),
                (fun c loc -> 
# 264 "g_tactic.mlg"
                        (None, c) 
                              ))])])
        in let () =
        Pcoq.grammar_extend occs_nums None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("-")))),
                                           (Extend.Aentry nat_or_var)),
                              (Extend.Alist0 (Extend.Aentry int_or_var))),
                 (fun nl n _ loc -> 
# 273 "g_tactic.mlg"
             AllOccurrencesBut (List.map (map_int_or_var abs) (n::nl)) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Alist1 (Extend.Aentry nat_or_var))),
                (fun nl loc -> 
# 270 "g_tactic.mlg"
                                   OnlyOccurrences nl 
                               ))])])
        in let () =
        Pcoq.grammar_extend occs None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 276 "g_tactic.mlg"
                                                  AllOccurrences 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("at")))),
                             (Extend.Aentry occs_nums)),
                (fun occs _ loc -> 
# 276 "g_tactic.mlg"
                                    occs 
                                   ))])])
        in let () =
        Pcoq.grammar_extend pattern_occ None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry constr)),
                              (Extend.Aentry occs)),
                 (fun nl c loc -> 
# 279 "g_tactic.mlg"
                                   (nl,c) 
                                  ))])])
        in let () =
        Pcoq.grammar_extend ref_or_pattern_occ None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry constr)),
                              (Extend.Aentry occs)),
                 (fun nl c loc -> 
# 285 "g_tactic.mlg"
                                   nl,Inr c 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry smart_global)),
                             (Extend.Aentry occs)),
                (fun nl c loc -> 
# 284 "g_tactic.mlg"
                                         nl,Inl c 
                                 ))])])
        in let () =
        Pcoq.grammar_extend unfold_occ None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry smart_global)),
                              (Extend.Aentry occs)),
                 (fun nl c loc -> 
# 288 "g_tactic.mlg"
                                         (nl,c) 
                                  ))])])
        in let () =
        Pcoq.grammar_extend intropatterns None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Alist0 (Extend.Aentry intropattern))),
                 (fun l loc -> 
# 291 "g_tactic.mlg"
                                    l 
                               ))])])
        in let () =
        Pcoq.grammar_extend ne_intropatterns None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Alist1 (Extend.Aentry intropattern))),
                 (fun l loc -> 
# 294 "g_tactic.mlg"
                                    l 
                               ))])])
        in let () =
        Pcoq.grammar_extend or_and_intropattern None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                        (Extend.Aentry simple_intropattern)),
                                                        (Extend.Atoken (Tok.PKEYWORD ("&")))),
                                           (Extend.Alist1sep ((Extend.Aentry simple_intropattern), (Extend.Atoken (Tok.PKEYWORD ("&")))))),
                              (Extend.Atoken (Tok.PKEYWORD (")")))),
                 (fun _ tc _ si _ loc -> 
# 306 "g_tactic.mlg"
            let rec pairify = function
            | ([]|[_]|[_;_]) as l -> l
            | t::q -> [t; CAst.make ?loc:(loc_of_ne_list q) (IntroAction (IntroOrAndPattern (IntroAndPattern (pairify q))))]
          in IntroAndPattern (pairify (si::tc)) 
                                         ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                                    (Extend.Aentry simple_intropattern)),
                                                       (Extend.Atoken (Tok.PKEYWORD (",")))),
                                          (Extend.Alist1sep ((Extend.Aentry simple_intropattern), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                             (Extend.Atoken (Tok.PKEYWORD (")")))),
                (fun _ tc _ si _ loc -> 
# 302 "g_tactic.mlg"
               IntroAndPattern (si::tc) 
                                        ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                          (Extend.Aentry simple_intropattern)),
                             (Extend.Atoken (Tok.PKEYWORD (")")))),
                (fun _ si _ loc -> 
# 299 "g_tactic.mlg"
                                                IntroAndPattern [si] 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("()")))),
                (fun _ loc -> 
# 298 "g_tactic.mlg"
                  IntroAndPattern [] 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD ("[")))),
                                          (Extend.Alist1sep ((Extend.Aentry intropatterns), (Extend.Atoken (Tok.PKEYWORD ("|")))))),
                             (Extend.Atoken (Tok.PKEYWORD ("]")))),
                (fun _ tc _ loc -> 
# 297 "g_tactic.mlg"
                                                        IntroOrPattern tc 
                                   ))])])
        in let () =
        Pcoq.grammar_extend equality_intropattern None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("[=")))),
                                           (Extend.Aentry intropatterns)),
                              (Extend.Atoken (Tok.PKEYWORD ("]")))),
                 (fun _ tc _ loc -> 
# 314 "g_tactic.mlg"
                                           IntroInjection tc 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("<-")))),
                (fun _ loc -> 
# 313 "g_tactic.mlg"
                  IntroRewrite false 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("->")))),
                (fun _ loc -> 
# 312 "g_tactic.mlg"
                  IntroRewrite true 
                              ))])])
        in let () =
        Pcoq.grammar_extend naming_intropattern None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry ident)),
                 (fun id loc -> 
# 319 "g_tactic.mlg"
                        IntroIdentifier id 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("?")))),
                (fun _ loc -> 
# 318 "g_tactic.mlg"
                 IntroAnonymous 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry pattern_ident)),
                (fun prefix loc -> 
# 317 "g_tactic.mlg"
                                    IntroFresh prefix 
                                   ))])])
        in let () =
        Pcoq.grammar_extend intropattern None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Atoken (Tok.PKEYWORD ("**")))),
                 (fun _ loc -> 
# 324 "g_tactic.mlg"
                  CAst.make ~loc @@ IntroForthcoming false 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("*")))),
                (fun _ loc -> 
# 323 "g_tactic.mlg"
                  CAst.make ~loc @@ IntroForthcoming true 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Aentry simple_intropattern)),
                (fun l loc -> 
# 322 "g_tactic.mlg"
                                     l 
                              ))])])
        in let () =
        Pcoq.grammar_extend simple_intropattern None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry simple_intropattern_closed)),
                              (Extend.Alist0 (Extend.Arules [Extend.Rules 
                                                            (Extend.NextNoRec 
                                                            (Extend.NextNoRec 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (Tok.PKEYWORD ("%")))),
                                                            (Extend.Aentryl (operconstr, "0"))),
                                                            (fun c _ loc ->
                                                            
# 328 "g_tactic.mlg"
                                                      c 
                                                            ))]))),
                 (fun l pat loc -> 
# 329 "g_tactic.mlg"
            let {CAst.loc=loc0;v=pat} = pat in
          let f c pat =
            let loc1 = Constrexpr_ops.constr_loc c in
            let loc = Loc.merge_opt loc0 loc1 in
            IntroAction (IntroApplyOn (CAst.(make ?loc:loc1 c),CAst.(make ?loc pat))) in
          CAst.make ~loc @@ List.fold_right f l pat 
                                   ))])])
        in let () =
        Pcoq.grammar_extend simple_intropattern_closed None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Aentry naming_intropattern)),
                 (fun pat loc -> 
# 340 "g_tactic.mlg"
                                       CAst.make ~loc @@ IntroNaming pat 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("_")))),
                (fun _ loc -> 
# 339 "g_tactic.mlg"
                 CAst.make ~loc @@ IntroAction IntroWildcard 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Aentry equality_intropattern)),
                (fun pat loc -> 
# 338 "g_tactic.mlg"
                                         CAst.make ~loc @@ IntroAction pat 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Aentry or_and_intropattern)),
                (fun pat loc -> 
# 337 "g_tactic.mlg"
                                         CAst.make ~loc @@ IntroAction (IntroOrAndPattern pat) 
                                ))])])
        in let () =
        Pcoq.grammar_extend simple_binding None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                        (Extend.Aentry natural)),
                                                        (Extend.Atoken (Tok.PKEYWORD (":=")))),
                                           (Extend.Aentry lconstr)),
                              (Extend.Atoken (Tok.PKEYWORD (")")))),
                 (fun _ c _ n _ loc -> 
# 344 "g_tactic.mlg"
                                                      CAst.make ~loc (AnonHyp n, c) 
                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                                    (Extend.Aentry ident)),
                                                       (Extend.Atoken (Tok.PKEYWORD (":=")))),
                                          (Extend.Aentry lconstr)),
                             (Extend.Atoken (Tok.PKEYWORD (")")))),
                (fun _ c _ id _ loc -> 
# 343 "g_tactic.mlg"
                                                     CAst.make ~loc (NamedHyp id, c) 
                                       ))])])
        in let () =
        Pcoq.grammar_extend bindings None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Alist1 (Extend.Aentry constr))),
                 (fun bl loc -> 
# 349 "g_tactic.mlg"
                               ImplicitBindings bl 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry test_lpar_idnum_coloneq)),
                             (Extend.Alist1 (Extend.Aentry simple_binding))),
                (fun bl _ loc -> 
# 348 "g_tactic.mlg"
            ExplicitBindings bl 
                                 ))])])
        in let () =
        Pcoq.grammar_extend constr_with_bindings None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry constr)),
                              (Extend.Aentry with_bindings)),
                 (fun l c loc -> 
# 352 "g_tactic.mlg"
                                           (c, l) 
                                 ))])])
        in let () =
        Pcoq.grammar_extend with_bindings None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 355 "g_tactic.mlg"
                                               NoBindings 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("with")))),
                             (Extend.Aentry bindings)),
                (fun bl _ loc -> 
# 355 "g_tactic.mlg"
                                   bl 
                                 ))])])
        in let () =
        Pcoq.grammar_extend red_flags None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PIDENT (Some
                                           ("delta"))))),
                              (Extend.Aentry delta_flag)),
                 (fun d _ loc -> 
# 364 "g_tactic.mlg"
                                           [d] 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("zeta"))))),
                (fun _ loc -> 
# 363 "g_tactic.mlg"
                          [FZeta] 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("cofix"))))),
                (fun _ loc -> 
# 362 "g_tactic.mlg"
                           [FCofix] 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("fix"))))),
                (fun _ loc -> 
# 361 "g_tactic.mlg"
                         [FFix] 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("match"))))),
                (fun _ loc -> 
# 360 "g_tactic.mlg"
                           [FMatch] 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("iota"))))),
                (fun _ loc -> 
# 359 "g_tactic.mlg"
                          [FMatch;FFix;FCofix] 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("beta"))))),
                (fun _ loc -> 
# 358 "g_tactic.mlg"
                          [FBeta] 
                              ))])])
        in let () =
        Pcoq.grammar_extend delta_flag None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 370 "g_tactic.mlg"
             FDeltaBut [] 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD ("[")))),
                                          (Extend.Alist1 (Extend.Aentry smart_global))),
                             (Extend.Atoken (Tok.PKEYWORD ("]")))),
                (fun _ idl _ loc -> 
# 369 "g_tactic.mlg"
                                                FConst idl 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("-")))),
                                                       (Extend.Atoken (Tok.PKEYWORD ("[")))),
                                          (Extend.Alist1 (Extend.Aentry smart_global))),
                             (Extend.Atoken (Tok.PKEYWORD ("]")))),
                (fun _ idl _ _ loc -> 
# 368 "g_tactic.mlg"
                                                     FDeltaBut idl 
                                      ))])])
        in let () =
        Pcoq.grammar_extend strategy_flag None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry delta_flag)),
                 (fun d loc -> 
# 375 "g_tactic.mlg"
                            all_with d 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Alist1 (Extend.Aentry red_flags))),
                (fun s loc -> 
# 374 "g_tactic.mlg"
                                 Redops.make_red_flag (List.flatten s) 
                              ))])])
        in let () =
        Pcoq.grammar_extend red_expr None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Atoken (Tok.PIDENT (None)))),
                 (fun s loc -> 
# 391 "g_tactic.mlg"
                       ExtraRedExpr s 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("pattern"))))),
                             (Extend.Alist1sep ((Extend.Aentry pattern_occ), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                (fun pl _ loc -> 
# 390 "g_tactic.mlg"
                                                            Pattern pl 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("fold"))))),
                             (Extend.Alist1 (Extend.Aentry constr))),
                (fun cl _ loc -> 
# 389 "g_tactic.mlg"
                                             Fold cl 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("unfold"))))),
                             (Extend.Alist1sep ((Extend.Aentry unfold_occ), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                (fun ul _ loc -> 
# 388 "g_tactic.mlg"
                                                           Unfold ul 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("native_compute"))))),
                             (Extend.Aopt (Extend.Aentry ref_or_pattern_occ))),
                (fun po _ loc -> 
# 387 "g_tactic.mlg"
                                                                 CbvNative po 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("vm_compute"))))),
                             (Extend.Aopt (Extend.Aentry ref_or_pattern_occ))),
                (fun po _ loc -> 
# 386 "g_tactic.mlg"
                                                             CbvVm po 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("compute"))))),
                             (Extend.Aentry delta_flag)),
                (fun delta _ loc -> 
# 385 "g_tactic.mlg"
                                                 Cbv (all_with delta) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("lazy"))))),
                             (Extend.Aentry strategy_flag)),
                (fun s _ loc -> 
# 384 "g_tactic.mlg"
                                             Lazy s 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("cbn"))))),
                             (Extend.Aentry strategy_flag)),
                (fun s _ loc -> 
# 383 "g_tactic.mlg"
                                            Cbn s 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("cbv"))))),
                             (Extend.Aentry strategy_flag)),
                (fun s _ loc -> 
# 382 "g_tactic.mlg"
                                            Cbv s 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("simpl"))))),
                                          (Extend.Aentry delta_flag)),
                             (Extend.Aopt (Extend.Aentry ref_or_pattern_occ))),
                (fun po d _ loc -> 
# 381 "g_tactic.mlg"
                                                                        Simpl (all_with d,po) 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("hnf"))))),
                (fun _ loc -> 
# 380 "g_tactic.mlg"
                         Hnf 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("red"))))),
                (fun _ loc -> 
# 379 "g_tactic.mlg"
                         Red false 
                              ))])])
        in let () =
        Pcoq.grammar_extend hypident None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                        (Extend.Atoken (Tok.PIDENT (Some
                                                        ("value"))))),
                                                        (Extend.Atoken (Tok.PIDENT (Some
                                                        ("of"))))),
                                           (Extend.Aentry id_or_meta)),
                              (Extend.Atoken (Tok.PKEYWORD (")")))),
                 (fun _ id _ _ _ loc -> 
# 401 "g_tactic.mlg"
          let id : lident = id in
        id,InHypValueOnly 
                                        ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("type"))))),
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("of"))))),
                                          (Extend.Aentry id_or_meta)),
                             (Extend.Atoken (Tok.PKEYWORD (")")))),
                (fun _ id _ _ _ loc -> 
# 398 "g_tactic.mlg"
          let id : lident = id in
        id,InHypTypeOnly 
                                       ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry id_or_meta)),
                (fun id loc -> 
# 395 "g_tactic.mlg"
          let id : lident = id in
        id,InHyp 
                               ))])])
        in let () =
        Pcoq.grammar_extend hypident_occ None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry hypident)),
                              (Extend.Aentry occs)),
                 (fun occs h loc -> 
# 407 "g_tactic.mlg"
          let (id,l) = h in
        let id : lident = id in
        ((occs,id),l) 
                                    ))])])
        in let () =
        Pcoq.grammar_extend in_clause None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Alist0sep ((Extend.Aentry hypident_occ), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                 (fun hl loc -> 
# 419 "g_tactic.mlg"
            {onhyps=Some hl; concl_occs=NoOccurrences} 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Alist0sep ((Extend.Aentry hypident_occ), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                                          (Extend.Atoken (Tok.PKEYWORD ("|-")))),
                             (Extend.Aentry concl_occ)),
                (fun occs _ hl loc -> 
# 417 "g_tactic.mlg"
            {onhyps=Some hl; concl_occs=occs} 
                                      ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD ("*")))),
                                          (Extend.Atoken (Tok.PKEYWORD ("|-")))),
                             (Extend.Aentry concl_occ)),
                (fun occs _ _ loc -> 
# 415 "g_tactic.mlg"
            {onhyps=None; concl_occs=occs} 
                                     ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("*")))),
                             (Extend.Aentry occs)),
                (fun occs _ loc -> 
# 413 "g_tactic.mlg"
            {onhyps=None; concl_occs=occs} 
                                   ))])])
        in let () =
        Pcoq.grammar_extend clause_dft_concl None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 424 "g_tactic.mlg"
             all_concl_occs_clause 
                                                       ));
                Extend.Rule (Extend.Next (Extend.Stop, (Extend.Aentry occs)),
                (fun occs loc -> 
# 423 "g_tactic.mlg"
                       {onhyps=Some[]; concl_occs=occs} 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("in")))),
                             (Extend.Aentry in_clause)),
                (fun cl _ loc -> 
# 422 "g_tactic.mlg"
                                  cl 
                                 ))])])
        in let () =
        Pcoq.grammar_extend clause_dft_all None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 428 "g_tactic.mlg"
             {onhyps=None; concl_occs=AllOccurrences} 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("in")))),
                             (Extend.Aentry in_clause)),
                (fun cl _ loc -> 
# 427 "g_tactic.mlg"
                                  cl 
                                 ))])])
        in let () =
        Pcoq.grammar_extend opt_clause None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 433 "g_tactic.mlg"
             None 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("at")))),
                             (Extend.Aentry occs_nums)),
                (fun occs _ loc -> 
# 432 "g_tactic.mlg"
                                    Some {onhyps=Some[]; concl_occs=occs} 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("in")))),
                             (Extend.Aentry in_clause)),
                (fun cl _ loc -> 
# 431 "g_tactic.mlg"
                                  Some cl 
                                 ))])])
        in let () =
        Pcoq.grammar_extend concl_occ None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 437 "g_tactic.mlg"
             NoOccurrences 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("*")))),
                             (Extend.Aentry occs)),
                (fun occs _ loc -> 
# 436 "g_tactic.mlg"
                              occs 
                                   ))])])
        in let () =
        Pcoq.grammar_extend in_hyp_list None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 441 "g_tactic.mlg"
             [] 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("in")))),
                             (Extend.Alist1 (Extend.Aentry id_or_meta))),
                (fun idl _ loc -> 
# 440 "g_tactic.mlg"
                                          idl 
                                  ))])])
        in let () =
        Pcoq.grammar_extend in_hyp_as None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 445 "g_tactic.mlg"
             None 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PKEYWORD ("in")))),
                                          (Extend.Aentry id_or_meta)),
                             (Extend.Aentry as_ipat)),
                (fun ipat id _ loc -> 
# 444 "g_tactic.mlg"
                                                   Some (id,ipat) 
                                      ))])])
        in let () =
        Pcoq.grammar_extend orient None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 450 "g_tactic.mlg"
             true 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("<-")))),
                (fun _ loc -> 
# 449 "g_tactic.mlg"
                  false 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Atoken (Tok.PKEYWORD ("->")))),
                (fun _ loc -> 
# 448 "g_tactic.mlg"
                  true 
                              ))])])
        in let () =
        Pcoq.grammar_extend simple_binder None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                        (Extend.Alist1 (Extend.Aentry name))),
                                                        (Extend.Atoken (Tok.PKEYWORD (":")))),
                                           (Extend.Aentry lconstr)),
                              (Extend.Atoken (Tok.PKEYWORD (")")))),
                 (fun _ c _ nal _ loc -> 
# 455 "g_tactic.mlg"
                                                      (nal,Default Explicit,c) 
                                         ));
                Extend.Rule (Extend.Next (Extend.Stop, (Extend.Aentry name)),
                (fun na loc -> 
# 453 "g_tactic.mlg"
                     ([na],Default Explicit, CAst.make ~loc @@
                    CHole (Some (Evar_kinds.BinderType na.CAst.v), IntroAnonymous, None)) 
                               ))])])
        in let () =
        Pcoq.grammar_extend fixdecl None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                        (Extend.Aentry ident)),
                                                        (Extend.Alist0 (Extend.Aentry simple_binder))),
                                                        (Extend.Aentry fixannot)),
                                                        (Extend.Atoken (Tok.PKEYWORD (":")))),
                                           (Extend.Aentry lconstr)),
                              (Extend.Atoken (Tok.PKEYWORD (")")))),
                 (fun _ ty _ ann bl id _ loc -> 
# 460 "g_tactic.mlg"
                                  (loc, id, bl, ann, ty) 
                                                ))])])
        in let () =
        Pcoq.grammar_extend fixannot None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 464 "g_tactic.mlg"
             None 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("{")))),
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("struct"))))),
                                          (Extend.Aentry name)),
                             (Extend.Atoken (Tok.PKEYWORD ("}")))),
                (fun _ id _ _ loc -> 
# 463 "g_tactic.mlg"
                                               Some id 
                                     ))])])
        in let () =
        Pcoq.grammar_extend cofixdecl None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                        (Extend.Aentry ident)),
                                                        (Extend.Alist0 (Extend.Aentry simple_binder))),
                                                        (Extend.Atoken (Tok.PKEYWORD (":")))),
                                           (Extend.Aentry lconstr)),
                              (Extend.Atoken (Tok.PKEYWORD (")")))),
                 (fun _ ty _ bl id _ loc -> 
# 468 "g_tactic.mlg"
          (loc, id, bl, None, ty) 
                                            ))])])
        in let () =
        Pcoq.grammar_extend bindings_with_parameters None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Aentry check_for_coloneq)),
                                                        (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                        (Extend.Aentry ident)),
                                                        (Extend.Alist0 (Extend.Aentry simple_binder))),
                                                        (Extend.Atoken (Tok.PKEYWORD (":=")))),
                                           (Extend.Aentry lconstr)),
                              (Extend.Atoken (Tok.PKEYWORD (")")))),
                 (fun _ c _ bl id _ _ loc -> 
# 472 "g_tactic.mlg"
                                    (id, mkCLambdaN_simple bl c) 
                                             ))])])
        in let () =
        Pcoq.grammar_extend eliminator None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PKEYWORD ("using")))),
                              (Extend.Aentry constr_with_bindings)),
                 (fun el _ loc -> 
# 475 "g_tactic.mlg"
                                                el 
                                  ))])])
        in let () =
        Pcoq.grammar_extend as_ipat None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 479 "g_tactic.mlg"
             None 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("as")))),
                             (Extend.Aentry simple_intropattern)),
                (fun ipat _ loc -> 
# 478 "g_tactic.mlg"
                                              Some ipat 
                                   ))])])
        in let () =
        Pcoq.grammar_extend or_and_intropattern_loc None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry identref)),
                 (fun locid loc -> 
# 483 "g_tactic.mlg"
                              ArgVar locid 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Stop,
                             (Extend.Aentry or_and_intropattern)),
                (fun ipat loc -> 
# 482 "g_tactic.mlg"
                                        ArgArg (CAst.make ~loc ipat) 
                                 ))])])
        in let () =
        Pcoq.grammar_extend as_or_and_ipat None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 487 "g_tactic.mlg"
             None 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("as")))),
                             (Extend.Aentry or_and_intropattern_loc)),
                (fun ipat _ loc -> 
# 486 "g_tactic.mlg"
                                                  Some ipat 
                                   ))])])
        in let () =
        Pcoq.grammar_extend eqn_ipat None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 495 "g_tactic.mlg"
             None 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("_eqn"))))),
                (fun _ loc -> 
# 494 "g_tactic.mlg"
           warn_deprecated_eqn_syntax ~loc "?"; Some (CAst.make ~loc IntroAnonymous) 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("_eqn"))))),
                                          (Extend.Atoken (Tok.PKEYWORD (":")))),
                             (Extend.Aentry naming_intropattern)),
                (fun pat _ _ loc -> 
# 492 "g_tactic.mlg"
           warn_deprecated_eqn_syntax ~loc "H"; Some (CAst.make ~loc pat) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("eqn"))))),
                                          (Extend.Atoken (Tok.PKEYWORD (":")))),
                             (Extend.Aentry naming_intropattern)),
                (fun pat _ _ loc -> 
# 490 "g_tactic.mlg"
                                                         Some (CAst.make ~loc pat) 
                                    ))])])
        in let () =
        Pcoq.grammar_extend as_name None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 498 "g_tactic.mlg"
                                                          Names.Name.Anonymous 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("as")))),
                             (Extend.Aentry ident)),
                (fun id _ loc -> 
# 498 "g_tactic.mlg"
                              Names.Name.Name id 
                                 ))])])
        in let () =
        Pcoq.grammar_extend by_tactic None
        (None, [(None, None,
                [Extend.Rule (Extend.Stop, (fun loc -> 
# 502 "g_tactic.mlg"
           None 
                                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("by")))),
                             (Extend.Aentryl (tactic_expr, "3"))),
                (fun tac _ loc -> 
# 501 "g_tactic.mlg"
                                               Some tac 
                                  ))])])
        in let () =
        Pcoq.grammar_extend rewriter None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Aentry constr_with_bindings_arg)),
                 (fun c loc -> 
# 510 "g_tactic.mlg"
                                          (Equality.Precisely 1, c) 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Aentry natural)),
                             (Extend.Aentry constr_with_bindings_arg)),
                (fun c n loc -> 
# 509 "g_tactic.mlg"
                                                       (Equality.Precisely n,c) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry natural)),
                                          (Extend.Arules [Extend.Rules 
                                                         (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (Tok.PLEFTQMARK))),
                                                         (fun _ loc -> 
                                                         
# 508 "g_tactic.mlg"
                                                     () 
                                                         ));
                                                         Extend.Rules 
                                                         (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (Tok.PKEYWORD ("?")))),
                                                         (fun _ loc -> 
                                                         
# 508 "g_tactic.mlg"
                               () 
                                                         ))])),
                             (Extend.Aentry constr_with_bindings_arg)),
                (fun c _ n loc -> 
# 508 "g_tactic.mlg"
                                                                                               (Equality.UpTo n,c) 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Aentry natural)),
                                          (Extend.Atoken (Tok.PKEYWORD ("!")))),
                             (Extend.Aentry constr_with_bindings_arg)),
                (fun c _ n loc -> 
# 507 "g_tactic.mlg"
                                                            (Equality.Precisely n,c) 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Arules [Extend.Rules 
                                                         (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (Tok.PLEFTQMARK))),
                                                         (fun _ loc -> 
                                                         
# 506 "g_tactic.mlg"
                                        () 
                                                         ));
                                                         Extend.Rules 
                                                         (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (Tok.PKEYWORD ("?")))),
                                                         (fun _ loc -> 
                                                         
# 506 "g_tactic.mlg"
                  () 
                                                         ))])),
                             (Extend.Aentry constr_with_bindings_arg)),
                (fun c _ loc -> 
# 506 "g_tactic.mlg"
                                                                                  (Equality.RepeatStar,c) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD ("!")))),
                             (Extend.Aentry constr_with_bindings_arg)),
                (fun c _ loc -> 
# 505 "g_tactic.mlg"
                                               (Equality.RepeatPlus,c) 
                                ))])])
        in let () =
        Pcoq.grammar_extend oriented_rewriter None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Aentry orient)),
                              (Extend.Aentry rewriter)),
                 (fun p b loc -> 
# 514 "g_tactic.mlg"
                                      let (m,c) = p in (b,m,c) 
                                 ))])])
        in let () =
        Pcoq.grammar_extend induction_clause None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Aentry destruction_arg)),
                                                        (Extend.Aentry as_or_and_ipat)),
                                           (Extend.Aentry eqn_ipat)),
                              (Extend.Aentry opt_clause)),
                 (fun cl eq pat c loc -> 
# 518 "g_tactic.mlg"
                             (c,(eq,pat),cl) 
                                         ))])])
        in let () =
        Pcoq.grammar_extend induction_clause_list None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Alist1sep ((Extend.Aentry induction_clause), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                                           (Extend.Aopt (Extend.Aentry eliminator))),
                              (Extend.Aentry opt_clause)),
                 (fun cl_tolerance el ic loc -> 
# 524 "g_tactic.mlg"
          match ic,el,cl_tolerance with
        | [c,pat,None],Some _,Some _ -> ([c,pat,cl_tolerance],el)
        | _,_,Some _ -> err ()
        | _,_,None -> (ic,el) 
                                                ))])])
        in let () =
        Pcoq.grammar_extend simple_tactic None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Atoken (Tok.PIDENT (Some
                                                        ("change_no_check"))))),
                                           (Extend.Aentry conversion)),
                              (Extend.Aentry clause_dft_concl)),
                 (fun cl c _ loc -> 
# 708 "g_tactic.mlg"
            let (oc, c) = c in
          let p,cl = merge_occurrences loc cl oc in
          TacAtom (CAst.make ~loc @@ TacChange (false,p,c,cl)) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("change"))))),
                                          (Extend.Aentry conversion)),
                             (Extend.Aentry clause_dft_concl)),
                (fun cl c _ loc -> 
# 704 "g_tactic.mlg"
            let (oc, c) = c in
          let p,cl = merge_occurrences loc cl oc in
          TacAtom (CAst.make ~loc @@ TacChange (true,p,c,cl)) 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("pattern"))))),
                                          (Extend.Alist1sep ((Extend.Aentry pattern_occ), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                             (Extend.Aentry clause_dft_concl)),
                (fun cl pl _ loc -> 
# 700 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacReduce (Pattern pl, cl)) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("fold"))))),
                                          (Extend.Alist1 (Extend.Aentry constr))),
                             (Extend.Aentry clause_dft_concl)),
                (fun cl l _ loc -> 
# 698 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacReduce (Fold l, cl)) 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("unfold"))))),
                                          (Extend.Alist1sep ((Extend.Aentry unfold_occ), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                             (Extend.Aentry clause_dft_concl)),
                (fun cl ul _ loc -> 
# 696 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacReduce (Unfold ul, cl)) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("native_compute"))))),
                                          (Extend.Aopt (Extend.Aentry ref_or_pattern_occ))),
                             (Extend.Aentry clause_dft_concl)),
                (fun cl po _ loc -> 
# 694 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacReduce (CbvNative po, cl)) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("vm_compute"))))),
                                          (Extend.Aopt (Extend.Aentry ref_or_pattern_occ))),
                             (Extend.Aentry clause_dft_concl)),
                (fun cl po _ loc -> 
# 692 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacReduce (CbvVm po, cl)) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("compute"))))),
                                          (Extend.Aentry delta_flag)),
                             (Extend.Aentry clause_dft_concl)),
                (fun cl delta _ loc -> 
# 690 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacReduce (Cbv (all_with delta), cl)) 
                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("lazy"))))),
                                          (Extend.Aentry strategy_flag)),
                             (Extend.Aentry clause_dft_concl)),
                (fun cl s _ loc -> 
# 688 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacReduce (Lazy s, cl)) 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("cbn"))))),
                                          (Extend.Aentry strategy_flag)),
                             (Extend.Aentry clause_dft_concl)),
                (fun cl s _ loc -> 
# 686 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacReduce (Cbn s, cl)) 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("cbv"))))),
                                          (Extend.Aentry strategy_flag)),
                             (Extend.Aentry clause_dft_concl)),
                (fun cl s _ loc -> 
# 684 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacReduce (Cbv s, cl)) 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("simpl"))))),
                                                       (Extend.Aentry delta_flag)),
                                          (Extend.Aopt (Extend.Aentry ref_or_pattern_occ))),
                             (Extend.Aentry clause_dft_concl)),
                (fun cl po d _ loc -> 
# 682 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacReduce (Simpl (all_with d, po), cl)) 
                                      ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("hnf"))))),
                             (Extend.Aentry clause_dft_concl)),
                (fun cl _ loc -> 
# 680 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacReduce (Hnf, cl)) 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("red"))))),
                             (Extend.Aentry clause_dft_concl)),
                (fun cl _ loc -> 
# 678 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacReduce (Red false, cl)) 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("inversion"))))),
                                                                    (Extend.Aentry quantified_hypothesis)),
                                                       (Extend.Atoken (Tok.PKEYWORD ("using")))),
                                          (Extend.Aentry constr)),
                             (Extend.Aentry in_hyp_list)),
                (fun cl c _ hyp _ loc -> 
# 674 "g_tactic.mlg"
              TacAtom (CAst.make ~loc @@ TacInversion (InversionUsing (c,cl), hyp)) 
                                         ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("inversion_clear"))))),
                                                       (Extend.Aentry quantified_hypothesis)),
                                          (Extend.Aentry as_or_and_ipat)),
                             (Extend.Aentry in_hyp_list)),
                (fun cl ids hyp _ loc -> 
# 671 "g_tactic.mlg"
              TacAtom (CAst.make ~loc @@ TacInversion (NonDepInversion (FullInversionClear, cl, ids), hyp)) 
                                         ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("inversion"))))),
                                                       (Extend.Aentry quantified_hypothesis)),
                                          (Extend.Aentry as_or_and_ipat)),
                             (Extend.Aentry in_hyp_list)),
                (fun cl ids hyp _ loc -> 
# 667 "g_tactic.mlg"
              TacAtom (CAst.make ~loc @@ TacInversion (NonDepInversion (FullInversion, cl, ids), hyp)) 
                                         ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("simple"))))),
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("inversion"))))),
                                                       (Extend.Aentry quantified_hypothesis)),
                                          (Extend.Aentry as_or_and_ipat)),
                             (Extend.Aentry in_hyp_list)),
                (fun cl ids hyp _ _ loc -> 
# 663 "g_tactic.mlg"
              TacAtom (CAst.make ~loc @@ TacInversion (NonDepInversion (SimpleInversion, cl, ids), hyp)) 
                                           ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("dependent"))))),
                                                                    (Extend.Arules 
                                                                    [Extend.Rules 
                                                                    (Extend.NextNoRec 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("inversion_clear"))))),
                                                                    (fun _
                                                                    loc -> 
                                                                    
# 656 "g_tactic.mlg"
                                         FullInversionClear 
                                                                    ));
                                                                    Extend.Rules 
                                                                    (Extend.NextNoRec 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("inversion"))))),
                                                                    (fun _
                                                                    loc -> 
                                                                    
# 655 "g_tactic.mlg"
                                   FullInversion 
                                                                    ));
                                                                    Extend.Rules 
                                                                    (Extend.NextNoRec 
                                                                    (Extend.NextNoRec 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("simple"))))),
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("inversion"))))),
                                                                    (fun _ _
                                                                    loc -> 
                                                                    
# 654 "g_tactic.mlg"
                                                   SimpleInversion 
                                                                    ))])),
                                                       (Extend.Aentry quantified_hypothesis)),
                                          (Extend.Aentry as_or_and_ipat)),
                             (Extend.Aopt (Extend.Arules [Extend.Rules 
                                                         (Extend.NextNoRec 
                                                         (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (Tok.PKEYWORD ("with")))),
                                                         (Extend.Aentry constr)),
                                                         (fun c _ loc -> 
                                                         
# 658 "g_tactic.mlg"
                                                                  c 
                                                         ))]))),
                (fun co ids hyp k _ loc -> 
# 659 "g_tactic.mlg"
              TacAtom (CAst.make ~loc @@ TacInversion (DepInversion (k,co,ids),hyp)) 
                                           ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("erewrite"))))),
                                                       (Extend.Alist1sep ((Extend.Aentry oriented_rewriter), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                                          (Extend.Aentry clause_dft_concl)),
                             (Extend.Aentry by_tactic)),
                (fun t cl l _ loc -> 
# 652 "g_tactic.mlg"
                                                  TacAtom (CAst.make ~loc @@ TacRewrite (true,l,cl,t)) 
                                     ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("rewrite"))))),
                                                       (Extend.Alist1sep ((Extend.Aentry oriented_rewriter), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                                          (Extend.Aentry clause_dft_concl)),
                             (Extend.Aentry by_tactic)),
                (fun t cl l _ loc -> 
# 650 "g_tactic.mlg"
                                                  TacAtom (CAst.make ~loc @@ TacRewrite (false,l,cl,t)) 
                                     ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("edestruct"))))),
                             (Extend.Aentry induction_clause_list)),
                (fun icl _ loc -> 
# 646 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacInductionDestruct(false,true,icl)) 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("destruct"))))),
                             (Extend.Aentry induction_clause_list)),
                (fun icl _ loc -> 
# 644 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacInductionDestruct(false,false,icl)) 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("einduction"))))),
                             (Extend.Aentry induction_clause_list)),
                (fun ic _ loc -> 
# 642 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacInductionDestruct(true,true,ic)) 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("induction"))))),
                             (Extend.Aentry induction_clause_list)),
                (fun ic _ loc -> 
# 640 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacInductionDestruct (true,false,ic)) 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("generalize"))))),
                                                                    (Extend.Aentry constr)),
                                                                    (Extend.Aentry lookup_at_as_comma)),
                                                       (Extend.Aentry occs)),
                                          (Extend.Aentry as_name)),
                             (Extend.Alist0 (Extend.Arules [Extend.Rules 
                                                           (Extend.NextNoRec 
                                                           (Extend.NextNoRec 
                                                           (Extend.NextNoRec 
                                                           (Extend.Stop,
                                                           (Extend.Atoken (Tok.PKEYWORD (",")))),
                                                           (Extend.Aentry pattern_occ)),
                                                           (Extend.Aentry as_name)),
                                                           (fun na c _ loc ->
                                                           
# 635 "g_tactic.mlg"
                                                             (c,na) 
                                                           ))]))),
                (fun l na nl _ c _ loc -> 
# 636 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacGeneralize (((nl,c),na)::l)) 
                                          ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("generalize"))))),
                                          (Extend.Aentry constr)),
                             (Extend.Alist1 (Extend.Aentry constr))),
                (fun l c _ loc -> 
# 631 "g_tactic.mlg"
            let gen_everywhere c = ((AllOccurrences,c),Names.Name.Anonymous) in
          TacAtom (CAst.make ~loc @@ TacGeneralize (List.map gen_everywhere (c::l))) 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("generalize"))))),
                             (Extend.Aentry constr)),
                (fun c _ loc -> 
# 629 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacGeneralize [((AllOccurrences,c),Names.Name.Anonymous)]) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("eenough"))))),
                                                       (Extend.Aentry constr)),
                                          (Extend.Aentry as_ipat)),
                             (Extend.Aentry by_tactic)),
                (fun tac ipat c _ loc -> 
# 626 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacAssert (true,false,Some tac,ipat,c)) 
                                         ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("enough"))))),
                                                       (Extend.Aentry constr)),
                                          (Extend.Aentry as_ipat)),
                             (Extend.Aentry by_tactic)),
                (fun tac ipat c _ loc -> 
# 624 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacAssert (false,false,Some tac,ipat,c)) 
                                         ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("epose"))))),
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("proof"))))),
                                          (Extend.Aentry lconstr)),
                             (Extend.Aentry as_ipat)),
                (fun ipat c _ _ loc -> 
# 622 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacAssert (true,true,None,ipat,c)) 
                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("pose"))))),
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("proof"))))),
                                          (Extend.Aentry lconstr)),
                             (Extend.Aentry as_ipat)),
                (fun ipat c _ _ loc -> 
# 620 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacAssert (false,true,None,ipat,c)) 
                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("eassert"))))),
                                                       (Extend.Aentry constr)),
                                          (Extend.Aentry as_ipat)),
                             (Extend.Aentry by_tactic)),
                (fun tac ipat c _ loc -> 
# 618 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacAssert (true,true,Some tac,ipat,c)) 
                                         ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("assert"))))),
                                                       (Extend.Aentry constr)),
                                          (Extend.Aentry as_ipat)),
                             (Extend.Aentry by_tactic)),
                (fun tac ipat c _ loc -> 
# 616 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacAssert (false,true,Some tac,ipat,c)) 
                                         ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("eenough"))))),
                                                                    (Extend.Aentry test_lpar_id_colon)),
                                                                    (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                                    (Extend.Aentry identref)),
                                                                    (Extend.Atoken (Tok.PKEYWORD (":")))),
                                                       (Extend.Aentry lconstr)),
                                          (Extend.Atoken (Tok.PKEYWORD (")")))),
                             (Extend.Aentry by_tactic)),
                (fun tac _ c _ lid _ _ _ loc -> 
# 612 "g_tactic.mlg"
            let { CAst.loc = loc; v = id } = lid in
          TacAtom (CAst.make ?loc @@ TacAssert (true,false,Some tac,Some (CAst.make ?loc @@ IntroNaming (IntroIdentifier id)),c)) 
                                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("enough"))))),
                                                                    (Extend.Aentry test_lpar_id_colon)),
                                                                    (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                                    (Extend.Aentry identref)),
                                                                    (Extend.Atoken (Tok.PKEYWORD (":")))),
                                                       (Extend.Aentry lconstr)),
                                          (Extend.Atoken (Tok.PKEYWORD (")")))),
                             (Extend.Aentry by_tactic)),
                (fun tac _ c _ lid _ _ _ loc -> 
# 608 "g_tactic.mlg"
            let { CAst.loc = loc; v = id } = lid in
          TacAtom (CAst.make ?loc @@ TacAssert (false,false,Some tac,Some (CAst.make ?loc @@ IntroNaming (IntroIdentifier id)),c)) 
                                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("eassert"))))),
                                                                    (Extend.Aentry test_lpar_id_colon)),
                                                                    (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                                    (Extend.Aentry identref)),
                                                                    (Extend.Atoken (Tok.PKEYWORD (":")))),
                                                       (Extend.Aentry lconstr)),
                                          (Extend.Atoken (Tok.PKEYWORD (")")))),
                             (Extend.Aentry by_tactic)),
                (fun tac _ c _ lid _ _ _ loc -> 
# 602 "g_tactic.mlg"
            let { CAst.loc = loc; v = id } = lid in
          TacAtom (CAst.make ?loc @@ TacAssert (true,true,Some tac,Some (CAst.make ?loc @@ IntroNaming (IntroIdentifier id)),c)) 
                                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("assert"))))),
                                                                    (Extend.Aentry test_lpar_id_colon)),
                                                                    (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                                    (Extend.Aentry identref)),
                                                                    (Extend.Atoken (Tok.PKEYWORD (":")))),
                                                       (Extend.Aentry lconstr)),
                                          (Extend.Atoken (Tok.PKEYWORD (")")))),
                             (Extend.Aentry by_tactic)),
                (fun tac _ c _ lid _ _ _ loc -> 
# 598 "g_tactic.mlg"
            let { CAst.loc = loc; v = id } = lid in
          TacAtom (CAst.make ?loc @@ TacAssert (false,true,Some tac,Some (CAst.make ?loc @@ IntroNaming (IntroIdentifier id)),c)) 
                                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("eassert"))))),
                                                                    (Extend.Aentry test_lpar_id_coloneq)),
                                                                    (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                                    (Extend.Aentry identref)),
                                                       (Extend.Atoken (Tok.PKEYWORD (":=")))),
                                          (Extend.Aentry lconstr)),
                             (Extend.Atoken (Tok.PKEYWORD (")")))),
                (fun _ c _ lid _ _ _ loc -> 
# 592 "g_tactic.mlg"
            let { CAst.loc = loc; v = id } = lid in
          TacAtom (CAst.make ?loc @@ TacAssert (true,true,None,Some (CAst.make ?loc @@ IntroNaming (IntroIdentifier id)),c)) 
                                            ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("assert"))))),
                                                                    (Extend.Aentry test_lpar_id_coloneq)),
                                                                    (Extend.Atoken (Tok.PKEYWORD ("(")))),
                                                                    (Extend.Aentry identref)),
                                                       (Extend.Atoken (Tok.PKEYWORD (":=")))),
                                          (Extend.Aentry lconstr)),
                             (Extend.Atoken (Tok.PKEYWORD (")")))),
                (fun _ c _ lid _ _ _ loc -> 
# 588 "g_tactic.mlg"
            let { CAst.loc = loc; v = id } = lid in
          TacAtom (CAst.make ?loc @@ TacAssert (false,true,None,Some (CAst.make ?loc @@ IntroNaming (IntroIdentifier id)),c)) 
                                            ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("eremember"))))),
                                                                    (Extend.Aentry constr)),
                                                       (Extend.Aentry as_name)),
                                          (Extend.Aentry eqn_ipat)),
                             (Extend.Aentry clause_dft_all)),
                (fun p e na c _ loc -> 
# 583 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacLetTac (true,na,c,p,false,e)) 
                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("remember"))))),
                                                                    (Extend.Aentry constr)),
                                                       (Extend.Aentry as_name)),
                                          (Extend.Aentry eqn_ipat)),
                             (Extend.Aentry clause_dft_all)),
                (fun p e na c _ loc -> 
# 580 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacLetTac (false,na,c,p,false,e)) 
                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("eset"))))),
                                                       (Extend.Aentry constr)),
                                          (Extend.Aentry as_name)),
                             (Extend.Aentry clause_dft_concl)),
                (fun p na c _ loc -> 
# 577 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacLetTac (true,na,c,p,true,None)) 
                                     ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("eset"))))),
                                          (Extend.Aentry bindings_with_parameters)),
                             (Extend.Aentry clause_dft_concl)),
                (fun p bl _ loc -> 
# 575 "g_tactic.mlg"
            let (id,c) = bl in TacAtom (CAst.make ~loc @@ TacLetTac (true,Names.Name id,c,p,true,None)) 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("set"))))),
                                                       (Extend.Aentry constr)),
                                          (Extend.Aentry as_name)),
                             (Extend.Aentry clause_dft_concl)),
                (fun p na c _ loc -> 
# 573 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacLetTac (false,na,c,p,true,None)) 
                                     ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("set"))))),
                                          (Extend.Aentry bindings_with_parameters)),
                             (Extend.Aentry clause_dft_concl)),
                (fun p bl _ loc -> 
# 571 "g_tactic.mlg"
            let (id,c) = bl in TacAtom (CAst.make ~loc @@ TacLetTac (false,Names.Name.Name id,c,p,true,None)) 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("epose"))))),
                                          (Extend.Aentry constr)),
                             (Extend.Aentry as_name)),
                (fun na b _ loc -> 
# 569 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacLetTac (true,na,b,Locusops.nowhere,true,None)) 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("epose"))))),
                             (Extend.Aentry bindings_with_parameters)),
                (fun bl _ loc -> 
# 567 "g_tactic.mlg"
            let (id,b) = bl in TacAtom (CAst.make ~loc @@ TacLetTac (true,Names.Name id,b,Locusops.nowhere,true,None)) 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("pose"))))),
                                          (Extend.Aentry constr)),
                             (Extend.Aentry as_name)),
                (fun na b _ loc -> 
# 565 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacLetTac (false,na,b,Locusops.nowhere,true,None)) 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("pose"))))),
                             (Extend.Aentry bindings_with_parameters)),
                (fun bl _ loc -> 
# 563 "g_tactic.mlg"
            let (id,b) = bl in TacAtom (CAst.make ~loc @@ TacLetTac (false,Names.Name.Name id,b,Locusops.nowhere,true,None)) 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("cofix")))),
                                                       (Extend.Aentry ident)),
                                          (Extend.Atoken (Tok.PKEYWORD ("with")))),
                             (Extend.Alist1 (Extend.Aentry cofixdecl))),
                (fun fd _ id _ loc -> 
# 560 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacMutualCofix (id,List.map mk_cofix_tac fd)) 
                                      ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                                    (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PKEYWORD ("fix")))),
                                                                    (Extend.Aentry ident)),
                                                       (Extend.Aentry natural)),
                                          (Extend.Atoken (Tok.PKEYWORD ("with")))),
                             (Extend.Alist1 (Extend.Aentry fixdecl))),
                (fun fd _ n id _ loc -> 
# 558 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacMutualFix (id,n,List.map mk_fix_tac fd)) 
                                        ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("ecase"))))),
                             (Extend.Aentry induction_clause_list)),
                (fun icl _ loc -> 
# 556 "g_tactic.mlg"
                                                        TacAtom (CAst.make ~loc @@ mkTacCase true icl) 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("case"))))),
                             (Extend.Aentry induction_clause_list)),
                (fun icl _ loc -> 
# 555 "g_tactic.mlg"
                                                       TacAtom (CAst.make ~loc @@ mkTacCase false icl) 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("eelim"))))),
                                          (Extend.Aentry constr_with_bindings_arg)),
                             (Extend.Aopt (Extend.Aentry eliminator))),
                (fun el cl _ loc -> 
# 554 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacElim (true,cl,el)) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("elim"))))),
                                          (Extend.Aentry constr_with_bindings_arg)),
                             (Extend.Aopt (Extend.Aentry eliminator))),
                (fun el cl _ loc -> 
# 552 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacElim (false,cl,el)) 
                                    ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("simple"))))),
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("eapply"))))),
                                          (Extend.Alist1sep ((Extend.Aentry constr_with_bindings_arg), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                             (Extend.Aentry in_hyp_as)),
                (fun inhyp cl _ _ loc -> 
# 550 "g_tactic.mlg"
                                 TacAtom (CAst.make ~loc @@ TacApply (false,true,cl,inhyp)) 
                                         ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("simple"))))),
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("apply"))))),
                                          (Extend.Alist1sep ((Extend.Aentry constr_with_bindings_arg), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                             (Extend.Aentry in_hyp_as)),
                (fun inhyp cl _ _ loc -> 
# 547 "g_tactic.mlg"
                                 TacAtom (CAst.make ~loc @@ TacApply (false,false,cl,inhyp)) 
                                         ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("eapply"))))),
                                          (Extend.Alist1sep ((Extend.Aentry constr_with_bindings_arg), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                             (Extend.Aentry in_hyp_as)),
                (fun inhyp cl _ loc -> 
# 544 "g_tactic.mlg"
                                 TacAtom (CAst.make ~loc @@ TacApply (true,true,cl,inhyp)) 
                                       ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("apply"))))),
                                          (Extend.Alist1sep ((Extend.Aentry constr_with_bindings_arg), (Extend.Atoken (Tok.PKEYWORD (",")))))),
                             (Extend.Aentry in_hyp_as)),
                (fun inhyp cl _ loc -> 
# 542 "g_tactic.mlg"
                                 TacAtom (CAst.make ~loc @@ TacApply (true,false,cl,inhyp)) 
                                       ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("eintros"))))),
                (fun _ loc -> 
# 539 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacIntroPattern (true,[CAst.make ~loc @@IntroForthcoming false])) 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("eintros"))))),
                             (Extend.Aentry ne_intropatterns)),
                (fun pl _ loc -> 
# 537 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacIntroPattern (true,pl)) 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("intros"))))),
                (fun _ loc -> 
# 535 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacIntroPattern (false,[CAst.make ~loc @@IntroForthcoming false])) 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("intros"))))),
                             (Extend.Aentry ne_intropatterns)),
                (fun pl _ loc -> 
# 533 "g_tactic.mlg"
            TacAtom (CAst.make ~loc @@ TacIntroPattern (false,pl)) 
                                 ))])])
        in ()

