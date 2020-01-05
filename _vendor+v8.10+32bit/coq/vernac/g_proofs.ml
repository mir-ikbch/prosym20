
# 11 "g_proofs.mlg"
 

open Glob_term
open Constrexpr
open Vernacexpr
open Hints
open Proof_global

open Pcoq
open Pcoq.Prim
open Pcoq.Constr
open Pvernac.Vernac_

let thm_token = G_vernac.thm_token

let hint = Entry.create "hint"

let warn_deprecated_focus =
  CWarnings.create ~name:"deprecated-focus" ~category:"deprecated"
         (fun () ->
           Pp.strbrk
             "The Focus command is deprecated; use bullets or focusing brackets instead"
         )

let warn_deprecated_focus_n n =
  CWarnings.create ~name:"deprecated-focus" ~category:"deprecated"
         (fun () ->
           Pp.(str "The Focus command is deprecated;" ++ spc ()
               ++ str "use '" ++ int n ++ str ": {' instead")
         )

let warn_deprecated_unfocus =
  CWarnings.create ~name:"deprecated-unfocus" ~category:"deprecated"
         (fun () -> Pp.strbrk "The Unfocus command is deprecated")



let _ = let opt_hintbases = Pcoq.Entry.create "opt_hintbases"
        and reference_or_constr = Pcoq.Entry.create "reference_or_constr"
        and constr_body = Pcoq.Entry.create "constr_body"
        and mode = Pcoq.Entry.create "mode"
        in
        let () =
        Pcoq.grammar_extend opt_hintbases None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PKEYWORD (":")))),
                              (Extend.Alist1 (Extend.Arules [Extend.Rules 
                                                            (Extend.NextNoRec 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (Tok.PIDENT (None)))),
                                                            (fun id loc -> 
                                                            
# 54 "g_proofs.mlg"
                                      id 
                                                            ))]))),
                 (fun l _ loc -> 
# 54 "g_proofs.mlg"
                                                  l 
                                 ));
                Extend.Rule (Extend.Stop, (fun loc -> 
# 53 "g_proofs.mlg"
           [] 
                                                      ))])])
        in let () =
        Pcoq.grammar_extend command None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                        (Extend.Atoken (Tok.PIDENT (Some
                                                        ("Hint"))))),
                                           (Extend.Aentry hint)),
                              (Extend.Aentry opt_hintbases)),
                 (fun dbnames h _ loc -> 
# 107 "g_proofs.mlg"
            VernacHints (dbnames, h) 
                                         ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Remove"))))),
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Hints"))))),
                                          (Extend.Alist1 (Extend.Aentry global))),
                             (Extend.Aentry opt_hintbases)),
                (fun dbnames ids _ _ loc -> 
# 105 "g_proofs.mlg"
            VernacRemoveHints (dbnames, ids) 
                                            ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Create"))))),
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("HintDb"))))),
                                          (Extend.Atoken (Tok.PIDENT (None)))),
                             (Extend.Arules [Extend.Rules (Extend.Stop, (fun
                                                          loc -> 
# 102 "g_proofs.mlg"
                                                                false 
                                                                 ));
                                            Extend.Rules (Extend.NextNoRec 
                                                         (Extend.Stop,
                                                         (Extend.Atoken (Tok.PKEYWORD ("discriminated")))),
                                                         (fun _ loc -> 
                                                         
# 102 "g_proofs.mlg"
                                                  true 
                                                         ))])),
                (fun b id _ _ loc -> 
# 103 "g_proofs.mlg"
              VernacCreateHintDb (id, b) 
                                     ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Guarded"))))),
                (fun _ loc -> 
# 99 "g_proofs.mlg"
                             VernacCheckGuard 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Show"))))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Match"))))),
                             (Extend.Aentry reference)),
                (fun id _ _ loc -> 
# 98 "g_proofs.mlg"
                                                         VernacShow (ShowMatch id) 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Show"))))),
                             (Extend.Atoken (Tok.PIDENT (Some ("Intros"))))),
                (fun _ _ loc -> 
# 97 "g_proofs.mlg"
                                          VernacShow (ShowIntros true) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Show"))))),
                             (Extend.Atoken (Tok.PIDENT (Some ("Intro"))))),
                (fun _ _ loc -> 
# 96 "g_proofs.mlg"
                                         VernacShow (ShowIntros false) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Show"))))),
                             (Extend.Atoken (Tok.PIDENT (Some ("Proof"))))),
                (fun _ _ loc -> 
# 95 "g_proofs.mlg"
                                         VernacShow ShowProof 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Show"))))),
                             (Extend.Atoken (Tok.PIDENT (Some
                             ("Conjectures"))))),
                (fun _ _ loc -> 
# 94 "g_proofs.mlg"
                                               VernacShow ShowProofNames 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Show"))))),
                             (Extend.Atoken (Tok.PIDENT (Some
                             ("Universes"))))),
                (fun _ _ loc -> 
# 93 "g_proofs.mlg"
                                             VernacShow ShowUniverses 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Show"))))),
                             (Extend.Atoken (Tok.PIDENT (Some
                             ("Existentials"))))),
                (fun _ _ loc -> 
# 92 "g_proofs.mlg"
                                                VernacShow ShowExistentials 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Show"))))),
                             (Extend.Atoken (Tok.PIDENT (Some ("Script"))))),
                (fun _ _ loc -> 
# 91 "g_proofs.mlg"
                                          VernacShow ShowScript 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Show"))))),
                             (Extend.Aentry ident)),
                (fun id _ loc -> 
# 90 "g_proofs.mlg"
                                      VernacShow (ShowGoal (GoalId id)) 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Show"))))),
                             (Extend.Aentry natural)),
                (fun n _ loc -> 
# 89 "g_proofs.mlg"
                                       VernacShow (ShowGoal (NthGoal n)) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Show"))))),
                (fun _ loc -> 
# 88 "g_proofs.mlg"
                          VernacShow (ShowGoal OpenSubgoals) 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Unfocused"))))),
                (fun _ loc -> 
# 87 "g_proofs.mlg"
                               VernacUnfocused 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Unfocus"))))),
                (fun _ loc -> 
# 85 "g_proofs.mlg"
           warn_deprecated_unfocus ~loc ();
         VernacUnfocus 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Focus"))))),
                             (Extend.Aentry natural)),
                (fun n _ loc -> 
# 82 "g_proofs.mlg"
           warn_deprecated_focus_n n ~loc ();
         VernacFocus (Some n) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Focus"))))),
                (fun _ loc -> 
# 79 "g_proofs.mlg"
           warn_deprecated_focus ~loc ();
         VernacFocus None 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Undo"))))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("To"))))),
                             (Extend.Aentry natural)),
                (fun n _ _ loc -> 
# 77 "g_proofs.mlg"
                                                   VernacUndoTo n 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Undo"))))),
                             (Extend.Aentry natural)),
                (fun n _ loc -> 
# 76 "g_proofs.mlg"
                                       VernacUndo n 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Undo"))))),
                (fun _ loc -> 
# 75 "g_proofs.mlg"
                          VernacUndo 1 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Restart"))))),
                (fun _ loc -> 
# 74 "g_proofs.mlg"
                             VernacRestart 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Defined"))))),
                             (Extend.Aentry identref)),
                (fun id _ loc -> 
# 73 "g_proofs.mlg"
            VernacEndProof (Proved (Transparent,Some id)) 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Defined"))))),
                (fun _ loc -> 
# 71 "g_proofs.mlg"
                             VernacEndProof (Proved (Transparent,None)) 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Save"))))),
                             (Extend.Aentry identref)),
                (fun id _ loc -> 
# 70 "g_proofs.mlg"
            VernacEndProof (Proved (Opaque, Some id)) 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Qed"))))),
                (fun _ loc -> 
# 68 "g_proofs.mlg"
                         VernacEndProof (Proved (Opaque,None)) 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Admitted"))))),
                (fun _ loc -> 
# 67 "g_proofs.mlg"
                              VernacEndProof Admitted 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Existential"))))),
                                          (Extend.Aentry natural)),
                             (Extend.Aentry constr_body)),
                (fun c n _ loc -> 
# 66 "g_proofs.mlg"
            VernacSolveExistential (n,c) 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Abort"))))),
                             (Extend.Aentry identref)),
                (fun id _ loc -> 
# 64 "g_proofs.mlg"
                                          VernacAbort (Some id) 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Abort"))))),
                             (Extend.Atoken (Tok.PIDENT (Some ("All"))))),
                (fun _ _ loc -> 
# 63 "g_proofs.mlg"
                                        VernacAbortAll 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Abort"))))),
                (fun _ loc -> 
# 62 "g_proofs.mlg"
                           VernacAbort None 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Proof"))))),
                             (Extend.Aentry lconstr)),
                (fun c _ loc -> 
# 61 "g_proofs.mlg"
                                        VernacExactProof c 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Proof"))))),
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Mode"))))),
                             (Extend.Aentry string)),
                (fun mn _ _ loc -> 
# 60 "g_proofs.mlg"
                                                        VernacProofMode mn 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Atoken (Tok.PIDENT (Some
                             ("Proof"))))),
                (fun _ loc -> 
# 59 "g_proofs.mlg"
                           VernacProof (None,None) 
                              ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Goal"))))),
                             (Extend.Aentry lconstr)),
                (fun c _ loc -> 
# 58 "g_proofs.mlg"
          VernacDefinition (Decl_kinds.(NoDischarge, Definition), ((CAst.make ~loc Names.Anonymous), None), ProveBody ([], c)) 
                                ))])])
        in let () =
        Pcoq.grammar_extend reference_or_constr None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop, (Extend.Aentry constr)),
                 (fun c loc -> 
# 111 "g_proofs.mlg"
                       HintsConstr c 
                               ));
                Extend.Rule
                (Extend.Next (Extend.Stop, (Extend.Aentry global)),
                (fun r loc -> 
# 110 "g_proofs.mlg"
                       HintsReference r 
                              ))])])
        in let () =
        Pcoq.grammar_extend hint None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Stop,
                                           (Extend.Atoken (Tok.PIDENT (Some
                                           ("Constructors"))))),
                              (Extend.Alist1 (Extend.Aentry global))),
                 (fun lc _ loc -> 
# 129 "g_proofs.mlg"
                                                     HintsConstructors lc 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Unfold"))))),
                             (Extend.Alist1 (Extend.Aentry global))),
                (fun lqid _ loc -> 
# 128 "g_proofs.mlg"
                                                 HintsUnfold lqid 
                                   ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Mode"))))),
                                          (Extend.Aentry global)),
                             (Extend.Aentry mode)),
                (fun m l _ loc -> 
# 127 "g_proofs.mlg"
                                                HintsMode (l, m) 
                                  ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Opaque"))))),
                             (Extend.Alist1 (Extend.Aentry global))),
                (fun lc _ loc -> 
# 126 "g_proofs.mlg"
                                               HintsTransparency (HintsReferences lc, false) 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Transparent"))))),
                             (Extend.Alist1 (Extend.Aentry global))),
                (fun lc _ loc -> 
# 125 "g_proofs.mlg"
                                                    HintsTransparency (HintsReferences lc, true) 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Constants"))))),
                             (Extend.Atoken (Tok.PIDENT (Some ("Opaque"))))),
                (fun _ _ loc -> 
# 124 "g_proofs.mlg"
                                               HintsTransparency (HintsConstants, false) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Constants"))))),
                             (Extend.Atoken (Tok.PIDENT (Some
                             ("Transparent"))))),
                (fun _ _ loc -> 
# 123 "g_proofs.mlg"
                                                    HintsTransparency (HintsConstants, true) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Variables"))))),
                             (Extend.Atoken (Tok.PIDENT (Some ("Opaque"))))),
                (fun _ _ loc -> 
# 122 "g_proofs.mlg"
                                               HintsTransparency (HintsVariables, false) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Variables"))))),
                             (Extend.Atoken (Tok.PIDENT (Some
                             ("Transparent"))))),
                (fun _ _ loc -> 
# 121 "g_proofs.mlg"
                                                    HintsTransparency (HintsVariables, true) 
                                ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PIDENT (Some
                                          ("Immediate"))))),
                             (Extend.Alist1 (Extend.Aentry reference_or_constr))),
                (fun lc _ loc -> 
# 120 "g_proofs.mlg"
                                                               HintsImmediate lc 
                                 ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Resolve"))))),
                                                       (Extend.Atoken (Tok.PKEYWORD ("<-")))),
                                          (Extend.Alist1 (Extend.Aentry global))),
                             (Extend.Aopt (Extend.Aentry natural))),
                (fun n lc _ _ loc -> 
# 119 "g_proofs.mlg"
            HintsResolveIFF (false, lc, n) 
                                     ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                                    (Extend.Atoken (Tok.PIDENT (Some
                                                                    ("Resolve"))))),
                                                       (Extend.Atoken (Tok.PKEYWORD ("->")))),
                                          (Extend.Alist1 (Extend.Aentry global))),
                             (Extend.Aopt (Extend.Aentry natural))),
                (fun n lc _ _ loc -> 
# 117 "g_proofs.mlg"
            HintsResolveIFF (true, lc, n) 
                                     ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Next (Extend.Stop,
                                                       (Extend.Atoken (Tok.PIDENT (Some
                                                       ("Resolve"))))),
                                          (Extend.Alist1 (Extend.Aentry reference_or_constr))),
                             (Extend.Aentry hint_info)),
                (fun info lc _ loc -> 
# 115 "g_proofs.mlg"
            HintsResolve (List.map (fun x -> (info, true, x)) lc) 
                                      ))])])
        in let () =
        Pcoq.grammar_extend constr_body None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Next (Extend.Next (Extend.Next 
                                                        (Extend.Stop,
                                                        (Extend.Atoken (Tok.PKEYWORD (":")))),
                                                        (Extend.Aentry lconstr)),
                                           (Extend.Atoken (Tok.PKEYWORD (":=")))),
                              (Extend.Aentry lconstr)),
                 (fun c _ t _ loc -> 
# 133 "g_proofs.mlg"
                                                 CAst.make ~loc @@ CCast(c,CastConv t) 
                                     ));
                Extend.Rule
                (Extend.Next (Extend.Next (Extend.Stop,
                                          (Extend.Atoken (Tok.PKEYWORD (":=")))),
                             (Extend.Aentry lconstr)),
                (fun c _ loc -> 
# 132 "g_proofs.mlg"
                               c 
                                ))])])
        in let () =
        Pcoq.grammar_extend mode None
        (None, [(None, None,
                [Extend.Rule
                 (Extend.Next (Extend.Stop,
                              (Extend.Alist1 (Extend.Arules [Extend.Rules 
                                                            (Extend.NextNoRec 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (Tok.PKEYWORD ("-")))),
                                                            (fun _ loc -> 
                                                            
# 138 "g_proofs.mlg"
                             ModeOutput 
                                                            ));
                                                            Extend.Rules 
                                                            (Extend.NextNoRec 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (Tok.PKEYWORD ("!")))),
                                                            (fun _ loc -> 
                                                            
# 137 "g_proofs.mlg"
                             ModeNoHeadEvar 
                                                            ));
                                                            Extend.Rules 
                                                            (Extend.NextNoRec 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (Tok.PKEYWORD ("+")))),
                                                            (fun _ loc -> 
                                                            
# 136 "g_proofs.mlg"
                             ModeInput 
                                                            ))]))),
                 (fun l loc -> 
# 138 "g_proofs.mlg"
                                                 l 
                               ))])])
        in ()

