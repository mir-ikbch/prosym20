
# 11 "g_toplevel.mlg"
 
open Pcoq
open Pcoq.Prim
open Vernacexpr

(* Vernaculars specific to the toplevel *)
type vernac_toplevel =
  | VernacBacktrack of int * int * int
  | VernacDrop
  | VernacQuit
  | VernacControl of vernac_control

module Toplevel_ : sig
  val vernac_toplevel : vernac_toplevel CAst.t option Entry.t
end = struct
  let gec_vernac s = Entry.create ("toplevel:" ^ s)
  let vernac_toplevel = gec_vernac "vernac_toplevel"
end

open Toplevel_



let _ = let () =
        Pcoq.grammar_extend vernac_toplevel None
        (Some
        (Gramlib.Gramext.First), [(None, None,
                                  [Extend.Rule
                                   (Extend.Next (Extend.Stop,
                                                (Extend.Aentry Pvernac.Vernac_.main_entry)),
                                   (fun cmd loc -> 
# 42 "g_toplevel.mlg"
                match cmd with
              | None -> None
              | Some {CAst.loc; v} -> Some (CAst.make ?loc (VernacControl v)) 
                                                   ));
                                  Extend.Rule
                                  (Extend.Next (Extend.Next (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Next 
                                                            (Extend.Stop,
                                                            (Extend.Atoken (Tok.PIDENT (Some
                                                            ("Backtrack"))))),
                                                            (Extend.Aentry natural)),
                                                            (Extend.Aentry natural)),
                                                            (Extend.Aentry natural)),
                                               (Extend.Atoken (Tok.PKEYWORD (".")))),
                                  (fun _ p m n _ loc -> 
# 40 "g_toplevel.mlg"
          Some (CAst.make (VernacBacktrack (n,m,p))) 
                                                        ));
                                  Extend.Rule
                                  (Extend.Next (Extend.Next (Extend.Stop,
                                                            (Extend.Atoken (Tok.PIDENT (Some
                                                            ("Quit"))))),
                                               (Extend.Atoken (Tok.PKEYWORD (".")))),
                                  (fun _ _ loc -> 
# 38 "g_toplevel.mlg"
                               Some (CAst.make VernacQuit) 
                                                  ));
                                  Extend.Rule
                                  (Extend.Next (Extend.Next (Extend.Stop,
                                                            (Extend.Atoken (Tok.PIDENT (Some
                                                            ("Drop"))))),
                                               (Extend.Atoken (Tok.PKEYWORD (".")))),
                                  (fun _ _ loc -> 
# 37 "g_toplevel.mlg"
                               Some (CAst.make VernacDrop) 
                                                  ))])])
        in ()


# 50 "g_toplevel.mlg"
 

let vernac_toplevel pm =
  Pvernac.Unsafe.set_tactic_entry pm;
  vernac_toplevel



