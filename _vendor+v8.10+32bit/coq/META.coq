package "clib" (
  directory = "clib"
  description = "Coq's Utility Library [general purpose]"
  requires = "str threads unix"
  archive(byte) = "clib.cma"
  archive(native) = "clib.cmxa"
  plugin(byte) = "clib.cma"
  plugin(native) = "clib.cmxs"
)
package "config" (
  directory = "config"
  description = "Coq Configuration Variables"
  requires = ""
  archive(byte) = "config.cma"
  archive(native) = "config.cmxa"
  plugin(byte) = "config.cma"
  plugin(native) = "config.cmxs"
)
package "engine" (
  directory = "engine"
  description = "Coq's Tactic Engine"
  requires = "coq.library"
  archive(byte) = "engine.cma"
  archive(native) = "engine.cmxa"
  plugin(byte) = "engine.cma"
  plugin(native) = "engine.cmxs"
)
package "gramlib" (
  directory = "gramlib"
  description = ""
  requires = "coq.lib"
  archive(byte) = "gramlib.cma"
  archive(native) = "gramlib.cmxa"
  plugin(byte) = "gramlib.cma"
  plugin(native) = "gramlib.cmxs"
)
package "interp" (
  directory = "interp"
  description = "Coq's Syntactic Interpretation for AST [notations, implicits]"
  requires = "coq.pretyping"
  archive(byte) = "interp.cma"
  archive(native) = "interp.cmxa"
  plugin(byte) = "interp.cma"
  plugin(native) = "interp.cmxs"
)
package "kernel" (
  directory = "kernel"
  description = "The Coq Kernel"
  requires = "coq.lib coq.vm dynlink"
  archive(byte) = "kernel.cma"
  archive(native) = "kernel.cmxa"
  plugin(byte) = "kernel.cma"
  plugin(native) = "kernel.cmxs"
)
package "lib" (
  directory = "lib"
  description = "Coq's Utility Library [coq-specific]"
  requires = "coq.clib coq.config"
  archive(byte) = "lib.cma"
  archive(native) = "lib.cmxa"
  plugin(byte) = "lib.cma"
  plugin(native) = "lib.cmxs"
)
package "library" (
  directory = "library"
  description = "Coq's Loadable Libraries (vo) Support"
  requires = "coq.kernel"
  archive(byte) = "library.cma"
  archive(native) = "library.cmxa"
  plugin(byte) = "library.cma"
  plugin(native) = "library.cmxs"
)
package "parsing" (
  directory = "parsing"
  description = ""
  requires = "coq.gramlib coq.proofs"
  archive(byte) = "parsing.cma"
  archive(native) = "parsing.cmxa"
  plugin(byte) = "parsing.cma"
  plugin(native) = "parsing.cmxs"
)
package "plugins" (
  directory = "plugins"
  package "btauto" (
    directory = "btauto"
    description = "Coq's btauto plugin"
    requires = "coq.plugins.ltac"
    archive(byte) = "btauto_plugin.cma"
    archive(native) = "btauto_plugin.cmxa"
    plugin(byte) = "btauto_plugin.cma"
    plugin(native) = "btauto_plugin.cmxs"
  )
  package "cc" (
    directory = "cc"
    description = "Coq's congruence closure plugin"
    requires = "coq.plugins.ltac"
    archive(byte) = "cc_plugin.cma"
    archive(native) = "cc_plugin.cmxa"
    plugin(byte) = "cc_plugin.cma"
    plugin(native) = "cc_plugin.cmxs"
  )
  package "derive" (
    directory = "derive"
    description = "Coq's derive plugin"
    requires = "coq.plugins.ltac"
    archive(byte) = "derive_plugin.cma"
    archive(native) = "derive_plugin.cmxa"
    plugin(byte) = "derive_plugin.cma"
    plugin(native) = "derive_plugin.cmxs"
  )
  package "extraction" (
    directory = "extraction"
    description = "Coq's extraction plugin"
    requires = "coq.plugins.ltac num"
    archive(byte) = "extraction_plugin.cma"
    archive(native) = "extraction_plugin.cmxa"
    plugin(byte) = "extraction_plugin.cma"
    plugin(native) = "extraction_plugin.cmxs"
  )
  package "firstorder" (
    directory = "firstorder"
    description = "Coq's first order logic solver plugin"
    requires = "coq.plugins.ltac"
    archive(byte) = "ground_plugin.cma"
    archive(native) = "ground_plugin.cmxa"
    plugin(byte) = "ground_plugin.cma"
    plugin(native) = "ground_plugin.cmxs"
  )
  package "fourier" (
    directory = "fourier"
    description = "Coq's fourier plugin"
    requires = "coq.plugins.ltac"
    archive(byte) = "fourier_plugin.cma"
    archive(native) = "fourier_plugin.cmxa"
    plugin(byte) = "fourier_plugin.cma"
    plugin(native) = "fourier_plugin.cmxs"
  )
  package "funind" (
    directory = "funind"
    description = "Coq's functional induction plugin"
    requires = "coq.plugins.extraction"
    archive(byte) = "recdef_plugin.cma"
    archive(native) = "recdef_plugin.cmxa"
    plugin(byte) = "recdef_plugin.cma"
    plugin(native) = "recdef_plugin.cmxs"
  )
  package "int63_syntax" (
    directory = "int63_syntax"
    description = "Coq syntax plugin: int63"
    requires = "coq.vernac"
    archive(byte) = "int63_syntax_plugin.cma"
    archive(native) = "int63_syntax_plugin.cmxa"
    plugin(byte) = "int63_syntax_plugin.cma"
    plugin(native) = "int63_syntax_plugin.cmxs"
  )
  package "ltac" (
    directory = "ltac"
    description = "Coq's LTAC tactic language"
    requires = "coq.stm"
    archive(byte) = "ltac_plugin.cma"
    archive(native) = "ltac_plugin.cmxa"
    plugin(byte) = "ltac_plugin.cma"
    plugin(native) = "ltac_plugin.cmxs"
  )
  package "micromega" (
    directory = "micromega"
    description = "Coq's micromega plugin"
    requires = "coq.plugins.ltac num"
    archive(byte) = "micromega_plugin.cma"
    archive(native) = "micromega_plugin.cmxa"
    plugin(byte) = "micromega_plugin.cma"
    plugin(native) = "micromega_plugin.cmxs"
  )
  package "nsatz" (
    directory = "nsatz"
    description = "Coq's nsatz solver plugin"
    requires = "coq.plugins.ltac num"
    archive(byte) = "nsatz_plugin.cma"
    archive(native) = "nsatz_plugin.cmxa"
    plugin(byte) = "nsatz_plugin.cma"
    plugin(native) = "nsatz_plugin.cmxs"
  )
  package "numeral_notation" (
    directory = "numeral_notation"
    description = "Coq numeral notation plugin"
    requires = "coq.vernac"
    archive(byte) = "numeral_notation_plugin.cma"
    archive(native) = "numeral_notation_plugin.cmxa"
    plugin(byte) = "numeral_notation_plugin.cma"
    plugin(native) = "numeral_notation_plugin.cmxs"
  )
  package "omega" (
    directory = "omega"
    description = "Coq's omega plugin"
    requires = "coq.plugins.ltac"
    archive(byte) = "omega_plugin.cma"
    archive(native) = "omega_plugin.cmxa"
    plugin(byte) = "omega_plugin.cma"
    plugin(native) = "omega_plugin.cmxs"
  )
  package "r_syntax" (
    directory = "r_syntax"
    description = "Coq syntax plugin: reals"
    requires = "coq.vernac"
    archive(byte) = "r_syntax_plugin.cma"
    archive(native) = "r_syntax_plugin.cmxa"
    plugin(byte) = "r_syntax_plugin.cma"
    plugin(native) = "r_syntax_plugin.cmxs"
  )
  package "rtauto" (
    directory = "rtauto"
    description = "Coq's rtauto plugin"
    requires = "coq.plugins.ltac"
    archive(byte) = "rtauto_plugin.cma"
    archive(native) = "rtauto_plugin.cmxa"
    plugin(byte) = "rtauto_plugin.cma"
    plugin(native) = "rtauto_plugin.cmxs"
  )
  package "setoid_ring" (
    directory = "setoid_ring"
    description = "Coq's setoid ring plugin"
    requires = "coq.plugins.ltac"
    archive(byte) = "newring_plugin.cma"
    archive(native) = "newring_plugin.cmxa"
    plugin(byte) = "newring_plugin.cma"
    plugin(native) = "newring_plugin.cmxs"
  )
  package "ssreflect" (
    directory = "ssreflect"
    description = "Coq's ssreflect plugin"
    requires = "coq.plugins.ssrmatching"
    archive(byte) = "ssreflect_plugin.cma"
    archive(native) = "ssreflect_plugin.cmxa"
    plugin(byte) = "ssreflect_plugin.cma"
    plugin(native) = "ssreflect_plugin.cmxs"
  )
  package "ssrmatching" (
    directory = "ssrmatching"
    description = "Coq ssrmatching plugin"
    requires = "coq.plugins.ltac"
    archive(byte) = "ssrmatching_plugin.cma"
    archive(native) = "ssrmatching_plugin.cmxa"
    plugin(byte) = "ssrmatching_plugin.cma"
    plugin(native) = "ssrmatching_plugin.cmxs"
  )
  package "string_notation" (
    directory = "string_notation"
    description = "Coq string notation plugin"
    requires = "coq.vernac"
    archive(byte) = "string_notation_plugin.cma"
    archive(native) = "string_notation_plugin.cmxa"
    plugin(byte) = "string_notation_plugin.cma"
    plugin(native) = "string_notation_plugin.cmxs"
  )
  package "tauto" (
    directory = "tauto"
    description = "Coq's tauto tactic"
    requires = "coq.plugins.ltac"
    archive(byte) = "tauto_plugin.cma"
    archive(native) = "tauto_plugin.cmxa"
    plugin(byte) = "tauto_plugin.cma"
    plugin(native) = "tauto_plugin.cmxs"
  )
  package "tutorial" (
    directory = "tutorial"
    package "p0" (
      directory = "p0"
      description = ""
      requires = "coq.plugins.ltac"
      archive(byte) = "tuto0_plugin.cma"
      archive(native) = "tuto0_plugin.cmxa"
      plugin(byte) = "tuto0_plugin.cma"
      plugin(native) = "tuto0_plugin.cmxs"
    )
    package "p1" (
      directory = "p1"
      description = ""
      requires = "coq.plugins.ltac"
      archive(byte) = "tuto1_plugin.cma"
      archive(native) = "tuto1_plugin.cmxa"
      plugin(byte) = "tuto1_plugin.cma"
      plugin(native) = "tuto1_plugin.cmxs"
    )
    package "p2" (
      directory = "p2"
      description = ""
      requires = "coq.plugins.ltac"
      archive(byte) = "tuto2_plugin.cma"
      archive(native) = "tuto2_plugin.cmxa"
      plugin(byte) = "tuto2_plugin.cma"
      plugin(native) = "tuto2_plugin.cmxs"
    )
    package "p3" (
      directory = "p3"
      description = ""
      requires = "coq.plugins.ltac"
      archive(byte) = "tuto3_plugin.cma"
      archive(native) = "tuto3_plugin.cmxa"
      plugin(byte) = "tuto3_plugin.cma"
      plugin(native) = "tuto3_plugin.cmxs"
    )
  )
)
package "pretyping" (
  directory = "pretyping"
  description = "Coq's Type Inference Component (Pretyper)"
  requires = "coq.engine"
  archive(byte) = "pretyping.cma"
  archive(native) = "pretyping.cmxa"
  plugin(byte) = "pretyping.cma"
  plugin(native) = "pretyping.cmxs"
)
package "printing" (
  directory = "printing"
  description = "Coq's Term Pretty Printing Library"
  requires = "coq.parsing coq.proofs"
  archive(byte) = "printing.cma"
  archive(native) = "printing.cmxa"
  plugin(byte) = "printing.cma"
  plugin(native) = "printing.cmxs"
)
package "proofs" (
  directory = "proofs"
  description = "Coq's Higher-level Refinement Proof Engine and Top-level Proof Structure"
  requires = "coq.interp"
  archive(byte) = "proofs.cma"
  archive(native) = "proofs.cmxa"
  plugin(byte) = "proofs.cma"
  plugin(native) = "proofs.cmxs"
)
package "stm" (
  directory = "stm"
  description = "Coq's Document Manager and Proof Checking Scheduler"
  requires = "coq.vernac"
  archive(byte) = "stm.cma"
  archive(native) = "stm.cmxa"
  plugin(byte) = "stm.cma"
  plugin(native) = "stm.cmxs"
)
package "tactics" (
  directory = "tactics"
  description = "Coq's Core Tactics [ML implementation]"
  requires = "coq.printing"
  archive(byte) = "tactics.cma"
  archive(native) = "tactics.cmxa"
  plugin(byte) = "tactics.cma"
  plugin(native) = "tactics.cmxs"
)
package "top_printers" (
  directory = "top_printers"
  description = "Coq's Debug Printers"
  requires = "coq.plugins.ltac coq.toplevel"
  archive(byte) = "top_printers.cma"
  archive(native) = "top_printers.cmxa"
  plugin(byte) = "top_printers.cma"
  plugin(native) = "top_printers.cmxs"
)
package "toplevel" (
  directory = "toplevel"
  description = "Coq's Interactive Shell [terminal-based]"
  requires = "coq.stm num"
  archive(byte) = "toplevel.cma"
  archive(native) = "toplevel.cmxa"
  plugin(byte) = "toplevel.cma"
  plugin(native) = "toplevel.cmxs"
)
package "vernac" (
  directory = "vernac"
  description = "Coq's Vernacular Language"
  requires = "coq.parsing coq.tactics"
  archive(byte) = "vernac.cma"
  archive(native) = "vernac.cmxa"
  plugin(byte) = "vernac.cma"
  plugin(native) = "vernac.cmxs"
)
package "vm" (
  directory = "vm"
  description = "Coq's Kernel Abstract Reduction Machine [C implementation]"
  requires = ""
  archive(byte) = "byterun.cma"
  archive(native) = "byterun.cmxa"
  plugin(byte) = "byterun.cma"
  plugin(native) = "byterun.cmxs"
)
