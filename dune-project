(lang dune 3.4)
(name qam)

(using menhir 2.1)

(generate_opam_files true)

(source (github sonvule59/QAM-OCAML))
(license MIT)
(authors "Son Vu")
(maintainers "Son Vu <vuleson59@gmail.com>")

(package
 (name qam)
 (synopsis "An OCaml implementation of the Quantum Abstract Machine (QAM)")
 (description
  "A toolchain for the Quantum Abstract Machine of Li, Chang, Cleaveland, Zhu\nand Wu (arXiv:2402.13469): a parser and reduction interpreter for QAM\nmembranes/processes, a normalize-then-canonicalize equivalence checker, a\ncompiler from QAM configurations to flat OpenQASM 2.0, and a built-in\nstatevector simulator used to verify compiled protocols (quantum\nteleportation, bit-commitment, superdense coding) end-to-end.")
 (depends
  (ocaml (>= 4.14))
  dune
  (menhir (>= 20211012))
  (ounit2 :with-test)))
