(lang dune 3.15)

(name cobs)

(generate_opam_files true)

(source
 (github Merlin04/ocaml-cobs))

(authors "Merlin04")

(maintainers "Merlin04")

(license MIT)

(documentation https://github.com/Merlin04/ocaml-cobs)

(package
 (name cobs)
 (synopsis "Consistent Overhead Byte Stuffing (COBS) encoding and decoding")
 (description "A lightweight Consistent Overhead Byte Stuffing (COBS) encoder and decoder written in pure OCaml with no dependencies")
 (depends (ocaml (>= 4.08)) (ocaml ( and :with-test (>= 4.13))) dune (ounit2 :with-test)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
