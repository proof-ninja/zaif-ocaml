(lang dune 3.4)

(name zaif)

(version 0.1.dev)

(generate_opam_files true)

(source
 (github proof-ninja/zaif-ocaml))

(authors "Yoshihiro Imai")

(maintainers "Yoshihiro Imai")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name zaif)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune cohttp lwt cohttp-lwt-unix yojson hacl-star hex dolog)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
