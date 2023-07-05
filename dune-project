(lang dune 2.0)

(generate_opam_files true)

(name zelang)

(version 0.1.0)

(authors "Matheus Bernardo")

(license MIT)

(source
 (uri "git+https://github.com/matheussbernardo/monadministranto.git"))

(maintainers "matheussbernardo@gmail.com")

(homepage "https://github.com/matheussbernardo/monadministranto")

(bug_reports "https://github.com/matheussbernardo/monadministranto/issues")

(documentation "https://github.com/matheussbernardo/monadministranto")

(package
 (name zelang)
 (synopsis "ZE LANG")
 (description "ZE LANG")
 (depends
  (dune
   (>= 3.9.0))
  (ocaml
   (>= 5.0.0))
  (core
   (>= v0.16.1))
  (ppx_show
   (>= 0.2.1))
  ;; Dev dependencies
  (utop :dev)
  (merlin :dev)
  (ocamlformat :dev)
  (ocp-indent :dev)))
