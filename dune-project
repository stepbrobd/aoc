(lang dune 3.11)
(generate_opam_files true)

(name aoc)
(license MIT)
(source (github stepbrobd/aoc))

(authors "Yifei Sun <ysun@hey.com>")
(maintainers "Yifei Sun <ysun@hey.com>")

(package
 (name aoc)
 (depends core curl dune ocaml))
