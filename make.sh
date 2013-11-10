./clean.sh
ocamlc -g -c jlite_structs.ml
ocamlyacc jlite_parser.mly
ocamlc -g -c jlite_parser.mli
ocamllex jlite_lexer.mll
ocamlc -g -c jlite_lexer.ml
ocamlc -g -c jlite_parser.ml
ocamlc -g -c ir3_structs.ml
ocamlc -g -c jlite_annotatedtyping.ml
ocamlc -g -c jlite_toir3.ml
ocamlc -g -c arm_structs.ml
ocamlc -g -c jlite_arm.ml
ocamlc -g -c jlite_main.ml
ocamlc -g -o program jlite_structs.cmo jlite_annotatedtyping.cmo jlite_toir3.cmo jlite_lexer.cmo jlite_parser.cmo ir3_structs.cmo arm_structs.cmo jlite_arm.cmo jlite_main.cmo
