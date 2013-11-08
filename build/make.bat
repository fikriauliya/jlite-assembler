rm main.cmo 2> null
rm rez.exe 2> null
ocamlc -g -c -o utils.cmo ../utils.ml
ocamlc -g -c -o jlite_structs.cmo ../jlite_structs.ml
ocamlyacc ../jlite_parser.mly 2> yaccout.txt
mv ../jlite_parser.ml jlite_parser.ml
mv ../jlite_parser.mli jlite_parser.mli
ocamlc -g -c jlite_parser.mli
ocamllex -o jlite_lexer.ml ../jlite_lexer.mll > lexout.txt
ocamlc -g -c jlite_lexer.ml
ocamlc -g -c jlite_parser.ml
ocamlc -g -o ir3_structs.cmo -c ../ir3_structs.ml
ocamlc -g -o jlite_annotatedtyping.cmo -c ../jlite_annotatedtyping.ml
ocamlc -g -o jlite_toir3.cmo -c ../jlite_toir3.ml
ocamlc -g -o arm_structs.cmo -c ../arm_structs.ml
ocamlc -g -o jlite_arm.cmo -c ../jlite_arm.ml
ocamlc -g -o jlite_main.cmo -c ../jlite_main.ml
ocamlc -g -o rez.exe utils.cmo jlite_structs.cmo jlite_annotatedtyping.cmo jlite_lexer.cmo jlite_parser.cmo ir3_structs.cmo jlite_toir3.cmo arm_structs.cmo jlite_arm.cmo jlite_main.cmo