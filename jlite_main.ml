
(* ===================================================== *)
(* ============== CS41212 Compiler Design ============== *)
(* ===================================================== *)

open Printf 

open Jlite_annotatedtyping
open Jlite_arm

open Ir3_structs


let source_files = ref []

let usage_msg = Sys.argv.(1) ^ " <source files>"

let set_source_file arg = source_files := arg :: !source_files

let parse_file file_name = 
  let org_in_chnl = open_in file_name in
  let lexbuf = Lexing.from_channel org_in_chnl in
  try
    print_string "Parsing...\n" ;
    print_string file_name ;
    print_string "\n" ;
    let prog =  Jlite_parser.input (Jlite_lexer.token file_name) lexbuf in
    close_in org_in_chnl;
    prog 
  with
  (*  End_of_file -> exit 0	  *)
  | exn ->
      begin
        let curr = lexbuf.Lexing.lex_curr_p in
        let line, cnum, tok =
          curr.Lexing.pos_lnum,
            curr.Lexing.pos_cnum - curr.Lexing.pos_bol,
          Lexing.lexeme lexbuf in
    let _ = prerr_string ("Error at ["^(string_of_int line)
                 ^" : "^(string_of_int cnum)
                 ^"] on '"^tok^"'\n") in
    raise (Failure (Printexc.to_string exn))
      end

let process file_name prog  = 
  begin
    print_string (Jlite_structs.string_of_jlite_program prog);
    let typedprog = (Jlite_annotatedtyping.type_check_jlite_program prog) in
    print_string (Jlite_structs.string_of_jlite_program typedprog);
    let ir3prog = Jlite_toir3.jlite_program_to_IR3 typedprog in
    print_string (Ir3_structs.string_of_ir3_program ir3prog);
    
    let armprog = (Jlite_arm.ir3_program_to_arm ir3prog) in
    let output = Arm_structs.string_of_arm_prog armprog in
    
    begin
      print_string output;
      
      let oc = open_out "out.s" in
      fprintf oc "%s\n" output;
      close_out oc;
      
    end
  end

let _ = 
  begin
    Arg.parse [] set_source_file usage_msg ;
    match !source_files with
    | [] -> print_string "No file provided \n"
    | x::_-> 
      let prog = parse_file x in
      process x prog
  end


