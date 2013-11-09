open Arm_structs
open Ir3_structs
open Jlite_structs
open Printf

type memory_address_type = int
type liveness_timeline_type = ((id3 * (int * int)) list)
type active_spill_variables_type = 
  (* (active variable set, spill variable set) *)
  ((id3 list) * (id3 list))

(* variable name -> reserved memory address in stack*)
type stack_memory_map_type =
  ((id3 * memory_address_type) list)

(* statement lists, IN block ids, OUT block ids *)
type basic_block_type = {
  stmts: ir3_stmt list;
  mutable in_blocks: int list;
  out_blocks: int list;
}

let println line = begin
  printf "%s\n" line;
end

(*
 * Calculate size of a variable.
 * Each variable occupies 4 bytes.
*)
let calc_var_size ((v_type, _): var_decl3) (clist: (cdata3 list)) =
  match v_type with
  | IntT | BoolT | StringT -> 4
  | ObjectT cname ->
    begin
      let cdata = List.find (fun ((cn, _): cdata3) -> cn = cname) clist in
      let (_, vars) = cdata in
      4 * (List.length vars)
    end
  | _ -> failwith ("calc_object_size: This shouldn't happen")

(* Returns the relative position of a field data in an object of a given type
  TODO
  TODO also take the class as an argument
*)
let get_field_shift (field_name: id3) (cdata: cdata3) = 0

let derive_liveness_timeline (stmts: ir3_stmt list) : liveness_timeline_type = begin
  let basic_blocks_map = Hashtbl.create 100 in
  
  let print_basic_blocks_map () =
    Hashtbl.iter (fun k v ->
      println ("======================================================================");
      println ("Block #" ^ (string_of_int k) ^ ": ");
      println ("In block(s): " ^ (string_of_list v.in_blocks string_of_int ", "));
      println ("Out block(s): " ^ (string_of_list v.out_blocks string_of_int ", "));
      println (string_of_list v.stmts string_of_ir3_stmt "\n");
      println ("======================================================================");
    )
      basic_blocks_map
  in
  (* Hashtbl.add basic_blocks_map "a" "b"; *)
  let derive_basic_blocks (mthd_stmts: ir3_stmt list) =
    (* END block *)
    Hashtbl.add basic_blocks_map 0
      {
        stmts = [];
        in_blocks = [];
        out_blocks = []
      };

    let rec split_into_blocks stmts stmts_accum labeled_block_id non_labeled_block_id appending_mode = 
      let cur_block_id = if appending_mode then non_labeled_block_id else labeled_block_id in
      match stmts with
      | [] -> 
        Hashtbl.add basic_blocks_map cur_block_id 
          {
            stmts = stmts_accum;
            in_blocks = [];
            out_blocks = [0]
          };
      | (stmt::rests) ->
        println ("split_into_blocks, cur_block_id: " ^ (string_of_int cur_block_id) ^ ", line: " ^ 
          (string_of_int ((List.length mthd_stmts) - (List.length stmts) + 1)) ^ " --> " ^ (string_of_ir3_stmt stmt));
        match stmt with
          | Label3 label -> begin 
            Hashtbl.add basic_blocks_map cur_block_id 
              {
                stmts = stmts_accum;
                in_blocks = [];
                out_blocks = [(label :>int)]
              };
            split_into_blocks rests [] (label:>int) non_labeled_block_id false
          end
          | IfStmt3 (_, label) | GoTo3 label -> begin
            (* println "IfStmt3 | GoTo3"; *)
            let next_block_id = (non_labeled_block_id - 1) in
            Hashtbl.add basic_blocks_map cur_block_id 
              {
                stmts = stmts_accum @ [stmt];
                in_blocks = [];
                out_blocks = [(label:> int); next_block_id]
              };
            split_into_blocks rests [] next_block_id next_block_id true
          end
          (* TODO: handle:
          | ReturnStmt3 of id3
          | ReturnVoidStmt3 *)
          | _ -> begin
            split_into_blocks rests (stmts_accum @ [stmt]) labeled_block_id non_labeled_block_id appending_mode
          end
    in

    let fill_in_in_blocks () =
      Hashtbl.iter (fun k v ->
        (* println (string_of_int k); *)
        let out_blocks = v.out_blocks in
        List.iter (fun x -> begin
          x.in_blocks <- (x.in_blocks @ [k]);
          ()
        end) (List.map (Hashtbl.find basic_blocks_map) out_blocks)
      ) basic_blocks_map
    in

    println "derive_basic_blocks";
    split_into_blocks mthd_stmts [] 0 (-1) true;
    println "fill_in_in_blocks";
    fill_in_in_blocks ();
    (* [] *)
  in
  derive_basic_blocks stmts;

  println "print_basic_blocks_map";
  print_basic_blocks_map ();

  [("", (0,0))]
end

let derive_active_spill_variable_set (liveness_timeline: liveness_timeline_type) =
  ([], [])

let derive_stack_memory_map (params: (var_decl3 list)) (localvars: (var_decl3 list)) =
  ([])

(* 5 *)
let get_register (asvs: active_spill_variables_type) (sm: stack_memory_map_type) (stmts: ir3_stmt list) (currstmt: ir3_stmt): (reg * (arm_instr list)) =
  ("v1", [])

(* 4 *)
let ir3_id3_to_arm (asvs: active_spill_variables_type) (sm: stack_memory_map_type) (stmts: ir3_stmt list) (currstmt: ir3_stmt) (vid: id3): (reg * (arm_instr list)) =
  ("v1", [])

(* 2 *)
let ir3_idc3_to_arm (asvs: active_spill_variables_type) (sm: stack_memory_map_type) (stmts: ir3_stmt list) (currstmt: ir3_stmt) (vidc3: idc3): (reg * (arm_instr list)) =
  match vidc3 with
  | Var3 vid -> ir3_id3_to_arm asvs sm stmts currstmt vid
  | IntLiteral3 i ->  "#" ^ (string_of_int i), [] (*TODO: replace this stub*)
  | BoolLiteral3 b ->  "#" ^ (if b = true then "1" else "0"), [] (*TODO: replace this stub*)
  | StringLiteral3 s ->  "#" ^ s, [] (*TODO: replace this stub*)

let rec ir3_exp_to_arm 
    (localvars: var_decl3 list) (asvs: active_spill_variables_type) (sm: stack_memory_map_type)
    (stmts: ir3_stmt list) (currstmt: ir3_stmt) (exp: ir3_exp): (reg * (arm_instr list)) =
  match exp with
  | Idc3Expr (idc) -> ir3_idc3_to_arm asvs sm stmts currstmt idc
  (* 3 *)
  | BinaryExp3 (op, idc1, idc2) ->
    begin
      match op with
      | BooleanOp bop ->
        begin
          match bop with
          | "||"
          | "&&"
          | _ -> failwith ("Boolean operand not supported")
        end
      | RelationalOp rop ->
        begin
          match rop with
          | "==" ->
            let (op1reg, op1instr) = ir3_idc3_to_arm asvs sm stmts currstmt idc1 in
            let (op2reg, op2instr) = ir3_idc3_to_arm asvs sm stmts currstmt idc2 in
            let eqinstr = CMP("", op1reg, RegOp(op2reg)) in
            let (dstreg, dstinstr) = get_register asvs sm stmts currstmt in
            begin
              match idc2 with
              (* Special case not allowed by parser but exists in code generated by
                 ir3 due to if- and while-stmt *)
              | BoolLiteral3 _ ->
                (dstreg, op1instr @ op2instr @ dstinstr @ [eqinstr])
              | _ ->  
                let mveqinstr = MOV("eq", false, dstreg, ImmedOp("#1")) in
                let mvneinstr = MOV("ne", false, dstreg, ImmedOp("#0")) in
                (dstreg, op1instr @ op2instr @ dstinstr @ [eqinstr; mveqinstr; mvneinstr])
            end
          | "<"
            let (op1reg, op1instr) = ir3_idc3_to_arm asvs sm stmts currstmt idc1 in
            let (op2reg, op2instr) = ir3_idc3_to_arm asvs sm stmts currstmt idc2 in
            let eqinstr = CMP("", op1reg, RegOp(op2reg)) in
            let (dstreg, dstinstr) = get_register asvs sm stmts currstmt in
            let mveqinstr = MOV("lt", false, dstreg, ImmedOp("#1")) in
            let mvneinstr = MOV("ge", false, dstreg, ImmedOp("#0")) in
            (dstreg, op1instr @ op2instr @ dstinstr @ [eqinstr; mveqinstr; mvneinstr])
          | "<="
          | ">"
          | ">="
          | "!="
          | _ -> failwith ("Relational operand not supported")
        end
      | AritmeticOp aop ->
        begin
          match aop with
          | "+" ->
            let (op1reg, op1instr) = ir3_idc3_to_arm asvs sm stmts currstmt idc1 in
            let (op2reg, op2instr) = ir3_idc3_to_arm asvs sm stmts currstmt idc2 in
            let addinstr = ADD("", false, op1reg, op1reg, RegOp(op2reg)) in
            (op1reg, op1instr @ op2instr @ [addinstr])
          | "*" ->
            let (op1reg, op1instr) = ir3_idc3_to_arm asvs sm stmts currstmt idc1 in
            let (op2reg, op2instr) = ir3_idc3_to_arm asvs sm stmts currstmt idc2 in
            (* TODO: destination register dstreg can not be r15 *)
            let (dstreg, dstinstr) = get_register asvs sm stmts currstmt in
            let mulinstr = MUL("", false, dstreg, op1reg, op2reg) in
            (op1reg, op1instr @ op2instr @ dstinstr @ [mulinstr])
          | "-" ->
            let (op1reg, op1instr) = ir3_idc3_to_arm asvs sm stmts currstmt idc1 in
            let (op2reg, op2instr) = ir3_idc3_to_arm asvs sm stmts currstmt idc2 in
            let subinstr = SUB("", false, op1reg, op1reg, RegOp(op2reg)) in
            (op1reg, op1instr @ op2instr @ [subinstr])
          | _ -> failwith ("Arithmetic operand not supported")
        end
      | _ -> failwith ("Not operand of binary exp")
    end
  (* 2 *)
  | UnaryExp3 (op, idc) ->
    begin
      match op with
      | UnaryOp op ->
        begin
          match op with
          | "!" ->
            let (op1reg, op1instr) = ir3_idc3_to_arm asvs sm stmts currstmt idc in
            let (dstreg, dstinstr) = get_register asvs sm stmts currstmt in
            let cmpfalseinstr = CMP("", op1reg, ImmedOp("#0")) in
            let mveqinstr = MOV("eq", false, dstreg, ImmedOp("#0")) in
            let mvneinstr = MOV("ne", false, dstreg, ImmedOp("#1")) in
            (dstreg, op1instr @ dstinstr @ [cmpfalseinstr; mveqinstr; mvneinstr])
          | "-" ->
            let (op1reg, op1instr) = ir3_idc3_to_arm asvs sm stmts currstmt idc in
            let (dstreg, dstinstr) = get_register asvs sm stmts currstmt in
            let revsubinstr = RSB("", false, dstreg, op1reg, ImmedOp("#0")) in
            (dstreg, op1instr @ dstinstr @ [revsubinstr])
          | _ -> failwith ("Unary operator not supported")
        end
      | _ -> failwith ("Operator not supported")
    end
  (* 4 *)
  | FieldAccess3 (var_id3, field_name_id3) -> (*failwith ("ir3_exp_to_arm: EXPRESSION NOT IMPLEMENTED: " ^ string_of_ir3_exp e)*)
    let (var_reg, var_instr) = ir3_id3_to_arm asvs sm stmts currstmt var_id3 in
    let (dstreg, dstinstr) = get_register asvs sm stmts currstmt in
    let ldr_instr = LDR("", "", dstreg, RegPreIndexed(var_reg, get_field_shift field_name_id3 (* DUMMY FOR COMPILATION *) ("DUMMY", []), false))
      (* TODO: handle non-word fields; *)
      (* TODO: how to get the variable type? *)
    in dstreg, var_instr @ dstinstr @ [ldr_instr]
  (* 5 *)
  | MdCall3 _ as e -> failwith ("ir3_exp_to_arm: EXPRESSION NOT IMPLEMENTED: " ^ string_of_ir3_exp e)
  (* 4 *)
  | ObjectCreate3 _ as e -> failwith ("ir3_exp_to_arm: EXPRESSION NOT IMPLEMENTED: " ^ string_of_ir3_exp e)

let ir3_stmt_to_arm
    (localvars: var_decl3 list) (asvs: active_spill_variables_type)
    (sm: stack_memory_map_type) (stmts: ir3_stmt list) (stmt: ir3_stmt): (arm_instr list) =
  let ir3_id3_partial = ir3_id3_to_arm asvs sm stmts in
  let ir3_exp_partial = ir3_exp_to_arm localvars asvs sm stmts in
  match stmt with
  (* 1 *)
  | Label3 label ->
    let label_result = Label(string_of_int label) in
    [label_result]
  (* 3 *)
  | IfStmt3 (exp, label) ->
    (* TODO: complete the implementation *)
    let (exp_reg, exp_instr) = ir3_exp_partial stmt exp in
    let if_result = B("eq", string_of_int(label)) in
    exp_instr @ [if_result]
  (* 1 *)
  | GoTo3 label -> 
    let goto_result = B("", (string_of_int label)) in
    [goto_result]
  (* 1 *)
  | ReadStmt3 _ -> failwith ("ReadStmt3: STATEMENT NOT IMPLEMENTED")
  (* 1 *)
  | PrintStmt3 _ -> failwith ("PrintStmt3: STATEMENT NOT IMPLEMENTED")
  (* 2 *)
  | AssignStmt3 (id, exp) ->
    let (id_reg_dst, id_instr) = ir3_id3_partial stmt id in
    let (exp_reg_dst, exp_instr) = ir3_exp_partial stmt exp in
    let move_result = MOV("", false, id_reg_dst, RegOp(exp_reg_dst)) in
    id_instr @ exp_instr @ [move_result]
  (* 1 *)
  | AssignDeclStmt3 _ -> failwith ("AssignDeclStmt3: STATEMENT NOT IMPLEMENTED")
  (* 2 *)
  | AssignFieldStmt3 _ -> failwith ("AssignFieldStmt3: STATEMENT NOT IMPLEMENTED")
  (* 3 *)
  | MdCallStmt3 _ -> failwith ("MdCallStmt3: STATEMENT NOT IMPLEMENTED")
  (* 1 *)
  | ReturnStmt3 _ -> failwith ("ReturnStmt3: STATEMENT NOT IMPLEMENTED")
  (* 1 *)
  | ReturnVoidStmt3 -> failwith ("ReturnVoidStmt3 STATEMENT NOT IMPLEMENTED")

let ir3_method_to_arm (clist: cdata3 list) (mthd: md_decl3): (arm_instr list) =
  let liveness_timeline = derive_liveness_timeline mthd.ir3stmts in
  let asvs = derive_active_spill_variable_set liveness_timeline in
  let localvars = mthd.localvars3 in
  let sm = derive_stack_memory_map mthd.params3 localvars in
  let ir3_stmt_partial = ir3_stmt_to_arm localvars asvs sm mthd.ir3stmts in
  List.flatten (List.map ir3_stmt_partial mthd.ir3stmts)

let ir3_program_to_arm ((classes, main_method, methods): ir3_program): arm_program =
  let ir3_method_partial = ir3_method_to_arm classes in
  List.flatten (List.map ir3_method_partial (main_method :: methods))
