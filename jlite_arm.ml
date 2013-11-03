open Arm_structs
open Ir3_structs
open Jlite_structs

(* 5 *)
let get_free_register (stmts: ir3_stmt list) (currstmt: ir3_stmt): (reg * (arm_instr list)) =
  ("v1", [])

(* 4 *)
let ir3_id3_to_arm (stmts: ir3_stmt list) (currstmt: ir3_stmt) (vid: id3): (reg * (arm_instr list)) =
  ("v1", [])

(* 2 *)
let ir3_idc3_to_arm (stmts: ir3_stmt list) (currstmt: ir3_stmt) (vidc3: idc3): (reg * (arm_instr list)) =
  match vidc3 with
  | Var3 vid -> ir3_id3_to_arm stmts currstmt vid
  | IntLiteral3 i -> get_free_register stmts currstmt
  | BoolLiteral3 b -> failwith ("BoolLiteral3: NOT IMPLEMENTED")
  | StringLiteral3 s -> failwith ("StringLiteral3: NOT IMPLEMENTED")

let rec ir3_exp_to_arm (stmts: ir3_stmt list) (currstmt: ir3_stmt) (exp: ir3_exp): (reg * (arm_instr list)) =
  match exp with
  | Idc3Expr (idc) -> ir3_idc3_to_arm stmts currstmt idc
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
          | "<"
          | "<="
          | ">"
          | ">="
          | "=="
          | "!="
          | _ -> failwith ("Relational operand not supported")
        end
      | AritmeticOp aop ->
        begin
          match aop with
          | "+" ->
            let (op1reg, op1instr) = ir3_idc3_to_arm stmts currstmt idc1 in
            let (op2reg, op2instr) = ir3_idc3_to_arm stmts currstmt idc2 in
            let addinstr = ADD("", false, op1reg, op1reg, RegOp(op2reg)) in
            (op1reg, op1instr @ op2instr @ [addinstr])
          | "*"
          | "-"
          | _ -> failwith ("Arithmetic operand not supported")
        end
      | _ -> failwith ("Not operand of binary exp")
    end
  (* 2 *)
  | UnaryExp3 _ as e -> failwith ("ir3_exp_to_arm: EXPRESSION NOT IMPLEMENTED: " ^ string_of_ir3_exp e)
  (* 4 *)
  | FieldAccess3 _ as e -> failwith ("ir3_exp_to_arm: EXPRESSION NOT IMPLEMENTED: " ^ string_of_ir3_exp e)
  (* 5 *)
  | MdCall3 _ as e -> failwith ("ir3_exp_to_arm: EXPRESSION NOT IMPLEMENTED: " ^ string_of_ir3_exp e)
  (* 4 *)
  | ObjectCreate3 _ as e -> failwith ("ir3_exp_to_arm: EXPRESSION NOT IMPLEMENTED: " ^ string_of_ir3_exp e)

let ir3_stmt_to_arm (stmts: ir3_stmt list) (stmt: ir3_stmt): (arm_instr list) =
  let ir3_id3_partial = ir3_id3_to_arm stmts in
  let ir3_exp_partial = ir3_exp_to_arm stmts in
  match stmt with
  (* 1 *)
  | Label3 _ -> failwith ("ir3_stmt_to_arm: STATEMENT NOT IMPLEMENTED")
  (* 3 *)
  | IfStmt3 _ -> failwith ("ir3_stmt_to_arm: STATEMENT NOT IMPLEMENTED")
  (* 1 *)
  | GoTo3 _ -> failwith ("ir3_stmt_to_arm: STATEMENT NOT IMPLEMENTED")
  (* 1 *)
  | ReadStmt3 _ -> failwith ("ir3_stmt_to_arm: STATEMENT NOT IMPLEMENTED")
  (* 1 *)
  | PrintStmt3 _ -> failwith ("ir3_stmt_to_arm: STATEMENT NOT IMPLEMENTED")
  (* 2 *)
  | AssignStmt3 (id, exp) ->
    let (id_reg_dst, id_instr) = ir3_id3_partial stmt id in
    let (exp_reg_dst, exp_instr) = ir3_exp_partial stmt exp in
    let move_result = MOV("", false, id_reg_dst, RegOp(exp_reg_dst)) in
    id_instr @ exp_instr @ [move_result]
  (* 1 *)
  | AssignDeclStmt3 _ -> failwith ("ir3_stmt_to_arm: STATEMENT NOT IMPLEMENTED")
  (* 2 *)
  | AssignFieldStmt3 _ -> failwith ("ir3_stmt_to_arm: STATEMENT NOT IMPLEMENTED")
  (* 3 *)
  | MdCallStmt3 _ -> failwith ("ir3_stmt_to_arm: STATEMENT NOT IMPLEMENTED")
  (* 1 *)
  | ReturnStmt3 _ -> failwith ("ir3_stmt_to_arm: STATEMENT NOT IMPLEMENTED")
  (* 1 *)
  | ReturnVoidStmt3 -> failwith ("ir3_stmt_to_arm: STATEMENT NOT IMPLEMENTED")

let ir3_method_to_arm (mthd: md_decl3): (arm_instr list) =
  let ir3_stmt_partial = ir3_stmt_to_arm mthd.ir3stmts in
  List.flatten (List.map ir3_stmt_partial mthd.ir3stmts)

let ir3_program_to_arm ((classes, main_method, methods): ir3_program): arm_program =
  List.flatten (List.map ir3_method_to_arm (main_method :: methods))
