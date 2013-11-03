open Arm_structs
open Ir3_structs

let ir3_exp_to_arm (exp: ir3_exp): (arm_instr list) =
    []

let ir3_stmt_to_arm (stmt: ir3_stmt): (arm_instr list) =
    []

let ir3_method_to_arm (methodd: md_decl3): (arm_instr list) =
    []

let ir3_program_to_arm ((classes, main_method, methods): ir3_program): arm_program =
    []
