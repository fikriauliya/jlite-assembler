open Arm_structs
open Arm_utils
open Ir3_structs
open Ir3_lifetimes
open List


let use_heuristic_for_spilling_registers = true
and display_register_cleanings = false

let register_exists (rallocs: reg_allocations) (r: reg) =
  try
    let (_, _) = List.find (fun (reg_name, _) -> reg_name = r) rallocs in
    true
  with
  | Not_found -> false

let var_of_register (rallocs: reg_allocations) (r: reg): (id3 option) =
  try
    let (_, id3) = List.find (fun (reg_name, _) -> reg_name = r) rallocs in !id3
  with
  | Not_found -> failwith ("var_of_register: Invalid register name: " ^ r)

let register_of_var (rallocs: reg_allocations) (v: id3): (reg option) =
  try
    let (reg, _) = List.find (fun (_, var) -> match !var with
    | Some var_name -> var_name = v
    | None -> false
    ) rallocs in
    (Some reg)
  with
  | Not_found -> None



(* Update content of r to be new_var *)
let update_rallocs_var_at_reg (rallocs: reg_allocations) (new_var: id3 option) (r: reg) =
  try
    let (_, var_option) = List.find (fun (reg_name, _) -> reg_name = r) rallocs in
    let _ = (var_option := new_var) in ()
  with
  | Not_found -> failwith ("update_rallocs_var_at_reg: Invalid register name: " ^ r)


(* Unassign var_name from src_reg and generate the arm instruction for that as well. *)
let spill_variable (stack_frame: type_layout) (src_reg: reg) (*(var_name: id3)*) (rallocs: reg_allocations): arm_instr list =
  let var_name =
    match var_of_register rallocs src_reg with
    | Some id3 -> id3
    | None -> failwith "trying to spill an unassigned register!" in
  let _ = update_rallocs_var_at_reg rallocs None src_reg in
  store_variable stack_frame src_reg var_name

(* Assign var_name to dst_reg and generate the arm instruction for that as well. *)
(* Added behavior: if the variable is already in a register, it moves it to the dst register and updates rallocs *)
let unspill_variable (stack_frame: type_layout) (dst_reg: reg) (var_name: id3) (rallocs: reg_allocations): arm_instr list =
  let ret = match register_of_var rallocs var_name with
  | Some reg ->
    let (_, id3_ref) = List.find (fun (reg_name, _) -> reg_name = reg) rallocs in
    let () = id3_ref := None in
    [make_move("", false, dst_reg, RegOp(reg))]
  | None ->
    load_variable stack_frame dst_reg var_name
  in let _ = update_rallocs_var_at_reg rallocs (Some var_name) dst_reg
  in ret
  



let mtd_regs = ["a1";"a2";"a3";"a4";"lr"]
let reset_mtd_reg rallocs =
  let mtd_regs_used = filter (register_exists rallocs) mtd_regs in
  let _ = map (update_rallocs_var_at_reg rallocs (None)) mtd_regs_used in
  ()

(*
let request_method_call_reg (stack_frame: type_layout) (rallocs: reg_allocations) (r: reg) =
  match var_of_register rallocs r with
  | Some v ->
    (* Some other variable exists in a_x register, spill and load *)
    (*spill_variable stack_frame r rallocs*)
    store_variable stack_frame r v
  | None ->
    (* No other variable exists in a_x register, just load *)
    []
*)

let request_method_call_regs stack_frame rallocs : 'a list =
  let mtd_regs_used = filter (register_exists rallocs) mtd_regs in
  let request_method_call_reg (stack_frame: type_layout) (rallocs: reg_allocations) (r: reg) =
    match var_of_register rallocs r with
    | Some v ->
      (* Some other variable exists in a_x register, spill and load *)
      store_variable stack_frame r v
    | None ->
      (* No other variable exists in a_x register, just load *)
      []
  in
  let ret = flatten (map (request_method_call_reg stack_frame rallocs) mtd_regs_used) in
  let () = reset_mtd_reg rallocs in
  ret





let clean_registers linfo rallocs =
  let is_alive vid =
    let lness = Hashtbl.find linfo.timelines vid in
    linfo.current_line <= lness.end_line
  in
  List.flatten (List.map (fun (regn,varn) -> match !varn with
    | Some v ->
      if not (is_alive v) then
        let () = println("line "^(string_of_int linfo.current_line)^": freed "^regn^" from "^v) in
        let () = varn := None in
        if display_register_cleanings then [COM("freed "^regn)]
        else []
      else []
    | None -> []
  ) rallocs)


(* 4 *)
let ir3_id3_to_arm  (linfo: lines_info) (rallocs: reg_allocations) (stack_frame: type_layout)
  (stmts: ir3_stmt list) (currstmt: ir3_stmt) (vid: id3)
  (write_only: bool)
  : reg * arm_instr list =
  
  let idc3_to_id3_list idc = match idc with Var3 id -> [id] | _ -> [] in
  let no_spill_vars exp = match exp with
  | BinaryExp3 (_, idc1, idc2) -> idc3_to_id3_list idc1 @ idc3_to_id3_list idc2
  | FieldAccess3 (id1, id2) -> [id1;id2]
  | Idc3Expr id | UnaryExp3 (_, id) -> idc3_to_id3_list id
  | MdCall3 _ (* MdCall is handled specially by explicitly setting registers a1..4 using load/store_variable *)
  | ObjectCreate3 _ -> []
  in
  let no_spill_vars = match currstmt with
  | Label3 _ | GoTo3 _ | ReadStmt3 _ | PrintStmt3 _ | ReturnStmt3 _ | ReturnVoidStmt3 -> []
  | IfStmt3 (exp, _) | MdCallStmt3 exp -> no_spill_vars exp
  | AssignFieldStmt3 (exp1, exp2) -> no_spill_vars exp1 @ no_spill_vars exp2
  | AssignStmt3 (id, exp) | AssignDeclStmt3 (_, id, exp) -> id::(no_spill_vars exp)
  in
  
  let get_var_register var_id =
    let f (regn,varn) = match !varn with None -> false | Some n -> n = var_id in
    if List.exists f rallocs then Some (List.find f rallocs) else None
  in
  
  let maybe_load regn varn =
    if write_only then []
    else load_variable stack_frame regn varn
  in
  
  (*
  let is_alive vid =
    let lness = Hashtbl.find linfo.timelines vid in
    linfo.current_line <= lness.end_line
  in
  
  let clean_registers() =
    List.flatten (List.map (fun (regn,varn) -> match !varn with
      | Some v ->
        if not (is_alive v) then
          let () = println("line "^(string_of_int linfo.current_line)^": freed "^regn^" from "^v) in
          let () = varn := None in
          [COM("freed "^regn^" from "^v)]
        else []
      | None -> []
    ) rallocs)
  in
  *)
  
  let allocate_var var_id: (reg * (arm_instr list)) =
    let free_com_instrs = clean_registers linfo rallocs in
    let free (regn,varn) = match !varn with None -> true | Some _ -> false in
    if List.exists free rallocs then
      let (regn,varn) = List.find free (List.rev rallocs) in
      let () = varn := Some var_id in
      regn, maybe_load regn var_id
    else (* we have to spill a register on the stack *)
      let pick_spill_reg() =
      if use_heuristic_for_spilling_registers then
        let (regn,varn), end_line = argmax (
          fun (regn,varn) ->
            if List.exists (fun n -> match !varn with None -> false | Some v -> n = v) no_spill_vars
              then min_int
              else match !varn with Some v -> (Hashtbl.find linfo.timelines v).end_line | None -> min_int
        ) rallocs in
        if end_line = min_int
          then failwith "Unable to find a register to spill"
          else regn, varn (*match !varn with Some v -> regn, v | None -> failwith "wut??"*)
      else
        List.find (fun (regn,varn) -> not
          (* picks the first register that doesn't hold a var in no_spill_vars *)
          (List.exists (fun n -> match !varn with None -> false | Some v -> n = v) no_spill_vars)) rallocs
		  in
      let spilled_reg, spilled_var_ref = pick_spill_reg() in
      let spilled_var = match !spilled_var_ref with Some v -> v | _ -> failwith "unexpected quirk" in
      let store_instrs = store_variable stack_frame spilled_reg spilled_var in
      let () = spilled_var_ref := Some var_id in
      
      spilled_reg,
        free_com_instrs @ store_instrs @ maybe_load spilled_reg var_id
  in
  
  match get_var_register vid with
  | Some (reg, _) -> reg, []
  | None -> allocate_var vid


(* 2 *)
let ir3_idc3_to_arm (linfo: lines_info) (rallocs: reg_allocations) (stack_frame: type_layout)
    (stmts: ir3_stmt list) (currstmt: ir3_stmt) (vidc3: idc3): (reg * (arm_instr list)) =
  match vidc3 with
  | Var3 vid -> ir3_id3_to_arm linfo rallocs stack_frame stmts currstmt vid false
  | IntLiteral3 i ->  "#" ^ (string_of_int i), []
  | BoolLiteral3 b ->  "#" ^ (if b = true then "1" else "0"), []
  | StringLiteral3 s ->  "#" ^ s, []
  | Null3 ->  "#0", []
    



