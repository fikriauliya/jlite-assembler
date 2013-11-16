open Arm_structs
open Arm_utils
open Arm_register_alloc
open Ir3_structs
open Ir3_lifetimes
open Jlite_structs
open Printf
open List


let debug_restrict_registers = false
and use_asm_comments = false


let make_EMPTY = if use_asm_comments then [] else [EMPTY]


let labelcount = ref 0 
let fresh_label () = 
  (labelcount := !labelcount+1; ".L" ^ (string_of_int !labelcount))

let stringlabelcount = ref 0
let fresh_string_label () =
  (stringlabelcount := !stringlabelcount+1; "str" ^ (string_of_int !stringlabelcount))

(* Contains the string literals and format specifiers *)
let string_table = Hashtbl.create 100

let get_from_string_table str =
  if Hashtbl.mem string_table str
    then Hashtbl.find string_table str
    else
      let lbl = fresh_string_label() in
      let () = Hashtbl.add string_table str lbl in
      lbl

(* 2 *)
let ir3_idc3_to_arm (linfo: lines_info) (rallocs: reg_allocations) (stack_frame: type_layout)
    (stmts: ir3_stmt list) (currstmt: ir3_stmt) (vidc3: idc3): (reg * (arm_instr list)) =
  match vidc3 with
  | Var3 vid -> ir3_id3_to_arm linfo rallocs stack_frame stmts currstmt vid false
  | IntLiteral3 i ->  "#" ^ (string_of_int i), []
  | BoolLiteral3 b ->  "#" ^ (if b = true then "1" else "0"), []
  | StringLiteral3 s ->  "#" ^ (get_from_string_table s), []
  | Null3 ->  "#0", []


(*
 * Calculate the size of a variable. In fact, every variable has size 4 :)
*)
let calc_var_size (clist: (cdata3 list)) ((v_type, _): var_decl3) = 4

(*
 * Calculate the size of an object.
 * Each variable occupies 4 bytes.
 * Note: this does not correspond to the size of the variable the object is stored in,
 * since objects are only stored by reference (pointer).
*)
let calc_obj_size (clist: (cdata3 list)) ((v_type, _): var_decl3) =
  match v_type with
  | IntT | BoolT | StringT -> 4
  | ObjectT cname ->
    begin
      let cdata = List.find (fun ((cn, _): cdata3) -> cn = cname) clist in
      let (_, vars) = cdata in
      4 * (List.length vars)
    end
  | _ -> failwith ("calc_object_size: This shouldn't happen")

(* Note: This assumes the allocated objects will be alignes on a 4 bytes boundary! *)
let derive_precise_layout (clist: cdata3 list) ((cname,decls): cdata3)
    (starting_offset: int) (ascending_order: bool): cname3 * type_layout =
  let offset = ref starting_offset in
  let dir = if ascending_order then 1 else -1 in
  cname, List.map (fun (t,id) -> id,
    let off = !offset in
    begin
      offset := !offset + (calc_var_size clist (t,id))*dir;
      off
    end) decls

let derive_layout (clist: cdata3 list) ((cname,decls): cdata3): cname3 * type_layout =
  derive_precise_layout clist (cname,decls) 0 true

let derive_stack_frame (clist: cdata3 list) (params: (var_decl3 list)) (localvars: (var_decl3 list)): type_layout =
  let _, params_layout = derive_precise_layout clist ("", params) 4 true in
  let _, vars_layout = derive_precise_layout clist ("", localvars) (-28) false in
  params_layout @ vars_layout


let label3_to_arm lbl = "L" ^ (string_of_int lbl)

let cname_from_id3 (localvars: var_decl3 list) (vid: id3) =
  let t,_ = List.find (fun (_,id) -> id = vid) localvars
  in match t with ObjectT cname -> cname | _ -> failwith "This type is not a class"

let rec ir3_exp_to_arm  (linfo: lines_info) 
    (clist: cdata3 list) (localvars: var_decl3 list) (rallocs: reg_allocations)
    (stack_frame: type_layout) (type_layouts: (cname3 * type_layout) list)
    (stmts: ir3_stmt list) (currstmt: ir3_stmt) (exp: ir3_exp): (reg * (arm_instr list) * (arm_instr list)) = 
  
  let get_assigned_register stmt = match stmt with
  | AssignStmt3(id,_)
  | AssignDeclStmt3(_,id,_)
    -> ir3_id3_to_arm linfo rallocs stack_frame stmts currstmt id true
  | _ -> failwith "Tried to retrieve the assigned register from a non-assignment statement"
  in
  
  match exp with
  | Idc3Expr (idc) ->
    let (reg, instr) = ir3_idc3_to_arm linfo rallocs stack_frame stmts currstmt idc in
    (reg, instr, [])
  (* 3 *)
  | BinaryExp3 (op, idc1, idc2) ->
    begin
      match op with
      | BooleanOp bop ->
        begin
          let (op1reg, op1instr) = ir3_idc3_to_arm linfo rallocs stack_frame stmts currstmt idc1 in
          let (op2reg, op2instr) = ir3_idc3_to_arm linfo rallocs stack_frame stmts currstmt idc2 in
          let (dstreg, dstinstr) = get_assigned_register currstmt in
          match bop with
          | "||" ->
            let instr = ORR("", false, dstreg, op1reg, RegOp(op2reg)) in
            (dstreg, op1instr @ op2instr @ dstinstr @ [instr], [])
          | "&&" ->
            let instr = AND("", false, dstreg, op1reg, RegOp(op2reg)) in
            (dstreg, op1instr @ op2instr @ dstinstr @ [instr], [])
          | _ -> failwith ("Boolean operand not supported")
        end
      | RelationalOp rop ->
        begin
          let relationalOpHelper (movcond1: string) (movcond2: string) =
            let (op1reg, op1instr) = ir3_idc3_to_arm linfo rallocs stack_frame stmts currstmt idc1 in
            let (op2reg, op2instr) = ir3_idc3_to_arm linfo rallocs stack_frame stmts currstmt idc2 in
            let (dstreg, dstinstr) = get_assigned_register currstmt in
            let eqinstr = CMP("", op1reg, RegOp(op2reg)) in
            let mveqinstr = MOV(movcond1, false, dstreg, ImmedOp("#1")) in
            let mvneinstr = MOV(movcond2, false, dstreg, ImmedOp("#0")) in
            (dstreg, op1instr @ op2instr @ dstinstr @ [eqinstr; mveqinstr; mvneinstr], []) in
          match rop with
          | "==" ->
            relationalOpHelper "eq" "ne"
          | "<" ->
            relationalOpHelper "lt" "ge"
          | "<=" ->
            relationalOpHelper "le" "gt"
          | ">" ->
            relationalOpHelper "gt" "le"
          | ">=" ->
            relationalOpHelper "ge" "lt"
          | "!=" ->
            relationalOpHelper "ne" "eq"
          | _ -> failwith ("Relational operand not supported")
        end
      | AritmeticOp aop ->
        begin
          let (op1reg, op1instr) = ir3_idc3_to_arm linfo rallocs stack_frame stmts currstmt idc1 in
          let (op2reg, op2instr) = ir3_idc3_to_arm linfo rallocs stack_frame stmts currstmt idc2 in
          let (dstreg, dstinstr) = get_assigned_register currstmt in
          match aop with
          | "+" ->
            let instr = ADD("", false, dstreg, op1reg, RegOp(op2reg)) in
            (dstreg, op1instr @ op2instr @ dstinstr @ [instr], [])
          
          | "*" -> if dstreg <> op1reg
            then
              let instr = MUL("", false, dstreg, op1reg, op2reg) in
              (dstreg, op1instr @ op2instr @ dstinstr @ [instr], [])
            else if op1reg <> op2reg then
              let instr = MUL("", false, dstreg, op2reg, op1reg) in
              (dstreg, op1instr @ op2instr @ dstinstr @ [instr], [])
            else
              let intermediate_reg = if op1reg = "a1" then "a2" else "a1" in
              let intermediate_instr = match var_of_register rallocs intermediate_reg with
                | Some v -> let () = update_rallocs_var_at_reg rallocs None intermediate_reg in
                  store_variable stack_frame intermediate_reg v
                | None -> [] in
              let instrs = [MUL("", false, intermediate_reg, op1reg, op2reg);
                make_move("", false, dstreg, RegOp(intermediate_reg))] in
              (dstreg, op1instr @ op2instr @ dstinstr @ intermediate_instr @ instrs, [])
            
          | "-" ->
            let instr = SUB("", false, dstreg, op1reg, RegOp(op2reg)) in
            (dstreg, op1instr @ op2instr @ dstinstr @ [instr], [])
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
          let (op1reg, op1instr) = ir3_idc3_to_arm linfo rallocs stack_frame stmts currstmt idc in
          let (dstreg, dstinstr) = get_assigned_register currstmt in
          match op with
          | "!" ->
            let cmpfalseinstr = CMP("", op1reg, ImmedOp("#0")) in
            let mveqinstr = MOV("eq", false, dstreg, ImmedOp("#1")) in
            let mvneinstr = MOV("ne", false, dstreg, ImmedOp("#0")) in
            (dstreg, op1instr @ dstinstr @ [cmpfalseinstr; mveqinstr; mvneinstr], [])
          | "-" ->
            let revsubinstr = RSB("", false, dstreg, op1reg, ImmedOp("#0")) in
            (dstreg, op1instr @ dstinstr @ [revsubinstr], [])
          | _ -> failwith ("Unary operator not supported")
        end
      | _ -> failwith ("Operator not supported")
    end
  (* 4 *)
  | FieldAccess3 (var_id3, field_name_id3) ->
    let (var_reg, var_instr) = ir3_id3_to_arm linfo rallocs stack_frame stmts currstmt var_id3 false in
    let (dstreg, dstinstr) = get_assigned_register currstmt in
    let cname = cname_from_id3 localvars var_id3 in
    let ldr_instr = LDR("", "", dstreg, RegPreIndexed(var_reg, get_field_offset cname type_layouts field_name_id3, false))
    in (dstreg, var_instr @ dstinstr @ [ldr_instr], [])
  (* 5 *)
  | MdCall3 (m_id, args) ->
    let mdargs_to_reg (idc: idc3) (dst: reg): (arm_instr list) =
      match idc with
      | IntLiteral3 _ | BoolLiteral3 _ | StringLiteral3 _ | Null3 -> failwith ("Give up! Modify IR3 generation to make it a variable first!!")
      | Var3 id3 ->
        begin
          match register_of_var rallocs id3 with
          (* Argument already in register, just move as necessary *)
          | Some r -> [make_move("", false, dst, RegOp(r))]
          (* Argument not in register yet, load *)
          | None -> load_variable stack_frame dst id3
        end
    in
    let mdargs_to_stack (idc: idc3) (arg_index: int) (args: idc3 list): (arm_instr list) =
      begin
        match idc with
        | IntLiteral3 _ | BoolLiteral3 _ | StringLiteral3 _ | Null3 -> failwith ("Give up! Modify IR3 generation to make it a variable first!!")
        | Var3 id3 ->
          let (var_reg, var_instr) = ir3_id3_to_arm linfo rallocs stack_frame stmts currstmt id3 false in
          var_instr @ [STR("", "", var_reg, RegPreIndexed("sp", ((List.length args) - arg_index)*(-4), false))]
      end
    in
    let rec prepare_reg_args arg_index args =
      if (List.length args) <= arg_index || arg_index >= 4 then []
      else mdargs_to_reg (List.nth args arg_index) ("a" ^ string_of_int (arg_index+1)) @ (prepare_reg_args (arg_index + 1) args)
    in
    let rec prepare_stack_args arg_index args =
      if (List.length args) <= arg_index then []
      (* Push arguments with reverse order.
       * Change if stack frame layout for parameter is changed. *)
      else (prepare_stack_args (arg_index + 1) args) @ (mdargs_to_stack (List.nth args arg_index) arg_index args)
    in
    let saves = request_method_call_regs stack_frame rallocs in
    let allocate_args_stack = SUB("", false, "sp", "sp", ImmedOp("#" ^ string_of_int (4 * List.length args))) in
    let deallocate_args_stack = ADD("", false, "sp", "sp", ImmedOp("#" ^ string_of_int (4 * List.length args))) in
    let prepare_args args = 
      begin
        let rest_args = prepare_stack_args 4 args in
        let first_four_args = prepare_reg_args 0 args in
        rest_args @ first_four_args
      end
    in
    let actual_call = BL("", m_id) in
    let result = ("a1", saves @ (prepare_args args) @ [allocate_args_stack] @ [actual_call] @ [deallocate_args_stack], []) in
    (* Set a1,a2,a3,a4 to free after function calls *)
    result
  (* 4 *)
  | ObjectCreate3 class_name ->
    let saves = request_method_call_regs stack_frame rallocs in
    let objectSize = calc_obj_size clist (ObjectT class_name, class_name) in
    let movinstr = MOV("",false,"a1",ImmedOp("#" ^ string_of_int objectSize)) in
    let blinstr = BL("","_Znwj(PLT)") in
    let result = ("a1", saves @ [movinstr] @ [blinstr], []) in
    result

let ir3_stmt_to_arm (linfo: lines_info) (clist: cdata3 list)
    (localvars: var_decl3 list) (rallocs: reg_allocations) (return_label: label)
    (stack_frame: type_layout) (type_layouts: (cname3 * type_layout) list)
    (stmts: ir3_stmt list) (stmt: ir3_stmt): (arm_instr list) =
  let ir3_id3_partial = ir3_id3_to_arm linfo rallocs stack_frame stmts in
  let ir3_exp_partial = ir3_exp_to_arm linfo clist localvars rallocs stack_frame type_layouts stmts in
  match stmt with
  (* 1 *)
  | Label3 label ->
    let label_result = Label(label3_to_arm label) in
    [label_result]
  (* 3 *)
  | IfStmt3 (exp, label) ->
    begin
      match exp with
      | BinaryExp3 (RelationalOp "==", idc1, (BoolLiteral3 false)) -> 
        let (op1reg, op1instr) = ir3_idc3_to_arm linfo rallocs stack_frame stmts stmt idc1 in
        let cmpinstr = CMP("", op1reg, ImmedOp("#0")) in
        let beqinstr = B("eq", label3_to_arm label) in
        op1instr @ [cmpinstr] @ [beqinstr]
      | _ -> failwith ("Expression of if-stmt does not follow the format op1 == false")
    end
    (* TODO: complete the implementation *)
    (* let (exp_reg, exp_instr, post_instr) = ir3_exp_partial stmt exp in *)
  (* 1 *)
  | GoTo3 label -> 
    let goto_result = B("", (label3_to_arm  label)) in
    [goto_result]
  (* 1 *)
  | ReadStmt3 _ -> failwith ("ReadStmt3: STATEMENT NOT IMPLEMENTED")
  (* 1 *)
  | PrintStmt3 idc3 ->
    begin
      let set_a1 value =
        LDR("","","a1",LabelAddr(get_from_string_table value))
      in
      let saves = request_method_call_regs stack_frame rallocs in
      let ret = saves @ (match idc3 with
      | StringLiteral3 str ->
        [set_a1 (str ^ "\\n")]
      | IntLiteral3 i ->
        (set_a1 "%i\\n") :: [MOV("",false,"a2",ImmedOp("#" ^ (string_of_int i)))]
      | BoolLiteral3 b ->
        let int_of_bool b = if b == true then 1 else 0 in
        (set_a1 "%i\\n") :: [MOV("",false,"a2",ImmedOp("#" ^ (string_of_int (int_of_bool b))))]
      | Var3 id3 ->
        let dst = "a2" in
        let a1 =
          let t,_ = List.find (fun (_,id) -> id = id3) localvars in
            match t with StringT -> set_a1 "%s\\n" | _ -> set_a1 "%i\\n"
        in a1 ::
        (
          match register_of_var rallocs id3 with
          | Some r ->
              [make_move("", false, dst, RegOp(r))]
          | None ->
              load_variable stack_frame dst id3
        )
      | _ -> failwith ("PrintStmt3: only supports variables and string or int literals")
      
      ) @ [BL("","printf(PLT)")] in
      ret
      
    end
  (* 1 *)
  | AssignDeclStmt3 (_, id, exp)
  (* 2 *)
  | AssignStmt3 (id, exp) -> begin match exp with
      | Idc3Expr (StringLiteral3 s) ->
        let (id_reg_dst, id_instr) = ir3_id3_partial stmt id true in
        id_instr @ [LDR("", "", id_reg_dst, LabelAddr(get_from_string_table s))]
      | _ ->
        let (exp_reg_dst, exp_instr, post_instr) = ir3_exp_partial stmt exp in
        let (id_reg_dst, id_instr) = ir3_id3_partial stmt id true in
        if id_reg_dst = exp_reg_dst then
          id_instr @ exp_instr @ post_instr
        else
          let result = MOV("", false, id_reg_dst, RegOp(exp_reg_dst)) in
          let _ = update_rallocs_var_at_reg rallocs (Some id) id_reg_dst in
          id_instr @ exp_instr @ [result] @ post_instr
      end

  (* 2 *)
  | AssignFieldStmt3 (fla, exp) ->
    
    let obj_var, field_name = (match fla with
      | FieldAccess3(o,f) -> o,f
      | _ -> failwith "field assignment is not applied on a field access"
    ) in
    
    let var_reg, var_instr = ir3_id3_partial stmt obj_var false in
    let exp_reg, exp_instr, post_instr = ir3_exp_partial stmt exp in
    
    let cname = cname_from_id3 localvars obj_var in
    
    let str = STR("", "", exp_reg,
      RegPreIndexed(var_reg, get_field_offset cname type_layouts field_name, false)) in
    
    var_instr @ exp_instr @ [str] @ post_instr
    
  (* 3 *)
  | MdCallStmt3 exp ->
    let (exp_reg_dst, exp_instr, post_instr) = ir3_exp_partial stmt exp in
    exp_instr @ post_instr
  (* 1 *)
  | ReturnStmt3 id ->
    (* Use register allocator's method to force a1 later *)
    let (return_reg, return_instr) = ir3_id3_to_arm linfo rallocs stack_frame stmts stmt id false in
    let move_result = MOV("", false, "a1", RegOp(return_reg)) in
    return_instr @ [move_result; B("", return_label)]
  (* 1 *)
  | ReturnVoidStmt3 ->
    [B("", return_label)]

let gen_md_comments (mthd: md_decl3) (stack_frame: type_layout) (linfo: lines_info) =
  [
    COM ("Function " ^ mthd.id3);
    COM "Local variable offsets and life intervals:";
  ]
  @ List.map (fun (id,off) -> 
      let _ = print_endline (id) in
      COM (
      "  " ^ id ^
      " : " ^ (string_of_int off) ^
      "   \t" ^ (string_of_timeline (Hashtbl.find linfo.timelines id))
    )) stack_frame
  @ make_EMPTY

let ir3_method_to_arm (clist: cdata3 list) (mthd: md_decl3): (arm_instr list) =
  
  let e_stmts = ir3stmts_to_enhanced_stmts mthd.ir3stmts in
  let param_id3s = (List.map (fun x -> match x with (_, param_var) -> param_var) mthd.params3) in
  let local_id3s = (List.map (fun x -> match x with (_, param_var) -> param_var) mthd.localvars3) in
  let e_stmts_and_declarations = 
    [{
      embedded_stmt= Label3 (-1);
      defs= id3_set_of_list (param_id3s @ local_id3s);
      uses= Id3Set.empty;
      stmt_in_variables= Id3Set.empty;
      stmt_out_variables= Id3Set.empty;
      line_number = 0;
    }]
    @ e_stmts
  in
  let basic_blocks_map = derive_basic_blocks e_stmts_and_declarations in
  let liveness_timeline = derive_liveness_timeline basic_blocks_map in
  let all_blocks = get_all_blocks basic_blocks_map in
  let all_stmts = get_all_stmts all_blocks in
  let sorted_all_stmts = List.tl (List.map (fun x -> x.embedded_stmt) (List.sort (fun x y -> Pervasives.compare x.line_number y.line_number) all_stmts)) in
  
  let rallocs = if debug_restrict_registers then [
    "a1", ref None;
    "a2", ref None;
    "a3", ref None;
    "a4", ref None;
    "v1", ref None;
  ] else [
    "a1", ref None;
    "a2", ref None;
    "a3", ref None;
    "a4", ref None;
    
    (* invalidated after each call to bl and blx; since we only use these for function calls,
    it is safe to use the register and reset it in reset_mtd_reg *)
    "lr", ref None; 
    
    "v1", ref None;
    "v2", ref None;
    "v3", ref None;
    "v4", ref None;
    "v5", ref None;
    
    (* These register can be used for stack checking, but we assume we do no such thing *)
    "v6", ref None; (* sb/v6: Stack base / register variable 6 *)
    "v7", ref None; (* sl/v7 Stack limit / register variable 7 *)

  ] in
  let localvars = mthd.localvars3 in
  (* Callee stack & register management *)
  let saved_reg = ["v1"; "v2"; "v3"; "v4"; "v5"; "v6"; "v7"; "fp"; "lr"] in
  let loaded_reg = ["v1"; "v2"; "v3"; "v4"; "v5"; "v6"; "v7"; "fp"; "pc"] in
  let callee_save = STMFD (saved_reg) in
  let fp_base_offset = 4 * ((List.length saved_reg) - 1) in
  let adjust_fp = ADD("", false, "fp", "sp", ImmedOp("#" ^ string_of_int fp_base_offset)) in
  let local_var_stack_size = 4 * List.length localvars in
  let adjust_sp = SUB("", false, "sp", "fp", ImmedOp("#" ^ string_of_int (fp_base_offset + local_var_stack_size))) in
  let exit_label_str = fresh_label() in
  let exit_label_instr = Label(exit_label_str) in
  let restore_sp = SUB("", false, "sp", "fp", ImmedOp("#" ^ string_of_int fp_base_offset)) in
  let callee_load = LDMFD (loaded_reg) in
  let method_header = [Label mthd.id3] in
  let method_prefix = [callee_save; adjust_fp; adjust_sp] in
  let method_suffix = [exit_label_instr; restore_sp; callee_load] in
  (* Callee stack & register management END *)
  let stack_frame = derive_stack_frame clist mthd.params3 localvars in
  let type_layouts = List.map (derive_layout clist) clist in
  
  let linfo = { current_line = 0; timelines = liveness_timeline } in
  let get_next_line() = let () = linfo.current_line <- linfo.current_line + 1
  in linfo in
    
  (*let ir3_stmt_partial = ir3_stmt_to_arm (get_next_line()) clist localvars rallocs exit_label_str stack_frame type_layouts mthd.ir3stmts in*)
  let ir3_stmt_partial stmt =
    let new_linfo = get_next_line() in
    let clean = clean_registers linfo rallocs in
    let coms = make_EMPTY @ [
      COM("line "^(string_of_int linfo.current_line)^": " ^ (string_of_ir3_stmt stmt));
      COM("rallocs: " ^ (string_of_rallocs rallocs " "));
    ] in
    let ret = ir3_stmt_to_arm new_linfo clist (mthd.params3 @ localvars) rallocs exit_label_str stack_frame type_layouts sorted_all_stmts stmt in
    coms @ clean @ ret
  in
  
  let md_comments = gen_md_comments mthd stack_frame linfo in
  begin
    (* Telling this function that some arguments are on registers *)
    (* Assuming 1st variable on a1, 2nd on a2 ..., 4th on a4 *)
    let set_nth_var n =
      let (_, v) = List.nth rallocs n in
      let (_, v_name) = List.nth mthd.params3 n in
      v := Some v_name
    in
    let rec set_nth_below n curr =
      if (curr < n) then (set_nth_var curr; set_nth_below n (curr+1))
      else ()
    in
    
    let () = set_nth_below (min (List.length mthd.params3) 4) 0 in
    
    method_header @ md_comments @ method_prefix
    @ (List.flatten (List.map ir3_stmt_partial sorted_all_stmts))
    @ method_suffix
    
  end

let ir3_program_to_arm ((classes, main_method, methods): ir3_program): arm_program =
  (*add_ir3_program_to_string_table (classes, main_method, methods);*)
  let ir3_method_partial = ir3_method_to_arm classes in
  let dataSegment = PseudoInstr ".data" in
  let methods_arm = List.flatten (List.map ir3_method_partial (main_method :: methods)) in
  let string_table_to_arm = Hashtbl.fold 
    (fun k v r -> [Label v] @ [PseudoInstr (".asciz \"" ^ k ^ "\"")] @ r) string_table [] in
  let textSegment = PseudoInstr ".text" in
  let mainExport = PseudoInstr ".global main" in
  let rez =
      [dataSegment]
    @ string_table_to_arm
    @ [EMPTY]
    @ [textSegment; mainExport]
    @ methods_arm
  in if use_asm_comments
  then filter (fun ins -> match ins with COM _ -> false | _ -> true) rez
  else rez


