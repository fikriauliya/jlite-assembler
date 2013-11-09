open Arm_structs
open Ir3_structs
open Jlite_structs
open Printf

(*type memory_address_type = int*)

(* Type corresponding to the position of an object inside a record
  should be between –4095 and +4095, cf: Flexible offset syntax, p.4-9
*)
type memory_address_offset = int

type liveness_timeline_type = ((id3 * (int * int)) list)
type active_spill_variables_type = 
  (* (active variable set, spill variable set) *)
  ((id3 list) * (id3 list))

(* variable name -> reserved memory address in stack
type stack_memory_map_type =
  ((id3 * memory_address_type) list)
*)

type type_layout =
  (id3 * memory_address_offset) list



type enhanced_stmt = {
  embedded_stmt: ir3_stmt;
  defs: id3 list;
  uses: id3 list;
}

(* statement lists, IN block ids, OUT block ids *)
type basic_block_type = {
  block_id: int;
  stmts: enhanced_stmt list;
  mutable in_blocks: int list;
  out_blocks: int list;
  mutable in_variables: id3 list;
  mutable out_variables: id3 list;
}

let println line = begin
  printf "%s\n" line;
end

let labelcount = ref 0 
let fresh_label () = 
  (labelcount := !labelcount+1; "L" ^ (string_of_int !labelcount))

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

let derive_liveness_timeline (stmts: ir3_stmt list) : liveness_timeline_type = begin
  let print_basic_blocks_map basic_blocks_map =
    let string_of_enhanced_stmt (e_stmt) =
      (string_of_ir3_stmt e_stmt.embedded_stmt) ^ " | defs = [ " ^ 
        (string_of_list e_stmt.defs (fun x -> x) ", ") ^ "] | uses = [ " ^ 
        (string_of_list e_stmt.uses (fun x -> x) ", ") ^ "]"
    in
    Hashtbl.iter (fun k (v:basic_block_type) ->
      println ("======================================================================");
      println ("Block #" ^ (string_of_int k) ^ ": ");
      println ("In block(s): " ^ (string_of_list v.in_blocks string_of_int ", "));
      println ("Out block(s): " ^ (string_of_list v.out_blocks string_of_int ", "));
      println ("In variable(s): " ^ (string_of_list v.in_variables (fun x -> x) ", "));
      println ("Out variable(s): " ^ (string_of_list v.out_variables (fun x -> x) ", "));
      println (string_of_list v.stmts string_of_enhanced_stmt "\n");
      println ("======================================================================");
    ) basic_blocks_map;
  in
  (* Hashtbl.add basic_blocks_map "a" "b"; *)
  let derive_basic_blocks (mthd_stmts: ir3_stmt list) =
    let basic_blocks_map = Hashtbl.create 100 in
    (* END block *)
    Hashtbl.add basic_blocks_map 0
      {
        stmts = [];
        in_blocks = [];
        out_blocks = [];
        in_variables = [];
        out_variables = [];
        block_id = 0;
      };

    let rec split_into_blocks stmts stmts_accum labeled_block_id non_labeled_block_id appending_mode skip = 
      let ir3stmts_to_enhanced_stmts (stmts) = 
        let get_uses_from_ir3exp(e: ir3_exp): (id3 list) = 
          let get_uses_from_idc3 (idc3_val : idc3) = 
            match idc3_val with
              | Var3 id3_val -> [id3_val]
              | _ -> []
          in
          match e with
            | BinaryExp3 (_, idc3_1, idc3_2) -> (get_uses_from_idc3 idc3_1) @ (get_uses_from_idc3 idc3_2)
            | UnaryExp3 (_, idc3_1) -> (get_uses_from_idc3 idc3_1)
            | FieldAccess3 (id3_1, id3_2) -> [id3_1; id3_2]
            | Idc3Expr (idc3_1) -> (get_uses_from_idc3 idc3_1)
            | MdCall3 (id3_1, idc3s) -> [id3_1] @ (List.fold_left (fun accum x -> accum @ (get_uses_from_idc3 x)) [] idc3s)
            | ObjectCreate3 _ -> []
            | _ -> []
        in
        List.map (fun x -> 
          let (calc_defs, calc_uses) = match x with
            | IfStmt3 (e, _)  -> ([], (get_uses_from_ir3exp e))
            | PrintStmt3 (Var3 res) -> ([], [res])
            | AssignStmt3 (res, e) -> ([res], (get_uses_from_ir3exp e))
            | AssignDeclStmt3 (_, res, e) -> ([res], (get_uses_from_ir3exp e))
            | AssignFieldStmt3 (FieldAccess3 (res_v, res_f), e) -> ([], [res_v] @ (get_uses_from_ir3exp e))
            | MdCallStmt3 e ->  ([], (get_uses_from_ir3exp e))
            | ReturnStmt3 res ->  ([], [res])
            (* The followings return empty *)
            (* | Label3 label3 ->  *)
            (* | GoTo3 label3  ->  *)
            (* | ReadStmt3 id3 ->  *)
            (* | ReturnVoidStmt3 ->  *)
            | _ -> ([], [])
          in
          {
            embedded_stmt = x;
            defs = calc_defs;
            uses = calc_uses;
          }
        ) stmts
      in
      let cur_block_id = if appending_mode then non_labeled_block_id else labeled_block_id in
      match stmts with
      | [] -> 
        Hashtbl.add basic_blocks_map cur_block_id 
          {
            stmts = ir3stmts_to_enhanced_stmts(stmts_accum);
            in_blocks = [];
            out_blocks = [0];
            in_variables = [];
            out_variables = [];
            block_id = cur_block_id;
          };
      | (stmt::rests) ->
        println ("split_into_blocks, cur_block_id: " ^ (string_of_int cur_block_id) ^ ", line: " ^ 
          (string_of_int ((List.length mthd_stmts) - (List.length stmts) + 1)) ^ " --> " ^ (string_of_ir3_stmt stmt));
        match stmt with
          | Label3 label -> begin 
            if (skip) then ()
            else 
              Hashtbl.add basic_blocks_map cur_block_id 
              {
                stmts = ir3stmts_to_enhanced_stmts(stmts_accum);
                in_blocks = [];
                out_blocks = [(label :>int)];
                in_variables = [];
                out_variables = [];
                block_id = cur_block_id;
              };
            split_into_blocks rests [] (label:>int) non_labeled_block_id false false
          end
          | GoTo3 label -> begin
            if (skip) then ()
            else 
              Hashtbl.add basic_blocks_map cur_block_id 
              {
                stmts = ir3stmts_to_enhanced_stmts(stmts_accum @ [stmt]);
                in_blocks = [];
                out_blocks = [(label:> int)];
                in_variables = [];
                out_variables = [];
                block_id = cur_block_id;
              };
            split_into_blocks rests [] labeled_block_id non_labeled_block_id true true
          end
          | IfStmt3 (_, label) -> begin
            (* println "IfStmt3 | GoTo3"; *)
            let next_block_id = (non_labeled_block_id - 1) in
            if (skip) then ()
            else 
              Hashtbl.add basic_blocks_map cur_block_id 
              {
                stmts = ir3stmts_to_enhanced_stmts(stmts_accum @ [stmt]);
                in_blocks = [];
                out_blocks = [(label:> int); next_block_id];
                in_variables = [];
                out_variables = [];
                block_id = cur_block_id;
              };
            split_into_blocks rests [] labeled_block_id next_block_id true skip
          end
          (* TODO: handle:
          | ReturnStmt3 of id3
          | ReturnVoidStmt3 *)
          | _ -> begin
            split_into_blocks rests (stmts_accum @ [stmt]) labeled_block_id non_labeled_block_id appending_mode skip
          end
    in

    let fill_in_in_blocks () =
      Hashtbl.iter (fun k v ->
        let out_blocks = v.out_blocks in
        List.iter (fun x -> begin
          x.in_blocks <- (x.in_blocks @ [k]);
          ()
        end) (List.map (Hashtbl.find basic_blocks_map) out_blocks)
      ) basic_blocks_map
    in

    println "derive_basic_blocks";
    split_into_blocks mthd_stmts [] 0 (-1) true false;
    println "fill_in_in_blocks";
    fill_in_in_blocks ();
    basic_blocks_map
    (* [] *)
  in

  let calculate_in_out_variables basic_blocks_map = 
    let get_succ_blocks (cur_block: basic_block_type): basic_block_type list = 
      let cur_block_id = cur_block.block_id in
      printf "%s" ("get_succ_blocks of " ^ (string_of_int cur_block_id) ^ ": ");
      let res = Hashtbl.fold (fun k v ret -> 
        if (List.exists (fun x -> x == cur_block_id) v.in_blocks) then
          ret @ [v]
        else 
          ret
      ) basic_blocks_map [] in
      println (string_of_list res (fun x -> (string_of_int x.block_id)) ", ");
      res
    in
    (* 
    true => no change in in_variables
    false => any changes in in_variables. Loop again
     *)
    Hashtbl.fold (fun k v ret ->
      (* Skip EXIT block *)
      if (k == 0) then (ret & true)
      else 
        let succ_blocks = (get_succ_blocks v) in
        v.out_variables = [];
        v.in_variables = [];
        ret & true
    ) basic_blocks_map true;
    basic_blocks_map
  in
    
  let basic_blocks_map = derive_basic_blocks stmts in
  let calculated_blocks_map = calculate_in_out_variables basic_blocks_map in

  println "print_basic_blocks_map";
  print_basic_blocks_map (calculated_blocks_map);

  [("", (0,0))]
end

let derive_active_spill_variable_set (liveness_timeline: liveness_timeline_type) =
  ([], [])


let derive_layout (clist: cdata3 list) ((cname,decls): cdata3): cname3 * type_layout =
  derive_precise_layout clist (cname,decls) 0 true

(* Note: This assumes the allocated objects will be alignes on a 4 bytes boundary! *)
let derive_precise_layout (clist: cdata3 list) ((cname,decls): cdata3)
    (starting_offset: int) (ascending_order: bool): cname3 * type_layout =derive_precise_layout
  let offset = ref starting_offset in
  let dir = if ascending_order then 1 else -1
  cname, List.map (fun (t,id) -> id,
    let off = !offset in
    begin
      offset := !offset + (calc_var_size clist (t,id))*dir;
      off
    end) decls

(*
(* Takes the first n element in a list and returns two list: those elements and the remaining ones *)
let rec vertical_split n ls =
  if n <= 0 then [], ls else match ls with
    | h::rest -> let frst,scnd = take_first (n-1) in h::frst, scnd
    | [] -> [], []
*)

(*let derive_stack_memory_map (params: (var_decl3 list)) (localvars: (var_decl3 list)) =
  ([])*)
let derive_stack_frame (clist: cdata3 list) (params: (var_decl3 list)) (localvars: (var_decl3 list)): type_layout =
(*  let first_4_params, rest_params = vertical_split 4 params
  let _, rest_params_layout = derive_layout clist ("", rest_params) 4 true in
  (* The first 4 parameters are passed in register; in order to be able to spill them on the stack, we reserve some space for them *)
  let _, vars_layout = derive_layout clist ("", first_4_params @ localvars) -28 false in
  rest_params_layout @ vars_layout*)
  let _, params_layout = derive_layout clist ("", params) 4 true in
  let _, vars_layout = derive_layout clist ("", localvars) -28 false in
  params_layout @ vars_layout

(* Returns the relative position of a field in an object of a given type
  TODO
  TODO also take the class as an argument
*)
let get_field_offset (cname: cname3) (type_layouts: (cname3 * type_layout) list) (field_name: id3) =
  let nam,lay = List.find (fun (nam,_) -> nam = cname) type_layouts in
  let id,offs = List.find (fun (id,offs) -> id = field_name) lay in
  offs

let get_variable_offset (stack_frame: type_layout) (var_name: id3) =
  let _, offset = List.find (fun (id,_) -> id = var_name) stack_frame
  in offset

let load_variable (stack_frame: type_layout) (dst_reg: reg) (var_name: id3): arm_instr list =
  let offset = get_variable_offset stack_frame var_name in
  let lrd = LDR("", "", dst_reg, RegPreIndexed("fp", offset, false)) in
  [ldr]

let store_variable (stack_frame: type_layout) (src_reg: reg) (var_name: id3): arm_instr list =
  let offset = get_variable_offset stack_frame var_name in
  let str = STR("", "", src_reg, RegPreIndexed("fp", offset, false)) in
  [str]

(* 5 *)
let get_register (asvs: active_spill_variables_type) (stack_frame: type_layout) (stmts: ir3_stmt list) (currstmt: ir3_stmt): (reg * (arm_instr list)) =
  ("v1", [])

(* 4 *)
let ir3_id3_to_arm (asvs: active_spill_variables_type) (stack_frame: type_layout) (stmts: ir3_stmt list) (currstmt: ir3_stmt) (vid: id3): (reg * (arm_instr list)) =
  ("v1", [])

(* 2 *)
let ir3_idc3_to_arm (asvs: active_spill_variables_type) (stack_frame: type_layout) (stmts: ir3_stmt list) (currstmt: ir3_stmt) (vidc3: idc3): (reg * (arm_instr list)) =
  match vidc3 with
  | Var3 vid -> ir3_id3_to_arm asvs stack_frame stmts currstmt vid
  | IntLiteral3 i ->  "#" ^ (string_of_int i), [] (*TODO: replace this stub*)
  | BoolLiteral3 b ->  "#" ^ (if b = true then "1" else "0"), [] (*TODO: replace this stub*)
  | StringLiteral3 s ->  "#" ^ s, [] (*TODO: replace this stub*)
(*
let rec cdata3_from_id3 (localvars: var_decl3 list) (vid: id3) =
  let t,_ = List.find (fun (_,id) -> id = vid) localvars
  in match t with ObjectT cname -> | *)
let rec cname_from_id3 (localvars: var_decl3 list) (vid: id3) =
  let t,_ = List.find (fun (_,id) -> id = vid) localvars
  in match t with ObjectT cname -> cname | _ -> failwith "This type is not a class"

let rec ir3_exp_to_arm 
    (localvars: var_decl3 list) (asvs: active_spill_variables_type)
    (stack_frame: type_layout) (type_layouts: (cname3 * type_layout) list)
    (stmts: ir3_stmt list) (currstmt: ir3_stmt) (exp: ir3_exp): (reg * (arm_instr list)) =
  
  let get_assigned_register stmt = match stmt with
  | AssignStmt3(id,_)
  | AssignDeclStmt3(_,id,_)
    -> ir3_id3_to_arm asvs stack_frame stmts currstmt id
  | _ -> failwith "Tried to retrieve the assigned register from a non-assignment statement"
  
  in match exp with
  | Idc3Expr (idc) -> ir3_idc3_to_arm asvs stack_frame stmts currstmt idc
  (* 3 *)
  | BinaryExp3 (op, idc1, idc2) ->
    begin
      match op with
      | BooleanOp bop ->
        begin
          let (op1reg, op1instr) = ir3_idc3_to_arm asvs stack_frame stmts currstmt idc1 in
          let (op2reg, op2instr) = ir3_idc3_to_arm asvs stack_frame stmts currstmt idc2 in
          let (dstreg, dstinstr) = get_assigned_register currstmt in
          match bop with
          | "||" ->
            let instr = ORR("", false, dstreg, op1reg, RegOp(op2reg)) in
            (dstreg, op1instr @ op2instr @ dstinstr @ [instr])
          | "&&" ->
            let instr = AND("", false, dstreg, op1reg, RegOp(op2reg)) in
            (dstreg, op1instr @ op2instr @ dstinstr @ [instr])
          | _ -> failwith ("Boolean operand not supported")
        end
      | RelationalOp rop ->
        begin
          let relationalOpHelper (movcond1: string) (movcond2: string) =
            let (op1reg, op1instr) = ir3_idc3_to_arm asvs stack_frame stmts currstmt idc1 in
            let (op2reg, op2instr) = ir3_idc3_to_arm asvs stack_frame stmts currstmt idc2 in
            let (dstreg, dstinstr) = get_assigned_register currstmt in
            let eqinstr = CMP("", op1reg, RegOp(op2reg)) in
            let mveqinstr = MOV(movcond1, false, op1reg, ImmedOp("#1")) in
            let mvneinstr = MOV(movcond2, false, op1reg, ImmedOp("#0")) in
            (dstreg, op1instr @ op2instr @ dstinstr @ [eqinstr; mveqinstr; mvneinstr]) in
          match rop with
          | "==" ->
            begin
            match idc2 with
            | BoolLiteral3 _ ->
              let (op1reg, op1instr) = ir3_idc3_to_arm asvs stack_frame stmts currstmt idc1 in
              let (op2reg, op2instr) = ir3_idc3_to_arm asvs stack_frame stmts currstmt idc2 in
              let eqinstr = CMP("", op1reg, RegOp(op2reg)) in
              (op1reg, op1instr @ op2instr @ [eqinstr])
            | _ ->  
              relationalOpHelper "eq" "ne"
            end
          | "<" ->
            relationalOpHelper "lt" "ge"
          | "<=" ->
            relationalOpHelper "le" "gt"
          | ">" ->
            relationalOpHelper "gt" "le"
          | ">=" ->
            relationalOpHelper "ge" "lt"
          | "<>" ->
            relationalOpHelper "ne" "eq"
          | _ -> failwith ("Relational operand not supported")
        end
      | AritmeticOp aop ->
        begin
          let (op1reg, op1instr) = ir3_idc3_to_arm asvs stack_frame stmts currstmt idc1 in
          let (op2reg, op2instr) = ir3_idc3_to_arm asvs stack_frame stmts currstmt idc2 in
          let (dstreg, dstinstr) = get_assigned_register currstmt in
          match aop with
          | "+" ->
            let instr = ADD("", false, dstreg, op1reg, RegOp(op2reg)) in
            (dstreg, op1instr @ op2instr @ dstinstr @ [instr])
          | "*" ->
            let instr = MUL("", false, dstreg, op1reg, op2reg) in
            (dstreg, op1instr @ op2instr @ dstinstr @ [instr]) 
          | "-" ->
            let instr = SUB("", false, op1reg, op1reg, RegOp(op2reg)) in
            (dstreg, op1instr @ op2instr @ dstinstr @ [instr]) 
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
          let (op1reg, op1instr) = ir3_idc3_to_arm asvs stack_frame stmts currstmt idc in
          let (dstreg, dstinstr) = get_assigned_register currstmt in
          match op with
          | "!" ->
            let cmpfalseinstr = CMP("", op1reg, ImmedOp("#0")) in
            let mveqinstr = MOV("eq", false, dstreg, ImmedOp("#0")) in
            let mvneinstr = MOV("ne", false, dstreg, ImmedOp("#1")) in
            (dstreg, op1instr @ dstinstr @ [cmpfalseinstr; mveqinstr; mvneinstr])
          | "-" ->
            let revsubinstr = RSB("", false, dstreg, op1reg, ImmedOp("#0")) in
            (dstreg, op1instr @ dstinstr @ [revsubinstr])
          | _ -> failwith ("Unary operator not supported")
        end
      | _ -> failwith ("Operator not supported")
    end
  (* 4 *)
  | FieldAccess3 (var_id3, field_name_id3) -> (*failwith ("ir3_exp_to_arm: EXPRESSION NOT IMPLEMENTED: " ^ string_of_ir3_exp e)*)
    let (var_reg, var_instr) = ir3_id3_to_arm asvs stack_frame stmts currstmt var_id3 in
    let (dstreg, dstinstr) = get_assigned_register currstmt in
    let cname = cname_from_id3 localvars var_id3 in
    let ldr_instr = LDR("", "", dstreg, RegPreIndexed(var_reg, get_field_offset cname type_layouts field_name_id3, false))
      (* TODO: handle non-word fields; *)
      (* TODO: how to get the variable type? *)
    in dstreg, var_instr @ dstinstr @ [ldr_instr]
  (* 5 *)
  | MdCall3 _ as e -> failwith ("ir3_exp_to_arm: EXPRESSION NOT IMPLEMENTED: " ^ string_of_ir3_exp e)
  (* 4 *)
  | ObjectCreate3 _ as e -> failwith ("ir3_exp_to_arm: EXPRESSION NOT IMPLEMENTED: " ^ string_of_ir3_exp e)

let ir3_stmt_to_arm
    (localvars: var_decl3 list) (asvs: active_spill_variables_type) (return_label: label)
    (stack_frame: type_layout) (type_layouts: (cname3 * type_layout) list)
    (stmts: ir3_stmt list) (stmt: ir3_stmt): (arm_instr list) =
  let ir3_id3_partial = ir3_id3_to_arm asvs stack_frame stmts in
  let ir3_exp_partial = ir3_exp_to_arm localvars asvs stack_frame type_layouts stmts in
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
  | MdCallStmt3 _ ->
    (* Manage caller registers *)
    (* Manage arguments!! *)
    (* Get return value from a1 *)
    let caller_save = STMFD (["a1"; "a2"; "a3"; "a4"]) in
    let caller_load = LDMFD (["a1"; "a2"; "a3"; "a4"]) in
    failwith ("MdCallStmt3: STATEMENT NOT IMPLEMENTED")
  (* 1 *)
  | ReturnStmt3 id ->
    (* Use register allocator's method to force a1 later *)
    let (return_reg, return_instr) = ir3_id3_to_arm asvs stack_frame stmts stmt id in
    let move_result = MOV("", false, "a1", RegOp(return_reg)) in
    return_instr @ [move_result; B("", return_label)]
  (* 1 *)
  | ReturnVoidStmt3 ->
    [B("", return_label)]

let ir3_method_to_arm (clist: cdata3 list) (mthd: md_decl3): (arm_instr list) =
  let liveness_timeline = derive_liveness_timeline mthd.ir3stmts in
  let asvs = derive_active_spill_variable_set liveness_timeline in
  let localvars = mthd.localvars3 in
  (* Callee stack & register management *)
  let callee_save = STMFD (["fp"; "lr"; "v1"; "v2"; "v3"; "v4"; "v5"]) in
  let fp_base_offset = 24 in
  let adjust_fp = ADD("", false, "fp", "sp", ImmedOp("#" ^ string_of_int fp_base_offset)) in
  let local_var_stack_size = 4 * List.length localvars in
  let adjust_sp = SUB("", false, "sp", "fp", ImmedOp("#" ^ string_of_int (fp_base_offset + local_var_stack_size))) in
  let exit_label_str = fresh_label() in
  let exit_label_instr = Label(exit_label_str) in
  let restore_sp = SUB("", false, "sp", "fp", ImmedOp("#" ^ string_of_int fp_base_offset)) in
  let callee_load = LDMFD (["fp"; "pc"; "v1"; "v2"; "v3"; "v4"; "v5"]) in
  let method_prefix = [callee_save; adjust_fp; adjust_sp] in
  let method_suffix = [exit_label_instr; restore_sp; callee_load] in
  (* Callee stack & register management END *)
  let stack_frame = derive_stack_frame clist mthd.params3 localvars in
  let type_layouts = List.map (derive_layout clist) clist in
  let ir3_stmt_partial = ir3_stmt_to_arm localvars asvs exit_label_str stack_frame type_layouts mthd.ir3stmts in
  method_prefix @ (List.flatten (List.map ir3_stmt_partial mthd.ir3stmts)) @ method_suffix

let ir3_program_to_arm ((classes, main_method, methods): ir3_program): arm_program =
  let ir3_method_partial = ir3_method_to_arm classes in
  List.flatten (List.map ir3_method_partial (main_method :: methods))
