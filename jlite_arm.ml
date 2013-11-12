open Arm_structs
open Ir3_structs
open Jlite_structs
open Printf

module Id3Set = Set.Make( 
  struct
    let compare = Pervasives.compare
    type t = id3
  end 
)

(*type memory_address_type = int*)

(* Type corresponding to the position of an object inside a record
  should be between –4095 and +4095, cf: Flexible offset syntax, p.4-9
*)
type memory_address_offset = int

(*type liveness_timeline_type = ((id3 * (int * int)) list)*)
type liveness_timeline_record = {
  variable_name: id3;
  mutable start_line: int;
  mutable end_line: int;  
}
type liveness_timeline_type = (string, liveness_timeline_record) Hashtbl.t

type lines_info = {
  mutable current_line: int;
  timelines: liveness_timeline_type;
  (*timelines: (string, liveness_timeline_record) Hashtbl.t;*)
}

(*
type active_spill_variables_type = 
  (* (active variable set, spill variable set) *)
  ((id3 list) * (id3 list))
*)

type reg_allocation = reg * id3 option ref
type reg_allocations = (reg_allocation) list

let string_of_ralloc ((reg, var): reg_allocation): string option =
  (*
  let idstr = match !var with
    | Some v -> v
    | None -> "None"
  in reg ^ " " ^ idstr*)
  match !var with
    | Some v -> Some (reg ^ "=" ^ v)
    | None -> None

let string_of_rallocs (rallocs: reg_allocations) (sep: string): string =
  (*string_of_list rallocs string_of_ralloc sep*)
  String.concat sep (List.flatten
    (List.map (fun s -> match string_of_ralloc s with Some s -> [s] | _ -> []) rallocs)
  )

(* variable name -> reserved memory address in stack
type stack_memory_map_type =
  ((id3 * memory_address_type) list)
*)

type type_layout =
(id3 * memory_address_offset) list

type enhanced_stmt = {
  mutable embedded_stmt: ir3_stmt;
  defs: Id3Set.t;
  uses: Id3Set.t;
  mutable stmt_in_variables: Id3Set.t;
  mutable stmt_out_variables: Id3Set.t;
  line_number: int;
}

(* statement lists, IN block ids, OUT block ids *)
type basic_block_type = {
  block_id: int;
  stmts: enhanced_stmt list;
  mutable in_blocks: int list;
  out_blocks: int list;
  mutable in_variables: Id3Set.t;
  mutable out_variables: Id3Set.t;
}

(* Only be turned on for debugging *)
let println_debug line = begin
  (* printf "%s\n" line; *)
  (* TODO: remove all the printl's cluttering the code *)
end

let println line = begin
  printf "%s\n" line;
end

let id3_set_of_list li = 
  List.fold_left (fun set elem -> Id3Set.add elem set) Id3Set.empty li

let labelcount = ref 0 
let fresh_label () = 
  (labelcount := !labelcount+1; ".L" ^ (string_of_int !labelcount))

let stringlabelcount = ref 0
let fresh_string_label () =
  (stringlabelcount := !stringlabelcount+1; "str" ^ (string_of_int !stringlabelcount))

(* Contains the string literals and format specifiers *)
let string_table = Hashtbl.create 100

(* Adds a string literal to the string table, adds an int literal format specifier
 * if the is print stmt is true *)
let add_idc3_to_string_table idc3 isPrintStmt =
  begin
    match idc3 with
    | StringLiteral3 str ->
      if Hashtbl.mem string_table str then () else
        Hashtbl.add string_table str (fresh_string_label())
    | Var3 _ | IntLiteral3 _ ->
      if isPrintStmt && not (Hashtbl.mem string_table "%i") then
        Hashtbl.add string_table "%i" (fresh_string_label())
      else ()
    | _ -> ()
  end

(* Adds a string literal to the string table if the expression contains a string literal *)
let add_ir3_exp_to_string_table exp3 =
  begin
    match exp3 with
    | BinaryExp3 (_,idc3,idc3') ->
      add_idc3_to_string_table idc3 false;
      add_idc3_to_string_table idc3' false
    | UnaryExp3 (_,idc3) ->
      add_idc3_to_string_table idc3 false
    | Idc3Expr (idc3) ->
      add_idc3_to_string_table idc3 false
    | MdCall3 (_,idc3list) ->
      let rec helper idc3s =
        begin
          match idc3s with
          | [] ->
            ()
          | idc3::idc3s' ->
            add_idc3_to_string_table idc3 false;
            helper idc3s'
        end
      in helper idc3list
    | _ ->
      ()
  end

(* Adds a string literal to the string table if the stmt contains an expression
 * with a string literal or adds a int format specifier to the string table if
 * it is a print stmt with an int or integer variable *)
let add_ir3_stmt_to_string_table stmt3 =
  begin
    match stmt3 with
    | IfStmt3 (exp3,_) ->
      add_ir3_exp_to_string_table exp3
    | PrintStmt3 (idc3) ->
      add_idc3_to_string_table idc3 true
    | AssignStmt3 (_,exp3) ->
      add_ir3_exp_to_string_table exp3
    | AssignDeclStmt3 (_,_,exp3) ->
      add_ir3_exp_to_string_table exp3
    | AssignFieldStmt3 (exp3,exp3') ->
      add_ir3_exp_to_string_table exp3;
      add_ir3_exp_to_string_table exp3'
    | MdCallStmt3 (exp3) -> 
      add_ir3_exp_to_string_table exp3
    | _ ->
      ()
  end

(* Update content of r to be new_var *)
let update_rallocs_var_at_reg (rallocs: reg_allocations) (new_var: id3 option) (r: reg) =
  try
    let (_, var_option) = List.find (fun (reg_name, _) -> reg_name = r) rallocs in
    let _ = (var_option := new_var) in ()
  with
  | Not_found -> failwith ("update_rallocs_var_at_reg: Invalid register name: " ^ r)

(* Adds string literals and/or integer format specifier to the string table *)
let add_ir3_program_to_string_table ((classes, main_method, methods): ir3_program) =
  let rec helper stmts =
    begin
      match stmts with
      | [] ->
        ()
      | s::stmts' ->
        add_ir3_stmt_to_string_table s;
        helper stmts'
    end      
  in helper (List.flatten (main_method.ir3stmts :: List.map (fun m -> m.ir3stmts) methods))

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

let string_of_enhanced_stmt (e_stmt) = begin
  (string_of_int e_stmt.line_number) ^ " : " ^ 
  (string_of_ir3_stmt e_stmt.embedded_stmt) ^ " | defs = [ " ^ 
  (string_of_list (Id3Set.elements e_stmt.defs) (fun x -> x) ", ") ^ "] | uses = [ " ^ 
  (string_of_list (Id3Set.elements e_stmt.uses) (fun x -> x) ", ") ^ "] | ins = [ " ^
  (string_of_list (Id3Set.elements e_stmt.stmt_in_variables) (fun x -> x) ", ") ^ "] | outs = [ " ^
  (string_of_list (Id3Set.elements e_stmt.stmt_out_variables) (fun x -> x) ", ") ^ "]";
end

let ir3stmts_to_enhanced_stmts (stmts) = begin
  let get_uses_from_ir3exp(e: ir3_exp): (id3 list) = 
    let get_uses_from_idc3 (idc3_val : idc3) = 
      match idc3_val with
        | Var3 id3_val -> [id3_val]
        | _ -> []
    in
    match e with
      | BinaryExp3 (_, idc3_1, idc3_2) -> (get_uses_from_idc3 idc3_1) @ (get_uses_from_idc3 idc3_2)
      | UnaryExp3 (_, idc3_1) -> (get_uses_from_idc3 idc3_1)
      | FieldAccess3 (id3_1, _) -> [id3_1]
      | Idc3Expr (idc3_1) -> (get_uses_from_idc3 idc3_1)
      | MdCall3 (_, idc3s) -> (List.fold_left (fun accum x -> accum @ (get_uses_from_idc3 x)) [] idc3s)
      | ObjectCreate3 _ -> []
      | _ -> []
  in
  let i = ref (0) in
  List.map(fun x -> 
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
    i:=!i+1;
    {
      embedded_stmt = x;
      defs = id3_set_of_list calc_defs;
      uses = id3_set_of_list calc_uses;
      stmt_in_variables = Id3Set.empty;
      stmt_out_variables = Id3Set.empty;
      line_number = !i;
    };
  ) stmts
end

let derive_basic_blocks (mthd_stmts: enhanced_stmt list) = begin
  let basic_blocks_map = Hashtbl.create 100 in
  (* END block *)
  Hashtbl.add basic_blocks_map 0
    {
      stmts = [];
      in_blocks = [];
      out_blocks = [];
      in_variables = Id3Set.empty;
      out_variables = Id3Set.empty;
      block_id = 0;
    };

  let rec split_into_blocks (stmts: enhanced_stmt list) (stmts_accum: enhanced_stmt list) labeled_block_id non_labeled_block_id appending_mode skip = 
    let cur_block_id = if appending_mode then non_labeled_block_id else labeled_block_id in
    match stmts with
    | [] -> 
      Hashtbl.add basic_blocks_map cur_block_id 
        {
          stmts = stmts_accum;
          in_blocks = [];
          out_blocks = [0];
          in_variables = Id3Set.empty;
          out_variables = Id3Set.empty;
          block_id = cur_block_id;
        };
    | (stmt::rests) ->
      println_debug ("split_into_blocks, cur_block_id: " ^ (string_of_int cur_block_id) ^ ", line: " ^ 
        (string_of_int ((List.length mthd_stmts) - (List.length stmts) + 1)) ^ " --> " ^ (string_of_enhanced_stmt stmt));
      match stmt.embedded_stmt with
        | Label3 label -> begin 
          if (skip) then ()
          else 
            Hashtbl.add basic_blocks_map cur_block_id 
            {
              stmts = stmts_accum;
              in_blocks = [];
              out_blocks = [(label :>int)];
              in_variables = Id3Set.empty;
              out_variables = Id3Set.empty;
              block_id = cur_block_id;
            };
          split_into_blocks rests [stmt] (label:>int) non_labeled_block_id false false
        end
        | GoTo3 label -> begin
          if (skip) then ()
          else 
            Hashtbl.add basic_blocks_map cur_block_id 
            {
              stmts = stmts_accum @ [stmt];
              in_blocks = [];
              out_blocks = [(label:> int)];
              in_variables = Id3Set.empty;
              out_variables = Id3Set.empty;
              block_id = cur_block_id;
            };
          split_into_blocks rests [] labeled_block_id non_labeled_block_id true true
        end
        | IfStmt3 (_, label) -> begin
          (* println_debug "IfStmt3 | GoTo3"; *)
          let next_block_id = (non_labeled_block_id - 1) in
          if (skip) then ()
          else 
            Hashtbl.add basic_blocks_map cur_block_id 
            {
              stmts = stmts_accum @ [stmt];
              in_blocks = [];
              out_blocks = [(label:> int); next_block_id];
              in_variables = Id3Set.empty;
              out_variables = Id3Set.empty;
              block_id = cur_block_id;
            };
          split_into_blocks rests [] labeled_block_id next_block_id true skip
        end
        | ReturnStmt3 _ | ReturnVoidStmt3 ->
          if (skip) then ()
          else 
            Hashtbl.add basic_blocks_map cur_block_id 
            {
              stmts = stmts_accum @ [stmt];
              in_blocks = [];
              out_blocks = [0];
              in_variables = Id3Set.empty;
              out_variables = Id3Set.empty;
              block_id = cur_block_id;
            };
          split_into_blocks rests [] labeled_block_id non_labeled_block_id true true
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

  println_debug "derive_basic_blocks";
  split_into_blocks mthd_stmts [] 0 (-1) true false;
  println_debug "fill_in_in_blocks";
  fill_in_in_blocks ();
  basic_blocks_map
  (* [] *)
end

let get_all_blocks basic_blocks_map =
  Hashtbl.fold (fun k v ret -> 
    ret @ [v]
  ) basic_blocks_map []

let get_all_stmts (blocks:basic_block_type list) : enhanced_stmt list=
  List.flatten (List.map (fun block -> block.stmts) blocks)

let derive_liveness_timeline (basic_blocks_map) (param_vars: id3 list) : liveness_timeline_type = begin
  let liveness_timeline_map = Hashtbl.create 100 in

  let print_basic_blocks_map basic_blocks_map =
    Hashtbl.iter (fun k (v:basic_block_type) ->
      println_debug ("======================================================================");
      println_debug ("Block #" ^ (string_of_int k) ^ ": ");
      println_debug ("In block(s): " ^ (string_of_list v.in_blocks string_of_int ", "));
      println_debug ("Out block(s): " ^ (string_of_list v.out_blocks string_of_int ", "));
      println_debug ("In variable(s): " ^ (string_of_list (Id3Set.elements v.in_variables) (fun x -> x) ", "));
      println_debug ("Out variable(s): " ^ (string_of_list (Id3Set.elements v.out_variables) (fun x -> x) ", "));
      println_debug (string_of_list v.stmts string_of_enhanced_stmt "\n");
      println_debug ("======================================================================");
    ) basic_blocks_map;
  in

  let rec calculate_in_out_variables basic_blocks_map = 
    let get_succ_blocks (cur_block: basic_block_type): basic_block_type list = 
      let cur_block_id = cur_block.block_id in
      println_debug ("get_succ_blocks of " ^ (string_of_int cur_block_id) ^ ": ");
      let res = Hashtbl.fold (fun k v ret -> 
        if (List.exists (fun x -> x == cur_block_id) v.in_blocks) then
          ret @ [v]
        else 
          ret
      ) basic_blocks_map [] in
      println_debug (string_of_list res (fun x -> (string_of_int x.block_id)) ", ");
      res
    in

    let calculate_out_variables (in_variables: Id3Set.t list): Id3Set.t =
      List.fold_left (fun res x -> Id3Set.union res x) Id3Set.empty in_variables
    in

    let calculate_in_variables (uses:Id3Set.t) (out_variables:Id3Set.t) (defs:Id3Set.t): Id3Set.t = 
      Id3Set.union uses (Id3Set.diff out_variables defs)
    in

    (* 
    false => no change in in_variables
    true => any changes in in_variables. Loop again
     *)
    let res = Hashtbl.fold (fun k v ret ->
      println_debug ("===================== Calculate in/out variables for block #" ^ (string_of_int k) ^ " =============================");
      (* Skip EXIT block *)
      if (k == 0) then (ret || false)
      else 
        let succ_blocks = (get_succ_blocks v) in
        let succ_blocks_in_variables = List.map (fun x -> x.in_variables) succ_blocks in

        let block_out_variables = calculate_out_variables succ_blocks_in_variables in
        let block_stmts = v.stmts in
        
        println_debug ("block_out_variables: " ^ (string_of_list (Id3Set.elements block_out_variables) (fun x -> x) ", "));
        (* println_debug "block_stmts: "; *)
        (* println_debug (string_of_list block_stmts string_of_enhanced_stmt " -> "); *)

        let (block_in_variables, is_in_changed) = List.fold_left 
          (fun ((succ_blocks_in_variables: Id3Set.t), is_in_changed) stmt ->
            let cur_stmt_out = calculate_out_variables [succ_blocks_in_variables] in
            let cur_stmt_in = calculate_in_variables stmt.uses cur_stmt_out stmt.defs in
            println_debug ("processing statement: " ^ (string_of_enhanced_stmt stmt) ^ " | " ^
              "cur_stmt_out: " ^ (string_of_list (Id3Set.elements cur_stmt_out) (fun x -> x) ", ") ^ " | " ^
              "prev_stmt_out: " ^ (string_of_list (Id3Set.elements stmt.stmt_out_variables) (fun x -> x) ", ") ^ " | " ^
              "cur_stmt_in: " ^ (string_of_list (Id3Set.elements cur_stmt_in) (fun x -> x) ", ") ^ " | " ^
              "prev_stmt_in: " ^ (string_of_list (Id3Set.elements stmt.stmt_in_variables) (fun x -> x) ", ")
            );
            let is_new_change = 
              if (Id3Set.equal cur_stmt_in stmt.stmt_in_variables) then false 
              else begin
                println_debug (" *************************************************** -> Changed");
                true
              end
            in
            
            stmt.stmt_in_variables <- cur_stmt_in;
            stmt.stmt_out_variables <- cur_stmt_out;
            (cur_stmt_in, is_new_change || is_in_changed)
          ) (block_out_variables, false)
          (List.rev block_stmts)
        in
        v.out_variables <- block_out_variables;
        v.in_variables <- block_in_variables;
        ret || is_in_changed
    ) basic_blocks_map false in
    if res then
      calculate_in_out_variables basic_blocks_map
    else
      basic_blocks_map
  in
    
  let calculated_blocks_map = calculate_in_out_variables basic_blocks_map in

  println_debug "";
  println_debug "";
  println_debug "";
  print_basic_blocks_map calculated_blocks_map;

  let all_blocks = get_all_blocks basic_blocks_map in
  let all_stmts = get_all_stmts all_blocks in
  let sorted_all_stmts = List.sort (fun x y -> Pervasives.compare x.line_number y.line_number) all_stmts in
  
  (* Add parameter variables into liveness *)
  List.iter (fun param_var -> 
      Hashtbl.add liveness_timeline_map param_var
      {
        variable_name = param_var;
        start_line = 0;
        end_line = 0;
      }
    ) param_vars;

  let e = List.fold_left (fun prev curr ->
    let diff = Id3Set.diff curr.stmt_out_variables prev.stmt_out_variables in

    println_debug (string_of_enhanced_stmt curr);
    (* Store newly lived variable *)
    Id3Set.iter (fun x -> 
      println_debug ("add: " ^ x);
      if (Hashtbl.mem liveness_timeline_map x) then
        ()
      else
        Hashtbl.add liveness_timeline_map x 
          {
            variable_name = x;
            start_line = curr.line_number;
            end_line = curr.line_number;
          }
    ) diff;

    (* Store just died variable *)
    let diff2 = Id3Set.diff prev.stmt_out_variables curr.stmt_out_variables in
    Id3Set.iter (fun x -> 
      println_debug ("find: " ^ x);

      let ht = Hashtbl.find liveness_timeline_map x in
      ht.end_line <- curr.line_number;
    ) diff2;

    (* Store never used variable *)
    (* Defined, but never become out *)
    let diff3 = Id3Set.diff curr.defs curr.stmt_out_variables in
    Id3Set.iter (fun x -> 
      println_debug ("add: " ^ x);

      Hashtbl.add liveness_timeline_map x 
      {
        variable_name = x;
        start_line = curr.line_number;
        end_line = curr.line_number;
      }
    ) diff3;

    curr
  ) 
    {
      embedded_stmt= Label3 0; (* Dummy *)
      stmt_out_variables= id3_set_of_list param_vars;
      stmt_in_variables= Id3Set.empty;
      defs= Id3Set.empty;
      uses= Id3Set.empty;
      line_number= 0;
    } 
    sorted_all_stmts in

  (* Check what variable exists in the END block *)
  let last_statement = (List.hd (List.rev sorted_all_stmts)) in
  Id3Set.iter (fun x -> 
    println_debug x;
    let ht = Hashtbl.find liveness_timeline_map x in
    ht.end_line <- last_statement.line_number + 1;
  ) last_statement.stmt_in_variables;

  println "=============================================================";
  println (string_of_list sorted_all_stmts string_of_enhanced_stmt "\n");
  println "=============================================================";
  Hashtbl.iter (fun k v -> 
    println (k  ^ ": start_line: " ^ (string_of_int v.start_line) ^ ", " ^ "end_line: " ^ (string_of_int v.end_line));
  ) liveness_timeline_map;
  println "=============================================================";

  liveness_timeline_map
  (*[("", (0,0))]*)
end

(* TODO rm
let derive_active_spill_variable_set (liveness_timeline: liveness_timeline_type) =
  ([], [])
*)

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
  let _, params_layout = derive_precise_layout clist ("", params) 4 true in
  let _, vars_layout = derive_precise_layout clist ("", localvars) (-28) false in
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


let make_move ((cnd,s,rd,op2): mov_instr_type) =
  match op2 with RegOp r | ImmedOp r ->
    let mov = MOV(cnd,s,rd,op2) in (*let () = println ("-->"^rd^" "^r) in*)
    if r = rd then COM ((*"[useless] " ^*) (string_of_arm_instr mov))
    else mov

let reset_mtd_reg rallocs =
  let _ = update_rallocs_var_at_reg rallocs (None) "a1" in
  let _ = update_rallocs_var_at_reg rallocs (None) "a2" in
  let _ = update_rallocs_var_at_reg rallocs (None) "a3" in
  let _ = update_rallocs_var_at_reg rallocs (None) "a4" in
  ()

let load_variable (stack_frame: type_layout) (dst_reg: reg) (var_name: id3): arm_instr list =
  let offset = get_variable_offset stack_frame var_name in
  let ldr = LDR("", "", dst_reg, RegPreIndexed("fp", offset, false)) in
  [ldr]

let store_variable (stack_frame: type_layout) (src_reg: reg) (var_name: id3): arm_instr list =
  let offset = get_variable_offset stack_frame var_name in
  let str = STR("", "", src_reg, RegPreIndexed("fp", offset, false)) in
  [str]

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
  in ret (*@ [COM "LOL!!!"]*)



(* 5 
let get_register (asvs: active_spill_variables_type) (stack_frame: type_layout) (stmts: ir3_stmt list) (currstmt: ir3_stmt): (reg * (arm_instr list)) =
  ("v1", [])
*)

let label3_to_arm lbl = "L" ^ (string_of_int lbl)

(* 4 *)
let ir3_id3_to_arm  (linfo: lines_info) (rallocs: reg_allocations) (stack_frame: type_layout)
  (stmts: ir3_stmt list) (currstmt: ir3_stmt) (vid: id3) (*(no_spill_vars: id3 list)*)
  (write_only: bool)
  : reg * arm_instr list =
(*  ("v1", [])*)
  
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
  
  let is_alive vid =
    (*
    let () = print_string ("OK " ^ vid ^ "  ") in
    let () = Hashtbl.iter (fun n ln -> print_string ln.variable_name) linfo.timelines in
    *)
    (*
    let _, (_, death_line) = List.find (fun (id,_) -> id = vid) linfo.timelines in
    linfo.current_line <= death_line
    *)

  (* TODONE fixme: use a lifetime for "this" *)
    (*if vid = "this" then true else*)

    let lness = Hashtbl.find linfo.timelines vid in (*(fun lness -> lness.variable_name = vid) in*)
    (*
    let () = print_string (vid^" alive? "); if linfo.current_line <= lness.end_line
      then print_string ((string_of_int linfo.current_line) ^"\t"^ vid ^ " is dead!\n") else () in
    *)
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
  
  let allocate_var var_id: (reg * (arm_instr list)) =
    let free_com_instrs = clean_registers() in
    let free (regn,varn) = match !varn with None -> true | Some _ -> false in
    if List.exists free rallocs then
      let (regn,varn) = List.find free (List.rev rallocs) in
      let () = varn := Some var_id in
(*      regn, load_variable stack_frame regn var_id *)
      regn, maybe_load regn var_id
    else (* we have to spill a register on the stack *)
      let pick_spill_reg() = (* TODO use heuristic *)
        List.find (fun (regn,varn) -> not
          (* picks the first register that doesn't hold a var in no_spill_vars *)
          (List.exists (fun n -> match !varn with None -> false | Some v -> n = v) no_spill_vars)) rallocs in
      let spilled_reg, spilled_var_ref = pick_spill_reg() in
      let spilled_var = match !spilled_var_ref with Some v -> v | _ -> failwith "unexpected quirk" in
      let store_instrs = store_variable stack_frame spilled_reg spilled_var in
      let () = spilled_var_ref := Some var_id in
      
      spilled_reg,
      (*
      COM("LOL") ::
      *)
      free_com_instrs @ store_instrs @ maybe_load spilled_reg var_id
(*      @ (load_variable stack_frame spilled_reg var_id) *)
  in
  
  match get_var_register vid with
  | Some (reg, _) -> reg, []
  | None -> (*let () = print_string (">>"^vid^"\n") in*) allocate_var vid


(* 2 *)
let ir3_idc3_to_arm (linfo: lines_info) (rallocs: reg_allocations) (stack_frame: type_layout)
    (stmts: ir3_stmt list) (currstmt: ir3_stmt) (vidc3: idc3): (reg * (arm_instr list)) =
  match vidc3 with
  | Var3 vid -> ir3_id3_to_arm linfo rallocs stack_frame stmts currstmt vid false
  | IntLiteral3 i ->  "#" ^ (string_of_int i), [] (*TODO: replace this stub*)
  | BoolLiteral3 b ->  "#" ^ (if b = true then "1" else "0"), [] (*TODO: replace this stub*)
  | StringLiteral3 s ->  "#" ^ s, [] (*TODO: replace this stub*)
    
(*
let rec cdata3_from_id3 (localvars: var_decl3 list) (vid: id3) =
  let t,_ = List.find (fun (_,id) -> id = vid) localvars
  in match t with ObjectT cname -> | *)
let cname_from_id3 (localvars: var_decl3 list) (vid: id3) =
  (*let () = println vid in*)
  (*let _ = List.iter println  (List.map snd localvars) in*)
  
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
  
  (*
  and ir3_idc3_to_arm (linfo: lines_info) (rallocs: reg_allocations) (stack_frame: type_layout)
      (stmts: ir3_stmt list) (currstmt: ir3_stmt) (vidc3: idc3) (must_be_register: bool)
      : (string * (arm_instr list)) =
    let (arm,instr), is_reg = match vidc3 with
      | Var3 vid -> (ir3_id3_to_arm linfo rallocs stack_frame stmts currstmt vid false), true
      | IntLiteral3 i ->  ("#" ^ (string_of_int i), []), false (*TODO: replace this stub*)
      | BoolLiteral3 b ->  ("#" ^ (if b = true then "1" else "0"), []), false (*TODO: replace this stub*)
      | StringLiteral3 s ->  ("#" ^ s, []), false (*TODO: replace this stub*)
    in
    if must_be_register and not(is_reg) then
      let (dstreg, dstinstr) = get_assigned_register currstmt in
      (*let (reg, reginstr) = ir3_idc3_to_arm linfo rallocs stack_frame stmts currstmt  in*)
      in reg, dstinstr @ instr
    else arm, instr
  *)
  
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
            begin
            match idc2 with
            | BoolLiteral3 _ ->
              let (op1reg, op1instr) = ir3_idc3_to_arm linfo rallocs stack_frame stmts currstmt idc1 in
              let (op2reg, op2instr) = ir3_idc3_to_arm linfo rallocs stack_frame stmts currstmt idc2 in
              let eqinstr = CMP("", op1reg, RegOp(op2reg)) in
              (op1reg, op1instr @ op2instr @ [eqinstr], [])
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
          let (op1reg, op1instr) = ir3_idc3_to_arm linfo rallocs stack_frame stmts currstmt idc1 in
          let (op2reg, op2instr) = ir3_idc3_to_arm linfo rallocs stack_frame stmts currstmt idc2 in
          let (dstreg, dstinstr) = get_assigned_register currstmt in
          match aop with
          | "+" ->
            let instr = ADD("", false, dstreg, op1reg, RegOp(op2reg)) in
            (dstreg, op1instr @ op2instr @ dstinstr @ [instr], [])
          | "*" ->
            let instr = MUL("", false, dstreg, op1reg, op2reg) in
            (dstreg, op1instr @ op2instr @ dstinstr @ [instr], [])
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
  | FieldAccess3 (var_id3, field_name_id3) -> (*failwith ("ir3_exp_to_arm: EXPRESSION NOT IMPLEMENTED: " ^ string_of_ir3_exp e)*)
    let (var_reg, var_instr) = ir3_id3_to_arm linfo rallocs stack_frame stmts currstmt var_id3 false in
    let (dstreg, dstinstr) = get_assigned_register currstmt in
    let cname = cname_from_id3 localvars var_id3 in
    let ldr_instr = LDR("", "", dstreg, RegPreIndexed(var_reg, get_field_offset cname type_layouts field_name_id3, false))
      (* TODO: handle non-word fields; *)
      (* TODO: how to get the variable type? *)
    in (dstreg, var_instr @ dstinstr @ [ldr_instr], [])
  (* 5 *)
  | MdCall3 (m_id, args) ->
    let mdargs_to_reg (idc: idc3) (dst: reg): (arm_instr list) = 
      match idc with
      | IntLiteral3 _ | BoolLiteral3 _ | StringLiteral3 _ -> failwith ("Give up! Modify IR3 generation to make it a variable first!!")
      | Var3 id3 ->
        begin
          match register_of_var rallocs id3 with
          (* Argument already in register, just move *)
          | Some r -> if r = dst then (* [] *) [make_move("", false, dst, RegOp(r))]
            else
              begin
(*                let mov_arg_to_reg = [MOV("", false, dst, RegOp(r))] in*)
                let mov_arg_to_reg = [make_move("", false, dst, RegOp(r))] in
                match var_of_register rallocs dst with
                | Some v ->
                  (* Some other variable exists in a_x register, spill and move *)
                  if v <> id3 then
                    let _ = update_rallocs_var_at_reg rallocs (Some id3) dst in
                    (spill_variable stack_frame dst rallocs) @ mov_arg_to_reg
                  else (* [] *) [make_move("", false, v, RegOp(id3))]
                | None ->
                  (* No other variable exists in a_x register, just move *)
                  let _ = update_rallocs_var_at_reg rallocs (Some id3) dst in
                  mov_arg_to_reg
              end
          (* Argument not in register yet, load *)
          | None ->
            match var_of_register rallocs dst with
            | Some v ->
              (* Some other variable exists in a_x register, spill and load *)
              if v <> id3 then (spill_variable stack_frame dst rallocs) @ (unspill_variable stack_frame dst id3 rallocs)
              else []
            | None ->
              (* No other variable exists in a_x register, just load *)
              unspill_variable stack_frame dst id3 rallocs
        end
    in
    let mdargs_to_stack (idc: idc3) (arg_index: int): (arm_instr list) = 
      begin
        match idc with
        | IntLiteral3 _ | BoolLiteral3 _ | StringLiteral3 _ -> failwith ("Give up! Modify IR3 generation to make it a variable first!!")
        | Var3 id3 ->
          let (var_reg, var_instr) = ir3_id3_to_arm linfo rallocs stack_frame stmts currstmt id3 false in
          var_instr @ [STR("", "", var_reg, RegPreIndexed("sp", (arg_index)*(-4), false))]
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
      else (prepare_stack_args (arg_index + 1) args) @ (mdargs_to_stack (List.nth args arg_index) arg_index)
    in
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
    let result = ("a1", (prepare_args args) @ [allocate_args_stack] @ [actual_call] @ [deallocate_args_stack], []) in
    (* Set a1,a2,a3,a4 to free after function calls *)
    let () = reset_mtd_reg rallocs in
    result
  (* 4 *)
  | ObjectCreate3 class_name ->
    let objectSize = calc_obj_size clist (ObjectT class_name, class_name) in
    let movinstr = MOV("",false,"a1",ImmedOp("#" ^ string_of_int objectSize)) in
    let blinstr = BL("","_Znwj(PLT)") in
    ("a1", [movinstr; blinstr], [])

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
    (* TODO: complete the implementation *)
    let (exp_reg, exp_instr, post_instr) = ir3_exp_partial stmt exp in
    let if_result = B("eq", label3_to_arm label) in
    exp_instr @ [if_result] @ post_instr
  (* 1 *)
  | GoTo3 label -> 
    let goto_result = B("", (label3_to_arm  label)) in
    [goto_result]
  (* 1 *)
  | ReadStmt3 _ -> failwith ("ReadStmt3: STATEMENT NOT IMPLEMENTED")
  (* 1 *)
  | PrintStmt3 idc3 ->
    begin
      (* THIS IS NOT SAFE! one cannot simply load registers like that...
      match idc3 with
      | StringLiteral3 str ->
        let label = Hashtbl.find string_table str in
        let ldrinstr = LDR("","","a1",LabelAddr("=" ^ label)) in
        let blinstr = BL("","printf(PLT)") in
        [ldrinstr; blinstr]
      | IntLiteral3 i ->
        let label = Hashtbl.find string_table "%i" in
        let ldrinstr = LDR("","","a1",LabelAddr("=" ^ label)) in
        let movinstr = MOV("",false,"a2",ImmedOp("#" ^ (string_of_int i))) in
        let blinstr = BL("","printf(PLT)") in
        [ldrinstr; movinstr; blinstr]
      | Var3 id3 ->
        let label = Hashtbl.find string_table "%i" in
        let (var_reg, var_instr) = ir3_id3_to_arm linfo rallocs stack_frame stmts stmt id3 false in
        let ldrinstr = LDR("","","a1",LabelAddr("=" ^ label)) in
        let movinstr = MOV("",false,"a2",RegOp(var_reg)) in
        let blinstr = BL("","printf(PLT)") in
        [ldrinstr; movinstr; blinstr]
      | _ -> failwith ("PrintStmt3: currently only supports string and int literals")
      *)
      
      let request_reg r =
        match var_of_register rallocs r with
        | Some v ->
          (* Some other variable exists in a_x register, spill and load *)
          spill_variable stack_frame r rallocs
        | None ->
          (* No other variable exists in a_x register, just load *)
          []
      in
      let set_a1 value =
        let dst = "a1" in
        let ldr = LDR("","",dst,LabelAddr("=" ^ (Hashtbl.find string_table value)))
        in (request_reg dst) @ [ldr]
      in
      let ret = (match idc3 with
      | StringLiteral3 str ->
        set_a1 str
      | IntLiteral3 i ->
        (set_a1 "%i") @ (request_reg "a2") @ [MOV("",false,"a2",ImmedOp("#" ^ (string_of_int i)))]
      | Var3 id3 ->
        let dst = "a2" in
        (set_a1 "%i") @ (request_reg dst) @
        (
          match register_of_var rallocs id3 with
          | Some r ->
            [make_move("", false, dst, RegOp(r))]
          | None ->
            load_variable stack_frame dst id3
        )
        (*
        (
          match var_of_register rallocs dst with
          | Some v ->
            (* Some other variable exists in a_x register, spill and load *)
            if v <> id3 then
              let spill = (spill_variable stack_frame dst rallocs) in
              spill @ (unspill_variable stack_frame dst id3 rallocs)
            else [make_move("", false, dst, RegOp(dst))]
          | None ->
            (* No other variable exists in a_x register, just load *)
            unspill_variable stack_frame dst id3 rallocs
        )
        *)
        
        (*TODO: support booleans?*)
      | _ -> failwith ("PrintStmt3: currently only supports variables and string and int literals")
      
      ) @ [BL("","printf(PLT)")] in
      let () = reset_mtd_reg rallocs in
      ret
      
      (*spill_variable  
      [ldrinstr; movinstr; BL("","printf(PLT)")]*)
      
    end
  (* 1 *)
  | AssignDeclStmt3 (_, id, exp)
  (* 2 *)
  | AssignStmt3 (id, exp) ->
    let (id_reg_dst, id_instr) = ir3_id3_partial stmt id true in (*ir3_id3_to_arm rallocs stack_frame stmts true in*) (* ir3_id3_partial stmt id in *)
    let (exp_reg_dst, exp_instr, post_instr) = ir3_exp_partial stmt exp in
    if id_reg_dst = exp_reg_dst then (*and List.length exp_instr = 0*)
      id_instr @ exp_instr @ post_instr
    else
      let move_result = MOV("", false, id_reg_dst, RegOp(exp_reg_dst)) in
      id_instr @ exp_instr @ [move_result] @ post_instr
  (* 2 *)
  | AssignFieldStmt3 (fla, exp) ->
    
    let obj_var, field_name = (match fla with
      | FieldAccess3(o,f) -> o,f
      | _ -> failwith "field assignment is not applied on a field access"
    ) in
    
    let var_reg, var_instr = ir3_id3_partial stmt obj_var false in
    let exp_reg, exp_instr, post_instr = ir3_exp_partial stmt exp in
    
    (*let typ, _ = List.find (fun (_,vname) -> vname = obj_var) localvars in
    let clasname = (match typ with ObjectT n -> n | _ -> failwith "accessing field of a non-object variable") in*)
    let cname = cname_from_id3 localvars obj_var in
    
    (*let _, lay = List.find (fun (clas,lay) -> clas = cname) type_layouts in
    let offset = get_variable_offset lay field_name in*)
    
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

let gen_md_comments (mthd: md_decl3) (stack_frame: type_layout) =
  [
    COM ("Function " ^ mthd.id3);
    COM "Local variable offsets:";
  ]
  @ List.map (fun (id,off) -> COM ("  " ^ id ^ " : " ^ (string_of_int off))) stack_frame
  @ [EMPTY]

let eliminate_local_common_subexpression (basic_blocks_map) = begin
  let compare_idc3 (idc3_1) (idc3_2) : bool =
    match (idc3_1, idc3_2) with
      | (IntLiteral3 v1, IntLiteral3 v2) -> (Pervasives.compare v1 v2) == 0
      | (BoolLiteral3 v1, BoolLiteral3 v2) -> (Pervasives.compare v1 v2) == 0
      | (StringLiteral3 v1, StringLiteral3 v2) -> (Pervasives.compare v1 v2) == 0
      | (Var3 v1, Var3 v2) -> (Pervasives.compare v1 v2) == 0
  in

  (* Op -> idc3_1, idc3_2, var_name *)
  let cache_map = Hashtbl.create 100 in
  let append_into_cache_map (e_stmts: enhanced_stmt list) (op: ir3_op) idc3_1 idc3_2 (var_name: id3) =
    println_debug ("append_into_cache_map: " ^ var_name ^ " = " ^ (string_of_jlite_op op) ^ " " ^ (string_of_idc3 idc3_1) ^ " " ^ (string_of_idc3 idc3_2));
    Hashtbl.add cache_map op (idc3_1, idc3_2, var_name);
  in
  
  let last_def_of (e_stmts: enhanced_stmt list) (id3_1: id3) (from_line_number: int) =
    let prev_defs = List.fold_left
      (fun res e_stmt -> 
        if e_stmt.line_number < from_line_number then
          if (Id3Set.mem id3_1 e_stmt.defs) then
            res @ [e_stmt]
          else
            res
        else
          res
      )
    [] (List.rev e_stmts) in
    if (List.length prev_defs == 0) then
      -1
    else 
      (List.hd prev_defs).line_number
  in

  Hashtbl.iter (fun k v -> 
    List.iter 
      (fun stmt -> 
        println_debug ("Matching stmts " ^ (string_of_enhanced_stmt stmt));
        match stmt.embedded_stmt with
          AssignStmt3 (id3_res, e) -> 
            begin
            match e with
              | BinaryExp3 (op, idc3_1, idc3_2) -> 
                if Hashtbl.mem cache_map op then begin
                  (* Contain entry in the map *)
                  let filtered_ops = Hashtbl.find_all cache_map op in
                  let same_ops = List.filter (fun (cache_idc3_1, cache_idc3_2, _) ->
                    (compare_idc3 idc3_1 cache_idc3_1) && (compare_idc3 idc3_2 cache_idc3_2)
                  ) filtered_ops in
                  
                  stmt.embedded_stmt <- AssignStmt3 (id3_res, e);
                  if (List.length same_ops) == 0 then begin
                    append_into_cache_map v.stmts op idc3_1 idc3_2 id3_res;
                  end else
                    match (List.hd same_ops) with
                      | (_, _, id3_prev) ->
                        (* TODO: Check last def of idc3_1 and idc3_2 *)
                        (* Compare with last def of  *)
                        stmt.embedded_stmt <- AssignStmt3 (id3_res, (Idc3Expr (Var3 id3_prev)))
                end else begin
                  append_into_cache_map v.stmts op idc3_1 idc3_2 id3_res;
                end
                  (* TODO: exclude variables that may be assigned more than once in the block *) 
              (* | UnaryExp3 (op, idc3_1) ->  *)
              (* TODO: need to match this?
              | FieldAccess3 of id3 * id3
              | Idc3Expr of idc3
              | MdCall3 of id3 * (idc3 list) 
               *)
            | _ -> ()
            end
          | _ -> ()
      )
    v.stmts
  ) basic_blocks_map;
  basic_blocks_map
end

let ir3_method_to_arm (clist: cdata3 list) (mthd: md_decl3): (arm_instr list) =
  
  let e_stmts = ir3stmts_to_enhanced_stmts mthd.ir3stmts in
  let basic_blocks_map = derive_basic_blocks e_stmts in
  let optimized_blocks_map = eliminate_local_common_subexpression basic_blocks_map in
  let liveness_timeline = derive_liveness_timeline optimized_blocks_map (List.map (fun x -> match x with (_, param_var) -> param_var) mthd.params3) in
  
  let all_blocks = get_all_blocks optimized_blocks_map in
  let all_stmts = get_all_stmts all_blocks in
  let sorted_all_stmts = List.map (fun x -> x.embedded_stmt) (List.sort (fun x y -> Pervasives.compare x.line_number y.line_number) all_stmts) in

  (*
    let () = print_string (string_of_int (List.length liveness_timeline)) in
    let () = List.iter (fun (id,_) -> print_string ("'"^id^"'")) liveness_timeline in
  *)
  (*let asvs = derive_active_spill_variable_set liveness_timeline in*)
  let rallocs = [
    "a1", ref None;
    "a2", ref None;
    "a3", ref None;
    "a4", ref None;
    "v1", ref None;
    "v2", ref None;
    "v3", ref None;
    "v4", ref None;
    "v5", ref None;
    (* TODO: use other registers for variables? *)
  ] in
  let localvars = mthd.localvars3 in
  (* Callee stack & register management *)
  let saved_reg = ["v1"; "v2"; "v3"; "v4"; "v5"; "fp"; "lr"] in
  let loaded_reg = ["v1"; "v2"; "v3"; "v4"; "v5"; "fp"; "pc"] in
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
  (*
  let current_line = ref 0 in
  let get_next_line() = let () = current_line := !current_line + 1 in !current_line in
  *)
  let linfo = { current_line = 0; timelines = liveness_timeline } in
  let get_next_line() = let () = linfo.current_line <- linfo.current_line + 1
  (*; print_string (string_of_int linfo.current_line)*) in linfo in
    
  (*let ir3_stmt_partial = ir3_stmt_to_arm (get_next_line()) clist localvars rallocs exit_label_str stack_frame type_layouts mthd.ir3stmts in*)
  let ir3_stmt_partial stmt =
    let new_linfo = get_next_line() in
    let coms = [
      EMPTY;
      COM("line "^(string_of_int linfo.current_line)^": " ^ (string_of_ir3_stmt stmt));
      COM("rallocs: " ^ (string_of_rallocs rallocs " "));
    ] in
    let ret = ir3_stmt_to_arm new_linfo clist (mthd.params3 @ localvars) rallocs exit_label_str stack_frame type_layouts sorted_all_stmts stmt in
    coms @ ret
  in
  
  let md_comments = gen_md_comments mthd stack_frame in
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
  add_ir3_program_to_string_table (classes, main_method, methods);
  let ir3_method_partial = ir3_method_to_arm classes in
  let dataSegment = PseudoInstr ".data" in
  let string_table_to_arm = Hashtbl.fold 
    (fun k v r -> [Label v] @ [PseudoInstr (".asciz \"" ^ k ^ "\"")] @ r) string_table [] in
  let textSegment = PseudoInstr ".text" in
  let mainExport = PseudoInstr ".global main" in
      [dataSegment]
    @ [EMPTY]
    @ string_table_to_arm
    @ [EMPTY]
    @ [textSegment; mainExport]
    @ (List.flatten (List.map ir3_method_partial (main_method :: methods)))

