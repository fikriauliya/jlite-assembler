open Arm_utils
open Ir3_structs
open Jlite_structs

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

let id3_set_of_list li = 
  List.fold_left (fun set elem -> Id3Set.add elem set) Id3Set.empty li


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

  let _ = List.fold_left (fun prev curr ->
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
end


let eliminate_local_common_subexpression (basic_blocks_map) = begin
  let compare_idc3 (idc3_1) (idc3_2) : bool =
    match (idc3_1, idc3_2) with
      | (IntLiteral3 v1, IntLiteral3 v2) -> (Pervasives.compare v1 v2) == 0
      | (BoolLiteral3 v1, BoolLiteral3 v2) -> (Pervasives.compare v1 v2) == 0
      | (StringLiteral3 v1, StringLiteral3 v2) -> (Pervasives.compare v1 v2) == 0
      | (Var3 v1, Var3 v2) -> (Pervasives.compare v1 v2) == 0
      | (Null3, Null3) -> true
      | _ -> false
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
