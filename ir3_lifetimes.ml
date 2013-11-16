open Arm_utils
open Ir3_structs
open Jlite_structs

type liveness_timeline_record = {
  variable_name: id3;
  mutable start_line: int;
  mutable end_line: int;  
}
type liveness_timeline_type = (string, liveness_timeline_record) Hashtbl.t

type lines_info = {
  mutable current_line: int;
  timelines: liveness_timeline_type;
}

let string_of_timeline (tl: liveness_timeline_record) =
	"[" ^ (string_of_int tl.start_line) ^ " - " ^ (string_of_int tl.end_line) ^ "]"

(* Ir3 stmt with:
- defs => defined variables in the statement
- uses => used variables in the statement
- stmt_in_variables => IN variables
- stmt_out_variables => OUT variables
- line_number: line number (started from 1) of the statement counted from the beginning of the method.
Line number 0 is reserved for parameters and local variable definitions
 *)
type enhanced_stmt = {
  mutable embedded_stmt: ir3_stmt;
  defs: Id3Set.t;
  uses: Id3Set.t;
  mutable stmt_in_variables: Id3Set.t;
  mutable stmt_out_variables: Id3Set.t;
  line_number: int;
}

(* Basic block containing enhanced_stmts 
- in_blocks => block_ids of incoming blocks
- out_blocks => block_ids of outcoming blocs
- in_variables => set of IN variables 
- out_variables => set of OUT variables

Block 0 is reserved for END block.
Block 1 for starting block.
*)
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

(* Wrap simple IR3 statements in Enhanced Statements *)
(* defs, uses, and line_number information is initialized *)
(* stmt_in_variables and stmt_out_variables are left empty 
  (to be filled in later by derive_lineness_timeline *)
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

(* Group enhanced statements into blocks
Each block is identified by block id
Block_id 0 is reserved to END Block
If the block is started due to "Label" statement, the block_id will be equal to Label name
Otherwise, the block id is negative, started from -1 and incremented by -1.
*)
let derive_basic_blocks (mthd_stmts: enhanced_stmt list) = begin
  let basic_blocks_map = Hashtbl.create 100 in
  (* Reserved END block *)
  Hashtbl.add basic_blocks_map 0
    {
      stmts = [];
      in_blocks = [];
      out_blocks = [];
      in_variables = Id3Set.empty;
      out_variables = Id3Set.empty;
      block_id = 0;
    };

  (* Group enhanced_stmts into blocks *)
  (* 
    stmts => initial stmts to be grouped into blocks
    stmts_accum => temporary stmts to store what statements are pending to be flushed into blocks
    labeled_block_id  => block id for those blocks started by "Label" statement. This value is always positive
    non_labeled_block_id  => block id for those blocks started by any but "Label" statement. This valus is always negative. Started from -1
    appending_mode skip => optimization to skip empty/dead code blocks
  *)
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
  split_into_blocks mthd_stmts [] 0 (-2) true false;
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

(* Derive liveness timeline from basic_block_map.
We don't allow partial livetime. Instead, we group together into one long lived variable.
 *)
let derive_liveness_timeline (basic_blocks_map) : liveness_timeline_type = begin
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

  (* Iteratively calculate in/out variables *)
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
    (* Initial block  *)
    {
     embedded_stmt= Label3 0; (* Dummy *)
     stmt_out_variables= Id3Set.empty;
     stmt_in_variables= Id3Set.empty;
     defs= Id3Set.empty;
     uses= Id3Set.empty;
     line_number= -1;
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

