open Arm_structs
open Ir3_structs
open Printf
open List


(* Type corresponding to the position of an object inside a record
  should be between –4095 and +4095, cf: Flexible offset syntax, p.4-9
*)
type memory_address_offset = int

type reg_allocation = reg * id3 option ref
type reg_allocations = (reg_allocation) list

type type_layout =
(id3 * memory_address_offset) list


module Id3Set = Set.Make( 
  struct
    let compare = Pervasives.compare
    type t = id3
  end 
)


(*
(* Takes the first n element in a list and returns two list: those elements and the remaining ones *)
let rec vertical_split n ls =
  if n <= 0 then [], ls else match ls with
    | h::rest -> let frst,scnd = take_first (n-1) in h::frst, scnd
    | [] -> [], []
*)


(* Only be turned on for debugging *)
let println_debug line = begin
  (* printf "%s\n" line; *)
  (* TODO: remove all the printl's cluttering the code *)
end

let println line = begin
  printf "%s\n" line;
end


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



let load_variable (stack_frame: type_layout) (dst_reg: reg) (var_name: id3): arm_instr list =
  let offset = get_variable_offset stack_frame var_name in
  let ldr = LDR("", "", dst_reg, RegPreIndexed("fp", offset, false)) in
  [ldr]

let store_variable (stack_frame: type_layout) (src_reg: reg) (var_name: id3): arm_instr list =
  let offset = get_variable_offset stack_frame var_name in
  let str = STR("", "", src_reg, RegPreIndexed("fp", offset, false)) in
  [str]

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


let mtd_regs = ["a1";"a2";"a3";"a4"(*;"lr"*)]
let reset_mtd_reg(*s*) rallocs =
  (*
  let _ = update_rallocs_var_at_reg rallocs (None) "a1" in
  let _ = update_rallocs_var_at_reg rallocs (None) "a2" in
  let _ = update_rallocs_var_at_reg rallocs (None) "a3" in
  let _ = update_rallocs_var_at_reg rallocs (None) "a4" in
  let _ = update_rallocs_var_at_reg rallocs (None) "lr" in
  *)
  let _ = map (update_rallocs_var_at_reg rallocs (None)) mtd_regs in
  ()


let request_method_call_reg (stack_frame: type_layout) (rallocs: reg_allocations) (r: reg) =
  match var_of_register rallocs r with
  | Some v ->
    (* Some other variable exists in a_x register, spill and load *)
    (*spill_variable stack_frame r rallocs*)
    store_variable stack_frame r v
  | None ->
    (* No other variable exists in a_x register, just load *)
    []

let request_method_call_regs stack_frame rallocs : 'a list =
  flatten (map (request_method_call_reg stack_frame rallocs) mtd_regs)


