open Arm_structs
open Ir3_structs
open Printf


(* Type corresponding to the position of an object inside a record
  should be between –4095 and +4095, cf: Flexible offset syntax, p.4-9
*)
type memory_address_offset = int


type type_layout =
(id3 * memory_address_offset) list


module Id3Set = Set.Make( 
  struct
    let compare = Pervasives.compare
    type t = id3
  end 
)

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


