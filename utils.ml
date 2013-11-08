open List


exception Error of string


let is_none x = match x with
| None -> true
| _ -> false

let get x = match x with
| Some x -> x
| _ -> failwith "Accessing a None object"


(* append lists *)
let (++) = (@)

(* substract lists _once_, cf: [1;2;3;2] -- [2] = [1; 3; 2] *)
let rec (--) lsa lsb =
	let rec helper x lsb = match lsb with
		| [] -> false, []
		| h::tail -> if x = h
			then true, tail
			else let c,t = helper x tail in c, h::t
	in match lsa with
	| [] -> []
	| h::tail ->
		let contains, newList = (helper h lsb) in
		if contains then tail -- newList
		else h::(tail -- lsb)
	

let (@) f x = f x;;
let (|>) x f = f x;;

let id x = x


let rec split3 = function
| [] -> [], [], []
| (a,b,c)::tail ->
	let aa,bb,cc = split3 tail
	in a::aa, b::bb, c::cc


(*let all_true ls = fold_left (&&) true ls
*)
let all f ls = fold_left (fun acc x -> acc && f x) true ls

let all_true ls = all id ls
let all_false ls = all (!) ls

let fmap f ls = concat @ map f ls

(*
let con sep ls = String.concat sep ls
*)
let scat sep ls = String.concat sep ls

let take_left  (l,_) = l
let take_right (_,r) = r


let rec trim = function
| [] -> []
| h::t -> h::(filter (fun x -> x != h) t |> trim)



(* finds elements x so that there is a y after x in the list
   so that f(x) = f(y)
*)

let rec commons ls f =
	match ls with
	| [] -> []
	| h::rest -> 
		let fh = f h in
		rest
		|> find_all (fun x -> f x = fh)
		|> map (fun x->h,x)
		|> append (commons rest f)


(* The following function traverses a list, 
	applies a function to each element and concatenates the results *)
let string_of_list lst func delim  = 
	String.concat delim (List.map func lst)


