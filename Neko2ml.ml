type value =
	| VNull
	| VBool of bool
	| VInt of int
	| VFloat of float
	| VString of string
	| VObject of vobject
	| VArray of value array
	| VAbstract of vabstract
	| VFunction of vfunction
	| VClosure of vfunction * value array

and vobject = {
	mutable ofields : (int * value) array;
	mutable oproto : vobject option;
}

and vabstract =
	| AInt32 of int32
	| AHash of (value, value) Hashtbl.t
	| ABuffer of Buffer.t

and vfunction =
	| Fun0 of (unit -> value)
	| Fun1 of (value -> value)
	| Fun2 of (value -> value -> value)
	| Fun3 of (value -> value -> value -> value)
	| Fun4 of (value -> value -> value -> value -> value)
	| Fun5 of (value -> value -> value -> value -> value -> value)
	| FunVar of (value list -> value)

exception Runtime of value

exception Return of value

let fields_cache = Hashtbl.create 0

let env : value array ref = ref [||]

let vtrue = VBool true

let vfalse = VBool false

let vthis = ref VNull

let throw v =
	raise (Runtime v)

let hash f =
	let h = ref 0 in
	for i = 0 to String.length f - 1 do
		h := !h * 223 + int_of_char (String.unsafe_get f i);
	done;
	if Sys.word_size = 64 then Int32.to_int (Int32.shift_right (Int32.shift_left (Int32.of_int !h) 1) 1) else !h
	
let todo_error = ref true

let debug str =
	prerr_endline str

let return v =
	if !todo_error then raise (Return v)
	
let todo str =
	if !todo_error then failwith str

let nargs = function
	| Fun0 _ -> 0
	| Fun1 _ -> 1
	| Fun2 _ -> 2
	| Fun3 _ -> 3
	| Fun4 _ -> 4
	| Fun5 _ -> 5
	| FunVar _ -> -1
	
let rec to_string n v =
	if n > 5 then
		"<...>"
	else let n = n + 1 in
	match v with
	| VNull -> "null"
	| VBool true -> "true"
	| VBool false -> "false"
	| VInt i -> string_of_int i
	| VFloat f ->
		let s = string_of_float f in
		let len = String.length s in
		if String.unsafe_get s (len - 1) = '.' then String.sub s 0 (len - 1) else s
	| VString s -> s
	| VArray vl -> "[" ^ String.concat "," (Array.to_list (Array.map (to_string n) vl)) ^ "]"
	| VAbstract a ->
		(match a with
		| AInt32 i -> Int32.to_string i
		| _ -> "#abstract")
	| VClosure (f,_) | VFunction f -> "#function:"  ^ string_of_int (nargs f)
	| VObject o ->
		"#object" (* TODO *)
		
	
let with_env nenv f =
	let old = !env in
	env := nenv;
	let ret = f() in
	env := old;
	ret
	
let do_call_gen o f pl =
	let old = !vthis in
	vthis := o;
	let ret = (match f, pl with
	| VFunction (Fun0 f), [] ->
		f()
	| VFunction (Fun1 f), [a] ->
		f a
	| VFunction (Fun2 f), [a;b] ->
		f a b
	| VFunction (Fun3 f), [a;b;c] ->
		f a b c
	| VFunction (Fun4 f), [a;b;c;d] ->
		f a b c d
	| VFunction (Fun5 f), [a;b;c;d;e] ->
		f a b c d e
	| VFunction (FunVar f), _ ->
		f pl
	| VClosure (Fun0 f,env), [] ->
		with_env env f
	| VClosure (Fun1 f,env), [a] ->
		with_env env (fun() -> f a)
	| VClosure (Fun2 f,env), [a;b] ->
		with_env env (fun() -> f a b)
	| VClosure (Fun3 f,env), [a;b;c] ->
		with_env env (fun() -> f a b c)
	| VClosure (Fun4 f,env), [a;b;c;d] ->
		with_env env (fun() -> f a b c d)
	| VClosure (Fun5 f,env), [a;b;c;d;e] ->
		with_env env (fun() -> f a b c d e)
	| VClosure (FunVar f,env), _ ->
		with_env env (fun() -> f pl)
	| _ ->
		vthis := old;
		throw (VString "Invalid call")
	) in
	vthis := old;
	ret

(* ------------------------------- HELPERS ----------------------------------------------------- *)
		
module Helpers = struct

let to_int f = int_of_float (mod_float f 2147483648.0)

let parse_int s =
	let rec loop_hex i =
		if i = String.length s then s else
		match String.unsafe_get s i with
		| '0'..'9' | 'a'..'f' | 'A'..'F' -> loop_hex (i + 1)
		| _ -> String.sub s 0 i
	in
	let rec loop sp i =
		if i = String.length s then (if sp = 0 then s else String.sub s sp (i - sp)) else
		match String.unsafe_get s i with
		| '0'..'9' -> loop sp (i + 1)
		| ' ' when sp = i -> loop (sp + 1) (i + 1)
		| '-' when i = 0 -> loop sp (i + 1)
		| ('x' | 'X') when i = 1 && String.get s 0 = '0' -> loop_hex (i + 1)
		| _ -> String.sub s sp (i - sp)
	in
	int_of_string (loop 0 0)

let parse_float s =
	let rec loop sp i =
		if i = String.length s then (if sp = 0 then s else String.sub s sp (i - sp)) else
		match String.unsafe_get s i with
		| ' ' when sp = i -> loop (sp + 1) (i + 1)
		| '0'..'9' | '-' | 'e' | 'E' | '.' -> loop sp (i + 1)
		| _ -> String.sub s sp (i - sp)
	in
	float_of_string (loop 0 0)

let find_sub str sub start =
	let sublen = String.length sub in
	if sublen = 0 then
		0
	else
		let found = ref 0 in
		let len = String.length str in
		try
			for i = start to len - sublen do
				let j = ref 0 in
				while String.unsafe_get str (i + !j) = String.unsafe_get sub !j do
					incr j;
					if !j = sublen then begin found := i; raise Exit; end;
				done;
			done;
			raise Not_found
		with
			Exit -> !found

end
			
(* ------------------------------- OBJECTS ----------------------------------------------------- *)

module Object = struct

let rec get_field o fid =
	let rec loop min max =
		if min < max then begin
			let mid = (min + max) lsr 1 in
			let cid, v = Array.unsafe_get o.ofields mid in
			if cid < fid then
				loop (mid + 1) max
			else if cid > fid then
				loop min mid
			else
				v
		end else
			match o.oproto with
			| None -> VNull
			| Some p -> get_field p fid
	in
	loop 0 (Array.length o.ofields)

let set_field o fid v =
	let rec loop min max =
		let mid = (min + max) lsr 1 in
		if min < max then begin
			let cid, _ = Array.unsafe_get o.ofields mid in
			if cid < fid then
				loop (mid + 1) max
			else if cid > fid then
				loop min mid
			else
				Array.unsafe_set o.ofields mid (cid,v)
		end else
			let fields = Array.make (Array.length o.ofields + 1) (fid,v) in
			Array.blit o.ofields 0 fields 0 mid;
			Array.blit o.ofields mid fields (mid + 1) (Array.length o.ofields - mid);
			o.ofields <- fields
	in
	loop 0 (Array.length o.ofields)

let rec remove_field o fid =
	let rec loop min max =
		let mid = (min + max) lsr 1 in
		if min < max then begin
			let cid, v = Array.unsafe_get o.ofields mid in
			if cid < fid then
				loop (mid + 1) max
			else if cid > fid then
				loop min mid
			else begin
				let fields = Array.make (Array.length o.ofields - 1) (fid,VNull) in
				Array.blit o.ofields 0 fields 0 mid;
				Array.blit o.ofields (mid + 1) fields mid (Array.length o.ofields - mid - 1);
				o.ofields <- fields;
				true
			end
		end else
			false
	in
	loop 0 (Array.length o.ofields)

let rec get_field_opt o fid =
	let rec loop min max =
		if min < max then begin
			let mid = (min + max) lsr 1 in
			let cid, v = Array.unsafe_get o.ofields mid in
			if cid < fid then
				loop (mid + 1) max
			else if cid > fid then
				loop min mid
			else
				Some v
		end else
			match o.oproto with
			| None -> None
			| Some p -> get_field_opt p fid
	in
	loop 0 (Array.length o.ofields)

end

(* ------------------------------- BUILTINS ----------------------------------------------------- *)
		
module Builtin = struct

let error() = throw (VString "Invalid builtin")
let vint v = match v with VInt i -> i | _ -> error()
let vstring v = match v with VString s -> s | _ -> error()
let vfun v = match v with VFunction f -> f | _ -> error()
let varray v = match v with VArray a -> a | _ -> error()
let vobj v = match v with VObject o -> o | _ -> error()
let vhash v = match v with VAbstract (AHash h) -> h | _ -> error()

(* ---------- OBJECTS ----------- *)

let f_new o =
	match o with
	| VNull -> VObject { ofields = [||]; oproto = None }
	| VObject o -> VObject { ofields = Array.copy o.ofields; oproto = o.oproto }
	| _ -> error()

let b_new = Fun1 f_new

let f_objfields o =
	VArray (Array.map (fun (fid,_) -> VInt fid) (vobj o).ofields)
	
let b_objfields = Fun1 f_objfields

let f_field v =
	try VString (Hashtbl.find fields_cache (vint v)) with Not_found -> VNull
	
let b_field = Fun1 f_field

let f_objget o f =
	match o with
	| VObject o -> Object.get_field o (vint f)
	| _ -> VNull

let b_objget = Fun2 f_objget
	
(* ---------- MISC --------------- *)

let f_rethrow e = throw e
let b_rethrow = Fun1 f_rethrow

let f_throw e = throw e
let b_throw = Fun1 f_throw

let f_print vl =
	List.iter (fun v ->
		print_string (to_string 0 v)
	) vl;
	VNull
	
let b_print = FunVar f_print

let f_typeof v =
	VInt (match v with
	| VNull -> 0
	| VInt _ -> 1
	| VFloat _ -> 2
	| VBool _ -> 3
	| VString _ -> 4
	| VObject _ -> 5
	| VArray _ -> 6
	| VFunction _ -> 7
	| VAbstract _ -> 8)

let b_typeof = Fun1 f_typeof

let f_callstack() = VArray [||]

let b_callstack = Fun0 f_callstack

let f_excstack() = VArray [||]

let b_excstack = Fun0 f_excstack

let f_version() = VInt 100

let b_version = Fun0 f_version

(* --------- NUMBERS -------------- *)

let f_int v =
	match v with
	| VInt i -> v
	| VFloat f -> VInt (Helpers.to_int f)
	| VString s -> (try VInt (Helpers.parse_int s) with _ -> VNull)
	| _ -> VNull
	
let b_int = Fun1 f_int

let f_float v =
	match v with
	| VInt i -> VFloat (float_of_int i)
	| VFloat _ -> v
	| VString s -> (try VFloat (Helpers.parse_float s) with _ -> VNull)
	| _ -> VNull
	
let b_float = Fun1 f_float

let f_idiv a b =
	match a, b with
	| VInt a, VInt b when b != 0 -> VInt (a/b)
	| _ -> error()
	
let b_idiv = Fun2 f_idiv

(* --------- STRING --------------- *)

let f_string s =
	VString (to_string 0 s)
	
let b_string = Fun1 f_string

let f_smake len =
	VString (String.make (vint len) '\000')

let b_smake = Fun1 f_smake

let f_ssize v =
	VInt (String.length (vstring v))
	
let b_ssize = Fun1 f_ssize

let f_sset s p c =
	let c = char_of_int ((vint c) land 0xFF) in
	try
		String.set (vstring s) (vint p) c;
		VInt (int_of_char c)
	with Invalid_argument _ -> VNull
	
let b_sset = Fun3 f_sset

let f_sget s p =
	try VInt (int_of_char (String.get (vstring s) (vint p))) with Invalid_argument _ -> VNull

let b_sget = Fun2 f_sget

let f_sblit dst dstp src p l =
	String.blit (vstring src) (vint p) (vstring dst) (vint dstp) (vint l);
	VNull

let b_sblit = Fun5 f_sblit

let f_ssub s p l =
	VString (String.sub (vstring s) (vint p) (vint l))
	
let b_ssub = Fun3 f_ssub

let f_sfind src pos pat =
	try VInt (Helpers.find_sub (vstring src) (vstring pat) (vint pos)) with Not_found -> VNull
	
let b_sfind = Fun3 f_sfind

(* --------- ARRAY --------------- *)

let f_asize a =
	VInt (Array.length (varray a))
	
let b_asize = Fun1 f_asize

let f_acopy a =
	VArray (Array.copy (varray a))

let b_acopy = Fun1 f_acopy

let f_array vl =
	VArray (Array.of_list vl)
	
let b_array = FunVar f_array

let f_amake v =
	VArray (Array.create (vint v) VNull)

let b_amake = Fun1 f_amake

let f_ablit dst dstp src p l =
	Array.blit (varray src) (vint p) (varray dst) (vint dstp) (vint l);
	VNull
	
let b_ablit = Fun5 f_ablit

let f_asub a p l =
	VArray (Array.sub (varray a) (vint p) (vint l))

let b_asub = Fun3 f_asub

(* --------- FUNCTION ------- *)

let f_nargs f =
	VInt (nargs (vfun f))

let b_nargs = Fun1 f_nargs

let f_call f o args =
	do_call_gen o f (Array.to_list (varray args))

let b_call = Fun3 f_call

(* --------- HASH ----------- *)

let f_hkey v =
	VInt (Hashtbl.hash v)

let b_hkey = Fun1 f_hkey

let f_hnew v =
	VAbstract (AHash (match v with
		| VNull -> Hashtbl.create 0
		| VInt n -> Hashtbl.create n
		| _ -> error()))

let b_hnew = Fun1 f_hnew

let f_hcount v =
	VInt (Hashtbl.length (vhash v))
	
let b_hcount = Fun1 f_hcount

let f_hget h k cmp =
	if cmp <> VNull then assert false;
	(try Hashtbl.find (vhash h) k with Not_found -> VNull)
	
let b_hget = Fun3 f_hget

let f_hmem h k cmp =
	if cmp <> VNull then assert false;
	VBool (Hashtbl.mem (vhash h) k)

let b_hmem = Fun3 f_hmem

let f_hadd h k v cmp =
	if cmp <> VNull then assert false;
	let h = vhash h in
	let old = Hashtbl.mem h k in
	Hashtbl.add h k v;
	VBool (not old)
	
let b_hadd = Fun4 f_hadd

let f_hremove h k cmp =
	if cmp <> VNull then assert false;
	let h = vhash h in
	let old = Hashtbl.mem h k in
	if old then Hashtbl.remove h k;
	VBool old
	
let b_hremove = Fun3 f_hremove

let f_hset h k v cmp =
	if cmp <> VNull then assert false;
	let h = vhash h in
	let old = Hashtbl.mem h k in
	Hashtbl.replace h k v;
	VBool (not old)
	
let b_hset = Fun4 f_hset

let f_hiter h f =
	Hashtbl.iter (fun k v -> ignore (do_call_gen VNull f [k;v])) (vhash h);
	VNull
	
let b_hiter = Fun2 f_hiter

end

(* ------------------- RUNTIME ----------------------------- *)

let module_loader =
	let o = {
		ofields = [|
			hash "loadprim",VFunction (Fun2 (fun prim nargs ->
				match prim, nargs with
				| VString p, VInt n ->
					VFunction (
						match p, n with
						| "std@sprintf", 2 -> (Fun2 (fun p args -> assert false))
						| _ -> failwith ("Unknow primitive " ^ p ^ ":" ^ string_of_int n)
					)
				| _ -> assert false
			));
		|];
		oproto = None;
	} in
	VObject o
	
(* ---- TODO : Array.unsafe ! *)

let get_env i =
	Array.unsafe_get (!env) i
	
let set_env i v =
	Array.set (!env) i v

let make_env f env =
	match f with
	| VFunction f -> VClosure (f,env)
	| _ -> assert false

let get_index a i =
	match a with
	| VArray a -> if i >= 0 && i < Array.length a then Array.unsafe_get a i else VNull
	| VObject _ -> assert false (* not supported *)
	| _ -> throw (VString "Invalid array access")

let set_index a i v =
	match a with
	| VArray a -> if i >= 0 && i < Array.length a then Array.unsafe_set a i v
	| VObject _ -> assert false (* not supported *)
	| _ -> throw (VString "Invalid array access")

let do_callN f pl =
	match f with
	| VFunction (FunVar f) -> f pl
	| VClosure (FunVar f,env) -> with_env env (fun() -> f pl)
	| _ -> assert false
		(* throw (VString ("Invalid call " ^ to_string 0 f ^ " " ^ string_of_int (List.length pl))) *)
	
let do_call0 f =
	match f with
	| VFunction (Fun0 f) -> f()
	| VClosure (Fun0 f,env) -> with_env env f
	| _ -> do_callN f []
	
let do_call1 f a =
	match f with
	| VFunction (Fun1 f) -> f a
	| VClosure (Fun1 f,env) -> with_env env (fun() -> f a)
	| _ -> do_callN f [a]
	
let do_call2 f a b =
	match f with
	| VFunction (Fun2 f) -> f a b
	| VClosure (Fun2 f,env) -> with_env env (fun() -> f a b)
	| _ -> do_callN f [a;b]
	
let do_call3 f a b c =
	match f with
	| VFunction (Fun3 f) -> f a b c
	| VClosure (Fun3 f,env) -> with_env env (fun() -> f a b c)
	| _ -> do_callN f [a;b;c]
	
let do_call4 f a b c d =
	match f with
	| VFunction (Fun4 f) -> f a b c d
	| VClosure (Fun4 f,env) -> with_env env (fun() -> f a b c d)
	| _ -> do_callN f [a;b;c;d]
	
let do_call5 f a b c d e =
	match f with
	| VFunction (Fun5 f) -> f a b c d e
	| VClosure (Fun5 f,env) -> with_env env (fun() -> f a b c d e)
	| _ -> do_callN f [a;b;c;d;e]

let do_add a b =
	match a, b with
	| VInt a, VInt b -> VInt (a + b)
	| VFloat a, VFloat b -> VFloat (a +. b)
	(* no support for polymorphism *)
	| _ -> throw (VString "+")
	
let do_sub a b =
	match a, b with
	| VInt a, VInt b -> VInt (a - b)
	| VFloat a, VFloat b -> VFloat (a -. b)
	(* no support for polymorphism *)
	| _ -> throw (VString "+")

let do_mult a b =
	match a, b with
	| VInt a, VInt b -> VInt (a * b)
	| VFloat a, VFloat b -> VFloat (a *. b)
	(* no support for polymorphism *)
	| _ -> throw (VString "+")
		
let do_div a b =
	match a, b with
	| VInt a, VInt b -> VFloat (float_of_int a /. float_of_int b)
	| VFloat a, VFloat b -> VFloat (a /. b)
	(* no support for polymorphism *)
	| _ -> throw (VString "+")

let do_compare a b =
	if a == b then
		0
	else match a, b with
	| VInt a, VInt b -> if a = b then 0 else if a < b then -1 else 1
	| VFloat a, VFloat b -> if a = b then 0 else if a < b then -1 else 1
	| VString a, VString b -> if a = b then 0 else if a < b then -1 else 1
	| VBool a, VBool b -> if a = b then 0 else if a then 1 else -1
	| _ -> assert false

let is_true = function
	| VBool true -> true
	| _ -> false

let get_field o f =
	match o with
	| VObject o -> Object.get_field o f
	| _ -> throw (VString "Invalid field access")
	
let set_field o f v =
	match o with
	| VObject o -> Object.set_field o f v
	| _ -> throw (VString "Invalid field access")
