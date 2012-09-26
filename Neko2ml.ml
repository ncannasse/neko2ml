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

let empty_env = [||]

let vtrue = VBool true

let vfalse = VBool false

let throw v =
	raise (Runtime v)
	
let return v =
	raise (Return v)
	
let todo str =
	failwith str
	
(* ---- TODO : Array.unsafe ! *)

let get_env (env : value array) i =
	Array.get env i
	
let set_env (env : value array) i v =
	Array.set env i v
	
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
	
let do_call0 f =
	match f with
	| VFunction (Fun0 f) -> f()
	| VFunction (FunVar f) -> f []
	| _ -> throw (VString "Invalid call")
	
let do_call1 f a =
	match f with
	| VFunction (Fun1 f) -> f a
	| VFunction (FunVar f) -> f [a]
	| _ -> throw (VString "Invalid call")
	
let do_call2 f a b =
	match f with
	| VFunction (Fun2 f) -> f a b
	| VFunction (FunVar f) -> f [a;b]
	| _ -> throw (VString "Invalid call")
	
let do_call3 f a b c =
	match f with
	| VFunction (Fun3 f) -> f a b c
	| VFunction (FunVar f) -> f [a;b;c]
	| _ -> throw (VString "Invalid call")
	
let do_call4 f a b c d =
	match f with
	| VFunction (Fun4 f) -> f a b c d
	| VFunction (FunVar f) -> f [a;b;c;d]
	| _ -> throw (VString "Invalid call")
	
let do_call5 f a b c d e =
	match f with
	| VFunction (Fun5 f) -> f a b c d e
	| VFunction (FunVar f) -> f [a;b;c;d;e]
	| _ -> throw (VString "Invalid call")

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

