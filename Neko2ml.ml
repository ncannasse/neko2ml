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

let acc = ref VNull

