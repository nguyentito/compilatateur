(* Note : this does not yet produced a labeled AST,
   it only checks the type-correctness of the source *)

open Ast
module SMap = Map.Make(String)

(*** Praeludium ***)

(** The typing environment, and related utilities **)

type typing_environment = { env_vars : ctype SMap.t ;
                            env_structs : decl_var list SMap.t ;
                            env_unions  : decl_var list SMap.t }

(* Note : the OCaml stdlib doesn't define a find function which uses an option
   type instead of the Not_found exception to signal failure
   This is f**king stupid ! *)
let maybe_find f = try Some (f ()) with Not_found -> None
let lookup_var    x env = maybe_find (fun () -> SMap.find x env.env_vars)
let lookup_struct x env = maybe_find (fun () -> SMap.find x env.env_structs)
let lookup_union  x env = maybe_find (fun () -> SMap.find x env.env_unions)

let rec assoc_field_type x = function
  | [] -> None
  | (v,k)::_ when k = x -> Some v
  | _::q -> assoc_field_type x q


(** Small definitions **)

let is_num_non_ptr t = List.mem t [Int; Char; TypeNull] 

let eqv a b = match (a,b) with
  | _ when a = b -> true
  | _ when is_num_non_ptr a && is_num_non_ptr b -> true
  | (TypeNull, Pointer _) | (Pointer _, TypeNull) -> true
  | (Pointer Void, Pointer _) | (Pointer _, Pointer Void) -> true
  | _ -> false

let is_num = function
  | Pointer _ -> true
  | t when is_num_non_ptr t -> true
  | _ -> false

let rec lvalue = function
  | Var   _ -> true
  | Deref _ -> true
  | Subfield (e, _) -> lvalue e
  | _ -> false


(*** The core of the type checker ***)

(** Type checking of expressions **)

(* A lot of possible semantic errors !
   TODO :
   Add location info later
   Come up with relevant human-readable error messages to print
*)

(* Most basic error of all *)
exception TypeMismatch of expr * expr

(* These identifiers came out of nowhere ! *)
exception UnknownVar    of string
exception UnknownStruct of string
exception UnknownUnion  of string
exception UnknownStructField of string * string
exception UnknownUnionField  of string * string

(* Not the right types for a primitive operator *)
exception NonNumeric of expr
exception SizeofVoid

(* What you tried to do made no sense ! *)
exception InvalidLValue of expr
exception InvalidPointer of expr
exception NonComposite of expr (* composite = struct or union *)

let rec type_expr env = function
  | IntV x when x = Int32.zero -> TypeNull

  | IntV    _ -> Int
  | StringV _ -> Pointer Char

  | Var x -> begin match lookup_var x env with
                     | None -> raise (UnknownVar x)
                     | Some t -> t
             end 

  | Sizeof Void -> raise SizeofVoid
  | Sizeof _    -> Int (* TODO : add the well-formed condition *)

  | Address e when lvalue e -> Pointer (type_expr env e)
  | Address e               -> raise (InvalidLValue e)

  (* Dereferencing the literal 0 or an int/char is not allowed
     (gcc does the same thing)
     Remember : no casts !
  *)
  | Deref e -> begin match type_expr env e with
                       | Pointer t -> t
                       | _ -> raise (InvalidPointer e)
               end

  | Subfield (e,x) -> begin (* TODO : clean up the 3 levels deep match/with *)
      match type_expr env e with
        | Struct s -> begin
            match lookup_struct s env with
              | None -> raise (UnknownStruct s)
              | Some fields -> match assoc_field_type x fields with
                  | None -> raise (UnknownStructField (s,x))
                  | Some t -> t
            end
        | Union s -> begin
            match lookup_union s env with
              | None -> raise (UnknownUnion s)
              | Some fields -> match assoc_field_type x fields with
                  | None -> raise (UnknownUnionField (s,x))
                  | Some t -> t
            end
        | _ -> raise (NonComposite e)
      end

  | Assign (l, r) when lvalue l ->
      let tl = type_expr env l and tr = type_expr env r in
      if eqv tl tr then tl else raise (TypeMismatch (l,r))
  | Assign (l,_) -> raise (InvalidLValue l)
  
