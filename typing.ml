(* Note : this does not yet produced a labeled AST,
   it only checks the type-correctness of the source *)

(*open Ast*)
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

let get_option_with_exn x exn = match x with
  | None   -> raise exn
  | Some y -> y

(* Pas très joli... *)
let (<??>) x exn = get_option_with_exn x exn

(** Small definitions **)

let is_num_non_ptr t = List.mem t [Int; Char; TypeNull] 

let (===) a b = match (a,b) with
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
   Add arguments to the exceptions to allow precise diagnostics
   Come up with relevant human-readable error messages to print
*)

(* Most basic error of all *)
exception TypeMismatch

(* These identifiers came out of nowhere ! *)
exception UnknownVar
exception UnknownStruct
exception UnknownUnion
exception UnknownStructField
exception UnknownUnionField

(* Not the right types for a primitive operator *)
exception NonNumeric
exception NonArith (* le nom est pas terrible *)
exception SizeofVoid

(* What you tried to do made no sense ! *)
exception InvalidLValue
exception InvalidPointer
exception NonComposite (* composite = struct or union *)

let assert_num t = if is_num t then t else raise NonNumeric
let assert_eqv t1 t2 = if t1 === t2 then t1 else raise TypeMismatch

let rec type_expr env = function
  | IntV x when x = Int32.zero -> TypeNull

  | IntV    _ -> Int
  | StringV _ -> Pointer Char

  | Var x -> lookup_var x env <??> UnknownVar

  | Sizeof Void -> raise SizeofVoid
  | Sizeof _    -> Int (* TODO : add the well-formed condition *)

  | Address e when lvalue e -> Pointer (type_expr env e)
  | Address e               -> raise InvalidLValue

  (* Dereferencing the literal 0 or an int/char is not allowed
     (gcc does the same thing)
     Remember : no casts !
  *)
  | Deref e -> begin match type_expr env e with
                       | Pointer t -> t
                       | _ -> raise InvalidPointer
               end

  | Subfield (e,x) -> begin
      match type_expr env e with
        | Struct s -> let fields = lookup_struct s env <??> UnknownStruct in
                      assoc_field_type x fields <??> UnknownStructField
        | Union s -> let fields = lookup_struct s env <??> UnknownUnion in
                     assoc_field_type x fields <??> UnknownUnionField
        | _ -> raise NonComposite
      end

  | Assign (l, r) when lvalue l ->
      let tl = type_expr env l and tr = type_expr env r in
      assert_eqv tl tr
  | Assign (l,_) -> raise InvalidLValue
  
  | PreInc e | PreDec e | PostInc e | PostDec e ->
      if not (lvalue e)
      then raise InvalidLValue
      else assert_num (type_expr env e)
      
  | Positive e | Negative e -> assert_num (type_expr env e)

  | Binop (op, e1, e2) -> begin
      let t1 = type_expr env e1 and t2 = type_expr env e2 in
      match op with
        | Equal | Different | Less | LessEq | Greater | GreaterEq -> begin
            ignore (assert_num t1); (* Perhaps define a InvalidOperand exception instead ? *)
            ignore (assert_eqv t1 t2);
            Int
          end
        | Mul | Div | Modulo | And | Or ->
            if not (is_num_non_ptr t1) then raise NonArith else begin
              ignore (assert_eqv t1 t2);
              Int
            end
      | Add | Sub -> failwith ""
  end
      
