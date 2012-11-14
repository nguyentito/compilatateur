(* Note : this does not yet produced a labeled AST,
   it only checks the type-correctness of the source *)

(*open Ast*)
module SMap = Map.Make(String)

(*** Praeludium ***)

(** The typing environment, and related utilities **)

type fun_prototype = ctype * ctype list

type typing_environment = { env_vars : ctype SMap.t ;
                            env_structs : decl_var list SMap.t ;
                            env_unions  : decl_var list SMap.t ;
                            env_functions : fun_prototype SMap.t }

(* Note : the OCaml stdlib doesn't define a find function which uses an option
   type instead of the Not_found exception to signal failure
   This is f**king stupid ! *)
let maybe_find f = try Some (f ()) with Not_found -> None
let maybe_find_in_env f x env = maybe_find (fun () -> SMap.find x (f env))
let lookup_var      = maybe_find_in_env (fun e -> e.env_vars)
let lookup_struct   = maybe_find_in_env (fun e -> e.env_structs)
let lookup_union    = maybe_find_in_env (fun e -> e.env_unions)
let lookup_function = maybe_find_in_env (fun e -> e.env_functions)

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

let is_ptr = function
  | Pointer _ -> true
  | TypeNull -> true
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
exception UnknownFunction

(* Not the right types for a primitive operator *)
exception NonNumeric
exception ArithFail (* le nom est pas terrible *)
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
            if not (is_num_non_ptr t1) then raise ArithFail else begin
              ignore (assert_eqv t1 t2);
              Int
            end
        (* Remember to give different constructors
           to int+int and ptr+int in the eventual typed AST *)
        (* The following is a way to solve the non-determinism which appears
           in the formal type system rules when trying to infer the type of 0+0 *)
        | Add | Sub when t1 = TypeNull && t2 = TypeNull -> TypeNull
        | Add | Sub when is_num_non_ptr t1 && (t1 === t2) -> Int
        (* Note : void* + int = void*, but typenull + int = int *)
        | Add | Sub when is_ptr t1 && is_num_non_ptr t2 -> t1
        | Add       when is_num_non_ptr t1 && is_ptr t2 -> t2
        | Sub when is_ptr t1 && t1 === t2 -> Int
        | Add | Sub (* all other cases *) -> raise ArithFail
        (* I'm not convinced all the above code actually does the Right Thing,
           but whatever... *)
    end

  | Apply (fn, args) ->
      let (return_type, arg_types) = lookup_function fn env
                                   <??> UnknownFunction in
      List.iter2 (fun t e -> ignore (assert_eqv t (type_expr env e)))
                 arg_types args;
      return_type


