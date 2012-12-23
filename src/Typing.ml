(* Note : this does not yet produced a labeled AST,
   it only checks the type-correctness of the source
 *)

open Ast
module SMap = Map.Make(String)

(*** Praeludium ***)

(** The typing environment, and related utilities **)

type fun_prototype = ctype * ctype list

type typing_environment = { env_vars : ctype SMap.t ;
                            env_structs : decl_var list SMap.t ;
                            env_unions  : decl_var list SMap.t ;
                            env_functions : fun_prototype SMap.t }

(* Maybe there's a more elegant way to handle primitive functions *)
let initial_fun_map = SMap.add "putchar" (Int, [Int])
                               (SMap.singleton "sbrk" (Pointer Void, [Int]))

let initial_env = { env_vars = SMap.empty ;
                    env_structs = SMap.empty ;
                    env_unions = SMap.empty ;
                    env_functions = initial_fun_map }

(* Utility functions to lookup the environment *)

(* Note : the OCaml stdlib doesn't define a find function which uses an option
   type instead of the Not_found exception to signal failure *)
let maybe_find f = try Some (f ()) with Not_found -> None
let maybe_find_in_env f x env = maybe_find (fun () -> SMap.find x (f env))

let lookup_var      = maybe_find_in_env (fun e -> e.env_vars)
let lookup_struct   = maybe_find_in_env (fun e -> e.env_structs)
let lookup_union    = maybe_find_in_env (fun e -> e.env_unions)
let lookup_function = maybe_find_in_env (fun e -> e.env_functions)

(* field name -> list of fields in a struct/union -> data type of the field
   (if it exists, else None) *)
let rec assoc_field_type x = function
  | [] -> None
  | (v,k)::_ when k = x -> Some v
  | _::q -> assoc_field_type x q


(** The big list of semantic errors **)

type semantic_error =

(* Most basic error of all *)
  | TypeMismatch of ctype * ctype

(* These identifiers came out of nowhere ! *)
  | UnknownVar of string
  | UnknownStruct of string 
  | UnknownUnion of string
  | UnknownStructField of string * string
  | UnknownUnionField of string * string
  | UnknownFunction of string

(* Not the right types for a primitive operator *)
  | NonNumeric of ctype
  | ArithFail (* le nom est pas terrible *)
  | NonNumericCondition of ctype (* for if/while/for *)
      
(* What you tried to do made no sense ! *)
  | InvalidLValue
  | InvalidPointer of ctype
  | InvalidArgumentList of string (* it means the arg list was not the right length *)
  | NonComposite of ctype (* composite = struct or union *)

(* Void is not a value type ! *)
  | SizeofVoid
  | InvalidReturnVoid
  | VoidVariable 

(* Uniqueness of identifiers *)
  | NonUniqueStructId of string
  | NonUniqueUnionId of string
  | NonUniqueField of string
  | NonUniqueGlobal of string
  | NonUniqueLocal of string

(* Incorrect type expressions *)
  | MalformedType of ctype
  | SelfReferentialType of ctype

exception Error of semantic_error * Ast.location
exception NoMainFunction
exception InvalidMainFunction

let get_option_with_exn loc x err = match x with
  | None   -> raise (Error (err, loc))
  | Some y -> y

let raise_err_with_loc loc err = raise (Error (err,loc))


(** Small definitions (see project specifications) **)

let is_num_non_ptr t = List.mem t [Int; Char; TypeNull] 

(* Note : according to the formal definition of === in the spec,
   int === char but int* =/= char*. A recursive version is needed
   if the relation actually commutes with pointers *)
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
  | Subfield ((e,_), _) -> lvalue e
  | _ -> false

let rec well_formed env = function
  | TypeNull -> assert false
  | Void | Int | Char -> true
  | Struct s -> lookup_struct s env <> None (* if a struct is in the environment,
                                               its fields should already be well-formed *)
  | Union s -> lookup_union s env <> None
  | Pointer t -> well_formed env t

(** Assertions **)

let assert_num loc t =
  if not (is_num t) then raise_err_with_loc loc (NonNumeric t) 
let assert_eqv loc t1 t2 =
  if not (t1 === t2) then raise_err_with_loc loc (TypeMismatch (t1, t2))
let assert_well_formed loc env t =
  if not (well_formed env t) then raise_err_with_loc loc (MalformedType t)


(** The typing environment, part 2 :
    utility functions to modify the environment **)

let left_biased_merge =
  let f k a b = match (a,b) with
    | (Some x, _) -> Some x
    | (None, _) -> b
  in
  SMap.merge f

(* Use with fold_left, and provide the environment and location *)
let add_var_with_err loc env err var_map (typ,var) =
  if SMap.mem var var_map
  then raise_err_with_loc loc (err var)
  else begin 
    assert_well_formed loc env typ;
    if typ = Void then raise_err_with_loc loc VoidVariable;
    SMap.add var typ var_map
  end


(*** The core of the type checker ***)

(** Type checking and inference for expressions **)

let rec type_expr env (expr, loc) =
  (* This can't be just partially applied because it would be restricted
     to a monomorphic function *)
  let (<??>) x err = get_option_with_exn loc x err in
  let raise_err = raise_err_with_loc loc in

  let assert_lvalue e = if not (lvalue e) then raise_err InvalidLValue in

  begin match expr with
    | IntV x when x = Int32.zero -> TypeNull

    | IntV    _ -> Int
    | StringV _ -> Pointer Char

    | Var x -> lookup_var x env <??> UnknownVar x

    | Sizeof Void -> raise_err SizeofVoid
    | Sizeof t -> assert_well_formed loc env t; Int

    | Address (e, loc_e) -> if lvalue e
                            then Pointer (type_expr env (e, loc_e))
                            else raise_err_with_loc loc_e InvalidLValue

    (* Dereferencing the literal 0 or an int/char is not allowed
       (gcc does the same thing)
       Remember : no casts !
    *)
    | Deref e -> begin match type_expr env e with
        | Pointer t -> t
        | x -> raise_err (InvalidPointer x)
    end
      
    | Subfield (e,x) -> begin match type_expr env e with
        | Struct s -> let fields = lookup_struct s env <??> UnknownStruct s in
                      assoc_field_type x fields <??> UnknownStructField (x,s)
        | Union s -> let fields = lookup_struct s env <??> UnknownUnion s in
                     assoc_field_type x fields <??> UnknownUnionField (x,s)
        | t -> raise_err (NonComposite t)
    end

    | Assign ((l_noloc, _) as l, r) ->
        assert_lvalue l_noloc;
        let tl = type_expr env l and tr = type_expr env r in
        assert_eqv loc tl tr;
        tl
          
    | PreInc e | PreDec e | PostInc e | PostDec e ->
        assert_lvalue (fst e);
        let t = type_expr env e in
        assert_num loc t;
        t
      
    | Positive e | Negative e -> if is_num_non_ptr (type_expr env e)
                                 then Int
                                 else raise_err ArithFail (* est-ce vraiment la bonne erreur ? *)

    | Not e -> assert_num loc (type_expr env e); Int
      
    | Binop (op, e1, e2) -> begin
        let t1 = type_expr env e1 and t2 = type_expr env e2 in
        match op with
          | Equal | Different | Less | LessEq | Greater | GreaterEq -> begin
            assert_num loc t1; (* Perhaps define a InvalidOperand exception instead ? *)
            assert_eqv loc t1 t2;
            Int
          end
          | Mul | Div | Modulo | And | Or ->
            if not (is_num_non_ptr t1) then raise_err ArithFail else begin
              assert_eqv loc t1 t2;
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
          | Add | Sub (* all other cases *) -> raise_err ArithFail
          (* I'm not convinced all the above code actually does the Right Thing,
             but whatever... *)
    end

  | Apply (fn, args) ->
      let (return_type, arg_types) = lookup_function fn env <??> (UnknownFunction fn) in
      begin
        try
          List.iter2 (fun t e -> assert_eqv loc t (type_expr env e))
                     arg_types args
        with
          (* raised by iter2 when the lists have different lengths *)
          | Invalid_argument _ -> raise_err (InvalidArgumentList fn)
      end;
      return_type
  end

(** Type checking of instructions **)

(* Idée : lors de la génération de l'AST typé, permettre la transformation
   d'une instruction en liste d'instructions qu'on concatènera
   d'où emptyinstr -> [] par exemple *)

(* For now, this function returns () if the instruction is well-typed,
   else it throws an exception *)

let rec typecheck_instr ret_type env (instr, loc) =
  let raise_err = raise_err_with_loc loc in

  let assert_valid_cond e =
    let t = type_expr env e in
    if not (is_num t) then raise_err (NonNumericCondition t)
  in

  begin match instr with
    | EmptyInstr -> ()
    | ExecExpr e -> ignore (type_expr env e)

    | Return None when ret_type = Void -> ()
    | Return None                      -> raise_err InvalidReturnVoid
    | Return (Some e) -> (* problem : do we allow return f(); with void f() ? *)
      let t = type_expr env e in 
        if not (ret_type === t) then raise_err (TypeMismatch (ret_type,t)) 

    | IfThenElse (e, i1, i2) -> begin
        assert_valid_cond e;
        typecheck_instr ret_type env i1;
        typecheck_instr ret_type env i2
    end
    | While (e,i) -> begin
        assert_valid_cond e;
        typecheck_instr ret_type env i
    end

    (* Dans le nouvel AST, il faudra transformer ça en boucle while,
       et ça ne va pas être très beau...
       Au moins, il n'y aura pas à gérer le comportement de break/continue *)
    | For (init, cond_option, update, body) -> begin
        List.iter (fun e -> ignore (type_expr env e)) init;
        begin match cond_option with
          | Some cond -> assert_valid_cond cond
          | None -> ()
        end;
        List.iter (fun e -> ignore (type_expr env e)) update;
        typecheck_instr ret_type env body
    end 

    | Block b -> begin
        (* local declarations override global declarations *)
        let local_vars_map = List.fold_left (add_var_with_err loc env (fun x -> NonUniqueLocal x))
                                            SMap.empty b.block_locals in
      let new_env = { env_vars = left_biased_merge local_vars_map env.env_vars ;
                      env_structs = env.env_structs ;
                      env_unions = env.env_unions ;
                      env_functions = env.env_functions } in
      List.iter (fun instr -> typecheck_instr ret_type new_env instr) b.block_instrs
  end
end


(** Type checking of entire programs **)

(* Idée : dans l'AST typé, ne pas garder les DVars qui correspondent à
   des déclarations du genre int *a, b, ***c; mais avoir 1 déclaration
   séparée pour chacun *)

(* Idée 2 : la structure représentant le programme après le typage devrait
   être un record avec des champs séparés pour les variables globales,
   les struct/union et les fonctions
   Idée 2 bis : étendre l'environnement de typage qu'on construit pour ça,
   et faire d'une pierre deux coups
 *)

let typecheck_program program =

  (* the folding function which checks the soundness of
     the declaration and adds it to the environment *)
  let f env (decl,loc) = 
    
    (* the repetition of this line of code would be avoided
       with a dynamically scoped variable for the current location *)
    let raise_err = raise_err_with_loc loc in
    
    (* In the case of a structure or a union,
       ensure the fields are all well-formed and unique *)
    let rec assert_unique_fields = function
      | [] -> ()
      | (_,f)::q -> if List.exists (fun (_,f') -> f' = f) q
                    then raise_err (NonUniqueField f)
                    else assert_unique_fields q (* O(n^2), deal with it =p *)
    in    
    let rec is_recursive_pointer self_type = function
      | Pointer t -> is_recursive_pointer self_type t
      | t when t = self_type -> true
      | _ -> false
    in
    let assert_valid_field self_type = function
      | (Void, _) -> raise_err VoidVariable
      | (t, _) when t = self_type -> raise_err (SelfReferentialType t)
      | (Pointer t', _) when is_recursive_pointer self_type t' -> ()
      | (t, _) -> assert_well_formed loc env t
    in
    let assert_valid_fields self_type fields =
      List.iter (assert_valid_field self_type) fields;
      assert_unique_fields fields
    in
      
    match decl with
      | DVars global_vars ->
        let new_vars_map = List.fold_left (add_var_with_err loc env (fun x -> NonUniqueGlobal x))
                                          env.env_vars global_vars in
        (* Note : global vars and functions are in the same namespace ! *)
        List.iter (fun (_,var) -> if SMap.mem var env.env_functions
                                  then raise_err (NonUniqueGlobal var))
                  global_vars;
        { env_vars = new_vars_map ;
          env_structs = env.env_structs ;
          env_unions = env.env_unions ;
          env_functions = env.env_functions }

      | DType (DStruct (name, fields)) ->
        if SMap.mem name env.env_structs then raise_err (NonUniqueStructId name)
        else begin
          assert_valid_fields (Struct name) fields;
          { env_vars = env.env_vars ;
            env_structs = SMap.add name fields env.env_structs ;
            env_unions = env.env_unions ;
            env_functions = env.env_functions }
        end

      | DType (DUnion (name, fields)) ->
        if SMap.mem name env.env_unions then raise_err (NonUniqueUnionId name)
        else begin
          assert_valid_fields (Union name) fields;
          { env_vars = env.env_vars ;
            env_structs = env.env_structs ;
            env_unions = SMap.add name fields env.env_unions ;
            env_functions = env.env_functions }
        end

      | DFun fn -> begin
        (* Refer to previous note on namespaces *)
        if SMap.mem fn.fun_name env.env_vars ||
           SMap.mem fn.fun_name env.env_functions
        then raise_err (NonUniqueGlobal fn.fun_name);
        let fn_proto = (fn.fun_return_type, List.map fst fn.fun_args) in

        let arg_map = List.fold_left (add_var_with_err loc env (fun x -> NonUniqueLocal x))
                                     SMap.empty fn.fun_args in
        List.iter (fun (_,var) -> if SMap.mem var arg_map then raise_err (NonUniqueLocal var))
                  fn.fun_body.block_locals;
        let function_env =  { env_vars = left_biased_merge arg_map env.env_vars ;
                              env_structs = env.env_structs ;
                              env_unions = env.env_unions ;
                              env_functions = SMap.add fn.fun_name fn_proto
                                                       env.env_functions } in
        typecheck_instr fn.fun_return_type function_env ((Block fn.fun_body), loc);

        { env_vars = env.env_vars ;
          env_structs = env.env_structs ;
          env_unions = env.env_unions ;
          env_functions = SMap.add fn.fun_name fn_proto env.env_functions }
      end
  in
  let final_env = List.fold_left f initial_env program in
  match lookup_function "main" final_env with
    | None -> raise NoMainFunction
    | Some (Int, []) | Some (Int, [Int; Pointer (Pointer Char)]) -> ()
    | _ -> raise InvalidMainFunction


(*** Le meilleur pour la fin : les messages d'erreur ! ***)

let rec string_of_type  = function
  | Void -> "void"
  | Int -> "int"
  | Char -> "char"
  | Struct a -> "struct "^a
  | Union a -> "union "^a
  | TypeNull -> "TypeNull"
  | Pointer a -> (string_of_type a )^"*"


let error_message = function
  | TypeMismatch (a, b) -> "Expected type "^(string_of_type a)^
    " isn't of type "^ (string_of_type b)

  | UnknownVar a -> "Variable name "^a^" is not defined"
  | UnknownStruct a -> "Struct name "^a^" is not defined"
  | UnknownUnion a -> "Union name "^a^" is not defined"
  | UnknownStructField (a, b) -> "Field name "^a^" is not defined in struct "^b
  | UnknownUnionField  (a, b) -> "Field name "^a^" is not defined in union "^b
  | UnknownFunction a -> "Function name "^a^" is not defined"

  | NonNumeric t -> "Cannot apply numeric operation to "^string_of_type t
  | ArithFail -> "This arithmetic operation is illegal"
  | NonNumericCondition t -> "Branching requires numeric type instead of "^string_of_type t
      
  | InvalidLValue -> "This is not an lvalue"
  | InvalidPointer x -> "Type "^(string_of_type x)^" cannot be dereferenced"
  | InvalidArgumentList f -> "Function "^f^" isn't applied to the correct number of arguments"
  | NonComposite t -> "Target element of type "^(string_of_type t)^
                      " is neither a struct nor an union and has no subfield"

  | SizeofVoid ->  "Sizeof is not applied to an adequate argument"
  | InvalidReturnVoid -> "This function cannot return void"
  | VoidVariable -> "A variable cannot be of type void"

  | NonUniqueStructId a -> "Struct identifier "^a^" is already defined"
  | NonUniqueUnionId  a -> "Union identifier "^a^" is already defined"
  | NonUniqueField a -> "Field identifier "^a^" is already defined"
  | NonUniqueGlobal a -> "Global identifier "^a^" is already defined"
  | NonUniqueLocal a -> "Local variable  "^a^" is already defined"

  | MalformedType t -> "Type "^(string_of_type t)^" is not well-formed"
  | SelfReferentialType a -> "Type "^(string_of_type a)^
    " cannot contain a field of type "^string_of_type a
      
