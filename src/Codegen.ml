open Ast.Typed
open Mips

module SMap = Map.Make(String)

(* Premier jet : on ne va gérer que des arguments de taille 4 octets *)

(* Do not handle recursive calls to main!
   Anyway, they are forbidden by the C++ standard... *)

(* No need for linking with outside: we can choose any
   naming convention whatsoever for labels!

   global variables and functions get "global_" prefix
   conditionals and loops get "local_" prefix
*)

(* global supply of fresh names starting with "local_" *)
let gensym : unit -> string =
  let r = ref 0 in
  fun () -> let x = !r in r := x + 1;
            "local_" ^ string_of_int x

let putchar : Mips.text =
  label "global_putchar"
  ++ lw a0 areg (0, sp)
  ++ li v0 11
  ++ syscall
  ++ move v0 a0
  ++ jr ra

let sbrk : Mips.text =
  label "global_sbrk"
  ++ nop (* TODO *)

let return_text = add sp fp oi 4
                  ++ lw ra areg (-4, fp)
                  ++ lw fp areg (0, fp)
                  ++ jr ra

(* no stream fusion :-( *)
let sequence : ([< `data | `text] as 'a) asm list -> 'a asm
  = fun xs -> List.fold_right (++) xs nop
let seqmap : ('a -> ([< `data | `text] as 'b) asm) -> 'a list -> 'b asm
  = fun f xs -> sequence (List.map f xs)


(** lvalues, addresses, scope **)

(* *static* memory locations *)
type memloc = Stack of int (* offset from $fp *)
            | StaticHeap of string (* label for global var *)
            (* TODO: pointers *)
                      
type stack_frame = { sf_locals : int SMap.t;
                     sf_top : int }

let sf_of_funargs args = 
  let f (map, depth) (t, id) =
    (SMap.add id depth map, depth + 4) (* TODO: handle chars and structs *)
  in
  { sf_locals = fst (List.fold_left f (SMap.empty, 4) (List.rev args));
    sf_top = -4 }

let push_locals_on_sf vars sf =
  let f (map, depth) (t, id) =
    let depth = depth - 4 in
    (SMap.add id depth map, depth)
  in
  { sf with sf_locals = fst (List.fold_left f (sf.sf_locals, sf.sf_top) vars) }

let memloc_of_lvalue : stack_frame -> lvalue -> memloc
  = fun sf -> function
    | Var id -> begin
        try Stack (SMap.find id sf.sf_locals)
        with Not_found -> StaticHeap ("global_" ^ id)
      end
    | _ -> raise (Invalid_argument "memloc_of_lvalue")

let load_memloc : register -> memloc -> text
  = fun reg -> function
    | Stack offset -> lw reg areg (offset, fp)
    | StaticHeap label -> lw reg alab label

let store_memloc : register -> memloc -> text
  = fun reg -> function
    | Stack offset -> sw reg areg (offset, fp)
    | StaticHeap label -> sw reg alab label


(** recursive AST traversal **)

let rec compile_expr : stack_frame -> expr -> text
  = fun sf -> function
    | (_, IntV x) -> li v0 (Int32.to_int x) (* TODO: how to load big immediates? *)
    | (_, LValue lv) -> eval_lvalue sf lv

    | (_, Apply (fn_id, args)) ->
      seqmap (eval_and_push sf) args
      ++ jal ("global_" ^ fn_id)
      ++ add sp sp oi (4 * List.length args)

    | (_, Assign (lv, e)) ->
      compile_expr sf e
      ++ store_memloc v0 (memloc_of_lvalue sf lv)


    | (__, Binop (op, e1, e2)) -> begin match op with
        (* short-circuiting logical operators *)
        | And | Or ->
          let jump = match op with
            | And -> beqz
            | Or  -> bnez
            | _   -> assert false
          in
          let end_label = gensym () in
          compile_expr sf e1
          ++ jump v0 end_label
          ++ compile_expr sf e2
          ++ label end_label
          
        (* comparisons *)
        | Equal | Different | Less | LessEq | Greater | GreaterEq ->
          let cmp = match op with
            | Equal     -> seq
            | Different -> sne
            | Less      -> slt
            | LessEq    -> sle 
            | Greater   -> sgt
            | GreaterEq -> sge
            | _         -> assert false
          in
          compile_expr sf e1
          ++ push v0
          ++ compile_expr sf e2
          ++ pop a0
          ++ cmp v0 a0 v0

        | Add | Sub | Mul | Div | Modulo ->
          let arith = match op with
            | Add -> add
            | Sub -> sub
            | Mul -> mul
            | Div -> div
            | Modulo -> rem
            | _ -> assert false
          in
          compile_expr sf e1
          ++ push v0
          ++ compile_expr sf e2
          ++ pop a0
          ++ arith v0 a0 oreg v0
      end

    | (_, Incr (op, lv)) -> begin
        let loc = memloc_of_lvalue sf lv
        and imm = match op with
          | PreIncr | PostIncr -> +1
          | PreDecr | PostDecr -> -1
        in
        load_memloc v0 loc
        ++ match op with
        | PreIncr  | PreDecr  -> add v0 v0 oi imm
                                 ++ store_memloc v0 loc
        | PostIncr | PostDecr -> add a0 v0 oi imm
                                 ++ store_memloc a0 loc
      end

    | _ -> failwith "not supported yet expr"

and eval_and_push : stack_frame -> expr -> text
  = fun sf -> function
    | (Int, e) -> compile_expr sf (Int, e)
                  ++ sub sp sp oi 4
                  ++ sw v0 areg (0, sp)
    (* do something about typenull! *)
    | (TypeNull, e) -> compile_expr sf (Int, e)
                  ++ sub sp sp oi 4
                  ++ sw v0 areg (0, sp)
    | _ -> failwith "not supported yet push"

and eval_lvalue : stack_frame -> lvalue -> text
  = fun sf -> function
    | (Var _) as x -> load_memloc v0 (memloc_of_lvalue sf x)
    | _ -> failwith "not supported yet lvalue"


let rec compile_instr : stack_frame -> instr -> text
  = fun sf -> function
    | EmptyInstr -> nop

    | ExecExpr e -> compile_expr sf e

    | IfThenElse (cond, then_branch, EmptyInstr) ->
      let end_label = gensym () in
      compile_expr sf cond
      ++ beqz v0 end_label
      ++ compile_instr sf then_branch
      ++ label end_label

    | IfThenElse (cond, then_branch, else_branch) ->
      let else_label = gensym () in
      let end_label  = gensym () in
      compile_expr sf cond
      ++ beqz v0 else_label
      ++ compile_instr sf then_branch
      ++ b end_label
      ++ label else_label
      ++ compile_instr sf else_branch
      ++ label end_label

    (* | While (cond, instr) -> *)
      
    | For (init, cond_option, update, body) ->
      seqmap (compile_expr sf) init ++ begin match cond_option with
        | None ->
          let start_label = gensym () in
          label start_label
          ++ compile_instr sf body
          ++ seqmap (compile_expr sf) update
          ++ b start_label
        | Some cond ->
          let start_label = gensym () in
          let end_label   = gensym () in
          b end_label
          ++ label start_label
          ++ compile_instr sf body
          ++ seqmap (compile_expr sf) update
          ++ label end_label
          ++ compile_expr sf cond
          ++ bnez v0 start_label
      end

    | Return None -> return_text
    | Return (Some e) -> compile_expr sf e ++ return_text

    | Block b -> compile_block sf b

    | _ -> failwith "not supported yet instr"

and compile_block : stack_frame -> block -> text = fun sf b ->
  let locals = b.block_locals in
  let sf = push_locals_on_sf locals sf in
  begin
    if locals = [] then nop
    else sub sp sp oi (4 * List.length locals)
  end
  ++ seqmap (compile_instr sf) b.block_instrs

let compile_function : decl_fun -> text =
  fun f -> 
    label ("global_" ^ f.fun_name)
    (* save $fp and $ra *)
    ++ sub sp sp oi 8
    ++ sw ra areg (0, sp)
    ++ sw fp areg (4, sp)
    ++ add fp sp oi 4

    ++ compile_block (sf_of_funargs f.fun_args) f.fun_body

    (* return without value in case the function doesn't call return;
       (possible is the function returns void, or if it's main() *)
    ++ return_text

let compile_program : Ast.Typed.program -> Mips.program
  = fun program ->
    { text =
        (* entry point of the program
           pass argc and argv to main() (whose label is global_main) *)
        label "main"
        ++ sub sp sp oi 8
        ++ sw a0 areg (4, sp)
        ++ sw a1 areg (0, sp)
        ++ jal "global_main"

        (* exit with return value of main *)
        ++ move a0 v0
        ++ li v0 17 
        ++ syscall

        (* all functions... *)
        ++ seqmap compile_function program.prog_funs

        ++ putchar
        ++ sbrk;
      
      data =
        seqmap (fun (t, id) -> label ("global_" ^ id)
                               ++ dword [0]) program.prog_globals
    }



