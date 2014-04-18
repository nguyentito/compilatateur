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
  ++ lw a0 areg (0, sp)
  ++ li v0 9
  ++ syscall
  ++ jr ra

let return_text = add sp fp oi 4
                  ++ lw ra areg (-4, fp)
                  ++ lw fp areg (0, fp)
                  ++ jr ra

let sizeof = function
  | Int -> 4
  | Char -> 1
  | Pointer _ -> 4

(* no stream fusion :-( *)
let sequence : ([< `data | `text] as 'a) asm list -> 'a asm
  = fun xs -> List.fold_right (++) xs nop
let seqmap : ('a -> ([< `data | `text] as 'b) asm) -> 'a list -> 'b asm
  = fun f xs -> sequence (List.map f xs)


(* See old git snapshots for the memloc type... *)
(* was used to provide a less inefficient support for
   reading/writing statically known locations *)
(* TODO: make the performance of incrementing
   a local variable less terrible *)

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

let load_loc : register -> text
  = fun r -> lw r areg (0, v0)

let store_loc : register -> text
  = fun r -> sw r areg (0, v0)

(** recursive AST traversal **)

let rec compile_expr : stack_frame -> expr -> text
  = fun sf -> function
    | (_, IntV x) -> li32 v0 x
    | (_, StringV s) -> assert false (* TODO *)
      
    | (_, LValue lv) -> eval_lvalue_loc sf lv
                        ++ load_loc v0

    | (_, Apply (fn_id, args)) ->
      seqmap (eval_and_push sf) args
      ++ jal ("global_" ^ fn_id)
      ++ add sp sp oi (4 * List.length args)

    | (_, Assign (lv, e)) ->
      compile_expr sf e
      ++ push v0
      ++ eval_lvalue_loc sf lv
      ++ pop a0
      ++ store_loc a0

    | (_, Unop (op, e)) -> compile_expr sf e
                           ++ begin match op with
                             | Positive -> nop
                             | Negative -> neg v0 v0
                             | Not -> seq v0 v0 zero
                           end

    | (__, Binop (op, ((t1, _) as e1), ((t2, _) as e2))) -> begin match op with
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
          ++ begin
            match op, t1, t2 with
            | Sub, Pointer t1', Pointer t2' ->
              sub v0 a0 oreg v0
              ++ div v0 v0 oi (sizeof t1')
            | (Add | Sub), Pointer t1', _ ->
              mul v0 v0 oi (sizeof t1')
              ++ arith v0 a0 oreg v0
            | Add, _, Pointer t2' ->
              mul a0 a0 oi (sizeof t2')
              ++ arith v0 a0 oreg v0
            | _ -> arith v0 a0 oreg v0
          end
      end

    | (t, Incr (op, lv)) -> 
      let factor = match t with
        | Pointer t' -> sizeof t'
        | _          -> 1
      in
      let imm = match op with
        | PreIncr | PostIncr -> +factor
        | PreDecr | PostDecr -> -factor
      in
      eval_lvalue_loc sf lv
      ++ load_loc a0
      ++ begin match op with
        | PreIncr  | PreDecr  -> add a0 a0 oi imm
                                 ++ store_loc a0
        | PostIncr | PostDecr -> add a1 a0 oi imm
                                 ++ store_loc a1
      end
      ++ move v0 a0
                                   

    (* don't handle upwards struct-args for now *)
    (* variable.field is LValue (LSubfield ...) *)
    | (_, Subfield _) -> assert false

    | (_, Address lv) -> eval_lvalue_loc sf lv

    | (_, Sizeof ctype) -> li v0 4

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

and eval_lvalue_loc : stack_frame -> lvalue -> text
  = fun sf -> function
    | Var id -> begin
        try
          let offset = SMap.find id sf.sf_locals in
          la v0 areg (offset, fp)
        with Not_found -> 
          la v0 alab ("global_" ^ id)
      end
    | Deref e -> compile_expr sf e
    | LSubfield _ -> failwith "not implemented yet subfield"



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

    | While (cond, instr) ->
      let start_label = gensym () in
      let end_label   = gensym () in
      b end_label
      ++ label start_label
      ++ compile_instr sf instr
      ++ label end_label
      ++ compile_expr sf cond
      ++ bnez v0 start_label
      
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



