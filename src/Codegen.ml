open Ast.Typed
open Mips

(* Premier jet : on ne va gérer que des arguments de taille 4 octets *)

(* Do not handle recursive calls to main!
   Anyway, they are forbidden by the C++ standard... *)

(* No need for linking with outside: we can whoose any
   naming convention whatsoever for labels!

   global variables and functions get "global_" prefix
   conditionals and loops get "local_" prefix
*)

(* global supply of fresh names starting with "local_" *)
let gensym : unit -> string =
  let r = ref 0 in
  fun () -> let x = !r in r := x + 1;
            "local_" ^ string_of_int x

(* TODO: virer ce cérémonial *)
let putchar : Mips.text =
  label "global_putchar"
  ++ sub sp sp oi 8
  ++ sw ra areg (0, sp)
  ++ sw fp areg (4, sp)
  ++ add fp sp oi 4

  ++ lw a0 areg (4, fp)
  ++ li v0 11
  ++ syscall

  ++ move v0 a0

  ++ add sp fp oi 4
  ++ lw ra areg (-4, fp)
  ++ lw fp areg (0, fp)
  ++ jr ra

let sbrk : Mips.text =
  label "global_sbrk"
  ++ nop (* TODO *)

(* no stream fusion :-( *)
let sequence : text list -> text
  = fun xs -> List.fold_right (++) xs nop
let seqmap : ('a -> text) -> 'a list -> text
  = fun f xs -> sequence (List.map f xs)


type stack_frame = { sf_args   : string list;
                     sf_locals : string list }

let find_index : 'a -> 'a list -> int
  = fun x ->
    let rec loop acc = function
    | y :: ys when y = x -> acc
    | _ :: ys -> loop (acc+1) ys
    | [] -> raise Not_found
    in
    loop 0

let get_fp_offset : string -> stack_frame -> int
  = fun id sf ->
    try
      4 * (1 + find_index id (List.rev sf.sf_args))
    with Not_found ->
      try
        -4 * (2 + find_index id sf.sf_locals)
      with Not_found ->
        (* the typing phase should guarantee this does not happen *)
        assert false


let rec compile_expr : stack_frame -> expr -> text
  = fun sf -> function
    | (_, IntV x) -> li v0 (Int32.to_int x) (* TODO: how to load big immediates? *)
    | (_, LValue lv) -> eval_lvalue sf lv

    | (_, Apply (fn_id, args)) ->
      seqmap (eval_and_push sf) args
      ++ jal ("global_" ^ fn_id)
      ++ add sp sp oi (4 * List.length args)

    | (_, Assign (Var id, e)) ->
      compile_expr sf e
      ++ sw v0 areg (get_fp_offset id sf, fp)

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
    | Var id -> lw v0 areg (get_fp_offset id sf, fp)
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

    | Block b -> compile_block sf b

    | _ -> failwith "not supported yet instr"

and compile_block : stack_frame -> block -> text = fun sf b ->
  let locals = b.block_locals in
  let sf = { sf with sf_locals = sf.sf_locals
                                 @ List.map snd b.block_locals } in
  begin
    if locals = [] then nop
    else sub sp sp oi (4 * List.length locals)
  end
  ++ seqmap (compile_instr sf) b.block_instrs

let compile_function : decl_fun -> text =
  fun f -> let sf = { sf_args = List.map snd f.fun_args;
                      sf_locals = [] } in
    label ("global_" ^ f.fun_name)
    (* save $fp and $ra *)
    ++ sub sp sp oi 8
    ++ sw ra areg (0, sp)
    ++ sw fp areg (4, sp)
    ++ add fp sp oi 4

    ++ compile_block sf f.fun_body

    (* return in case the function doesn't call return;
       (possible is the function returns void, or if it's main() *)
    ++ add sp fp oi 4
    ++ lw ra areg (-4, fp)
    ++ lw fp areg (0, fp)
    ++ jr ra


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

        (* ++ compile_block main_block *)
        (* ++ li v0 10 *)
        (* ++ syscall *)

        ++ putchar
        ++ sbrk;
      
      data =
        nop
    }



