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


type stack_frame = string list (* loool *)

let rec compile_expr : expr -> text = function
  | (_, IntV x) -> li v0 (Int32.to_int x) (* TODO: how to load big immediates? *)
  | (_, Apply (fn_id, args)) ->
    seqmap eval_and_push args
    ++ jal ("global_" ^ fn_id)
    ++ add sp sp oi (4 * List.length args)
  | _ -> failwith "not supported yet"

and eval_and_push : expr -> text = function
  | (Int, e) -> compile_expr (Int, e)
                ++ sub sp sp oi 4
                ++ sw v0 areg (0, sp)
  | _ -> failwith "not supported yet"


let compile_instr : instr -> text = fun i ->
  match i with
  | ExecExpr e -> compile_expr e
  | _ -> failwith "not supported yet"

let compile_block : block -> text = fun b ->
  seqmap compile_instr b.block_instrs

let compile_function : decl_fun -> text =
  fun f -> label ("global_" ^ f.fun_name)
           (* save $fp and $ra *)
           ++ sub sp sp oi 8
           ++ sw ra areg (0, sp)
           ++ sw fp areg (4, sp)
           ++ add fp sp oi 4

           ++ compile_block f.fun_body

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



