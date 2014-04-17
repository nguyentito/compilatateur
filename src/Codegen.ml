open Ast.Typed
open Mips

(* Premier jet : on ne va gérer que des arguments de taille 4 octets *)

(* Do not handle recursive calls to main!
   Anyway, they are forbidden by the C++ standard... *)

let putchar =
  label "putchar"
  ++ sub sp sp oi 8
  ++ sw ra areg (0, sp)
  ++ sw fp areg (4, sp)
  ++ add fp sp oi 4

  ++ lw a0 areg (4, fp)
  ++ li v0 11
  ++ syscall

  ++ add sp fp oi 4
  ++ lw ra areg (-4, fp)
  ++ lw fp areg (0, fp)
  ++ jr ra

let compile_instr : instr -> Mips.text = fun i ->
  match i with
  | ExecExpr (Int, Apply (fn_id, [(Int, IntV x)])) ->
    sub sp sp oi 8
    ++ li a0 (Int32.to_int x) (* TODO: how to load big immediates? *)
    ++ sw a0 areg (0, sp)
    ++ jal fn_id
    ++ add sp sp oi 8
    
  | _ -> failwith "not supported yet"

let compile_block : block -> Mips.text = fun b ->
  let instrs = b.block_instrs in
  List.fold_right (++) (List.map compile_instr instrs) nop

let compile_program : Ast.Typed.program -> Mips.program
  = fun program ->
    let funs = program.prog_funs in
    let main_block = (List.find (fun x -> x.fun_name = "main") funs).fun_body in
    
    { text =
        label "main"
        ++ compile_block main_block
        ++ li v0 10
        ++ syscall

        ++ putchar;

      data = nop
    }



