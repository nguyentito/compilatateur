open Ast.Typed
open Mips

module SMap = Map.Make(String)

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

(* no stream fusion :-( *)
let sequence : ([< `data | `text] as 'a) asm list -> 'a asm
  = fun xs -> List.fold_right (++) xs nop
let seqmap : ('a -> ([< `data | `text] as 'b) asm) -> 'a list -> 'b asm
  = fun f xs -> sequence (List.map f xs)


(***** Global vars! *****)

let register_string, data_segment_string =
  let ctr = ref (-1) and dss = ref nop in
  (fun s ->
     incr ctr;
     let l = "string_" ^ string_of_int !ctr in
     dss := !dss ++ label l ++ byte s;
     l
  ),
  (fun () -> !dss)

let fun_protos : ctype list SMap.t ref = ref SMap.empty

type aggregate_desc = { ad_size    : int;
                        ad_aligned : bool;
                        ad_fields  : (int * ctype) SMap.t }

let aggregate_descs : (aggregate * string, aggregate_desc) Hashtbl.t
  = Hashtbl.create 42


(***********************)

let next_multiple x y = ((x + y - 1) / y) * y

(* Type which reflects the mapping of types to word sizes *)
type mtype = MWord | MByte | MAgg of aggregate * string

let mtype_of : ctype -> mtype = function
  | Void | TypeNull  -> assert false
  | Int | Pointer _  -> MWord
  | Char             -> MByte
  | Aggregate (k, s) -> MAgg (k, s)

let sizeof : mtype -> int = function
  | MWord -> 4
  | MByte -> 1
  | MAgg (k, s) -> (Hashtbl.find aggregate_descs (k, s)).ad_size

let is_aligned : mtype -> bool = function
  | MWord -> true
  | MByte -> false
  | MAgg (k, s) -> (Hashtbl.find aggregate_descs (k, s)).ad_aligned

let desc_of_aggregate : aggregate -> decl_var list -> aggregate_desc
  = fun k fields -> 
    let any_aligned_field =
      List.exists (fun (t, _) -> is_aligned (mtype_of t)) fields
    in
    match k with
    | Union -> { ad_size    = List.fold_left max 0
                     (List.map (fun (t, _) -> sizeof (mtype_of t)) fields);
                 ad_aligned = any_aligned_field;
                 ad_fields  =
                   List.fold_left
                     (fun acc (t, x) -> SMap.add x (0, t) acc)
                     SMap.empty
                     fields
               }
    | Struct ->
      let f (height, map) (t, x) =
        let mtype = mtype_of t in
        let slot = if is_aligned mtype then next_multiple height 4 else height in
        (slot + sizeof mtype, SMap.add x (slot, t) map)
      in
      let (height, map) = List.fold_left f (0, SMap.empty) fields in
      { ad_size    = height;
        ad_aligned = any_aligned_field;
        ad_fields  = map }

(* See old git snapshots for the memloc type... *)
(* was used to provide a less inefficient support for
   reading/writing statically known locations *)
(* TODO: make the performance of incrementing
   a local variable less terrible *)

type stack_frame = { sf_locals : int SMap.t;
                     sf_top : int }

let push_locals_on_sf : decl_var list -> stack_frame -> stack_frame 
  = fun vars sf ->
    let f (locals, top) (t, id) =
      let mtype = mtype_of t in
      let top = top - sizeof mtype in
      if is_aligned mtype then
        (* top is a *negative* offset! *)
        let next_align = - next_multiple (-top) 4 in
        (SMap.add id next_align locals, next_align)
      else (SMap.add id top locals, top)
    in
    let (locals, top) = List.fold_left f (sf.sf_locals, sf.sf_top) vars in
    { sf_top = top; sf_locals = locals }

let sf_of_funargs : decl_var list -> stack_frame 
  = fun args ->
    let sf0 = push_locals_on_sf args { sf_top = 0;
                                       sf_locals = SMap.empty } in
    let minus_next_align = next_multiple (-sf0.sf_top) 4 in
    let shift = minus_next_align + 4 (* room for $fp *) in
    { sf_locals = SMap.map ((+) shift) sf0.sf_locals;
      sf_top = -4 }

(* let realign_stack : stack_frame -> stack_frame * int *)
(*   = fun sf -> *)
(*     let top = sf.sf_top in (\* top is a *negative* offset! *\) *)
(*     let next_align = - next_multiple (-top) 4 in *)
(*     ({ sf with sf_top = next_align }, top - next_align) *)

let load_loc : register -> ctype -> text
  = fun r t -> match mtype_of t with
    | MWord -> lw  r areg (0, v0)
    | MByte -> lbu r areg (0, v0)

let store_loc : register -> ctype -> text
  = fun r t -> match mtype_of t with
    | MWord -> sw r areg (0, v0)
    | MByte -> sb r areg (0, v0)


type stack_alteration = stack_frame -> text * stack_frame

let identity_alteration : stack_alteration
  = fun sf -> (nop, sf)

(* version 1: let the alteration do the work of modifying $sp by default,
   and provide an auto_push flag *)
let with_stack_alteration
  : ?auto_push:bool -> stack_frame -> stack_alteration -> (stack_frame -> text) -> text
  = fun ?(auto_push=false) sf alt code ->
    let (alt_code, sf') = alt sf in
    let offset = sf.sf_top - sf'.sf_top in (* idea: offset >= 0 *)
    alt_code
    ++ (if auto_push then sub sp sp oi offset else nop)
    ++ code sf'
    ++ (if offset = 0 then nop else add sp sp oi offset)

(* version 2: the alteration only computes the new top of the stack *)
let with_stack_alteration'
  : stack_frame -> (stack_frame -> stack_frame) -> (stack_frame -> text) -> text
  = fun sf alt' code ->
    let alt sf' = (nop, alt' sf') in
    with_stack_alteration ~auto_push:true sf alt code

let realign_stack_then_do : stack_alteration -> stack_alteration
  = fun alt -> fun sf ->
    let next_align = - next_multiple (-sf.sf_top) 4 in
    let offset = sf.sf_top - next_align in
    let (alt_code, sf') = alt { sf with sf_top = next_align } in
    ((if offset = 0 then nop else sub sp sp oi offset) ++ alt_code, sf')

let realign_stack : stack_alteration
  = realign_stack_then_do identity_alteration

(*** Recursive AST traversal ***)

let rec compile_expr : stack_frame -> expr -> text
  = fun sf -> function
    | (_, IntV x) -> li32 v0 x
    | (_, StringV s) -> la v0 alab (register_string s)
      
    | (t, LValue lv) -> eval_lvalue_loc sf lv
                        ++ load_loc v0 t

    | (_, Apply (fn_id, args)) ->
      with_stack_alteration sf
        (realign_stack_then_do (eval_and_push_args fn_id args))
        (fun _ -> jal ("global_" ^ fn_id))

    | (t, Assign (lv, e)) ->
      with_stack_alteration sf realign_stack begin fun sf ->
        compile_expr sf e
        ++ push v0
        ++ eval_lvalue_loc sf lv
        ++ pop a0
        ++ store_loc a0 t
        ++ move v0 a0
      end

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
              ++ div v0 v0 oi (sizeof (mtype_of t1'))
            | (Add | Sub), Pointer t1', _ ->
              mul v0 v0 oi (sizeof (mtype_of t1'))
              ++ arith v0 a0 oreg v0
            | Add, _, Pointer t2' ->
              mul a0 a0 oi (sizeof (mtype_of t2'))
              ++ arith v0 a0 oreg v0
            | _ -> arith v0 a0 oreg v0
          end
      end

    | (t, Incr (op, lv)) -> 
      let factor = match t with
        | Pointer t' -> sizeof (mtype_of t')
        | _          -> 1
      in
      let imm = match op with
        | PreIncr | PostIncr -> +factor
        | PreDecr | PostDecr -> -factor
      in
      eval_lvalue_loc sf lv
      ++ load_loc a0 t
      ++ begin match op with
        | PreIncr  | PreDecr  -> add a0 a0 oi imm
                                 ++ store_loc a0 t
        | PostIncr | PostDecr -> add a1 a0 oi imm
                                 ++ store_loc a1 t
      end
      ++ move v0 a0
                                   

    (* don't handle upwards struct-args for now *)
    (* variable.field is LValue (LSubfield ...) *)
    | (_, Subfield _) -> assert false

    | (_, Address lv) -> eval_lvalue_loc sf lv

    | (_, Sizeof ctype) -> li v0 (sizeof (mtype_of ctype))


and eval_and_push_args : string -> expr list -> stack_frame -> text * stack_frame
  = fun fn_id args sf0 -> 
    let f (sf, code) arg_type arg =
      let mtype = mtype_of arg_type in
      let top = sf.sf_top - sizeof mtype in
      let top = if is_aligned mtype then - next_multiple (-top) 4 else top in
      let offset = sf.sf_top - top in
      ({ sf with sf_top = top },
       code
       ++ compile_expr sf arg
       ++ sub sp sp oi offset
       ++ match mtype_of arg_type with
       | MWord -> sw v0 areg (0, sp)
       | MByte -> sb v0 areg (0, sp)
       | _ -> failwith "not supported yet push"
      )
    in
    let proto = SMap.find fn_id !fun_protos in
    let (sf, code) = List.fold_left2 f (sf0, nop) proto args in
    let (final_realignment, sf) = realign_stack sf in
    (code ++ final_realignment, sf)

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
  with_stack_alteration' sf (push_locals_on_sf b.block_locals)
    (fun sf -> seqmap (compile_instr sf) b.block_instrs)

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

    List.iter
      (fun ((k, s), fields) ->
        Hashtbl.add aggregate_descs (k, s) (desc_of_aggregate k fields))
      program.prog_aggregates;

    (* execute this before constructing the record
       b/c of side effects *)
    fun_protos := SMap.add "putchar" [Int] !fun_protos;
    fun_protos := SMap.add "sbrk"    [Int] !fun_protos;
    List.iter
      (fun f -> fun_protos := SMap.add f.fun_name (List.map fst f.fun_args) !fun_protos)
      program.prog_funs;
    let functions = seqmap compile_function program.prog_funs in

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
        ++ functions

        ++ putchar
        ++ sbrk;
      
      data =
        seqmap (fun (t, id) -> label ("global_" ^ id)
                               ++ dword [0]) program.prog_globals
        ++ data_segment_string ()
    }



