open Core_kernel.Std
open Bap.Std
open Regular.Std

open Powerpc_rtl
open Powerpc_model
open Powerpc_utils
open Hardware

type 'a p = bool -> 'a

(** will also try to find R# when got X#, (e.g. R3 when got X3)
    for reasons depended only from llvm side *)
let find_gpr reg =
  let find name = String.Map.find gpr name in
  let reg_name = Reg.name reg in
  match find reg_name with
  | Some r -> Some r
  | None ->
    if String.is_prefix reg_name ~prefix:"X" then
      let name = String.substr_replace_first
          reg_name ~pattern:"X" ~with_:"R" in
      find name
    else None

let find_cr_bit reg = String.Map.find crn (Reg.name reg)
let find_cr_field reg = String.Map.find cr_fields (Reg.name reg)

let reg_searches = [find_gpr; find_cr_bit; find_cr_field;]

let find reg =
  List.filter_map reg_searches ~f:(fun f -> f reg) |> function
  | [] ->
    ppc_fail "Register not found: %s" (Reg.name reg)
  | hd :: [] -> hd
  | _ -> ppc_fail "Register name %s is ambigous!!!" (Reg.name reg)

let int_of_imm = function
  | Op.Reg _ | Op.Fmm _ -> ppc_fail "imm operand expected"
  | Op.Imm x -> match Imm.to_int x with
    | Some x -> x
    | None -> ppc_fail "failed to convert imm operand to int"

let imm signed op =
  let imm = int_of_imm op in
  let w = Word.of_int ~width:64 imm in
  Exp.of_word w

let signed f = f true
let unsigned f = f false

(** TODO: don't forget to fix this  *)
let var signed =
  (* let width = 64 in *)
  (* let v = Var.create ~fresh:true "tmp" (Type.Imm width) in *)
  (* let e = Exp.of_var v in *)
  Exp.tmp ()
  (* let e = Exp.tmp () in *)
  (* if signed then Exp.signed e *)
  (* else Exp.unsigned e *)

let reg signed op = match op with
  | Op.Imm _ | Op.Fmm _ ->
    ppc_fail "reg operand expected"
  | Op.Reg x ->
    let e =
      try find x
      with _ ->
        Exp.of_word (Word.zero 64) in
    if signed then Exp.signed e
    else Exp.unsigned e

let int signed value =
  let x = Word.of_int ~width:64 value in
  let e = Exp.of_word x in
  if signed then Exp.signed e
  else Exp.unsigned e

type cpu = {
  load  : exp -> size -> exp;
  store : exp -> exp -> size -> rtl;
  jmp   : exp -> rtl;
  addr  : exp;
  addr_size : size;
}

let byte = `r8
let halfword = `r16
let word = `r32
let doubleword = `r64
let bit_t = Type.imm 1
let byte_t = Type.imm 8
let halfword_t = Type.imm 16
let word_t = Type.imm 32
let doubleword_t = Type.imm 64
let zero = Exp.of_word Word.b0
let one  = Exp.of_word Word.b1

let low size e =
  let n = Size.in_bits size in
  Exp.extract (n - 1) 0 e

let high size e =
  let n = Size.in_bits size in
  let w = Exp.width e in
  Exp.extract (w - 1) (w - n) e

let last e bits = Exp.extract (bits - 1) 0 e

let first e bits =
  let w = Exp.width e in
  Exp.extract (w - 1) (w - bits) e

(** TODO: addr_size is weird
    TODO: probably, it's not right place for this function *)
let make_cpu addr_size endian memory =
  let extract_addr a = match addr_size with
    | `r32 -> low word a
    | `r64 -> a in
  let mem = match addr_size with
    | `r32 -> mem32
    | `r64 -> mem64 in
  let load exp size =
    let addr = extract_addr exp in
    Exp.load mem addr endian size in
  let store addr data size =
    let addr = extract_addr addr in
    store mem addr data endian size in
  let addr = Exp.of_word @@ Memory.min_addr memory in
  let addr_size : size = match addr_size with
    | `r32 -> `r32
    | `r64 -> `r64 in
  let jmp e = jmp (low addr_size e) in
  { load; store; jmp; addr; addr_size; }

let msb e =
  let h = Exp.width e - 1 in
  Exp.extract h h e

let lsb e = Exp.extract 0 0 e

(** TODO: don't forget to think about those functions with extract  *)
let nbit e n =
  let f e =
    let x = Exp.width e - n - 1 in
    Exp.extract x x e in
  Exp.apply f e

(* let nbit e n = *)
(*   let x = Exp.width e - n - 1 in *)
(*   Exp.extract x x e *)

let nbyte e n =
  let x = Exp.width e / 8 - n in
  let hi = (x + 1) * 8 - 1 in
  let lo = x * 8 in
  Exp.extract hi lo e

let extract e left right =
  let hi = Exp.width e - left - 1 in
  let lo = Exp.width e - right - 1 in
  Exp.extract hi lo e
