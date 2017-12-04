open Core_kernel.Std
open Bap.Std
open Regular.Std

open Powerpc_rtl
open Powerpc_model
open Powerpc_utils
open Hardware

type 'a p = bool -> 'a

type width =
  | Bit
  | Byte
  | Halfword
  | Word
  | Doubleword
  | Custom of int

let bit = Bit
let byte = Byte
let halfword = Halfword
let word = Word
let doubleword = Doubleword
let width x = Custom x

let int_of_width = function
  | Bit -> 1
  | Byte -> 8
  | Halfword -> 16
  | Word -> 32
  | Doubleword -> 64
  | Custom x -> x

let width_of_size = function
  | `r8 -> Byte
  | `r16 -> Halfword
  | `r32 -> Word
  | `r64 -> Doubleword
  | `r128 -> Custom 128
  | `r256 -> Custom 256

let size_of_width = function
  | Byte -> `r8
  | Halfword -> `r16
  | Word -> `r32
  | Doubleword -> `r64
  | Custom 128 -> `r128
  | Custom 256 -> `r256
  | Bit -> ppc_fail "unknown size: bit"
  | Custom x -> ppc_fail "unknown size: %d" x

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

let var signed width =
  let e = Exp.tmp (int_of_width width) in
  if signed then Exp.signed e
  else Exp.unsigned e

let reg signed op = match op with
  | Op.Imm _ | Op.Fmm _ -> ppc_fail "reg operand expected"
  | Op.Reg x ->
    let e =
      try find x
      with _ ->
        Exp.of_word (Word.zero 64) in
    if signed then Exp.signed e
    else Exp.unsigned e

let const signed width value =
  let width = int_of_width width in
  let x = Word.of_int ~width value in
  let e = Exp.of_word x in
  if signed then Exp.signed e
  else Exp.unsigned e

let zero = Exp.of_word Word.b0
let one  = Exp.of_word Word.b1

let first e bits =
  let w = Exp.width e in
  Exp.extract (w - 1) (w - bits) e

let last e bits = Exp.extract (bits - 1) 0 e
let high w e = first e (int_of_width w)
let low w e = last e (int_of_width w)

let msb e =
  let h = Exp.width e - 1 in
  Exp.extract h h e

let lsb e = Exp.extract 0 0 e

let nth w e index =
  let width = Exp.width e in
  let step = int_of_width w in
  let x = width / step - index - 1 in
  let hi = (x + 1) * step - 1 in
  let lo = x * step in
  Exp.extract hi lo e

let extract e left right =
  let width = Exp.width e in
  let target_width = right - left + 1 in
  if width >= target_width then
    let hi = width - left - 1 in
    let lo = width - right - 1 in
    Exp.extract hi lo e
  else
    Exp.extract (target_width - 1) 0 e

let when_ cond then_ = if_ cond then_ []
let until cond else_ = if_ cond [] else_


(* let foreach size e f = loop e (Size.in_bits size) f *)

type cpu = {
  load  : exp -> width -> exp;
  store : exp -> exp -> width -> rtl;
  jmp   : exp -> rtl;
  addr  : exp;
  addr_size : width;
}

(** TODO: addr_size is weird
    TODO: probably, it's not right place for this function *)
let make_cpu addr_size endian memory =
  let extract_addr a = match addr_size with
    | `r32 -> low word a
    | `r64 -> a in
  let mem = match addr_size with
    | `r32 -> mem32
    | `r64 -> mem64 in
  let load exp width =
    let size = size_of_width width in
    let addr = extract_addr exp in
    Exp.load mem addr endian size in
  let store addr data width =
    let size = size_of_width width in
    let addr = extract_addr addr in
    store mem addr data endian size in
  let addr = Exp.of_word @@ Memory.min_addr memory in
  let addr_size = match addr_size with
    | `r32 -> Word
    | `r64 -> Doubleword in
  let jmp e = jmp (low addr_size e) in
  { load; store; jmp; addr; addr_size; }
