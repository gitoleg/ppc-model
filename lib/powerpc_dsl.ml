open Core_kernel.Std
open Bap.Std
open Regular.Std

open Powerpc_rtl
open Powerpc_model
open Powerpc_utils
open Hardware

type 'a p = bool -> 'a

type bitwidth = int

let bit = 1
let byte = 8
let halfword = 16
let word = 32
let doubleword = 64
let quadroword = 128
let bitwidth x = x

let int_of_width = ident

let width_of_size = Size.in_bits

let size_of_width x =
  match Size.of_int x with
  | Ok s -> s
  | Error _ -> ppc_fail "invalid size: %d" x

let find_reg regs reg = String.Map.find regs (Reg.name reg)
let find_gpr = find_reg gpr
let find_vr  = find_reg vr
let find_fpr = find_reg fpr
let find_cr_bit = find_reg crn
let find_cr_field = find_reg cr_fields

let reg_searches = [find_gpr; find_cr_bit; find_cr_field; find_fpr; find_vr]

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
  let n = width / step in
  let hi, lo =
    if n * step < width then
      let sh = width - n * step in
      hi + sh, lo + sh
    else hi, lo in
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
let ifnot cond else_ = if_ cond [] else_

let foreach = foreach

type clause = [
  | `Case of (exp * rtl list)
  | `Default of rtl list
]

let case x y = `Case (x,y)
let default y = `Default y

let switch exp cases =
  let default = List.filter_map ~f:(function
      | `Default y -> Some y
      |  _ -> None) cases in
  let default = Option.value ~default:[] (List.hd default) in
  let cases = List.filter_map ~f:(function
      | `Case (x,y) -> Some (x,y)
      | _ -> None) cases in
  let cond x = Infix.(exp = x) in
  match cases with
  | [] -> ppc_fail "empty switch"
  | (x, code) :: [] -> (if_ (cond x) code default;)
  | (x, code) :: cases ->
    let else_ =
      List.fold (List.rev cases) ~init:default ~f:(fun acc (x,code) ->
          [if_ (cond x) code acc;]) in
    (if_ (cond x) code else_)

type cpu = {
  load      : exp -> bitwidth -> exp;
  store     : exp -> exp -> bitwidth -> rtl;
  jmp       : exp -> rtl;
  addr      : exp;
  addr_size : bitwidth;
  gpr       : int -> exp;
  fpr       : int -> exp;
  vr        : int -> exp;
  xer       : exp;
  ctr       : exp;
  lr        : exp;
  tar       : exp;
  cr        : exp;
  so        : exp;
  ca        : exp;
  ov        : exp;
  ca32      : exp;
  ov32      : exp;
}

(** TODO: probably, it's not right place for this function *)
let make_cpu addr_size endian memory =
  let extract_addr a = match addr_size with
    | `r32 -> low word a
    | `r64 -> a in
  let mem,addr_size = match addr_size with
    | `r32 -> mem32, word
    | `r64 -> mem64, doubleword in
  let load exp width =
    let size = size_of_width width in
    let addr = extract_addr exp in
    Exp.load mem addr endian size in
  let store addr data width =
    let size = size_of_width width in
    let addr = extract_addr addr in
    store mem addr data endian size in
  let addr = Exp.of_word @@ Memory.min_addr memory in
  let jmp e = jmp (low addr_size e) in
  let find name regs n =
    try
      Int.Map.find_exn regs n
    with _ ->
      ppc_fail "%s with number %d not found" name n in
  let gpr n = find "GPR" gpri n in
  let fpr n = find "FPR" fpri n in
  let vr n = find "VR" vri n in
  { load; store; jmp; addr; addr_size;
    gpr; fpr; vr; cr; xer; ctr; lr; tar;
    so; ca; ov; ca32; ov32; }
