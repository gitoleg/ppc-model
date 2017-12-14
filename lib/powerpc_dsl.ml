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

let int_of_bitwidth = ident

let width_of_size = Size.in_bits

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
  let e = Exp.of_word w in
  if signed then Exp.signed e
  else Exp.unsigned e

let signed f = f true
let unsigned f = f false

let var signed width =
  let e = Exp.tmp (int_of_bitwidth width) in
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
  let width = int_of_bitwidth width in
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
let high w e = first e (int_of_bitwidth w)
let low w e = last e (int_of_bitwidth w)

let msb e =
  let h = Exp.width e - 1 in
  Exp.extract h h e

let lsb e = Exp.extract 0 0 e

let nth w e index =
  let width = Exp.width e in
  let step = int_of_bitwidth w in
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



let var_of_exp x = failwith ""
let to_bil = bil_of_t

class move_finder var = object(self)
  inherit [bool] Stmt.visitor
  method! enter_move v _ x = Var.equal var v || x
end

let has_assignments var rtl =
  let bil = to_bil rtl in
  let vis = new move_finder var in
  vis#run bil false

let test step_e e code =
  let step_var = var_of_exp step_e in
  let iters = Exp.width e / Exp.width step_e in
  let step = Exp.width step_e in
  let has_assignments = has_assignments step_var code in
  to_bil @@ List.concat
    (List.init iters
       ~f:(fun i ->
           let i = iters - i - 1 in
           let hi = (i + 1) * step - 1 in
           let lo = i * step in
           if has_assignments then
             let last = Infix.(Exp.extract hi lo e := step_e) in
             Infix.(step_e := Exp.extract hi lo e) :: code @ [last]
           else
             Infix.(step_e := Exp.extract hi lo e) :: code))
