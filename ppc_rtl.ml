open Core_kernel.Std
open Bap.Std

module Dsl = struct

  type cast  = Bil.cast  [@@deriving bin_io, compare, sexp]
  type binop = Bil.binop [@@deriving bin_io, compare, sexp]
  type unop  = Bil.unop  [@@deriving bin_io, compare, sexp]
  type exp   = Bil.exp   [@@deriving bin_io, compare, sexp]
  type stmt  = Bil.stmt  [@@deriving bin_io, compare, sexp]

  type t = stmt list [@@deriving bin_io, compare, sexp]

  module Infix = Bil.Infix
  include Infix

  let cast = Bil.cast
  let var = Bil.var
  let unsigned = Bil.unsigned
  let signed = Bil.signed
  let high = Bil.high
  let low = Bil.low
  let int = Bil.int
  let extract = Bil.extract
  let concat = Bil.concat
end

type dsl = Dsl.t [@@deriving bin_io, compare, sexp]
type exp = Dsl.exp [@@deriving bin_io, compare, sexp]
type stmt = Dsl.stmt [@@deriving bin_io, compare, sexp]

open Ppc_model
open Ppc_model.Hardware

exception Invalid_instruction of string

let bil_of_dsl = ident

let find_var vars name =
  Var.Set.find vars
    ~f:(fun v -> String.equal name (Var.name v))

let find_register regs reg =
  match find_var regs (Reg.name reg) with
  | None ->
    (Or_error.errorf "unknown register %s" (Reg.name reg))
  | Some reg -> Ok reg

let find_register_exn regs reg =
  match find_var regs (Reg.name reg) with
  | None -> failwith (sprintf "Register not found: %s" (Reg.name reg))
  | Some reg -> reg

let find_gpr reg = find_register_exn gpr reg
let find_gpr_opt reg = Result.ok (find_register gpr reg)
let find_gpr_err reg = find_register gpr reg

let load32 addr endian size =
  Bil.(load ~mem:(var PPC32.mem) ~addr endian size)

let load64 addr endian size =
  Bil.(load ~mem:(var PPC64.mem) ~addr endian size)

let set_cond_reg0 addr_size res =
  let open Dsl in
  let zero,res = match addr_size with
    | `r32 -> extract 31 0 res, int (Word.zero 32)
    | `r64 -> res, int (Word.zero 64) in
  let bit0 = res < zero in
  let bit1 = res > zero in
  let bit2 = res = zero in
  let field0 = var so ^ bit2 ^ bit1 ^ bit0 in
  cr := extract 31 4 (var cr) ^ field0
