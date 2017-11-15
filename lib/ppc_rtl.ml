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
  let if_ = Bil.if_
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

let load32 ~addr endian size =
  Bil.(load ~mem:(var PPC32.mem) ~addr endian size)

let load64 ~addr endian size =
  Bil.(load ~mem:(var PPC64.mem) ~addr endian size)

let store32 ~addr endian size data =
  Bil.(PPC32.mem := store ~mem:(var PPC32.mem) ~addr data endian size)

let store64 ~addr endian size data =
  Bil.(PPC64.mem := store ~mem:(var PPC64.mem) ~addr data endian size)

let is_negative mode x = match mode with
    | `r32 -> Dsl.(extract 31 0 (var x) <$ int @@ Word.zero 32)
    | `r64 -> Dsl.(var x <$ int @@ Word.zero 64)

let is_positive mode x = match mode with
    | `r32 -> Dsl.(extract 31 0 (var x) >$ int @@ Word.zero 32)
    | `r64 -> Dsl.(var x >$ int @@ Word.zero 64)

let is_zero mode x = match mode with
    | `r32 -> Dsl.(extract 31 0 (var x) = int @@ Word.zero 32)
    | `r64 -> Dsl.(var x = int @@ Word.zero 64)
