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

let reg_not_found reg =
  failwith (sprintf "Register not found: %s" (Reg.name reg))

let find_register_exn regs reg =
  match find_var regs (Reg.name reg) with
  | None -> reg_not_found reg
  | Some reg -> reg

let find_gpr_opt reg = Result.ok (find_register gpr reg)
let find_gpr_err reg = find_register gpr reg

(** will also try to find R# when got X#, (e.g. R3 when got X3)
    for reasons depended only from llvm side *)
let find_gpr reg = match find_gpr_opt reg with
  | Some r -> r
  | None ->
    let reg_name = Reg.name reg in
    if String.is_prefix reg_name ~prefix:"X" then
      let name = String.substr_replace_first
          reg_name ~pattern:"X" ~with_:"R" in
      Var.Set.find_exn gpr ~f:(fun v ->
          String.equal (Var.name v) name)
    else reg_not_found reg

let load addr_size ~addr endian size = match addr_size with
  | `r32 -> Bil.(load ~mem:(var PPC32.mem) ~addr endian size)
  | `r64 -> Bil.(load ~mem:(var PPC64.mem) ~addr endian size)

let store addr_size ~addr endian size data =
  match addr_size with
  | `r32 ->
    Bil.(PPC32.mem := store ~mem:(var PPC32.mem) ~addr data endian size)
  | `r64 ->
    Bil.(PPC64.mem := store ~mem:(var PPC64.mem) ~addr data endian size)

let extract_low_32 exp = Bil.extract 31 0 exp

let is_negative mode exp = match mode with
    | `r32 -> Bil.(extract_low_32 exp <$ int @@ Word.zero 32)
    | `r64 -> Bil.(exp <$ int @@ Word.zero 64)

let is_positive mode exp = match mode with
    | `r32 -> Bil.(extract_low_32 exp >$ int @@ Word.zero 32)
    | `r64 -> Bil.(exp >$ int @@ Word.zero 64)

let is_zero mode exp = match mode with
    | `r32 -> Bil.(extract_low_32 exp = int @@ Word.zero 32)
    | `r64 -> Bil.(exp = int @@ Word.zero 64)

let write_result_bits addr_size res =
  Bil.[
    nf := is_negative addr_size (var res);
    pf := is_positive addr_size (var res);
    zf := is_zero addr_size (var res);
  ]
