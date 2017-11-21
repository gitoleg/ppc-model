open Core_kernel.Std
open Bap.Std


type cast  = Bil.cast  [@@deriving bin_io, compare, sexp]
type binop = Bil.binop [@@deriving bin_io, compare, sexp]
type unop  = Bil.unop  [@@deriving bin_io, compare, sexp]
type exp   = Bil.exp   [@@deriving bin_io, compare, sexp]
type stmt  = Bil.stmt  [@@deriving bin_io, compare, sexp]

type t = stmt list [@@deriving bin_io, compare, sexp]

module Infix = Bil.Infix

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
let jmp = Bil.jmp

open Ppc_model
open Ppc_model.Hardware

let bil_of_t = ident

let ppc_fail format =
  let fail str = failwith (sprintf "PowerPC lifter fail: %s" str) in
  Printf.ksprintf fail format

let load addr_size ~addr endian size = match addr_size with
  | `r32 -> Bil.(load ~mem:(var PPC32.mem) ~addr endian size)
  | `r64 -> Bil.(load ~mem:(var PPC64.mem) ~addr endian size)

let store addr_size ~addr endian size data =
  match addr_size with
  | `r32 ->
    Bil.(PPC32.mem := store ~mem:(var PPC32.mem) ~addr data endian size)
  | `r64 ->
    Bil.(PPC64.mem := store ~mem:(var PPC64.mem) ~addr data endian size)

let fresh name typ = Var.create ~fresh:true name typ

let low32 exp = Bil.extract 31 0 exp

let is_negative mode exp = match mode with
  | `r32 -> Bil.(low32 exp <$ int @@ Word.zero 32)
  | `r64 -> Bil.(exp <$ int @@ Word.zero 64)

let is_positive mode exp = match mode with
  | `r32 -> Bil.(low32 exp >$ int @@ Word.zero 32)
  | `r64 -> Bil.(exp >$ int @@ Word.zero 64)

let is_zero mode exp = match mode with
  | `r32 -> Bil.(low32 exp = int @@ Word.zero 32)
  | `r64 -> Bil.(exp = int @@ Word.zero 64)

let find_var vars name =
  Var.Set.find vars
    ~f:(fun v -> String.equal name (Var.name v))

let find_register regs reg =
  match find_var regs (Reg.name reg) with
  | None ->
    (Or_error.errorf "unknown register %s" (Reg.name reg))
  | Some reg -> Ok reg

let reg_not_found reg = ppc_fail "Register not found: %s" (Reg.name reg)

let find_register_exn regs reg =
  match find_var regs (Reg.name reg) with
  | None -> reg_not_found reg
  | Some reg -> reg

let find_gpr_opt reg = Result.ok (find_register gpr reg)

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

let cr_bit reg =
  let name = Reg.name reg in
  Int.Map.data cr |>
  List.find ~f:(fun v -> String.equal (Var.name v) name) |> function
  | Some b -> b
  | None -> ppc_fail "CR bit with name %s not found" name

let cr_bit' n =
  let n = cr_bitwidth - n - 1 in
  match Int.Map.find cr n with
  | Some b -> b
  | None -> ppc_fail "CR bit number %d does not found" n

let cr_field reg =
  let name = Reg.name reg in
  String.Map.find cr_fields name |> function
  | None -> ppc_fail "CR fields %s does not found" name
  | Some (w,x,y,z) -> z,y,x,w

let write_fixpoint_result addr_size res =
  Bil.[
    cr_bit' 0 := is_negative addr_size (var res);
    cr_bit' 1 := is_positive addr_size (var res);
    cr_bit' 2 := is_zero addr_size (var res);
  ]

include Infix
