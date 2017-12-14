open Core_kernel.Std
open Bap.Std

module Model = Powerpc_model
open Powerpc_rtl

include Powerpc_dsl
include Powerpc_utils
include Powerpc_cpu

module RTL = struct
  include Powerpc_rtl
  include Infix
end

type rtl = RTL.rtl
type exp = RTL.exp
type lift = cpu -> op array -> rtl list

let bil_of_rtl = RTL.bil_of_t

let lifters = String.Table.create ()

let register name lifter =
  String.Table.change lifters name ~f:(fun _ -> Some lifter)

let (>>) = register

let dot fc cpu ops =
  let res = signed reg ops.(0) in
  fc cpu ops @
  RTL.[
    nth bit cpu.cr 0 := low cpu.addr_size res <$ zero;
    nth bit cpu.cr 1 := low cpu.addr_size res >$ zero;
    nth bit cpu.cr 2 := low cpu.addr_size res = zero;
  ]

let register_dot name lifter = register name (dot lifter)

let (>.) = register_dot

(** TODO: endian is dynamic property!!  *)
let endian = BigEndian

let lift addr_size mem insn =
  let insn = Insn.of_basic insn in
  let insn_name = Insn.name insn in
  let cpu = make_cpu addr_size endian mem  in
  let lift lifter =
    try
      lifter cpu (Insn.ops insn) |>
      bil_of_rtl |>
      Result.return
    with
    | Failure str -> Error (Error.of_string str) in
  match String.Table.find lifters (Insn.name insn) with
  | None -> Or_error.errorf "unknown instruction %s" insn_name
  | Some lifter -> lift lifter

module T32 = struct
  module CPU = Model.PowerPC_32_cpu
  let lift = lift `r32
end

module T64 = struct
  module CPU = Model.PowerPC_64_cpu
  let lift = lift `r64
end

let () = register_target `ppc (module T32)
let () = register_target `ppc64 (module T64)
