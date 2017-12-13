open Core_kernel.Std
open Bap.Std

module Model = Powerpc_model

include Powerpc_dsl
include Powerpc_utils

module RTL = struct
  include Powerpc_rtl
  include Infix
end

type rtl = RTL.rtl
type exp = RTL.exp

let bil_of_rtl = RTL.bil_of_t

let lifters = String.Table.create ()

let add name lifter =
  String.Table.change lifters name ~f:(fun _ -> Some lifter)

let (>:) = add

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
