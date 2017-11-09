open Core_kernel.Std
open Bap.Std

open Ppc_model
open Ppc_rtl

(** TODO: endian is dynamic property!!  *)
let endian = BigEndian

type opcode = [
  | Ppc_load.t
  | Ppc_add.t
] [@@deriving sexp]

let lift mode mem insn =
  let insn = Insn.of_basic insn in
  let opcode = opcode_of_sexp @@ Sexp.of_string @@ Insn.name insn in
  match opcode with
  | #Ppc_load.t as op -> Ok (Ppc_load.lift mode endian mem op (Insn.ops insn))
  | #Ppc_add.t as op -> Ok (Ppc_add.lift mode endian mem op (Insn.ops insn))

module T32 = struct
  module CPU = PowerPC_32_cpu
  let lift = lift `r32
end

module T64 = struct
  module CPU = PowerPC_64_cpu
  let lift = lift `r64
end

let () = register_target `ppc (module T32)
let () = register_target `ppc64 (module T64)
