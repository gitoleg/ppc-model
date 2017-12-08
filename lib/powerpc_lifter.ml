open Core_kernel.Std
open Bap.Std

open Powerpc
module Model = Powerpc_model

(** TODO: endian is dynamic property!!  *)
let endian = BigEndian

let lifters : (module Lifter) list = [
  (module Powerpc_add);
  (module Powerpc_arith);
  (module Powerpc_branch);
  (module Powerpc_compare);
  (module Powerpc_load);
  (module Powerpc_logical);
  (module Powerpc_rotate);
  (module Powerpc_shift);
  (module Powerpc_store);
]

type ppc_lift = cpu -> op array -> rtl list

let lifts : ppc_lift String.Table.t = String.Table.create ()

let register =
  List.iter lifters ~f:(fun x ->
      let module L = (val x) in
      List.iter ~f:(fun t ->
          let name = Sexp.to_string (L.sexp_of_t t) in
          String.Table.add_exn lifts name (L.lift t)) L.all)

let register insn lift =
  String.Table.change lifts insn (fun _ -> Some lift)

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
  match String.Table.find lifts (Insn.name insn) with
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
