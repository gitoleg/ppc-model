open Core_kernel.Std
open Bap.Std

open Ppc_model
open Ppc_rtl

(** TODO: endian is dynamic property!!  *)
let endian = BigEndian

module type Lifter = sig
  type t [@@deriving sexp, enumerate]
  val lift : t -> addr_size -> endian -> mem -> op array -> dsl
end

let lifters : (module Lifter) list = [
  (module Ppc_load);
  (module Ppc_store);
  (module Ppc_add);
]

type ppc_lift = addr_size -> endian -> mem -> op array -> dsl

let lifts : ppc_lift String.Table.t = String.Table.create ()

let register =
  List.iter lifters ~f:(fun x ->
      let module L = (val x) in
      List.iter ~f:(fun t ->
          let name = Sexp.to_string (L.sexp_of_t t) in
          String.Table.add_exn lifts name (L.lift t)) L.all)

let register insn lift =
  String.Table.change lifts insn (fun _ -> Some lift)

let lift mode mem insn =
  let insn = Insn.of_basic insn in
  let insn_name = Insn.name insn in
  let lift lifter =
    try
      lifter mode endian mem (Insn.ops insn) |>
      bil_of_dsl |>
      Result.return
    with
      Failure str -> Error (Error.of_string str) in
  match String.Table.find lifts (Insn.name insn) with
  | None -> Or_error.errorf "unknown instruction %s" insn_name
  | Some lifter -> lift lifter

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
