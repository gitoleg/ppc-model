open Core_kernel.Std
open Bap.Std

let ppc_fail format =
  let fail str = failwith (sprintf "PowerPC lifter fail: %s" str) in
  Printf.ksprintf fail format

let var_bitwidth v =
  match Var.typ v with
  | Type.Imm w -> w
  | _ ->
    ppc_fail "variable %s doesn't has notion of bitwidth" (Var.name v)
