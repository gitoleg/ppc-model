open Core_kernel.Std

open Ppc_rtl
open Ppc_model.Hardware

let set_ov x y res =
  let exp = Dsl.((x lxor y) land (x lxor res)) in
  let exp1 = Dsl.(cast high 1 exp) in
  Dsl.(ov := exp1)

let set_ov32 x y res =
  Dsl.(ov := cast high 1
           (extract 31 0 ((x lxor y) land (x lxor res))))
