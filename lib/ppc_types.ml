open Bap.Std

module Dsl = Ppc_dsl
module Model = Ppc_model
module Hardware = Model.Hardware

type dsl = Dsl.t [@@deriving bin_io, compare, sexp]

type operand = Op.t =
  | Reg of reg
  | Imm of imm
  | Fmm of fmm
[@@deriving bin_io, compare, sexp_of]
