open Bap.Std

module Dsl = Powerpc_dsl
module RTL = Dsl.RTL
module Model = Powerpc_model
module Hardware = Model.Hardware

type rtl = Dsl.rtl [@@deriving bin_io, compare, sexp]
type cpu = Dsl.cpu

type operand = Op.t =
  | Reg of reg
  | Imm of imm
  | Fmm of fmm
[@@deriving bin_io, compare, sexp_of]
