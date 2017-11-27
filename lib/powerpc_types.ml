open Bap.Std

module Dsl = Powerpc_dsl
module Exp = Powerpc_rtl.Exp
module RTL = struct
  include Powerpc_rtl
  include Infix
  include Exp.Infix
end
module Model = Powerpc_model
module Hardware = Model.Hardware

type rtl = RTL.t [@@deriving bin_io, compare, sexp]
type cpu = Dsl.cpu

type operand = Op.t =
  | Reg of reg
  | Imm of imm
  | Fmm of fmm
[@@deriving bin_io, compare, sexp_of]
