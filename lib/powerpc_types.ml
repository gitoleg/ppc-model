open Bap.Std

module Dsl = Powerpc_dsl
module Exp = Powerpc_rtl.Exp
module Model = Powerpc_model
module Hardware = Model.Hardware

module RTL = struct
  include Powerpc_rtl
  include Infix
end

include Powerpc_utils

type rtl = RTL.t [@@deriving bin_io, compare, sexp]
type cpu = Dsl.cpu
