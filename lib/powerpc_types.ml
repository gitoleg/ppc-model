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

type rtl = RTL.rtl
type cpu = Dsl.cpu

module type Lifter = sig
  type t [@@deriving sexp, enumerate]
  val lift : t -> cpu -> op array -> rtl list
end
