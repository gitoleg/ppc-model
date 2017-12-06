open Bap.Std

include Powerpc_dsl
include Powerpc_utils

module RTL = struct
  include Powerpc_rtl
  include Infix
end

type rtl = RTL.rtl
type exp = RTL.exp

let bil_of_rtl = RTL.bil_of_t

module type Lifter = sig
  type t [@@deriving sexp, enumerate]
  val lift : t -> cpu -> op array -> rtl list
end
