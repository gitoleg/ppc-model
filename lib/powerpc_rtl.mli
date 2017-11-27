open Core_kernel.Std
open Bap.Std

type bap_exp = exp

type t [@@deriving bin_io, compare, sexp]
type rtl = t [@@deriving bin_io, compare, sexp]
type exp [@@deriving bin_io, compare, sexp]

module Exp : sig
  type t = exp [@@deriving bin_io, compare, sexp]

  val of_var  : var -> exp
  val of_vars : var list -> exp
  val of_word : word -> exp

  val load : var -> exp -> endian -> size -> exp
  val extract : int -> int -> exp -> exp

  val signed : exp -> exp
  val unsigned : exp -> exp

  module Infix : sig
    val (+)  : exp -> exp -> exp
    val (-)  : exp -> exp -> exp
    val (^)  : exp -> exp -> exp
    val (<)  : exp -> exp -> exp
    val (>)  : exp -> exp -> exp
    val (=)  : exp -> exp -> exp
    val (lsl) : exp -> exp -> exp
    val (lsr) : exp -> exp -> exp
  end

  val body : exp -> bap_exp
end

(** [bil_of_t d] - returns a program in BIL language   *)
val bil_of_t : t list -> bil

val store : var -> exp -> exp -> endian -> size -> t

module Infix : sig
  val (:=)  : exp -> exp -> t
end