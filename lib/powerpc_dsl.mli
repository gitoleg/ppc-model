open Core_kernel.Std
open Bap.Std

type exp [@@deriving bin_io, compare, sexp]

type 'a p

val imm : (op -> exp) p
val reg : (op -> exp) p
val var : exp p
val int :  (int -> exp) p
val signed : 'a p -> 'a
val unsigned : 'a p -> 'a

module RTL : sig

  type t [@@deriving bin_io, compare, sexp]

  (** [bil d] - returns a program in BIL language   *)
  val bil_of_rtl : t list -> bil

  module Infix : sig
    val (:=) : exp -> exp -> t
    val (+)  : exp -> exp -> exp
    val (^)  : exp -> exp -> exp
    val (lsl) : exp -> exp -> exp
  end

  include module type of Infix

end

type rtl = RTL.t [@@deriving bin_io, compare, sexp]

type cpu = {
  load   : exp -> size -> exp;
  store  : exp -> exp -> size -> rtl;
  addr  : addr;
}

val byte : size
val halfword : size
val word : size
val doubleword : size

val bit_t : typ
val byte_t : typ
val halfword_t : typ
val word_t : typ
val doubleword_t : typ

val zero : exp
val one : exp

val make_cpu : addr_size -> endian -> mem -> cpu

(** [ppc_fail error_string] - raise a failure with [error_string] *)
val ppc_fail : ('a, unit, string, 'b) format4 -> 'a
