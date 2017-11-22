open Core_kernel.Std
open Bap.Std

type s
type exp [@@deriving bin_io, compare, sexp]

val imm : s -> op -> exp
val reg : s -> op -> exp
val var : s -> exp
val signed : (s -> 'a) -> 'a
val unsigned : (s -> 'a) -> 'a

module RTL : sig

  type t [@@deriving bin_io, compare, sexp]

  (** [bil d] - returns a program in BIL language   *)
  val bil : t list -> bil

  module Infix : sig

    val (:=) : exp -> exp -> t
    val (+) : exp -> exp -> exp

  end

  include module type of Infix

end

type rtl = RTL.t [@@deriving bin_io, compare, sexp]

type cpu = {
  load   : exp -> size -> exp;
  store  : exp -> exp -> size -> rtl;
  mem    : mem;
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

(* val zero : exp *)
(* val one : exp *)

val make_cpu : addr_size -> endian -> mem -> cpu

(** [ppc_fail error_string] - raise a failure with [error_string] *)
val ppc_fail : ('a, unit, string, 'b) format4 -> 'a
