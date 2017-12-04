open Core_kernel.Std
open Bap.Std
open Powerpc_rtl

type 'a p

type width

val bit  : width
val byte : width
val halfword : width
val word : width
val doubleword : width
val width : int -> width

val imm : (op -> exp) p
val reg : (op -> exp) p
val var : (width -> exp) p
val const : (width -> int -> exp) p
val signed : 'a p -> 'a
val unsigned : 'a p -> 'a

val zero : exp
val one  : exp

(** [extract e lx rx] extracts portion of [e] starting
    at bit [lx] and ending at bit [rx], all bounds
    are inclusive. Bits indexes start from the most
    significant bit. *)
val extract : exp -> int -> int -> exp

(** [low width e] - extracts low [width] bits from [e]  *)
val low : width -> exp -> exp

(** [high width e] - extracts high [width] bits from [e]  *)
val high : width -> exp -> exp

(** [first e n] - extracts first [n] bits from [e], starting from
    the most significant bit *)
val first : exp -> int -> exp

(** [last e n] - extracts last [n] bits from [e], where the
    last bit is the least significant bit *)
val last : exp -> int -> exp

(** [nth width e n] - extracts a portion of [e] of width [width] at
    index [n], where each index points to a portion of width [width].
    Indexes are zero based and started from most significant portion.
    E.g. [nth halfword e 1] extracts a second halfword from [e] *)
val nth : width -> exp -> int -> exp

(** [msb e] - extracts the most significant bit from [e] *)
val msb : exp -> exp

(** [lsb e] - extracts the least significant bit from [e] *)
val lsb : exp -> exp

(** [foreach width e rtl] - repeat [rtl] for each [width] of [e] *)
(* val foreach : width -> exp -> rtl list -> rtl *)

(** [when_ cond rtl] = if_ cond rtl [] *)
val when_ : exp -> rtl list -> rtl

(** [until cond rtl] = if_ cond [] rtl *)
val until : exp -> rtl list -> rtl

type cpu = {
  load  : exp -> width -> exp;
  store : exp -> exp -> width -> rtl;
  jmp   : exp -> rtl;
  addr  : exp;
  addr_size : width;
}

val make_cpu : addr_size -> endian -> mem -> cpu
