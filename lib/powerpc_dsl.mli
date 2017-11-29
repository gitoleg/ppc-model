open Core_kernel.Std
open Bap.Std
open Powerpc_rtl

type 'a p

type cpu = {
  load  : exp -> size -> exp;
  store : exp -> exp -> size -> rtl;
  jmp   : exp -> rtl;
  addr  : exp;
  addr_size : size;
}

val make_cpu : addr_size -> endian -> mem -> cpu

val imm : (op -> exp) p
val reg : (op -> exp) p
val var : exp p
val int : (int -> exp) p
val signed : 'a p -> 'a
val unsigned : 'a p -> 'a

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

val low : size -> exp -> exp

(** [nbit e n] - extracts a bit with index [n] from [e].
    Indexes are zero-based and started from most significant bit.  *)
val nbit : exp -> int -> exp

(** [nbyte e n] - extracts a byte with index [n] from [e].
    Indexes are zero based and started from most significant byte.  *)
val nbyte : exp -> int -> exp

(** [msb e] - extracts the most significant bit from [e] *)
val msb : exp -> exp

(** [lsb e] - extracts the least significant bit from [e] *)
val lsb : exp -> exp

(** [extract e lx rx] extracts portion of [e] starting
    at bit [lx] and ending at bit [rx], all bounds
    are inclusive. Bits indexes start from the most
    significant bit. *)
val extract : exp -> int -> int -> exp
