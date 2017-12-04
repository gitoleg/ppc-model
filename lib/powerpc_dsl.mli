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

val zero : exp
val one  : exp

(** [extract e lx rx] extracts portion of [e] starting
    at bit [lx] and ending at bit [rx], all bounds
    are inclusive. Bits indexes start from the most
    significant bit. *)
val extract : exp -> int -> int -> exp

(** [low size e] - extracts low [size] bits from [e]  *)
val low : size -> exp -> exp

(** [high size e] - extracts high [size] bits from [e]  *)
val high : size -> exp -> exp

(** [first e n] - extracts first [n] bits from [e], starting from
    the most significant bit *)
val first : exp -> int -> exp

(** [last e n] - extracts last [n] bits from [e], where the
    last bit is the least significant bit *)
val last : exp -> int -> exp

(** [nbit e n] - extracts a bit with index [n] from [e].
    Indexes are zero-based and started from most significant bit.  *)
val nbit : exp -> int -> exp

(** [nbyte e n] - extracts a byte with index [n] from [e].
    Indexes are zero based and started from most significant byte.  *)
val nbyte : exp -> int -> exp

(** [nsize e size n] - extracts a portion of [e] of size [size] at
    index [n], where each index points to a portion of size [size].
    Indexes are zero based and started from most significant portion.
    E.g. [nsize e halfword 1] extracts a second halfword from [e] *)
val nsize : exp -> size -> int -> exp

(** [msb e] - extracts the most significant bit from [e] *)
val msb : exp -> exp

(** [lsb e] - extracts the least significant bit from [e] *)
val lsb : exp -> exp

val foreach : size -> exp -> (int -> exp -> rtl list) -> rtl
val foreach_bit : exp -> (int -> exp -> rtl list) -> rtl
