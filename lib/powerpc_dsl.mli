open Core_kernel.Std
open Bap.Std
open Powerpc_rtl

type 'a p

type bitwidth

val bit  : bitwidth
val byte : bitwidth
val halfword : bitwidth
val word : bitwidth
val doubleword : bitwidth
val quadroword : bitwidth
val bitwidth : int -> bitwidth

val imm : (op -> exp) p
val reg : (op -> exp) p
val var : (bitwidth -> exp) p
val const : (bitwidth -> int -> exp) p
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
val low : bitwidth -> exp -> exp

(** [high width e] - extracts high [width] bits from [e]  *)
val high : bitwidth -> exp -> exp

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
val nth : bitwidth -> exp -> int -> exp

(** [msb e] - extracts the most significant bit from [e] *)
val msb : exp -> exp

(** [lsb e] - extracts the least significant bit from [e] *)
val lsb : exp -> exp

(** [foreach step e rtl] - repeat [rtl] for each [step] of [e] *)
val foreach : exp -> exp -> rtl list -> rtl

(** [when_ cond rtl] = if_ cond rtl [] *)
val when_ : exp -> rtl list -> rtl

(** [ifnot cond rtl] = if_ cond [] rtl *)
val ifnot : exp -> rtl list -> rtl

type cpu = {
  load      : exp -> bitwidth -> exp;
  store     : exp -> exp -> bitwidth -> rtl;
  jmp       : exp -> rtl;
  addr      : exp;
  addr_size : bitwidth;

  (** registers  *)
  gpr       : int -> exp; (** general purpose registers 0..31 *)
  fpr       : int -> exp; (** floating-point registers 0..31  *)
  vr        : int -> exp; (** vector register 0..31           *)
  xer       : exp;       (** fixed point exception register  *)
  ctr       : exp;       (** count register      *)
  lr        : exp;       (** link register       *)
  tar       : exp;       (** target register     *)
  cr        : exp;       (** condition register  *)

  (** fixed precision flags *)
  so        : exp; (** summary overflow        *)
  ca        : exp; (** carry flag              *)
  ov        : exp; (** overflow flag           *)
  ca32      : exp; (** carry out of 32 bits    *)
  ov32      : exp; (** overflow of 32 bits     *)
}

val make_cpu : addr_size -> endian -> mem -> cpu
