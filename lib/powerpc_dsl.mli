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
val int_of_bitwidth : bitwidth -> int

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

(** switch clause  *)
type clause

(** [switch x clauses] - create a switch construction.
    Example:
    ...
    ra := <...>
    switch (x) [
      case one   [ rs := <...>; ];
      case zero  [ rt := <...>; rs := <...> ];
      default [rs := zero];
    ]
    ...
 *)
val switch  : exp -> clause list -> rtl

(** [case exp code] - creates a switch case *)
val case    : exp -> rtl list -> clause

(** [default code] - creates a switch default *)
val default : rtl list -> clause
