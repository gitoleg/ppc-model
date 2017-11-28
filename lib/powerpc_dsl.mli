open Core_kernel.Std
open Bap.Std
open Powerpc_rtl

type 'a p

type cpu = {
  load  : exp -> size -> exp;
  store : exp -> exp -> size -> rtl;
  addr  : addr;
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

val low : exp -> size -> exp

val bit : exp -> int -> exp
val byten : exp -> int -> exp

val hbit : exp -> exp

val lbit : exp -> exp
