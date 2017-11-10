open Core_kernel.Std
open Bap.Std

module Dsl : sig

  type cast  = Bil.cast [@@deriving bin_io, compare, sexp]
  type binop = Bil.binop [@@deriving bin_io, compare, sexp]
  type unop  = Bil.unop [@@deriving bin_io, compare, sexp]

  type exp  [@@deriving bin_io, compare, sexp]
  type stmt [@@deriving bin_io, compare, sexp]

  type t = stmt list [@@deriving bin_io, compare, sexp]

  (** [var v -> Var v]   *)
  val var : var -> exp

  (** [cast t w x -> Cast (t,w,x)]  *)
  val cast  : cast -> int -> exp -> exp
  val unsigned : cast
  val signed : cast
  val high : cast
  val low : cast

  (** [int w -> Int w]  *)
  val int : word -> exp

  (** [extract ~hi ~lo x -> Extract (hi,lo,x)]  *)
  val extract : hi:int -> lo:int -> exp -> exp

  (** [concat x y -> Concat (x,y)]  *)
  val concat : exp -> exp -> exp

  (** [if_ cond s1 s2 -> If(cond,s1,s2)]  *)
  val if_ : exp -> stmt list -> stmt list -> stmt

  (** Infix operators  *)
  module Infix : sig

    (** [x := y -> Move (x,y)]  *)
    val (:=) : var -> exp -> stmt

    (** {2 Arithmetic operations} *)

    (** [x + y -> BinOp (PLUS,x,y)]   *)
    val ( + )   : exp -> exp -> exp

    (** [x - y -> BinOp(MINUS,x,y)]  *)
    val ( - )   : exp -> exp -> exp

    (** [x * y -> BinOp(TIMES,x,y)]  *)
    val ( * )   : exp -> exp -> exp

    (** [x / y -> BinOp(DIVIDE,x,y)]  *)
    val ( / )   : exp -> exp -> exp

    (** [x /$ y -> BinOp(SDIVIDE,x,y)]  *)
    val ( /$ )  : exp -> exp -> exp

    (** [x mod y -> BinOp (MOD,x,y)]  *)
    val ( mod ) : exp -> exp -> exp

    (** [x %$ y -> BinOp (SMOD,x,y)]  *)
    val ( %$ )  : exp -> exp -> exp

    (** {2 Bit operations} *)

    (** [x lsl y = BinOp (LSHIFT,x,y)]  *)
    val ( lsl ) : exp -> exp -> exp

    (** [x lsr y = BinOp (RSHIFT,x,y)]  *)
    val ( lsr ) : exp -> exp -> exp

    (** [x asr y = BinOp (ARSHIFT,x,y)]  *)
    val ( asr ) : exp -> exp -> exp

    (** [x land y = BinOp (AND,x,y)]  *)
    val ( land) : exp -> exp -> exp

    (** [x lor y = BinOp (OR,x,y)]  *)
    val ( lor ) : exp -> exp -> exp

    (** [x lxor y = BinOp (XOR,x,y)]  *)
    val ( lxor) : exp -> exp -> exp

    (** [lnot x = UnOp (NOT,x,y)]  *)
    val lnot    : exp -> exp

    (** {2 Equality tests} *)

    (** [x = y -> BinOp(EQ,x,y)]  *)
    val ( = )   : exp -> exp -> exp

    (** [x = y -> BinOp(NEQ,x,y)]  *)
    val ( <> )   : exp -> exp -> exp

    (** [x < y -> BinOp(LT,x,y)]  *)
    val ( < )   : exp -> exp -> exp

    (** [x > y -> Binop(LT,y,x) ]  *)
    val ( > )   : exp -> exp -> exp

    (** [x <= y -> Binop(LE,x,y)]  *)
    val ( <= )   : exp -> exp -> exp

    (** [x <= y -> Binop(LE,y,x)]  *)
    val ( >= )   : exp -> exp -> exp

    (** {3 Signed comparison}  *)

    (** [x <$ x -> Binop(SLT,x,y)]  *)
    val ( <$ )  : exp -> exp -> exp

    (** [x >$ x -> Binop(SLT,y,x)]  *)
    val ( >$ )  : exp -> exp -> exp

    (** [x <=$ x -> Binop(SLE,x,y)]  *)
    val ( <=$ ) : exp -> exp -> exp

    (** [x >=$ x -> Binop(SLE,y,x)]  *)
    val ( >=$ ) : exp -> exp -> exp

    (** {2 Misc operations} *)

    (** [a ^ b -> Concat (a,b)] *)
    val ( ^ )   : exp -> exp -> exp
  end

  (** Brings infix operations into scope of the [Bil] module.  *)
  include module type of Infix

end

type dsl = Dsl.t [@@deriving bin_io, compare, sexp]
type exp = Dsl.exp [@@deriving bin_io, compare, sexp]
type stmt = Dsl.stmt [@@deriving bin_io, compare, sexp]

exception Invalid_instruction of string

(** [bil_of_dsl d] - returns a program in BIL language   *)
val bil_of_dsl : dsl -> bil

(** [find_gpr reg] - returns variable with the same name as
    register or thrown failure if variable is not found*)
val find_gpr : reg -> var

(** [find_gpr_opt reg] - returns [Some var] with the same name as
    reg or [None] if variable is not found *)
val find_gpr_opt : reg -> var option

(** [find_gpr_err reg] - returns [Ok var] with the same name as
    reg or [Error] if variable is not found *)
val find_gpr_err : reg -> var Or_error.t

(** [load32 addr size endian] - load from a 32-bit addressed memory *)
val load32 : addr:exp -> endian -> size -> exp

(** [load64 addr size endian] - load from a 64-bit addressed memory *)
val load64 : addr:exp -> endian -> size -> exp

(** [store32 addr size endian data] - store to a 32-bit addressed memory *)
val store32 : addr:exp -> endian -> size -> exp -> stmt

(** [store64 addr size endian data] - store to a 64-bit addressed memory *)
val store64 : addr:exp -> endian -> size -> exp -> stmt

(** [set_cr_field0 mode x] - set conditional register field 0
    according to comparisons x with zero. Also set SO bit:
    |bit| set to |
    | 0 |  x < 0 |
    | 1 |  x > 0 |
    | 2 |  x = 0 |
    | 3 |  SO    |  *)
val set_cond_reg0 : addr_size -> exp -> stmt
