open Core_kernel.Std
open Bap.Std

type cast  = Bil.cast [@@deriving bin_io, compare, sexp]
type binop = Bil.binop [@@deriving bin_io, compare, sexp]
type unop  = Bil.unop [@@deriving bin_io, compare, sexp]

type exp  [@@deriving bin_io, compare, sexp]
type stmt [@@deriving bin_io, compare, sexp]

type t = stmt list [@@deriving bin_io, compare, sexp]

(** Infix operators  *)

(** [x := y -> x assign y]  *)
val (:=) : var -> exp -> stmt

(** {2 Arithmetic operations} *)

(** [x + y ]   *)
val ( + )   : exp -> exp -> exp

(** [x - y ]  *)
val ( - )   : exp -> exp -> exp

(** [x * y -> x TIMES y]  *)
val ( * )   : exp -> exp -> exp

(** [x / y -> x DIVIDE y]  *)
val ( / )   : exp -> exp -> exp

(** [x /$ y -> x SDIVIDE y ]  *)
val ( /$ )  : exp -> exp -> exp

(** [x mod y -> x MOD y]  *)
val ( mod ) : exp -> exp -> exp

(** [x %$ y -> x SMOD y)]  *)
val ( %$ )  : exp -> exp -> exp

(** {2 Bit operations} *)

(** [x lsl y = x LSHIFT y]  *)
val ( lsl ) : exp -> exp -> exp

(** [x lsr y = x RSHIFT y]  *)
val ( lsr ) : exp -> exp -> exp

(** [x asr y = x ARSHIFT y]  *)
val ( asr ) : exp -> exp -> exp

(** [x land y = x AND y]  *)
val ( land) : exp -> exp -> exp

(** [x lor y = x OR y]  *)
val ( lor ) : exp -> exp -> exp

(** [x lxor y = x XOR y]  *)
val ( lxor) : exp -> exp -> exp

(** [lnot x = NOT x]  *)
val lnot    : exp -> exp

(** {2 Equality tests} *)

(** [x = y -> x EQ y]  *)
val ( = )   : exp -> exp -> exp

(** [x = y -> x NEQ y]  *)
val ( <> )   : exp -> exp -> exp

(** [x < y -> x LT y]  *)
val ( < )   : exp -> exp -> exp

(** [x > y -> x LT y]  *)
val ( > )   : exp -> exp -> exp

(** [x <= y -> x LE y]  *)
val ( <= )   : exp -> exp -> exp

(** [x <= y -> x LE y)]  *)
val ( >= )   : exp -> exp -> exp

(** {3 Signed comparison}  *)

(** [x <$ x -> x SLT y]  *)
val ( <$ )  : exp -> exp -> exp

(** [x >$ x -> x SLT y)]  *)
val ( >$ )  : exp -> exp -> exp

(** [x <=$ x -> x SLE y]  *)
val ( <=$ ) : exp -> exp -> exp

(** [x >=$ x -> x SLE y]  *)
val ( >=$ ) : exp -> exp -> exp

(** {2 Misc operations} *)

(** [x ^ y -> x Concat y] *)
val ( ^ )   : exp -> exp -> exp

(** [var v]  *)
val var : var -> exp

(** [cast t w x]  *)
val cast  : cast -> int -> exp -> exp
val unsigned : cast
val signed : cast
val high : cast
val low : cast

(** [int w ]  *)
val int : word -> exp

(** [extract ~hi ~lo x ]  *)
val extract : hi:int -> lo:int -> exp -> exp

(** [concat x y -> Concat (x,y)]  *)
val concat : exp -> exp -> exp

(** [if_ cond s1 s2]  *)
val if_ : exp -> stmt list -> stmt list -> stmt

(** [jmp x ] *)
val jmp : exp -> stmt

(** [bil_of_t d] - returns a program in BIL language   *)
val bil_of_t : t -> bil

(** [ppc_fail error_string] - raise a failure with [error_string] *)
val ppc_fail : ('a, unit, string, 'b) format4 -> 'a

(** [load addr_size ~addr size endian] - load from a memory *)
val load : addr_size -> addr:exp -> endian -> size -> exp

(** [load addr_size ~addr size endian] - store to a memory *)
val store : addr_size -> addr:exp -> endian -> size -> exp -> stmt

(** [fresh name type] - returns a fresh variable of [type] *)
val fresh : string -> typ -> var

(** [low32 e] - extracts low 32 bits from [e] *)
val low32 : exp -> exp

(** [is_negative addr_size exp] - returns an expression that
    checks an [exp] for negative depending on address size *)
val is_negative : addr_size -> exp -> exp

(** [is_positive addr_size exp] - returns an expression that
    checks an [exp] for positive depending on address size *)
val is_positive : addr_size -> exp -> exp

(** [is_zero addr_size exp] - returns an expression that
    checks an [exp] for zero depending on address size *)
val is_zero : addr_size -> exp -> exp

(** [find_reg reg] - returns variable with the same name as
    register or thrown failure if variable is not found*)
val find_gpr : reg -> var

(** [find_gpr_opt reg] - returns [Some var] with the same name as
    reg or [None] if variable is not found *)
val find_gpr_opt : reg -> var option

(** [cr_bit reg] returns a CR bit from [reg]. *)
val cr_bit : reg -> var

(** [cr_bit' n] returns a CR bit number [n]. *)
val cr_bit' : int -> var

(** [cr_field reg] returns a CR field bits from [reg]. *)
val cr_field : reg -> var * var * var * var

(** [write_fixpoint_result addr_size result] - writes three bits
    of condition register according to the [result] *)
val write_fixpoint_result : addr_size -> var -> stmt list
