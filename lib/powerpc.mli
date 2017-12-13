open Core_kernel.Std
open Bap.Std

(** Operands and registers bitwidth.  *)
type bitwidth

val bit  : bitwidth
val byte : bitwidth
val word : bitwidth
val halfword : bitwidth
val doubleword : bitwidth
val quadroword : bitwidth
val bitwidth : int -> bitwidth

type 'a p
type exp
type rtl

val imm : (op -> exp) p
val reg : (op -> exp) p
val var : (bitwidth -> exp) p
val const : (bitwidth -> int -> exp) p

(** those functions return exp of appropriative bitwidth
    from a given integer or operand *)
val signed : 'a p -> 'a
val unsigned : 'a p -> 'a

module RTL : sig
  val ( := ) : exp -> exp -> rtl
  val ( + ) : exp -> exp -> exp
  val ( - ) : exp -> exp -> exp
  val ( * ) : exp -> exp -> exp
  val ( / ) : exp -> exp -> exp
  val ( /$ ) : exp -> exp -> exp
  val ( ^ ) : exp -> exp -> exp
  val ( % ) : exp -> exp -> exp
  val ( %$ ) : exp -> exp -> exp
  val ( < ) : exp -> exp -> exp
  val ( > ) : exp -> exp -> exp
  val ( <= ) : exp -> exp -> exp
  val ( >= ) : exp -> exp -> exp
  val ( = ) : exp -> exp -> exp
  val ( <> ) : exp -> exp -> exp
  val ( <$ ) : exp -> exp -> exp
  val ( >$ ) : exp -> exp -> exp
  val ( <=$ )  : exp -> exp -> exp
  val ( >=$ )  : exp -> exp -> exp
  val ( lsl )  : exp -> exp -> exp
  val ( lsr )  : exp -> exp -> exp
  val ( lor )  : exp -> exp -> exp
  val ( land ) : exp -> exp -> exp
  val ( lxor ) : exp -> exp -> exp
  val lnot : exp -> exp

  (** [if_ cond then_ else_] *)
  val if_ : exp -> rtl list -> rtl list -> rtl

  (** [foreach step e rtl] - repeat [rtl] for each [step] of [e].
      One must create an iteration variable to iterate over some
      expression. So, in example below, assuming the first operand
      is a 64-bit register, [cnt] will be equal to 8:
      ...
      let reg = unsigned reg ops.(0) in
      let cnt = unsigned const byte in
      let byte_i = unsigned var byte in
      RTL.[
         cnt := zero;
         foreach byte_i reg [
             cnt := cnt + one;
         ]
      ]
      ...

      One can use iteration variable to change content of register,
      e.g. :
      ...
      if_ (cnt = zero) [
          byte_i := zero;
      ]
      ...
      will set a most significant byte of [reg] to zero *)
  val foreach : exp -> exp -> rtl list -> rtl
end

(** [zero] is a one bit length expression set to zero *)
val zero : exp

(** [one] is a one bit length expression set to one *)
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

(** [when_ cond rtl] = if_ cond rtl [] *)
val when_ : exp -> rtl list -> rtl

(** [ifnot cond rtl] = if_ cond [] rtl *)
val ifnot : exp -> rtl list -> rtl

(** [ppc_fail error_string] - raise a failure with [error_string] *)
val ppc_fail : ('a, unit, string, 'b) format4 -> 'a

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

(** [bil_of_rtl rtl] - returns a bil code *)
val bil_of_rtl : rtl list -> bil

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
  cr0       : exp;       (** condition register field 0 *)
  cr1       : exp;       (** condition register field 1 *)
  cr2       : exp;       (** condition register field 2 *)
  cr3       : exp;       (** condition register field 3 *)
  cr4       : exp;       (** condition register field 4 *)
  cr5       : exp;       (** condition register field 5 *)
  cr6       : exp;       (** condition register field 6 *)
  cr7       : exp;       (** condition register field 7 *)

  (** fixed precision flags *)
  so        : exp; (** summary overflow        *)
  ca        : exp; (** carry flag              *)
  ov        : exp; (** overflow flag           *)
  ca32      : exp; (** carry out of 32 bits    *)
  ov32      : exp; (** overflow of 32 bits     *)
}

type lift = cpu -> op array -> rtl list

val make_cpu : addr_size -> endian -> mem -> cpu

(** [register name lift] - registers a lifter for instruction [name] *)
val register : string -> lift -> unit

(** [name >: lift]  - registers a lifter for instruction [name]  *)
val (>>) : string -> lift -> unit

(** [dot insn cr_field res] - returns a lift for dot version of [insn],
    with an additinal code for writing {lt,gt,eq} bits of [cr_field] *)
val dot : lift -> cpu -> op array -> rtl list

(** [name >: lift] - registers a lifter for instruction [name]  *)
val (>.) : string -> lift -> unit
