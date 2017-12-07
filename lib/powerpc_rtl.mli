open Core_kernel.Std
open Bap.Std

type bil_exp = exp

type t
type rtl = t
type exp

module Exp : sig

  val of_var  : var -> exp
  val of_vars : var list -> exp
  val of_word : word -> exp
  val tmp : int -> exp

  val load : var -> exp -> endian -> size -> exp
  val extract : int -> int -> exp -> exp

  val signed : exp -> exp
  val unsigned : exp -> exp

  val bil_exp : exp -> bil_exp

  val width : exp -> int

end

val store : var -> exp -> exp -> endian -> size -> t
val if_ : exp -> t list -> t list -> t
val jmp : exp -> t
val foreach : exp -> exp -> t list -> t

module Infix : sig
  val ( := )  : exp -> exp -> rtl
  val ( + )  : exp -> exp -> exp
  val ( - )  : exp -> exp -> exp
  val ( * )  : exp -> exp -> exp
  val ( / )  : exp -> exp -> exp
  val ( ^ )  : exp -> exp -> exp
  val ( < )  : exp -> exp -> exp
  val ( > )  : exp -> exp -> exp
  val ( <= )  : exp -> exp -> exp
  val ( >= )  : exp -> exp -> exp
  val ( = )  : exp -> exp -> exp
  val ( <> )  : exp -> exp -> exp
  val ( <$ ) : exp -> exp -> exp
  val ( >$ ) : exp -> exp -> exp
  val ( lsl )  : exp -> exp -> exp
  val ( lsr )  : exp -> exp -> exp
  val ( lor )  : exp -> exp -> exp
  val ( land ) : exp -> exp -> exp
  val ( lxor ) : exp -> exp -> exp
  val lnot : exp -> exp
end


(** [bil_of_t d] - returns a program in BIL language   *)
val bil_of_t : t list -> bil
