open Bap.Std

(** [ppc_fail error_string] - raise a failure with [error_string] *)
val ppc_fail : ('a, unit, string, 'b) format4 -> 'a

val var_bitwidth : var -> int
