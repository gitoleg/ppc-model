open Core_kernel.Std
open Bap.Std

open Powerpc_rtl

module type Hardware = sig
  type t
  (** all general purpose registers *)
  val gpr  : t String.Map.t
  val gpri : t Int.Map.t

  (** all floating point registers *)
  val fpr : t String.Map.t
  val fpri : t Int.Map.t

  (** all vector registers *)
  val vr : t String.Map.t
  val vri : t Int.Map.t

  (** xer register bits *)
  val xer : t

  (** count register  *)
  val ctr : t

  (** link register  *)
  val lr : t

  (** target register  *)
  val tar : t

  (** condition register bits, starting from msb *)
  val cri : t Int.Map.t

  (** condition register bits *)
  val crn : t String.Map.t

  (** fixed precision flags *)
  val so : t   (** summary overflow        *)
  val ca : t   (** carry flag              *)
  val ov : t   (** overflow flag           *)
  val ca32 : t (** carry out of 32 bits    *)
  val ov32 : t (** overflow of 32 bits     *)
end

val gpr_bitwidth : int
val fpr_bitwidth : int
val vr_bitwidth  : int
val cr_bitwidth  : int
val xer_bitwidth : int
val lr_bitwidth  : int
val ctr_bitwidth : int
val tar_bitwidth : int


module Hardware_vars : Hardware with type t := var

module Hardware : sig
  include Hardware with type t := exp

  (** condition register  *)
  val cr : exp

  (** condition register fields *)
  val cr_fields : exp String.Map.t
end

(** 32-bit addressed memory *)
val mem32 : var

(** 64-bit addressed memory *)
val mem64 : var

module PowerPC_32_cpu : CPU
module PowerPC_64_cpu : CPU
