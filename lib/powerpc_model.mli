open Core_kernel.Std
open Bap.Std

open Powerpc_rtl

(** general purpose registers bitwidth *)
val gpr_bitwidth : int

(** floating point registers bitwidth *)
val fpr_bitwidth : int

(** vector registers bitwidth *)
val vr_bitwidth  : int

(** condition register bitwidth *)
val cr_bitwidth  : int

(** xer register bitwidth *)
val xer_bitwidth : int

(** link register bitwidth *)
val lr_bitwidth  : int

(** count register bitwidth *)
val ctr_bitwidth : int

(** target address register bitwidth *)
val tar_bitwidth : int

module Hardware : sig

  (** all general purpose registers *)
  val gpr : exp String.Map.t

  (** all floating point registers *)
  val fpr : exp String.Map.t

  (** all vector registers *)
  val vr : exp String.Map.t

  (** xer register bits *)
  val xer : exp

  (** conditional register bits *)
  val cr : exp Int.Map.t

  (** count register  *)
  val ctr : exp

  (** link register  *)
  val lr : exp

  (** target register  *)
  val tar : exp

  (** condition register bits *)
  val cri : exp Int.Map.t

  (** condition register bits *)
  val cr : exp String.Map.t

  (** condition register fields *)
  val cr_fields : exp String.Map.t

  (** fixed precision flags *)
  val so : exp   (** summary overflow        *)
  val ca : exp   (** carry flag              *)
  val ov : exp   (** overflow flag           *)
  val ca32 : exp (** carry out of 32 bits    *)
  val ov32 : exp (** overflow of 32 bits     *)
end

(** 32-bit addressed memory *)
val mem32 : var

(** 64-bit addressed memory *)
val mem64 : var

module PowerPC_32_cpu : CPU
module PowerPC_64_cpu : CPU
