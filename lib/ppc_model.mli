open Core_kernel.Std
open Bap.Std

module Hardware : sig

  val gpr_bitwidth : int
  val fpr_bitwidth : int
  val vr_bitwidth  : int
  val cr_bitwidth  : int
  val xer_bitwidth : int

  val gpr : Var.Set.t

  (** floating point registers *)
  val fpr : Var.Set.t

  (** vector registers *)
  val vr : Var.Set.t

  (** xer register bits *)
  val xer : var Int.Map.t

  (** conditional register bits *)
  val cr : var Int.Map.t

  (** count register  *)
  val ctr : var

  (** link register  *)
  val lr : var

  (** target register  *)
  val tar : var

  (** fixed preciion flags *)
  val so : var   (** summary overflow        *)
  val ca : var   (** carry flag              *)
  val ov : var   (** overflow flag           *)
  val ca32 : var (** carry out of 32 bits    *)
  val ov32 : var (** overflow of 32 bits     *)
  val zf : var   (** the result is zero      *)
  val nf : var   (** the result is negative  *)
  val pf : var   (** the result is positive  *)
end

module PPC32 : sig
  include module type of Hardware
  val mem : var
end

module PPC64 : sig
  include module type of Hardware
  val mem : var
end

module PowerPC_32_cpu : CPU
module PowerPC_64_cpu : CPU
