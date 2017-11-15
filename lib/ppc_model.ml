open Core_kernel.Std
open Bap.Std

let index_range = List.range 0 32

let make_var_i typ prefix i = Var.create (sprintf "%s%d" prefix i) typ

let make_regs typ prefix =
  List.fold ~init:Var.Set.empty ~f:(fun regs i ->
      Var.Set.add regs (make_var_i typ prefix i)) index_range

let make_bits range prefix =
  List.fold index_range ~init:Int.Map.empty ~f:(fun bits i ->
      Int.Map.add bits ~key:i ~data:(make_var_i (Type.imm 1) prefix i))

let flag name = Var.create name (Type.imm 1)

module Hardware = struct

  let gpr_bitwidth = 64
  let fpr_bitwidth = 64
  let vr_bitwidth  = 128
  let cr_bitwidth  = 32
  let xer_bitwidth = 64

  let gpr = make_regs (Type.imm gpr_bitwidth) "R"

  (** floating point registers *)
  let fpr = make_regs (Type.imm fpr_bitwidth) "F"

  (** vector registers *)
  let vr = make_regs (Type.imm vr_bitwidth) "VR"

  (** count register  *)
  let ctr = Var.create "CTR" (Type.imm 64)

  (** link register  *)
  let lr = Var.create "LR" (Type.imm 64)

  (** target register  *)
  let tar = Var.create "TAR" (Type.imm 64)

  (** fixed precision flags  *)
  let so = flag "SO" (** summary overflow *)
  let ca = flag "CA"
  let ov = flag "OV"
  let zf = flag "ZF" (** the result is zero      *)
  let nf = flag "NF" (** the result is negative  *)
  let pf = flag "PF" (** the result is positive  *)
  let ca32 = flag "CA32" (** carry of low-order 32 bit result *)
  let ov32 = flag "OV32" (** overflow of low-order 32 bit result *)

  (** FPRF floating point result flags  *)
  let float_c = flag "C"          (** Result Class Descriptor        *)
  let float_less = flag "FL"      (** Less Than or Negative           *)
  let float_equal = flag "FE"     (** Greater Than or Positive        *)
  let float_greater = flag "FG"   (** Floating-Point Equal or Zero    *)
  let float_unordered = flag "FU" (** Floating-Point Unordered or NaN *)

  (** conditional register bits *)
  let cr =
    List.fold index_range ~init:Int.Map.empty
      ~f:(fun bits i ->
          let bit = match i with
            | 0 -> nf
            | 1 -> pf
            | 2 -> zf
            | 3 -> so
            | _ -> make_var_i (Type.imm 1) "CR" i in
          Int.Map.add bits ~key:i ~data:bit)

  (** fixed point exception register  *)
  let xer =
    List.fold index_range ~init:Int.Map.empty
      ~f:(fun bits i ->
          let bit = match i with
            | 33-> ov
            | 34 -> ca
            | 44 -> ov32
            | 45 -> ca32
            | _ -> make_var_i (Type.imm 1) "XER" i in
          Int.Map.add bits ~key:i ~data:bit)

  let flags = Var.Set.of_list [
      so; ca; ov; zf; nf; pf; ca32; ov32;
      float_c; float_less; float_equal;
      float_greater; float_unordered
    ]
end

module PPC32 = struct
  include Hardware
  let mem = Var.create "mem" (Type.mem `r32 `r8)
end

module PPC64 = struct
  include Hardware
  let mem = Var.create "mem" (Type.mem `r64 `r8)
end

module type PPC_cpu = sig
  val gpr : Var.Set.t
  val mem : var
  val flags : Var.Set.t
  val zf : var
  val ca : var
  val ov : var
  val nf : var
end

module Make_cpu(P : PPC_cpu) : CPU = struct
  include P

  let sp = Var.Set.find_exn gpr ~f:(fun v -> Var.name v = "R1")
  let vf = ov
  let cf = ca

  let is = Var.same
  let is_reg r = Set.mem gpr (Var.base r)
  let is_flag r = Set.mem flags (Var.base r)
  let is_zf = is zf
  let is_cf = is ca
  let is_vf = is vf
  let is_nf = is nf
  let is_mem = is mem
  let is_sp = is sp
  let is_bp _ = false
end

module PowerPC_32_cpu = Make_cpu(PPC32)
module PowerPC_64_cpu = Make_cpu(PPC64)
