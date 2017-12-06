open Core_kernel.Std
open Bap.Std

open Powerpc_rtl

module type Hardware = sig
  type t
  val gpr  : t String.Map.t
  val gpri : t Int.Map.t
  val fpr : t String.Map.t
  val fpri : t Int.Map.t
  val vr : t String.Map.t
  val vri : t Int.Map.t
  val xer : t
  val ctr : t
  val lr : t
  val tar : t
  val cri : t Int.Map.t
  val crn : t String.Map.t
  val so : t
  val ca : t
  val ov : t
  val ca32 : t
  val ov32 : t
end

let range32 = List.range 0 32
let range64 = List.range 0 64

let make_var_i typ prefix i = Var.create (sprintf "%s%d" prefix i) typ

let make_regs typ prefix range =
  List.fold ~init:String.Map.empty ~f:(fun regs i ->
      let var = make_var_i typ prefix i in
      let name = Var.name var in
      String.Map.add regs name var) range

let make_regs_i typ prefix range =
  List.fold ~init:Int.Map.empty ~f:(fun regs i ->
      Int.Map.add regs i (make_var_i typ prefix i)) range

let flag name = Var.create name (Type.imm 1)

let gpr_bitwidth = 64
let fpr_bitwidth = 64
let vr_bitwidth  = 128
let cr_bitwidth  = 32
let xer_bitwidth = 64
let lr_bitwidth  = 64
let ctr_bitwidth = 64
let tar_bitwidth = 64

module Hardware_vars = struct

  (** general purpose registers  *)
  let gpr = make_regs (Type.imm gpr_bitwidth) "R" range32
  let gpri = make_regs_i (Type.imm gpr_bitwidth) "R" range32

  (** floating point registers *)
  let fpr = make_regs (Type.imm fpr_bitwidth) "F" range32
  let fpri = make_regs_i (Type.imm fpr_bitwidth) "F" range32

  (** vector registers *)
  let vr = make_regs (Type.imm vr_bitwidth) "VR" range32
  let vri = make_regs_i (Type.imm vr_bitwidth) "VR" range32

  (** count register  *)
  let ctr = Var.create "CTR" (Type.imm ctr_bitwidth)

  (** link register  *)
  let lr = Var.create "LR" (Type.imm lr_bitwidth)

  (** target register  *)
  let tar = Var.create "TAR" (Type.imm tar_bitwidth)

  (** fixed point exception register  *)
  let xer = Var.create "TAR" (Type.imm xer_bitwidth)

  (** fixed precision flags  *)
  let so = flag "SO" (** summary overflow *)
  let ca = flag "CA"
  let ov = flag "OV"
  let ca32 = flag "CA32" (** carry of low-order 32 bit result *)
  let ov32 = flag "OV32" (** overflow of low-order 32 bit result *)

  (** FPRF floating point result flags  *)
  let float_c = flag "C"          (** Result Class Descriptor        *)
  let float_less = flag "FL"      (** Less Than or Negative           *)
  let float_equal = flag "FE"     (** Greater Than or Positive        *)
  let float_greater = flag "FG"   (** Floating-Point Equal or Zero    *)
  let float_unordered = flag "FU" (** Floating-Point Unordered or NaN *)

  (** condition register bits  *)
  let cr0  = flag "CR7UN"
  let cr1  = flag "CR7EQ"
  let cr2  = flag "CR7GT"
  let cr3  = flag "CR7LT"
  let cr4  = flag "CR6UN"
  let cr5  = flag "CR6EQ"
  let cr6  = flag "CR6GT"
  let cr7  = flag "CR6LT"
  let cr8  = flag "CR5UN"
  let cr9  = flag "CR5EQ"
  let cr10 = flag "CR5GT"
  let cr11 = flag "CR5LT"
  let cr12 = flag "CR4UN"
  let cr13 = flag "CR4EQ"
  let cr14 = flag "CR4GT"
  let cr15 = flag "CR4LT"
  let cr16 = flag "CR3UN"
  let cr17 = flag "CR3EQ"
  let cr18 = flag "CR3GT"
  let cr19 = flag "CR3LT"
  let cr20 = flag "CR2UN"
  let cr21 = flag "CR2EQ"
  let cr22 = flag "CR2GT"
  let cr23 = flag "CR2LT"
  let cr24 = flag "CR1UN"
  let cr25 = flag "CR1EQ"
  let cr26 = flag "CR1GT"
  let cr27 = flag "CR1LT"
  let cr28 = flag "CR0UN"
  let cr29 = flag "CR0EQ"
  let cr30 = flag "CR0GT"
  let cr31 = flag "CR0LT"

  let cr_bits = [
    cr0;  cr1;  cr2;  cr3;  cr4;  cr5;  cr6;  cr7;
    cr8;  cr9;  cr10; cr11; cr12; cr13; cr14; cr15;
    cr16; cr17; cr18; cr19; cr20; cr21; cr22; cr23;
    cr24; cr25; cr26; cr27; cr28; cr29; cr30; cr31;
  ]

  let cri =
    let _, bits =
      List.fold (List.rev cr_bits) ~init:(0,Int.Map.empty)
        ~f:(fun (num, bits) bit ->
            num + 1, Int.Map.add bits ~key:num ~data:bit) in
    bits

  let crn =
    Int.Map.fold cri ~init:String.Map.empty
      ~f:(fun ~key ~data:var acc ->
          String.Map.add acc (Var.name var) var)

  let cr_fields =
    let fields = [
      "CR0", (cr28, cr29, cr30, cr31);
      "CR1", (cr24, cr25, cr26, cr27);
      "CR2", (cr20, cr21, cr22, cr23);
      "CR3", (cr16, cr17, cr18, cr19);
      "CR4", (cr12, cr13, cr14, cr15);
      "CR5", (cr8,  cr9,  cr10, cr11);
      "CR6", (cr4,  cr5,  cr6,  cr7);
      "CR7", (cr0,  cr1,  cr2,  cr3);
    ] in
    List.fold fields ~init:String.Map.empty ~f:(fun fs (name, fd) ->
        String.Map.add fs name fd)

end


module Hardware = struct
  open Hardware_vars

  let of_vars vars =
    String.Map.map vars ~f:(fun v -> Exp.of_var v)

  let of_vars_i vars =
    Int.Map.map vars ~f:(fun v -> Exp.of_var v)

  let gpri = of_vars_i gpri
  let gpr = of_vars gpr
  let fpri = of_vars_i fpri
  let fpr = of_vars fpr
  let vri = of_vars_i vri
  let vr = of_vars vr
  let xer = Exp.of_var xer
  let ctr = Exp.of_var ctr
  let lr  = Exp.of_var lr
  let tar = Exp.of_var tar
  let so  = Exp.of_var so
  let ca  = Exp.of_var ca
  let ov  = Exp.of_var ov
  let ca32 = Exp.of_var ca32
  let ov32 = Exp.of_var ov32

  let crn =
    Int.Map.fold cri ~init:String.Map.empty
      ~f:(fun ~key ~data:var acc ->
          String.Map.add acc (Var.name var) (Exp.of_var var))

  let cri = Int.Map.map cri ~f:(fun v -> Exp.of_var v)

  let cr = Exp.of_vars (List.rev cr_bits)

  let cr_fields =
    String.Map.map cr_fields ~f:(fun (b3,b2,b1,b0) -> Exp.of_vars [b0;b1;b2;b3])

end

let mem32 = Var.create "mem" (Type.mem `r32 `r8)
let mem64 = Var.create "mem" (Type.mem `r64 `r8)


module PPC32 = struct
  include Hardware_vars
  let mem = mem32
end

module PPC64 = struct
  include Hardware_vars
  let mem = mem64
end

module type PPC_cpu = sig
  include module type of Hardware_vars
  val mem : var
end

module Make_cpu(P : PPC_cpu) : CPU = struct
  include P

  let gpr =
    let data = Map.data gpr in
    List.fold data ~init:Var.Set.empty
      ~f:(fun regs v -> Var.Set.add regs v)

  let sp = Var.Set.find_exn gpr ~f:(fun v -> Var.name v = "R1")
  let vf = ov
  let cf = ca
  let nf = cr0
  let zf = cr2

  let flags = Var.Set.of_list [
      so; ca; ov; cf; nf; zf; ca32; ov32;
      float_c; float_less; float_equal;
      float_greater; float_unordered
    ]

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
