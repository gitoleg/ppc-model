open Core_kernel.Std
open Bap.Std

open Ppc_model.Hardware
open Ppc_rtl

let addr_of_exp addr_size exp = match addr_size with
  | `r32 -> extract_low_32 exp
  | `r64 -> exp

(** TODO:  That's weird!, so it's just a temporary solution *)
let get_cr_bit reg =
  let name = Reg.name reg in
  let field_num = String.subo ~pos:2 ~len:1 name in
  let field_num = int_of_string field_num in
  let to_bit = Bool.to_int in
  let bit_lt = to_bit @@ String.is_suffix ~suffix:"LT" name in
  let bit_gt = to_bit @@ String.is_suffix ~suffix:"GT" name in
  let bit_eq = to_bit @@ String.is_suffix ~suffix:"EQ" name in
  let bit_un = to_bit @@ String.is_suffix ~suffix:"UN" name in
  let bit_num = match bit_lt,bit_gt,bit_eq,bit_un with
    | 1,_,_,_ -> 0
    | _,1,_,_ -> 1
    | _,_,1,_ -> 2
    | _,_,_,1 -> 3
    | _ -> ppc_fail "unexpected CR bit name %s" name in
  let x = field_num * 4 + bit_num in
  cr_bitwidth - x - 1

(** Branch Instructions, Branch
    Page 37 of IBM Power ISATM Version 3.0 B
    examples:
    4b ff fe f0  b .+67108592
    4b ff fe f2  ba 67108592
    4b ff fe f1  bl .+67108592
    4b ff fe f3  bla 67108592 *)
let b mem addr_size imm =
  let insn_addr = Memory.min_addr mem in
  let imm = Imm.to_int64 imm in
  let addr = Word.of_int64 Int64.(imm lsl 2) in
  let addr = Word.(addr + insn_addr) in
  Dsl.[jmp (addr_of_exp addr_size (int addr))]

let ba mem addr_size imm =
  let imm = Imm.to_int64 imm in
  let addr = Word.of_int64 Int64.(imm lsl 2) in
  Dsl.[ jmp (addr_of_exp addr_size (int addr)) ]

let bl mem addr_size imm =
  let insn_addr = Memory.min_addr mem in
  let next_insn_addr = Word.(of_int ~width:32 4 + insn_addr) in
  let imm = Imm.to_int64 imm in
  let addr = Word.of_int64 Int64.(imm lsl 2) in
  Dsl.[
    jmp (addr_of_exp addr_size (int addr));
    lr := cast unsigned lr_bitwidth (int next_insn_addr);
  ]

let bla mem addr_size imm =
  let insn_addr = Memory.min_addr mem in
  let next_insn_addr = Word.(of_int ~width:32 4 + insn_addr) in
  let imm = Imm.to_int64 imm in
  let addr = Word.(insn_addr +  of_int64 Int64.(imm lsl 2)) in
  Dsl.[
    jmp (addr_of_exp addr_size (int addr));
    lr := cast unsigned lr_bitwidth (int next_insn_addr);
  ]

(** [bo_field_bits bo] - returns an indexed list of bits of BO field  *)
let bo_field_bits bo =
  let bo = Option.value_exn (Imm.to_int bo) in
  let bit = Word.of_bool in
  [ 0, bit (bo land 2 <> 1);
    1, bit (bo land 2 <> 0);
    2, bit (bo land 4 <> 0);
    3, bit (bo land 8 <> 0);
    4, bit (bo land 16 <> 0); ]

let bo_bit bo n =
  let bo = bo_field_bits bo in
  match List.nth bo n with
  | None ->
    ppc_fail "requested BO field of branch insn with unexpected bit %d" n
  | Some (_,x) -> Dsl.int x

(** Branch Instructions, Branch Conditional
    Page 37 of IBM Power ISATM Version 3.0 B
    examples:
    42 9f 00 04  bc 20, 31, .+4
    42 9f 00 06  bca 20, 31, 4
    42 9f 00 05  bcl 20, 31, .+4
    42 9f 00 07  bcla 20, 31, 4   *)
let bc mem addr_size endian bo bi bd =
  let bo_bit = bo_bit bo in
  let bd = Word.of_int64 Int64.(Imm.to_int64 bd lsl 2) in
  let next_insn_addr = Word.(bd + Memory.min_addr mem) in
  let cr_bit = condition_register_bit (get_cr_bit bi) in
  let ctr_ok = Var.create ~fresh:true "ctr_ok" (Type.imm 1) in
  let cond_ok = Var.create ~fresh:true "cond_ok" (Type.imm 1) in
  Dsl.[
    if_ (lnot (bo_bit 2)) [ decrement_counter_register ] [];
    ctr_ok := bo_bit 2 lor ((lnot (is_zero addr_size (var ctr))) lxor bo_bit 1);
    cond_ok := bo_bit 4 lor (var cr_bit lxor (lnot (bo_bit 3)));
    if_ (var ctr_ok land var cond_ok) [
      jmp (addr_of_exp addr_size (int next_insn_addr));
    ] [  ]
  ]

let bca bo reg bd =  []

let bcl bo reg bd =  []

let bcla bo reg bd = []

type b = [
  | `B
  | `BA
  | `BL
  | `BLA
] [@@deriving sexp, enumerate]

type bc = [
  | `gBC
  | `gBCA
  | `gBCL
  | `gBCLA
] [@@deriving sexp, enumerate]

type t = [ b | bc ] [@@deriving sexp, enumerate]

let lift opcode addr_size endian mem ops =
  let open Op in
  match opcode, ops with
  | `B,   [| Imm imm; |] -> b mem addr_size imm
  | `BA,  [| Imm imm; |] -> ba mem addr_size imm
  | `BL,  [| Imm imm; |] -> bl mem addr_size imm
  | `BLA, [| Imm imm; |] -> bla mem addr_size imm
  | `gBC,   [| Imm bo; Reg reg; Imm bd |] -> bc mem addr_size endian bo reg bd
  | `gBCA,  [| Imm bo; Reg reg; Imm bd |] -> bca bo reg bd
  | `gBCL,  [| Imm bo; Reg reg; Imm bd |] -> bcl bo reg bd
  | `gBCLA, [| Imm bo; Reg reg; Imm bd |] -> bcla bo reg bd
  | opcode, _ ->
    (* let opcode = Sexp.to_string (sexp_of_t opcode) in *)
    (* ppc_fail "%s: unexpected operand set" opcode *)
    ppc_fail "unexpected operand set"
