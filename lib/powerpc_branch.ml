open Core_kernel.Std
open Bap.Std

open Powerpc_types
open Hardware
open Dsl

(** Branch Instructions, Branch
    Page 37 of IBM Power ISATM Version 3.0 B
    examples:
    4b ff fe f0  b .+67108592
    4b ff fe f2  ba 67108592
    4b ff fe f1  bl .+67108592
    4b ff fe f3  bla 67108592 *)
let b cpu ops =
  let im = unsigned imm ops.(0) in
  let sh = unsigned int 2 in
  RTL.[cpu.jmp (cpu.addr + (im lsl sh))]

let ba cpu ops =
  let im = unsigned imm ops.(0) in
  let sh = unsigned int 2 in
  RTL.[ cpu.jmp (im lsl sh)]

let bl cpu ops =
  let im = unsigned imm ops.(0) in
  let sh = unsigned int 2 in
  let ad = unsigned int 4 in
  RTL.[
    cpu.jmp (cpu.addr + (im lsl sh));
    lr := cpu.addr + ad;
  ]

let bla cpu ops =
  let im = unsigned imm ops.(0) in
  let sh = unsigned int 2 in
  let ad = unsigned int 4 in
  RTL.[
    cpu.jmp (im lsl sh);
    lr := cpu.addr + ad;
  ]

(** [bo_field_bits bo] - returns an indexed list of bits of BO field  *)
let bo_field_bits bo =
  let bo = Option.value_exn (Imm.to_int bo) in
  let bit = Word.of_bool in
  [ 0, bit (bo land 1 <> 0);
    1, bit (bo land 2 <> 0);
    2, bit (bo land 4 <> 0);
    3, bit (bo land 8 <> 0);
    4, bit (bo land 16 <> 0); ]

let bo_bit bo n =
  let bo = bo_field_bits bo in
  match List.nth bo n with
  | None ->
    ppc_fail "requested BO field of branch insn with unexpected bit %d" n
  | Some (_,x) -> x


(** Branch Instructions, Branch Conditional
    Page 37 of IBM Power ISATM Version 3.0 B
    examples:
    42 9f 00 04  bc 20, 31, .+4
    42 9f 00 06  bca 20, 31, 4
    42 9f 00 05  bcl 20, 31, .+4
    42 9f 00 07  bcla 20, 31, 4   *)
let bc cpu ops =
  let bo = unsigned imm ops.(0) in
  let bi = unsigned reg ops.(1) in
  let bd = unsigned imm ops.(2) in
  let sh = unsigned int 2 in
  let ctr_ok = unsigned var in
  let cond_ok = unsigned var in
  let temp = unsigned int 42 in
  RTL.[
    if_ (nbit bo 2 = zero) [
      ctr := ctr - one;
    ] [];
    if_ ((nbit bo 2 = one) land (nbit bo 0 = one)) [
      cpu.jmp (cpu.addr + (bd lsl sh));
    ] [
      ctr_ok := (lnot (ctr = zero)) lxor (nbit bo 3);
      cond_ok := bi lxor (lnot (nbit bo 1));
      if_ (ctr_ok land cond_ok) [
        cpu.jmp (cpu.addr + (bd lsl sh));
      ] [
        cpu.jmp temp;
      ]
    ]
  ]

(* let bca mem addr_size  bo bi bd = *)
(*   let bo_bit = bo_bit bo in *)
(*   let jmp_addr = Word.of_int64 Int64.(Imm.to_int64 bd lsl 2) in *)
(*   let crbit = RTL.find bi in *)
(*   let ctr_ok = RTL.fresh "ctr_ok" (Type.imm 1) in *)
(*   let cond_ok = RTL.fresh "cond_ok" (Type.imm 1) in *)
(*   with_decrement_counter bo @@ *)
(*   if Word.is_one (bo_bit 2) && Word.is_one (bo_bit 4) then *)
(*     RTL.[jmp (addr_of_exp addr_size (int jmp_addr));] *)
(*   else *)
(*     RTL.[ *)
(*       ctr_ok := (lnot (is_zero addr_size (var ctr))) lxor (int @@ bo_bit 1); *)
(*       cond_ok := var crbit lxor (lnot (int @@ bo_bit 3)); *)
(*       if_ (var ctr_ok land var cond_ok) [ *)
(*         jmp (addr_of_exp addr_size (int jmp_addr)); *)
(*       ] [  ] *)
(*     ] *)

(* let bcl mem addr_size  bo bi bd = *)
(*   bc mem addr_size  bo bi bd  @ update_lr_register mem addr_size *)

(* let bcla mem addr_size  bo bi bd = *)
(*   bca mem addr_size  bo bi bd  @ update_lr_register mem addr_size *)

(* (\** bdnz  target = bc 16,0, target *\) *)
(* let bdnz mem addr_size  bd = *)
(*   let bd = Word.of_int64 Int64.(Imm.to_int64 bd lsl 2) in *)
(*   let jmp_addr = Word.(bd + Memory.min_addr mem) in *)
(*   RTL.[ *)
(*     decrement_counter_register; *)
(*     if_ ( lnot (is_zero addr_size (var ctr))) [ *)
(*       jmp (addr_of_exp addr_size (int jmp_addr)); *)
(*     ] [  ] *)
(*   ] *)

(* (\** Branch Instructions, Branch Conditional to Link Register *)
(*     Page 38 of IBM Power ISATM Version 3.0 B *)
(*     examples: *)
(*     4e 9f 00 20 	bclr	 20, 31 *)
(*     4e 9f 00 21 	bclrl	 20, 31 *\) *)
(* let bclr mem addr_size  bo bi bh = *)
(*   let bo_bit = bo_bit bo in *)
(*   let crbit = RTL.find bi in *)
(*   let ctr_ok = RTL.fresh "ctr_ok" (Type.imm 1) in *)
(*   let cond_ok = RTL.fresh "cond_ok" (Type.imm 1) in *)
(*   let zeros = Word.zero 2 in *)
(*   with_decrement_counter bo @@ *)
(*   if Word.is_one (bo_bit 2) && Word.is_one (bo_bit 4) then *)
(*     RTL.[ *)
(*       jmp (addr_of_exp addr_size ~exp_size:66 (var lr ^ int zeros)); *)
(*     ] *)
(*   else *)
(*     RTL.[ *)
(*       ctr_ok := (lnot (is_zero addr_size (var ctr))) lxor (int @@ bo_bit 1); *)
(*       cond_ok := var crbit lxor (lnot (int @@ bo_bit 3)); *)
(*       if_ (var ctr_ok land var cond_ok) [ *)
(*         jmp (addr_of_exp addr_size ~exp_size:66 (var lr ^ int zeros)); *)
(*       ] [  ]; *)
(*     ] *)

(* let bclrl mem addr_size  bo bi bh = *)
(*   bclr mem addr_size  bo bi bh @ update_lr_register mem addr_size *)

(* (\** bdnzlr = bclr 16,0,0  *\) *)
(* let bdnzlr mem addr_size = *)
(*   let zeros = Word.zero 2 in *)
(*   RTL.[ *)
(*     decrement_counter_register; *)
(*     if_ (lnot (is_zero addr_size (var ctr))) [ *)
(*       jmp (addr_of_exp addr_size ~exp_size:66 (var lr ^ int zeros)); *)
(*     ] [  ]; *)
(*   ] *)

(* (\** Branch Instructions, Branch Conditional to Count Register *)
(*     Page 38 of IBM Power ISATM Version 3.0 B *)
(*     examples: *)
(*     4d 5f 04 20    bcctr 10,31 *)
(*     4d 5f 04 21    bcctrl 10,31 *\) *)
(* let bcctr mem addr_size  bo bi bh = *)
(*   let bo_bit = bo_bit bo in *)
(*   let crbit = RTL.find bi in *)
(*   let cond_ok = RTL.fresh "cond_ok" (Type.imm 1) in *)
(*   let zeros = Word.zero 2 in *)
(*   if  Word.is_one (bo_bit 4) then *)
(*     RTL.[ *)
(*       jmp (addr_of_exp addr_size ~exp_size:66 (var ctr ^ int zeros)); *)
(*     ] *)
(*   else *)
(*     RTL.[ *)
(*       cond_ok := var crbit lxor (lnot (int @@ bo_bit 3)); *)
(*       if_ (var cond_ok) [ *)
(*         jmp (addr_of_exp addr_size ~exp_size:66 (var ctr ^ int zeros)); *)
(*       ] [  ]; *)
(*     ] *)

(* let bcctrl mem addr_size  bo bi bh = *)
(*   bcctr mem addr_size  bo bi bh @ update_lr_register mem addr_size *)

(* (\** Branch Instructions, Branch Conditional to Target Register *)
(*     Page 39 of IBM Power ISATM Version 3.0 B *)
(*     examples: *)
(*     4e 9f 04 60    bctar *)
(*     4e 9f 04 61    bctarl *)
(*     bctar insn aren't implemented ny llvm right now *\) *)
(* let bctar mem addr_size  bo bi bh = *)
(*   let bo_bit = bo_bit bo in *)
(*   let crbit = RTL.find bi in *)
(*   let ctr_ok = RTL.fresh "ctr_ok" (Type.imm 1) in *)
(*   let cond_ok = RTL.fresh "cond_ok" (Type.imm 1) in *)
(*   let zeros = Word.zero 2 in *)
(*   with_decrement_counter bo @@ *)
(*   if Word.is_one (bo_bit 2) && Word.is_one (bo_bit 4) then *)
(*     RTL.[ *)
(*       jmp (addr_of_exp addr_size ~exp_size:66 (var tar ^ int zeros)); *)
(*     ] *)
(*   else *)
(*     RTL.[ *)
(*       ctr_ok := (lnot (is_zero addr_size (var ctr))) lxor (int @@ bo_bit 1); *)
(*       cond_ok := var crbit lxor (lnot (int @@ bo_bit 3)); *)
(*       if_ (var ctr_ok land var cond_ok) [ *)
(*         jmp (addr_of_exp addr_size ~exp_size:66 (var tar ^ int zeros)); *)
(*       ] [  ]; *)
(*     ] *)

(* let bctarl mem addr_size  bo bi bh = *)
(*   bctar mem addr_size  bo bi bh @ update_lr_register mem addr_size *)

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
  | `BDNZ
] [@@deriving sexp, enumerate]

type bc_reg = [
  | `gBCLR
  | `gBCLRL
  | `gBCCTR
  | `gBCCTRL
  | `gBCTAR
  | `gBCTARL
  | `BDNZLR
] [@@deriving sexp, enumerate]

type t = [ b | bc | bc_reg] [@@deriving sexp, enumerate]

let string_of_opcode op = Sexp.to_string (sexp_of_t op)

let lift opcode cpu ops =
  let open Op in
  match opcode, ops with
  | `B,       [| Imm imm; |] -> b cpu ops
  | `BA,      [| Imm imm; |] -> ba cpu ops
  | `BL,      [| Imm imm; |] -> bl cpu ops
  | `BLA,     [| Imm imm; |] -> bla cpu ops
  | `gBC,     [| Imm bo; Reg reg; Imm bd |] -> bc cpu ops
  (* | `gBCA,    [| Imm bo; Reg reg; Imm bd |] -> bca    mem addr_size bo reg bd *)
  (* | `gBCL,    [| Imm bo; Reg reg; Imm bd |] -> bcl    mem addr_size bo reg bd *)
  (* | `gBCLA,   [| Imm bo; Reg reg; Imm bd |] -> bcla   mem addr_size bo reg bd *)
  (* | `BDNZ,    [| Imm bd |]                  -> bdnz   mem addr_size bd *)
  (* | `gBCLR,   [| Imm bo; Reg reg; Imm bd |] -> bclr   mem addr_size bo reg bd *)
  (* | `gBCLRL,  [| Imm bo; Reg reg; Imm bd |] -> bclrl  mem addr_size bo reg bd *)
  (* | `gBCCTR,  [| Imm bo; Reg reg; Imm bd |] -> bcctr  mem addr_size bo reg bd *)
  (* | `gBCCTRL, [| Imm bo; Reg reg; Imm bd |] -> bcctrl mem addr_size bo reg bd *)
  (* | `BDNZLR,  [| |]                         -> bdnzlr mem addr_size *)
  (* | `gBCTAR,  [| Imm bo; Reg reg; Imm bd |] -> bctar  mem addr_size bo reg bd *)
  (* | `gBCTARL, [| Imm bo; Reg reg; Imm bd |] -> bctarl mem addr_size bo reg bd *)
  | opcode, _ ->
    ppc_fail "%s: unexpected operand set" (string_of_opcode opcode)
