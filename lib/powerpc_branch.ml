open Core_kernel.Std
open Bap.Std

open Powerpc_types
open Hardware

let addr_of_exp addr_size ?(exp_size=64) exp = match addr_size with
  | `r32 -> Dsl.low32 exp
  | `r64 ->
    if exp_size = 64 then exp
    else Dsl.(cast unsigned 64 exp)

let update_lr_register mem addr_size  =
  let addr_bits = Size.in_bits addr_size in
  let current = Memory.min_addr mem in
  let next = Word.(of_int ~width:addr_bits 4 + current) in
  match addr_size with
  | `r32 -> Dsl.[lr := cast unsigned lr_bitwidth (int next)]
  | `r64 -> Dsl.[lr := int next]

(** Branch Instructions, Branch
    Page 37 of IBM Power ISATM Version 3.0 B
    examples:
    4b ff fe f0  b .+67108592
    4b ff fe f2  ba 67108592
    4b ff fe f1  bl .+67108592
    4b ff fe f3  bla 67108592 *)
let b mem addr_size imm =
  let current = Memory.min_addr mem in
  let imm = Imm.to_int64 imm in
  let imm = Word.of_int64 Int64.(imm lsl 2) in
  let jmp_addr = Word.(current + imm) in
  Dsl.[jmp (addr_of_exp addr_size (int jmp_addr))]

let ba mem addr_size imm =
  let imm = Imm.to_int64 imm in
  let jmp_addr = Word.of_int64 Int64.(imm lsl 2) in
  Dsl.[ jmp (addr_of_exp addr_size (int jmp_addr)) ]

let bl mem addr_size imm =
  b mem addr_size imm  @ update_lr_register mem addr_size

let bla mem addr_size imm =
  ba mem addr_size imm  @ update_lr_register mem addr_size

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
    Dsl.ppc_fail "requested BO field of branch insn with unexpected bit %d" n
  | Some (_,x) -> x

let decrement_counter_register =
  let one = Word.one ctr_bitwidth in
  Dsl.(ctr := var ctr - int one)

let with_decrement_counter bo code =
  if Word.is_zero (bo_bit bo 2) then
    decrement_counter_register :: code
  else code

(** Branch Instructions, Branch Conditional
    Page 37 of IBM Power ISATM Version 3.0 B
    examples:
    42 9f 00 04  bc 20, 31, .+4
    42 9f 00 06  bca 20, 31, 4
    42 9f 00 05  bcl 20, 31, .+4
    42 9f 00 07  bcla 20, 31, 4   *)
let bc mem addr_size bo bi bd =
  let bo_bit = bo_bit bo in
  let bd = Word.of_int64 Int64.(Imm.to_int64 bd lsl 2) in
  let jmp_addr = Word.(bd + Memory.min_addr mem) in
  let crbit = Dsl.find bi in
  let ctr_ok = Dsl.fresh "ctr_ok" (Type.imm 1) in
  let cond_ok = Dsl.fresh"cond_ok" (Type.imm 1) in
  with_decrement_counter bo @@
  if Word.is_one (bo_bit 2) && Word.is_one (bo_bit 4) then
    Dsl.[jmp (addr_of_exp addr_size (int jmp_addr));]
  else
    Dsl.[
      ctr_ok := (lnot (is_zero addr_size (var ctr))) lxor (int @@ bo_bit 1);
      cond_ok := var crbit lxor (lnot (int @@ bo_bit 3));
      if_ (var ctr_ok land var cond_ok) [
        jmp (addr_of_exp addr_size (int jmp_addr));
      ] [  ]
    ]

let bca mem addr_size  bo bi bd =
  let bo_bit = bo_bit bo in
  let jmp_addr = Word.of_int64 Int64.(Imm.to_int64 bd lsl 2) in
  let crbit = Dsl.find bi in
  let ctr_ok = Dsl.fresh "ctr_ok" (Type.imm 1) in
  let cond_ok = Dsl.fresh "cond_ok" (Type.imm 1) in
  with_decrement_counter bo @@
  if Word.is_one (bo_bit 2) && Word.is_one (bo_bit 4) then
    Dsl.[jmp (addr_of_exp addr_size (int jmp_addr));]
  else
    Dsl.[
      ctr_ok := (lnot (is_zero addr_size (var ctr))) lxor (int @@ bo_bit 1);
      cond_ok := var crbit lxor (lnot (int @@ bo_bit 3));
      if_ (var ctr_ok land var cond_ok) [
        jmp (addr_of_exp addr_size (int jmp_addr));
      ] [  ]
    ]

let bcl mem addr_size  bo bi bd =
  bc mem addr_size  bo bi bd  @ update_lr_register mem addr_size

let bcla mem addr_size  bo bi bd =
  bca mem addr_size  bo bi bd  @ update_lr_register mem addr_size

(** bdnz  target = bc 16,0, target *)
let bdnz mem addr_size  bd =
  let bd = Word.of_int64 Int64.(Imm.to_int64 bd lsl 2) in
  let jmp_addr = Word.(bd + Memory.min_addr mem) in
  Dsl.[
    decrement_counter_register;
    if_ ( lnot (is_zero addr_size (var ctr))) [
      jmp (addr_of_exp addr_size (int jmp_addr));
    ] [  ]
  ]

(** Branch Instructions, Branch Conditional to Link Register
    Page 38 of IBM Power ISATM Version 3.0 B
    examples:
    4e 9f 00 20 	bclr	 20, 31
    4e 9f 00 21 	bclrl	 20, 31 *)
let bclr mem addr_size  bo bi bh =
  let bo_bit = bo_bit bo in
  let crbit = Dsl.find bi in
  let ctr_ok = Dsl.fresh "ctr_ok" (Type.imm 1) in
  let cond_ok = Dsl.fresh "cond_ok" (Type.imm 1) in
  let zeros = Word.zero 2 in
  with_decrement_counter bo @@
  if Word.is_one (bo_bit 2) && Word.is_one (bo_bit 4) then
    Dsl.[
      jmp (addr_of_exp addr_size ~exp_size:66 (var lr ^ int zeros));
    ]
  else
    Dsl.[
      ctr_ok := (lnot (is_zero addr_size (var ctr))) lxor (int @@ bo_bit 1);
      cond_ok := var crbit lxor (lnot (int @@ bo_bit 3));
      if_ (var ctr_ok land var cond_ok) [
        jmp (addr_of_exp addr_size ~exp_size:66 (var lr ^ int zeros));
      ] [  ];
    ]

let bclrl mem addr_size  bo bi bh =
  bclr mem addr_size  bo bi bh @ update_lr_register mem addr_size

(** bdnzlr = bclr 16,0,0  *)
let bdnzlr mem addr_size =
  let zeros = Word.zero 2 in
  Dsl.[
    decrement_counter_register;
    if_ (lnot (is_zero addr_size (var ctr))) [
      jmp (addr_of_exp addr_size ~exp_size:66 (var lr ^ int zeros));
    ] [  ];
  ]

(** Branch Instructions, Branch Conditional to Count Register
    Page 38 of IBM Power ISATM Version 3.0 B
    examples:
    4d 5f 04 20    bcctr 10,31
    4d 5f 04 21    bcctrl 10,31 *)
let bcctr mem addr_size  bo bi bh =
  let bo_bit = bo_bit bo in
  let crbit = Dsl.find bi in
  let cond_ok = Dsl.fresh "cond_ok" (Type.imm 1) in
  let zeros = Word.zero 2 in
  if  Word.is_one (bo_bit 4) then
    Dsl.[
      jmp (addr_of_exp addr_size ~exp_size:66 (var ctr ^ int zeros));
    ]
  else
    Dsl.[
      cond_ok := var crbit lxor (lnot (int @@ bo_bit 3));
      if_ (var cond_ok) [
        jmp (addr_of_exp addr_size ~exp_size:66 (var ctr ^ int zeros));
      ] [  ];
    ]

let bcctrl mem addr_size  bo bi bh =
  bcctr mem addr_size  bo bi bh @ update_lr_register mem addr_size

(** Branch Instructions, Branch Conditional to Target Register
    Page 39 of IBM Power ISATM Version 3.0 B
    examples:
    4e 9f 04 60    bctar
    4e 9f 04 61    bctarl
    bctar insn aren't implemented ny llvm right now *)
let bctar mem addr_size  bo bi bh =
  let bo_bit = bo_bit bo in
  let crbit = Dsl.find bi in
  let ctr_ok = Dsl.fresh "ctr_ok" (Type.imm 1) in
  let cond_ok = Dsl.fresh "cond_ok" (Type.imm 1) in
  let zeros = Word.zero 2 in
  with_decrement_counter bo @@
  if Word.is_one (bo_bit 2) && Word.is_one (bo_bit 4) then
    Dsl.[
      jmp (addr_of_exp addr_size ~exp_size:66 (var tar ^ int zeros));
    ]
  else
    Dsl.[
      ctr_ok := (lnot (is_zero addr_size (var ctr))) lxor (int @@ bo_bit 1);
      cond_ok := var crbit lxor (lnot (int @@ bo_bit 3));
      if_ (var ctr_ok land var cond_ok) [
        jmp (addr_of_exp addr_size ~exp_size:66 (var tar ^ int zeros));
      ] [  ];
    ]

let bctarl mem addr_size  bo bi bh =
  bctar mem addr_size  bo bi bh @ update_lr_register mem addr_size

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

let lift opcode addr_size  endian mem ops =
  let open Op in
  match opcode, ops with
  | `B,       [| Imm imm; |] -> b   mem addr_size imm
  | `BA,      [| Imm imm; |] -> ba  mem addr_size imm
  | `BL,      [| Imm imm; |] -> bl  mem addr_size imm
  | `BLA,     [| Imm imm; |] -> bla mem addr_size imm
  | `gBC,     [| Imm bo; Reg reg; Imm bd |] -> bc     mem addr_size bo reg bd
  | `gBCA,    [| Imm bo; Reg reg; Imm bd |] -> bca    mem addr_size bo reg bd
  | `gBCL,    [| Imm bo; Reg reg; Imm bd |] -> bcl    mem addr_size bo reg bd
  | `gBCLA,   [| Imm bo; Reg reg; Imm bd |] -> bcla   mem addr_size bo reg bd
  | `BDNZ,    [| Imm bd |]                  -> bdnz   mem addr_size bd
  | `gBCLR,   [| Imm bo; Reg reg; Imm bd |] -> bclr   mem addr_size bo reg bd
  | `gBCLRL,  [| Imm bo; Reg reg; Imm bd |] -> bclrl  mem addr_size bo reg bd
  | `gBCCTR,  [| Imm bo; Reg reg; Imm bd |] -> bcctr  mem addr_size bo reg bd
  | `gBCCTRL, [| Imm bo; Reg reg; Imm bd |] -> bcctrl mem addr_size bo reg bd
  | `BDNZLR,  [| |]                         -> bdnzlr mem addr_size
  | `gBCTAR,  [| Imm bo; Reg reg; Imm bd |] -> bctar  mem addr_size bo reg bd
  | `gBCTARL, [| Imm bo; Reg reg; Imm bd |] -> bctarl mem addr_size bo reg bd
  | opcode, _ ->
    Dsl.ppc_fail "%s: unexpected operand set" (string_of_opcode opcode)
