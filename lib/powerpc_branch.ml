open Core_kernel.Std
open Bap.Std

open Powerpc_types
open Hardware
open Dsl

let update_link_register cpu =
  RTL.[lr := cpu.addr + unsigned int 4]

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
  let x = unsigned var in

  let y = unsigned var in
  let z = unsigned var in
  let w = unsigned var in
  RTL.[
    x := last bo 5;
    if_ (nbit x 2 = zero) [
      ctr := ctr - one;
    ] [];
    y := nbit x 2;
    z := ctr = zero;
    w := bi lxor (lnot (nbit x 1));
    ctr_ok := nbit x 2 lor ((ctr <> zero) lxor (nbit x 3));
    cond_ok := nbit x 0 lor (bi lxor (lnot (nbit x 1)));
    if_ (ctr_ok land cond_ok) [
      cpu.jmp (cpu.addr + (bd lsl sh));
    ] [ ]
  ]

let bca cpu ops =
  let bo = unsigned imm ops.(0) in
  let bi = unsigned reg ops.(1) in
  let bd = unsigned imm ops.(2) in
  let sh = unsigned int 2 in
  let ctr_ok = unsigned var in
  let cond_ok = unsigned var in
  let x = unsigned var in
  RTL.[
    x := last bo 5;
    if_ (nbit x 2 = zero) [
      ctr := ctr - one;
    ] [];
    ctr_ok := nbit x 2 lor ((ctr <> zero) lxor (nbit x 3));
    cond_ok := nbit x 0 lor (bi lxor (lnot (nbit x 1)));
    if_ (ctr_ok land cond_ok) [
      cpu.jmp (bd lsl sh);
    ] [ ]
  ]

let bcl cpu ops = bc cpu ops @ update_link_register cpu
let bcla cpu ops = bca cpu ops @ update_link_register cpu

(** bdnz  target = bc 16,0, target *)
let bdnz cpu ops =
  let bd = unsigned imm ops.(0) in
  let sh = unsigned int 2 in
  RTL.[
    ctr := ctr - one;
    if_ (low cpu.addr_size ctr <> zero) [
      cpu.jmp (cpu.addr + (bd lsl sh))
    ] [  ]
  ]

(** Branch Instructions, Branch Conditional to Link Register
    Page 38 of IBM Power ISATM Version 3.0 B
    examples:
    4e 9f 00 20 	bclr	 20, 31
    4e 9f 00 21 	bclrl	 20, 31 *)
let bclr cpu ops =
  let bo = unsigned imm ops.(0) in
  let bi = unsigned reg ops.(1) in
  let sh = unsigned int 2 in
  let ctr_ok = unsigned var in
  let cond_ok = unsigned var in
  let x = unsigned var in
  RTL.[
    x := last bo 5;
    if_ (nbit x 2 = zero) [
      ctr := ctr - one;
    ] [];
    ctr_ok := nbit x 2 lor ((ctr <> zero) lxor (nbit x 3));
    cond_ok := nbit x 0 lor (bi lxor (lnot (nbit x 1)));
    if_ (ctr_ok land cond_ok) [
      cpu.jmp (lr lsl sh);
    ] [ ]
  ]

let bclrl cpu ops = bclr cpu ops @ update_link_register cpu

(** bdnzlr = bclr 16,0,0  *)
let bdnzlr cpu ops =
  let sh = unsigned int 2 in
  RTL.[
    ctr := ctr - one;
    if_ (ctr <> zero) [
      cpu.jmp (lr lsl sh);
    ] [  ];
  ]

(** Branch Instructions, Branch Conditional to Count Register
    Page 38 of IBM Power ISATM Version 3.0 B
    examples:
    4d 5f 04 20    bcctr 10,31
    4d 5f 04 21    bcctrl 10,31 *)
let bcctr cpu ops =
  let bo = unsigned imm ops.(0) in
  let bi = unsigned reg ops.(1) in
  let sh = unsigned int 2 in
  let cond_ok = unsigned var in
  let x = unsigned var in
  RTL.[
    x := last bo 5;
    cond_ok := (nbit x 0 = one) lor ( bi lxor (lnot (nbit x 1)));
    if_ (cond_ok) [
      cpu.jmp (ctr lsl sh);
    ] []
  ]

let bcctrl cpu ops =
  bcctr cpu ops @ update_link_register cpu

(** Branch Instructions, Branch Conditional to Target Register
    Page 39 of IBM Power ISATM Version 3.0 B
    examples:
    4e 9f 04 60    bctar
    4e 9f 04 61    bctarl
    bctar insn isn't implemented ny llvm right now *)
let bctar cpu ops =
  let bo = unsigned imm ops.(0) in
  let bi = unsigned reg ops.(1) in
  let sh = unsigned int 2 in
  let cond_ok = unsigned var in
  let ctr_ok = unsigned var in
  let x = unsigned var in
  RTL.[
    x := last bo 5;
    if_ (nbit x 2 = zero) [
      ctr := ctr - one;
    ] [];
    ctr_ok := nbit x 2 lor ((ctr <> zero) lxor (nbit x 3));
    cond_ok := nbit x 0 lor (bi lxor (lnot (nbit x 1)));
    if_ (ctr_ok land cond_ok) [
      cpu.jmp (tar lsl sh);
    ] []
  ]

let bctarl cpu ops =
  bctar cpu ops @ update_link_register cpu

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
  match opcode with
  | `B       -> b cpu ops
  | `BA      -> ba cpu ops
  | `BL      -> bl cpu ops
  | `BLA     -> bla cpu ops
  | `gBC     -> bc cpu ops
  | `gBCA    -> bca cpu ops
  | `gBCL    -> bcl cpu ops
  | `gBCLA   -> bcla cpu ops
  | `BDNZ    -> bdnz cpu ops
  | `gBCLR   -> bclr cpu ops
  | `gBCLRL  -> bclrl cpu ops
  | `gBCCTR  -> bcctr cpu ops
  | `gBCCTRL -> bcctrl cpu ops
  | `BDNZLR  -> bdnzlr cpu ops
  | `gBCTAR  -> bctar cpu ops
  | `gBCTARL -> bctarl cpu ops
