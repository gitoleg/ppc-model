open Core_kernel.Std
open Bap.Std

open Powerpc

let update_link_register cpu =
  RTL.[cpu.lr := cpu.addr + unsigned const byte 4]

(** Branch Instructions, Branch
    Page 37 of IBM Power ISATM Version 3.0 B
    examples:
    4b ff fe f0  b .+67108592
    4b ff fe f2  ba 67108592
    4b ff fe f1  bl .+67108592
    4b ff fe f3  bla 67108592 *)
let b cpu ops =
  let im = unsigned imm ops.(0) in
  let sh = unsigned const byte 2 in
  RTL.[cpu.jmp (cpu.addr + (im lsl sh))]

let ba cpu ops =
  let im = unsigned imm ops.(0) in
  let sh = unsigned const byte 2 in
  RTL.[ cpu.jmp (im lsl sh)]

let bl cpu ops =
  let im = unsigned imm ops.(0) in
  let sh = unsigned const byte 2 in
  let ad = unsigned const byte 4 in
  RTL.[
    cpu.jmp (cpu.addr + (im lsl sh));
    cpu.lr := cpu.addr + ad;
  ]

let bla cpu ops =
  let im = unsigned imm ops.(0) in
  let sh = unsigned const byte 2 in
  let ad = unsigned const byte 4 in
  RTL.[
    cpu.jmp (im lsl sh);
    cpu.lr := cpu.addr + ad;
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
  let sh = unsigned const byte 2 in
  let ctr_ok = unsigned var bit in
  let cond_ok = unsigned var bit in
  let x = unsigned var (bitwidth 5) in
  RTL.[
    x := last bo 5;
    when_ (nth bit x 2 = zero) [
      cpu.ctr := cpu.ctr - one;
    ];
    ctr_ok := nth bit x 2 lor ((cpu.ctr <> zero) lxor (nth bit x 3));
    cond_ok := nth bit x 0 lor (bi lxor (lnot (nth bit x 1)));
    when_ (ctr_ok land cond_ok) [
      cpu.jmp (cpu.addr + (bd lsl sh));
    ]
  ]

let bca cpu ops =
  let bo = unsigned imm ops.(0) in
  let bi = unsigned reg ops.(1) in
  let bd = unsigned imm ops.(2) in
  let sh = unsigned const byte 2 in
  let ctr_ok = unsigned var bit in
  let cond_ok = unsigned var bit in
  let x = unsigned var (bitwidth 5) in
  RTL.[
    x := last bo 5;
    when_ (nth bit x 2 = zero) [
      cpu.ctr := cpu.ctr - one;
    ];
    ctr_ok := nth bit x 2 lor ((cpu.ctr <> zero) lxor (nth bit x 3));
    cond_ok := nth bit x 0 lor (bi lxor (lnot (nth bit x 1)));
    when_ (ctr_ok land cond_ok) [
      cpu.jmp (bd lsl sh);
    ]
  ]

let bcl cpu ops = bc cpu ops @ update_link_register cpu
let bcla cpu ops = bca cpu ops @ update_link_register cpu

(** bdz  target = bc 18,0, target *)
let bdz cpu ops =
  let bd = unsigned imm ops.(0) in
  let sh = unsigned const byte 2 in
  RTL.[
    cpu.ctr := cpu.ctr - one;
    when_ (low cpu.addr_size cpu.ctr = zero) [
      cpu.jmp (cpu.addr + (bd lsl sh))
    ]
  ]

(** bdnz  target = bc 16,0, target *)
let bdnz cpu ops =
  let bd = unsigned imm ops.(0) in
  let sh = unsigned const byte 2 in
  RTL.[
    cpu.ctr := cpu.ctr - one;
    when_ (low cpu.addr_size cpu.ctr <> zero) [
      cpu.jmp (cpu.addr + (bd lsl sh))
    ]
  ]

(** Branch Instructions, Branch Conditional to Link Register
    Page 38 of IBM Power ISATM Version 3.0 B
    examples:
    4e 9f 00 20 	bclr	 20, 31
    4e 9f 00 21 	bclrl	 20, 31 *)
let bclr cpu ops =
  let bo = unsigned imm ops.(0) in
  let bi = unsigned reg ops.(1) in
  let sh = unsigned const byte 2 in
  let ctr_ok = unsigned var bit in
  let cond_ok = unsigned var bit in
  let x = unsigned var (bitwidth 5) in
  RTL.[
    x := last bo 5;
    when_ (nth bit x 2 = zero) [
      cpu.ctr := cpu.ctr - one;
    ];
    ctr_ok := nth bit x 2 lor ((cpu.ctr <> zero) lxor (nth bit x 3));
    cond_ok := nth bit x 0 lor (bi lxor (lnot (nth bit x 1)));
    when_ (ctr_ok land cond_ok) [
      cpu.jmp (cpu.lr lsl sh);
    ];
  ]

let bclrl cpu ops = bclr cpu ops @ update_link_register cpu

(** Branch Instructions extended mnemonic, branch to LR unconditionally.
    Page 792 of IBM Power ISATM Version 3.0 B
    examples:
    4e 80 00 20   blr
    4e 80 00 21   blrl *)
let blr cpu ops =
  let sh = unsigned const byte 2 in
  RTL.[
    cpu.jmp (cpu.lr lsl sh)
  ]

let blrl cpu ops =
  let sh = unsigned const byte 2 in
  RTL.[
    cpu.jmp (cpu.lr lsl sh);
    cpu.lr := cpu.addr + unsigned const byte 4
  ]

(** bdnzlr = bclr 16,0,0  *)
let bdnzlr cpu ops =
  let sh = unsigned const byte 2 in
  RTL.[
    cpu.ctr := cpu.ctr - one;
    when_ (cpu.ctr <> zero) [
      cpu.jmp (cpu.lr lsl sh);
    ];
  ]

(** Branch Instructions, Branch Conditional to Count Register
    Page 38 of IBM Power ISATM Version 3.0 B
    examples:
    4d 5f 04 20    bcctr 10,31
    4d 5f 04 21    bcctrl 10,31 *)
let bcctr cpu ops =
  let bo = unsigned imm ops.(0) in
  let bi = unsigned reg ops.(1) in
  let sh = unsigned const byte 2 in
  let cond_ok = unsigned var bit in
  let x = unsigned var (bitwidth 5) in
  RTL.[
    x := last bo 5;
    cond_ok := (nth bit x 0 = one) lor ( bi lxor (lnot (nth bit x 1)));
    when_ (cond_ok) [
      cpu.jmp (cpu.ctr lsl sh);
    ];
  ]

let bcctrl cpu ops =
  bcctr cpu ops @ update_link_register cpu

(** Branch Instructions extended mnemonic, branch to CTR unconditionally.
    Page 792 of IBM Power ISATM Version 3.0 B
    examples:
    4e 80 04 20   bctr
    4e 80 04 21   bctrl *)
let bctr cpu ops =
  let sh = unsigned const byte 2 in
  RTL.[
    cpu.jmp (cpu.cr lsl sh)
  ]

let bctrl cpu ops =
  let sh = unsigned const byte 2 in
  RTL.[
    cpu.jmp (cpu.cr lsl sh);
    cpu.lr := cpu.addr + unsigned const byte 4
  ]

(** Branch Instructions, Branch Conditional to Target Register
    Page 39 of IBM Power ISATM Version 3.0 B
    examples:
    4e 9f 04 60    bctar
    4e 9f 04 61    bctarl *)
let bctar cpu ops =
  let bo = unsigned imm ops.(0) in
  let bi = unsigned reg ops.(1) in
  let sh = unsigned const byte 2 in
  let cond_ok = unsigned var bit in
  let ctr_ok = unsigned var bit in
  let x = unsigned var (bitwidth 5) in
  RTL.[
    x := last bo 5;
    when_ (nth bit x 2 = zero) [
      cpu.ctr := cpu.ctr - one;
    ];
    ctr_ok := nth bit x 2 lor ((cpu.ctr <> zero) lxor (nth bit x 3));
    cond_ok := nth bit x 0 lor (bi lxor (lnot (nth bit x 1)));
    when_ (ctr_ok land cond_ok) [
      cpu.jmp (cpu.tar lsl sh);
    ]
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
  | `BDZ
  | `BDNZ
  | `BCC
  | `BCCL
  | `BCCLA
] [@@deriving sexp, enumerate]

type bc_lr = [
  | `gBCLR
  | `gBCLRL
  | `BDNZLR
  | `BLR
  | `BLRL
  | `BCCLR
  | `BCCLRL
] [@@deriving sexp, enumerate]

type bc_ctr = [
  | `gBCCTR
  | `gBCCTRL
  | `BCTR
  | `BCTRL
  | `BCCCTR
  | `BCCCTRL
] [@@deriving sexp, enumerate]

type bc_tar = [
  | `gBCTAR
  | `gBCTARL
] [@@deriving sexp, enumerate]

type t = [ b | bc | bc_lr | bc_ctr | bc_tar ] [@@deriving sexp, enumerate]

let lift opcode cpu ops = match opcode with
  | `B       -> b cpu ops
  | `BA      -> ba cpu ops
  | `BL      -> bl cpu ops
  | `BLA     -> bla cpu ops
  | `gBC     -> bc cpu ops
  | `gBCA    -> bca cpu ops
  | `gBCL    -> bcl cpu ops
  | `gBCLA   -> bcla cpu ops
  | `BDZ     -> bdz cpu ops
  | `BDNZ    -> bdnz cpu ops
  | `BCC     -> bc cpu ops
  | `BCCL    -> bcl cpu ops
  | `BCCLA   -> bcla cpu ops
  | `gBCLR   -> bclr cpu ops
  | `gBCLRL  -> bclrl cpu ops
  | `gBCCTR  -> bcctr cpu ops
  | `gBCCTRL -> bcctrl cpu ops
  | `BDNZLR  -> bdnzlr cpu ops
  | `gBCTAR  -> bctar cpu ops
  | `gBCTARL -> bctarl cpu ops
  | `BLR     -> blr cpu ops
  | `BLRL    -> blrl cpu ops
  | `BCTR    -> bctr cpu ops
  | `BCTRL   -> bctrl cpu ops
  | `BCCLR   -> bclr cpu ops
  | `BCCLRL  -> bclrl cpu ops
  | `BCCCTR  -> bcctr cpu ops
  | `BCCCTRL -> bcctrl cpu ops
