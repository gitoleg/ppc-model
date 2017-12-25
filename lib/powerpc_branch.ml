open Powerpc

let update_link_register cpu ops =
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
  let tm = signed var word in
  RTL.[
    tm := last (im lsl sh) 26;
    cpu.jmp (cpu.addr + tm)
  ]

let ba cpu ops =
  let im = unsigned imm ops.(0) in
  let sh = unsigned const byte 2 in
  let tm = signed var cpu.addr_size in
  RTL.[
    tm := last (im lsl sh) 26;
    cpu.jmp tm;
  ]

let bl cpu ops =
  let im = unsigned imm ops.(0) in
  let sh = unsigned const byte 2 in
  let ad = unsigned const byte 4 in
  let tm = signed var word in
  RTL.[
    tm := last (im lsl sh) 26;
    cpu.jmp (cpu.addr + tm);
    cpu.lr := cpu.addr + ad;
  ]

let bla cpu ops =
  let im = unsigned imm ops.(0) in
  let sh = unsigned const byte 2 in
  let ad = unsigned const byte 4 in
  let tm = signed var cpu.addr_size in
  RTL.[
    tm := last (im lsl sh) 26;
    cpu.jmp tm;
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
  let bi = unsigned cpu.reg ops.(1) in
  let bd = unsigned imm ops.(2) in
  let sh = unsigned const byte 2 in
  let ctr_ok = unsigned var bit in
  let cond_ok = unsigned var bit in
  let x = unsigned var (bitwidth 5) in
  let tm = signed var halfword in
  RTL.[
    x := last bo 5;
    when_ (nth bit x 2 = zero) [
      cpu.ctr := cpu.ctr - one;
    ];
    ctr_ok := nth bit x 2 lor ((cpu.ctr <> zero) lxor (nth bit x 3));
    cond_ok := nth bit x 0 lor (bi lxor (lnot (nth bit x 1)));
    when_ (ctr_ok land cond_ok) [
      tm := bd lsl sh;
      cpu.jmp (cpu.addr + tm);
    ]
  ]

let bca cpu ops =
  let bo = unsigned imm ops.(0) in
  let bi = unsigned cpu.reg ops.(1) in
  let bd = unsigned imm ops.(2) in
  let sh = unsigned const byte 2 in
  let ctr_ok = unsigned var bit in
  let cond_ok = unsigned var bit in
  let x = unsigned var (bitwidth 5) in
  let tm = signed var cpu.addr_size in
  RTL.[
    x := last bo 5;
    when_ (nth bit x 2 = zero) [
      cpu.ctr := cpu.ctr - one;
    ];
    ctr_ok := nth bit x 2 lor ((cpu.ctr <> zero) lxor (nth bit x 3));
    cond_ok := nth bit x 0 lor (bi lxor (lnot (nth bit x 1)));
    when_ (ctr_ok land cond_ok) [
      tm := bd lsl sh;
      cpu.jmp tm;
    ]
  ]

let bcl = concat bc update_link_register
let bcla = concat bca update_link_register

(** bdz  target = bc 18,0, target *)
let bdz cpu ops =
  let bd = unsigned imm ops.(0) in
  let sh = unsigned const byte 2 in
  let tm = signed var cpu.addr_size in
  RTL.[
    cpu.ctr := cpu.ctr - one;
    when_ (low cpu.addr_size cpu.ctr = zero) [
      tm := bd lsl sh;
      cpu.jmp (cpu.addr + tm)
    ]
  ]

(** bdnz  target = bc 16,0, target *)
let bdnz cpu ops =
  let bd = unsigned imm ops.(0) in
  let sh = unsigned const byte 2 in
  let tm = signed var cpu.addr_size in
  RTL.[
    cpu.ctr := cpu.ctr - one;
    when_ (low cpu.addr_size cpu.ctr <> zero) [
      tm := bd lsl sh;
      cpu.jmp (cpu.addr + tm)
    ]
  ]

(** Branch Instructions, Branch Conditional to Link Cpu.Register
    Page 38 of IBM Power ISATM Version 3.0 B
    examples:
    4e 9f 00 20 	bclr	 20, 31
    4e 9f 00 21 	bclrl	 20, 31 *)
let bclr cpu ops =
  let bo = unsigned imm ops.(0) in
  let bi = unsigned cpu.reg ops.(1) in
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

let bclrl = concat bclr update_link_register

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

(** Branch Instructions, Branch Conditional to Count Cpu.Register
    Page 38 of IBM Power ISATM Version 3.0 B
    examples:
    4d 5f 04 20    bcctr 10,31
    4d 5f 04 21    bcctrl 10,31 *)
let bcctr cpu ops =
  let bo = unsigned imm ops.(0) in
  let bi = unsigned cpu.reg ops.(1) in
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

let bcctrl = concat bcctr update_link_register

(** Branch Instructions extended mnemonic, branch to CTR unconditionally.
    Page 792 of IBM Power ISATM Version 3.0 B
    examples:
    4e 80 04 20   bctr
    4e 80 04 21   bctrl *)
let bctr cpu ops =
  let sh = unsigned const byte 2 in
  RTL.[
    cpu.jmp (cpu.ctr lsl sh)
  ]

let bctrl = concat bctr update_link_register

(** Branch Instructions, Branch Conditional to Target Cpu.Register
    Page 39 of IBM Power ISATM Version 3.0 B
    examples:
    4e 9f 04 60    bctar
    4e 9f 04 61    bctarl *)
let bctar cpu ops =
  let bo = unsigned imm ops.(0) in
  let bi = unsigned cpu.reg ops.(1) in
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

let bctarl = concat bctar update_link_register

let () =
  "B"       >> b;
  "BA"      >> ba;
  "BL"      >> bl;
  "BLA"     >> bla;
  "gBC"     >> bc;
  "gBCA"    >> bca;
  "gBCL"    >> bcl;
  "gBCLA"   >> bcla;
  "BDZ"     >> bdz;
  "BDNZ"    >> bdnz;
  "BCC"     >> bc;
  "BCCL"    >> bcl;
  "BCCLA"   >> bcla;
  "gBCLR"   >> bclr;
  "gBCLRL"  >> bclrl;
  "gBCCTR"  >> bcctr;
  "gBCCTRL" >> bcctrl;
  "BDNZLR"  >> bdnzlr;
  "gBCTAR"  >> bctar;
  "gBCTARL" >> bctarl;
  "BLR"     >> blr;
  "BLRL"    >> blrl;
  "BCTR"    >> bctr;
  "BCTRL"   >> bctrl;
  "BCCLR"   >> bclr;
  "BCCLRL"  >> bclrl;
  "BCCCTR"  >> bcctr;
  "BCCCTRL" >> bcctrl;
