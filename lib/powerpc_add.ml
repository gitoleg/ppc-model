(** There is some sort of confusion with add instructions. Some of them
    are not recognizable by llvm, other has name that similar to other
    instructions in ISA specification, so one can found useful next table
    of examples (oe and rc are 21 and 31 bits):

    --------------------------------------------------------------------------------
    |  opcode     |      asm          |  bits   |recognized by llvm| name in llvm  |
    |-------------|-------------------|---------|------------------|---------------|
    | 7d 62 5a 14 |add    r11, r2, r11|rc=0,oe=0|  add  11,2,11    | ADD4          |
    | 7d 62 5a 15 |add.   r11, r2, r11|rc=1,oe=0|  add. 11,2,11    | ADD4o         |
    | 7d 62 5e 14 |addo   r11, r2, r11|rc=0,oe=1|  not recognized    |  ---          |
    | 7d 62 5e 15 |addo.  r11, r2, r11|rc=1,oe=1|  not recognized    |  ---          |
    | 7d 62 58 14 |addc   r11, r2, r11|rc=0,oe=0| addc  11,2,11    | ADDC          |
    | 7d 62 58 15 |addc.  r11, r2, r11|rc=1,oe=0| addc.  11,2,11   | ADDCo         |
    | 7d 62 5c 14 |addco  r11, r2, r11|rc=0,oe=1|  not recognized    |  ---          |
    | 7d 62 5c 15 |addco. r11, r2, r11|rc=1,oe=1|  not recognized    |  ---          |
    --------------------------------------------------------------------------------

    And so on. Basicly, instructions
    addo addco addeo addmeo addzeo addo. addco. addeo. addmeo. addzeo.
    doesn't recognized by llvm.

    And theirs oe-less analogs - addc. adde. addme. addze. -
    contains "o" suffix in thers llvm names: ADDo ADDeo ADDMEo ADDZEo,
    where "o" stands for .(dot) . *)

open Core_kernel.Std
open Bap.Std

open Powerpc_types
open Hardware
open Dsl

(** Extended mnemonics:

    lis rx, value      = addis  rx, 0,  value
    li  rx, value      = addi   rx, 0,  value
    la  rx, disp(ry)   = addi   rx, ry, disp
    subis rx,ry,value  = addis  rx, ry, -value
    subic rx,ry,value  = addic  rx, ry, -value
    subic. rx,ry,value = addic. rx, ry, -value *)

let compute_ca mode x result =
  match mode with
  | `r64 -> RTL.(result < x)
  | `r32 -> RTL.(low result word < low x word)

let compute_ca32 x result = RTL.(low result word < low x word)

(** Fixed-Point Arithmetic Instructions - Add Immediate
    Page 67 of IBM Power ISATM Version 3.0 B
    examples:
    38 21 00 10     addi    r1,r1,16
    3b de fd 28     addi    r30,r30,-728
    38 20 00 10     addi    r1,0,16 (OR li r1, 16) *)
let addi ops =
  let rt = signed reg ops.(0) in
  let ra = signed reg ops.(1) in
  let im = signed imm ops.(2) in
  RTL.[ rt := ra + im; ]

let li ops =
  let rt = signed reg ops.(0) in
  let im = signed imm ops.(1) in
  RTL.[ rt := im; ]

(** Fixed-Point Arithmetic Instructions - Add Immediate Shifted
    Page 67 of IBM Power ISATM Version 3.0 B
    examples:
    3f de 00 02     addis   r30,r30,2
    3d 6b f0 00     addis   r11,r11,-4096 *)
let addis ops =
  let rt = signed reg ops.(0) in
  let ra = signed reg ops.(1) in
  let im = signed imm ops.(2) in
  let sh = unsigned int 16 in
  RTL.[ rt := ra + (im lsl sh); ]

let lis ops =
  let rt = signed reg ops.(0) in
  let im = signed imm ops.(1) in
  let sh = unsigned int 16 in
  RTL.[ rt := im lsl sh; ]

(** Fixed-Point Arithmetic Instructions - Add PC Immediate Shifted
    Page 68 of IBM Power ISATM Version 3.0 B
    example:
    4c 22 00 04     addpcis r1,4
    llvm doesn't recognize this instruction  *)
let addpcis rt imm = ppc_fail "unimplemented"

(** Fixed-Point Arithmetic Instructions - Add
    Page 69 of IBM Power ISATM Version 3.0 B

    7d 62 5a 14 add   r11, r2, r11
    7d 62 5a 15 add.  r11, r2, r11 *)
let add ops =
  let rt = signed reg ops.(0) in
  let ra = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[rt := ra + rb]

let write_fixpoint_result addr_size res =
  let res = signed reg res in
  RTL.[
    bit cr 0 := res < zero;
    bit cr 1 := res > zero;
    bit cr 2 := res = zero;
  ]

let add_dot addr_size ops =
  add ops @ write_fixpoint_result addr_size ops.(0)

(** Fixed-Point Arithmetic Instructions - Add Immediate Carrying
    Page 69 of IBM Power ISATM Version 3.0 B
    examples:
    30 21 00 10    addic r1, r1, 16
    33 de fd 28    addic r30, r30, -728  *)
let addic addr_size ops =
  let rt = signed reg ops.(0) in
  let ra = signed reg ops.(1) in
  let im = signed imm ops.(2) in
  let tm = signed var in
  RTL.[
    tm := ra;
    rt := ra + im;
    ca := compute_ca addr_size tm rt;
    ca32 := compute_ca32 tm rt;
  ]

(** Fixed-Point Arithmetic Instructions - Add Immediate Carrying and Record
    Page 69 of IBM Power ISATM Version 3.0 B
    examples:
    34 21 00 10    addic. r1, r1, 16
    37 de fd 28    addic. r30, r30, -728  *)
let addic_dot addr_size ops =
  addic addr_size ops @ write_fixpoint_result addr_size ops.(0)

(** Fixed-Point Arithmetic Instructions - Add Carrying
    Page 70 of IBM Power ISATM Version 3.0 B
    examples:
    7d 62 58 14  addc   r11, r2, r11
    7d 62 58 15  addc.  r11, r2, r11 *)
let addc addr_size ops =
  let rt = signed reg ops.(0) in
  let ra = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  let tm = signed var in
  RTL.[
    tm := ra;
    rt := ra + rb;
    ca := compute_ca addr_size tm rt;
    ca32 := compute_ca32 tm rt;
  ]

let addc_dot addr_size ops =
  addc addr_size ops @ write_fixpoint_result addr_size ops.(0)

(** Fixed-Point Arithmetic Instructions - Add Extend
    Page 71 of IBM Power ISATM Version 3.0 B
    examples:
    7c 21 81 14  adde   r11, r2, r11
    7c 21 81 15  adde.  r11, r2, r11 *)
let adde addr_size ops =
  let rt = signed reg ops.(0) in
  let ra = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  let tm = signed var in
  RTL.[
    tm := ra;
    rt := ra + rb + ca;
    ca := compute_ca addr_size tm rt;
    ca32 := compute_ca32 tm rt; ]

let adde_dot addr_size ops =
  adde addr_size ops @ write_fixpoint_result addr_size ops.(0)

(** Fixed-Point Arithmetic Instructions - Add to Minus One Extend
    Page 71 of IBM Power ISATM Version 3.0 B
    examples:
    7c 22 01 d4  addme  r1,r2
    7c 22 01 d5  addme. r1,r2  *)
let addme addr_size ops =
  let rt = signed reg ops.(0) in
  let ra = signed reg ops.(1) in
  let tm = signed var in
  RTL.[
    tm := ra;
    rt := ra + ca - one;
    ca := compute_ca addr_size tm rt;
    ca32 := compute_ca32 tm rt;
  ]

let addme_dot addr_size ops =
  addme addr_size ops @ write_fixpoint_result addr_size ops.(0)

(** Fixed-Point Arithmetic Instructions - Add Extended using alternate carry bit
    Page 72 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 19 54   addex r1,r2,r3
    llvm doesn't recognize this instruction *)
let addex rt ra rb = ppc_fail "unimplemented"

(** Fixed-Point Arithmetic Instructions - Add to Zero extended
    Page 72 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 01 94   addze r1,r2
    7c 22 01 95   addze. r1,r2 *)
let addze addr_size ops =
  let rt = signed reg ops.(0) in
  let ra = signed reg ops.(1) in
  let tm = signed var in
  RTL.[
    tm := ra;
    rt := ra + ca;
    ca := compute_ca addr_size tm rt;
    ca32 := compute_ca32 tm rt;
  ]

let addze_dot addr_size ops =
  addze addr_size ops @ write_fixpoint_result addr_size ops.(0)

type t = [
  | `ADD4
  | `ADD4o
  | `ADDI
  | `ADDIS
  | `ADDPCIS
  | `ADDIC
  | `ADDICo
  | `ADDC
  | `ADDCo
  | `ADDE
  | `ADDEo
  | `ADDME
  | `ADDMEo
  | `ADDZE
  | `ADDZEo
  | `LI
  | `LIS
  | `LA
] [@@deriving sexp, enumerate]

let lift opcode cpu ops =
  match opcode with
  | `ADD4   -> add ops
  | `ADD4o  -> add_dot cpu.addr_size ops
  | `ADDI   -> addi ops
  | `ADDIS  -> addis ops
  | `ADDIC  -> addic cpu.addr_size ops
  | `ADDICo -> addic_dot cpu.addr_size ops
  | `ADDC   -> addc cpu.addr_size ops
  | `ADDCo  -> addc_dot cpu.addr_size ops
  | `ADDE   -> adde cpu.addr_size ops
  | `ADDEo  -> adde_dot cpu.addr_size ops
  | `ADDME  -> addme cpu.addr_size ops
  | `ADDMEo -> addme_dot cpu.addr_size ops
  | `ADDZE  -> addze cpu.addr_size ops
  | `ADDZEo -> addze_dot cpu.addr_size ops
  | `LI     -> li ops
  | `LIS    -> lis ops
  | `LA     -> addi ops
  | opcode ->
    let opcode = Sexp.to_string (sexp_of_t opcode) in
    ppc_fail "%s: unexpected operand set" opcode
