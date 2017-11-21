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

open Ppc_types
open Hardware

(** Extended mnemonics:

    lis rx, value      = addis  rx, 0,  value
    li  rx, value      = addi   rx, 0,  value
    la  rx, disp(ry)   = addi   rx, ry, disp
    subis rx,ry,value  = addis  rx, ry, -value
    subic rx,ry,value  = addic  rx, ry, -value
    subic. rx,ry,value = addic. rx, ry, -value *)
let compute_ov32 x y result =
  let open Dsl in
  let r = extract 31 0 result in
  let x_msb = cast high 1 x in
  let y_msb = cast high 1 y in
  let r_msb = cast high 1 r in
  (x_msb = y_msb) land (x_msb lxor r_msb)

let compute_ov mode x y result =
  let open Dsl in
  let high_bit e = match mode with
    | `r32 -> cast high 1 (extract 31 0 e)
    | `r64 -> cast high 1 e in
  let x_msb = high_bit x in
  let y_msb = high_bit y in
  let r_msb = high_bit result in
  (x_msb = y_msb) land (x_msb lxor r_msb)

let compute_ca mode x result =
  match mode with
  | `r64 -> Dsl.(result < x)
  | `r32 -> Dsl.(extract 31 0 result < extract 31 0 x)

let compute_ca32 x result = Dsl.(extract 31 0 result < extract 31 0 x)

(** Fixed-Point Arithmetic Instructions - Add Immediate
    Page 67 of IBM Power ISATM Version 3.0 B
    examples:
    38 21 00 10     addi    r1,r1,16
    3b de fd 28     addi    r30,r30,-728
    38 20 00 10     addi    r1,0,16 (OR li r1, 16) *)
let addi rt ra imm =
  let rt = Dsl.find_gpr rt in
  let imm = Word.of_int64 ~width:gpr_bitwidth (Imm.to_int64 imm) in
  match Dsl.find_gpr_opt ra with
  | None -> Dsl.[ rt := cast signed gpr_bitwidth (int imm); ]
  | Some ra -> Dsl.[ rt := var ra + cast signed gpr_bitwidth (int imm); ]

let li rt imm =
  let rt = Dsl.find_gpr rt in
  let imm = Word.of_int64 ~width:gpr_bitwidth (Imm.to_int64 imm) in
  Dsl.[ rt := cast signed gpr_bitwidth (int imm); ]

(** Fixed-Point Arithmetic Instructions - Add Immediate Shifted
    Page 67 of IBM Power ISATM Version 3.0 B
    examples:
    3f de 00 02     addis   r30,r30,2
    3d 6b f0 00     addis   r11,r11,-4096 *)
let addis rt ra imm =
  let rt = Dsl.find_gpr rt in
  let zero16 = Word.zero 16 in
  let imm =
    Word.of_int64 ~width:16 (Imm.to_int64 imm) in
  match Dsl.find_gpr_opt ra with
  | None ->
    Dsl.[ rt := cast signed gpr_bitwidth (int imm ^ int zero16); ]
  | Some ra ->
    Dsl.[ rt := var ra + cast signed gpr_bitwidth (int imm ^ int zero16); ]

let lis rt imm =
  let rt = Dsl.find_gpr rt in
  let zero16 = Word.zero 16 in
  let imm = Word.of_int64 ~width:16 (Imm.to_int64 imm) in
  Dsl.[ rt := cast signed gpr_bitwidth (int imm ^ int zero16); ]

(** Fixed-Point Arithmetic Instructions - Add PC Immediate Shifted
    Page 68 of IBM Power ISATM Version 3.0 B
    example:
    4c 22 00 04     addpcis r1,4
    llvm doesn't recognize this instruction  *)
let addpcis rt imm = Dsl.ppc_fail "unimplemented"

(** Fixed-Point Arithmetic Instructions - Add
    Page 69 of IBM Power ISATM Version 3.0 B

    7d 62 5a 14 add   r11, r2, r11
    7d 62 5a 15 add.  r11, r2, r11 *)
let add rt ra rb =
  let rt = Dsl.find_gpr rt in
  let ra = Dsl.find_gpr ra in
  let rb = Dsl.find_gpr rb in
  Dsl.[rt := var ra + var rb]

let add_dot addr_size rt ra rb =
  add rt ra rb @ Dsl.write_fixpoint_result addr_size (Dsl.find_gpr rt)

(** Fixed-Point Arithmetic Instructions - Add Immediate Carrying
    Page 69 of IBM Power ISATM Version 3.0 B
    examples:
    30 21 00 10    addic r1, r1, 16
    33 de fd 28    addic r30, r30, -728  *)
let addic addr_size rt ra imm =
  let rt = Dsl.find_gpr rt in
  let ra = Dsl.find_gpr ra in
  let imm =
    Word.of_int64 ~width:gpr_bitwidth (Imm.to_int64 imm) in
  let tmp = Dsl.fresh "tmp" (Type.imm gpr_bitwidth) in
  Dsl.[ tmp := var ra;
    rt := var ra + int imm;
    ca := compute_ca addr_size (var tmp) (var rt);
    ca32 := compute_ca32 (var tmp) (var rt); ]

(** Fixed-Point Arithmetic Instructions - Add Immediate Carrying and Record
    Page 69 of IBM Power ISATM Version 3.0 B
    examples:
    34 21 00 10    addic. r1, r1, 16
    37 de fd 28    addic. r30, r30, -728  *)
let addic_dot addr_size rt ra imm =
  addic addr_size rt ra imm @ Dsl.write_fixpoint_result addr_size (Dsl.find_gpr rt)

(** Fixed-Point Arithmetic Instructions - Add Carrying
    Page 70 of IBM Power ISATM Version 3.0 B
    examples:
    7d 62 58 14  addc   r11, r2, r11
    7d 62 58 15  addc.  r11, r2, r11 *)
let addc addr_size rt ra rb =
  let rt = Dsl.find_gpr rt in
  let ra = Dsl.find_gpr ra in
  let rb = Dsl.find_gpr rb in
  let tmp = Dsl.fresh "tmp" (Type.imm gpr_bitwidth) in
  Dsl.[ tmp := var ra;
    rt := var ra + var rb;
    ca := compute_ca addr_size (var tmp) (var rt);
    ca32 := compute_ca32 (var tmp) (var rt); ]

let addc_dot addr_size rt ra rb =
  addc addr_size rt ra rb @ Dsl.write_fixpoint_result addr_size (Dsl.find_gpr rt)

(** Fixed-Point Arithmetic Instructions - Add Extend
    Page 71 of IBM Power ISATM Version 3.0 B
    examples:
    7c 21 81 14  adde   r11, r2, r11
    7c 21 81 15  adde.  r11, r2, r11 *)
let adde addr_size rt ra rb =
  let rt = Dsl.find_gpr rt in
  let ra = Dsl.find_gpr ra in
  let rb = Dsl.find_gpr rb in
  let tmp = Dsl.fresh "tmp" (Type.imm gpr_bitwidth) in
  Dsl.[ tmp := var ra;
    rt := var ra + var rb + cast unsigned gpr_bitwidth (var ca);
    ca := compute_ca addr_size (var tmp) (var rt);
    ca32 := compute_ca32 (var tmp) (var rt); ]

let adde_dot addr_size rt ra rb =
  adde addr_size rt ra rb @ Dsl.write_fixpoint_result addr_size (Dsl.find_gpr rt)

(** Fixed-Point Arithmetic Instructions - Add to Minus One Extend
    Page 71 of IBM Power ISATM Version 3.0 B
    examples:
    7c 22 01 d4  addme  r1,r2
    7c 22 01 d5  addme. r1,r2  *)
let addme addr_size rt ra =
  let rt = Dsl.find_gpr rt in
  let ra = Dsl.find_gpr ra in
  let one = Word.one gpr_bitwidth in
  let tmp = Dsl.fresh "tmp" (Type.imm gpr_bitwidth) in
  Dsl.[ tmp := var ra;
    rt := var ra + cast unsigned gpr_bitwidth (var ca) - int one;
    ca := compute_ca addr_size (var tmp) (var rt);
    ca32 := compute_ca32 (var tmp) (var rt);  ]

let addme_dot addr_size rt ra =
  addme addr_size rt ra @ Dsl.write_fixpoint_result addr_size (Dsl.find_gpr rt)

(** Fixed-Point Arithmetic Instructions - Add Extended using alternate carry bit
    Page 72 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 19 54   addex r1,r2,r3
    llvm doesn't recognize this instruction *)
let addex rt ra rb = Dsl.ppc_fail "unimplemented"

(** Fixed-Point Arithmetic Instructions - Add to Zero extended
    Page 72 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 01 94   addze r1,r2
    7c 22 01 95   addze. r1,r2 *)
let addze addr_size rt ra =
  let rt = Dsl.find_gpr rt in
  let ra = Dsl.find_gpr ra in
  let tmp = Dsl.fresh "tmp" (Type.imm gpr_bitwidth) in
  Dsl.[ tmp := var ra;
    rt := var ra + cast unsigned gpr_bitwidth (var ca);
    ca := compute_ca addr_size (var tmp) (var rt);
    ca32 := compute_ca32 (var tmp) (var rt); ]

let addze_dot addr_size rt ra =
  addze addr_size rt ra @ Dsl.write_fixpoint_result addr_size (Dsl.find_gpr rt)

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

let lift opcode addr_size endian mem ops = match opcode, ops with
  | `ADD4,   [| Reg rt; Reg ra; Reg rb  |] -> add rt ra rb
  | `ADD4o,  [| Reg rt; Reg ra; Reg rb  |] -> add_dot addr_size rt ra rb
  | `ADDI,   [| Reg rt; Reg ra; Imm imm |] -> addi rt ra imm
  | `ADDIS,  [| Reg rt; Reg ra; Imm imm |] -> addis rt ra imm
  | `ADDIC,  [| Reg rt; Reg ra; Imm imm |] -> addic addr_size rt ra imm
  | `ADDICo, [| Reg rt; Reg ra; Imm imm |] -> addic_dot addr_size rt ra imm
  | `ADDC,   [| Reg rt; Reg ra; Reg rb  |] -> addc addr_size rt ra rb
  | `ADDCo,  [| Reg rt; Reg ra; Reg rb  |] -> addc_dot addr_size rt ra rb
  | `ADDE,   [| Reg rt; Reg ra; Reg rb  |] -> adde addr_size rt ra rb
  | `ADDEo,  [| Reg rt; Reg ra; Reg rb  |] -> adde_dot addr_size rt ra rb
  | `ADDME,  [| Reg rt; Reg ra;         |] -> addme addr_size rt ra
  | `ADDMEo, [| Reg rt; Reg ra;         |] -> addme_dot addr_size rt ra
  | `ADDZE,  [| Reg rt; Reg ra;         |] -> addze addr_size rt ra
  | `ADDZEo, [| Reg rt; Reg ra;         |] -> addze_dot addr_size rt ra
  | `LI,     [| Reg rt; Imm imm;        |] -> li rt imm
  | `LIS,    [| Reg rt; Imm imm;        |] -> lis rt imm
  | `LA,     [| Reg rt; Imm imm; Reg ra |] -> addi rt ra imm
  | opcode, _ ->
    let opcode = Sexp.to_string (sexp_of_t opcode) in
    Dsl.ppc_fail "%s: unexpected operand set" opcode
