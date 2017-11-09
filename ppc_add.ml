(** There is some sort of confusion with add instructions. Some of them
    are not recognizable by llvm, other has name that similar to other
    instructions in ISA specification, so one can found useful next table
    of examples:

    --------------------------------------------------------------------------------
    |  opcode     |      asm          |  bits   |recognized by llvm| name in llvm  |
    |-------------|-------------------|---------|------------------|---------------|
    | 7d 62 5a 14 |add   r11, r2, r11 |rc=0,oe=0|  add  11,2,11    | ADD4          |
    | 7d 62 5a 15 |add.  r11, r2, r11 |rc=1,oe=0|  add. 11,2,11    | ADD4o         |
    | 7d 62 5e 14 |addo  r11, r2, r11 |rc=0,oe=1|  not recognized    |  ---          |
    | 7d 62 5e 15 |addo. r11, r2, r11 |rc=1,oe=1|  not recognized    |  ---          |
    | 7d 62 58 14 |addc  r11, r2, r11 |rc=0,oe=0| addc  11,2,11    | ADDC          |
    | 7d 62 58 15 |addc. r11, r2, r11 |rc=1,oe=0| addc.  11,2,11   | ADDCo         |
    | 7d 62 5c 14 |addco r11, r2, r11 |rc=0,oe=1|  not recognized    |  ---          |
    | 7d 62 5c 15 |addco. r11, r2, r11|rc=1,oe=1|  not recognized    |  ---          |
    --------------------------------------------------------------------------------

    And so on. Basicly, instructions
    addo addco addeo addmeo addzeo addo. addco. addeo. addmeo. addzeo.
    doesn't recognized by llvm.

    And theirs oe-less analogs - addc. adde. addme. addze. -
    contains "o" suffix in thers llvm names: ADDo ADDeo ADDMEo ADDZEo,
    where "o" stands for .(dot). *)

open Core_kernel.Std
open Bap.Std
open Op

open Ppc_model.Hardware
open Ppc_rtl

let set_ov32 x y r =
  let open Dsl in
  let r = extract 31 0 r in
  let x_msb = cast high 1 x in
  let y_msb = cast high 1 y in
  let r_msb = cast high 1 r in
  ov := (x_msb = y_msb) land (x_msb lxor r_msb)

let set_ov x y r =
  let open Dsl in
  let x_msb = cast high 1 x in
  let y_msb = cast high 1 y in
  let r_msb = cast high 1 r in
  ov := (x_msb = y_msb) land (x_msb lxor r_msb)

let set_ca x r = Dsl.(ca := r < x)

let set_ca32 x =
  Dsl.(ca := cast unsigned gpr_bitwidth (extract 31 0 x) <> x)

(** Fixed-Point Arithmetic Instructions - Add Immediate
    Page 67 of IBM Power ISATM Version 3.0 B
    examples:
    38 21 00 10     addi    r1,r1,16
    3b de fd 28     addi    r30,r30,-728 *)
let addi mode rt ra imm =
  let rt = find_gpr rt in
  let imm = Word.of_int64 ~width:gpr_bitwidth (Imm.to_int64 imm) in
  match find_gpr_opt ra with
  | None ->
    Dsl.[
      rt := cast signed gpr_bitwidth (int imm);
      set_cond_reg0 mode (var rt);
    ]
  | Some ra ->
    Dsl.[
      rt := var ra + cast signed gpr_bitwidth (int imm);
      set_cond_reg0 mode (var rt);
    ]

(** Fixed-Point Arithmetic Instructions - Add Immediate Shifted
    Page 67 of IBM Power ISATM Version 3.0 B
    examples:
    3f de 00 02     addis   r30,r30,2
    3d 6b f0 00     addis   r11,r11,-4096 *)
let addis mode rt ra imm =
  let rt = find_gpr rt in
  let imm =
    Int64.(Imm.to_int64 imm lsl 16) |>
    Word.of_int64 ~width:gpr_bitwidth in
  match find_gpr_opt ra with
  | None ->
    Dsl.[
      rt := cast signed gpr_bitwidth (int imm);
      set_cond_reg0 mode (var rt);
    ]
  | Some ra ->
    Dsl.[
      rt := var ra + cast signed gpr_bitwidth (int imm);
      set_cond_reg0 mode (var rt);
    ]

(** Fixed-Point Arithmetic Instructions - Add PC Immediate Shifted
    Page 68 of IBM Power ISATM Version 3.0 B
    example:
    4c 22 00 04     addpcis r1,4
    llvm doesn't recognize this instruction  *)
let addpcis rt imm = failwith "unimplemented"

(** Fixed-Point Arithmetic Instructions - Add
    Page 69 of IBM Power ISATM Version 3.0 B

    7d 62 5a 14 add   r11, r2, r11
    7d 62 5a 15 add.  r11, r2, r11 *)
let add rt ra rb =
  let rt = find_gpr rt in
  let ra = find_gpr ra in
  let rb = find_gpr rb in
  Dsl.[rt := var ra + var rb]

let add_dot_32 rt ra rb =
  let rt = find_gpr rt in
  let ra = find_gpr ra in
  let rb = find_gpr rb in
  Dsl.[
    rt := var ra + var rb;
    set_ov32 (var ra) (var rb) (var rt);
    ov := var ov32;
    so := var ov;
  ]

let add_dot_64 rt ra rb =
  let rt = find_gpr rt in
  let ra = find_gpr ra in
  let rb = find_gpr rb in
  Dsl.[
    rt := var ra + var rb;
    set_ov (var ra) (var rb) (var rt);
    set_ov32 (var ra) (var rb) (var rt);
    so := var ov;
  ]

(** Fixed-Point Arithmetic Instructions - Add Immediate Carrying
    Page 69 of IBM Power ISATM Version 3.0 B
    examples:
    30 21 00 10    addic r1, r1, 16
    33 de fd 28    addic r30, r30, -728  *)
let addic rt ra imm =
  let rt = find_gpr rt in
  let ra = find_gpr ra in
  let imm =
    Word.of_int64 ~width:gpr_bitwidth (Imm.to_int64 imm) in
  Dsl.[
    rt := var ra + int imm;
    set_ca (var ra) (var rt);
    set_ca32 (var rt);
  ]

(** Fixed-Point Arithmetic Instructions - Add Immediate Carrying and Record
    Page 69 of IBM Power ISATM Version 3.0 B
    examples:
    34 21 00 10    addic. r1, r1, 16
    37 de fd 28    addic. r30, r30, -728  *)
let addic_dot mode rt ra imm =
  let rt = find_gpr rt in
  let ra = find_gpr ra in
  let imm =
    Word.of_int64 ~width:gpr_bitwidth (Imm.to_int64 imm) in
  Dsl.[
    rt := var ra + int imm;
    set_ca (var ra) (var rt);
    set_ca32 (var rt);
    set_cond_reg0 mode (var rt);
  ]

(** Fixed-Point Arithmetic Instructions - Add Carrying
    Page 70 of IBM Power ISATM Version 3.0 B
    examples:
    7d 62 58 14  addc   r11, r2, r11
    7d 62 58 15  addc.  r11, r2, r11 *)
let addc rt ra rb =
  let rt = find_gpr rt in
  let ra = find_gpr ra in
  let rb = find_gpr rb in
  Dsl.[
    rt := var ra + var rb;
    set_ca (var ra) (var rt);
    set_ca32 (var rt);
  ]

let addc_dot mode rt ra rb =
  let rt = find_gpr rt in
  let ra = find_gpr ra in
  let rb = find_gpr rb in
  Dsl.[
    rt := var ra + var rb;
    set_ca (var ra) (var rt);
    set_ca32 (var rt);
    set_cond_reg0 mode (var rt);
  ]

(** Fixed-Point Arithmetic Instructions - Add Extend
    Page 71 of IBM Power ISATM Version 3.0 B
    examples:
    7c 21 81 14  adde   r11, r2, r11
    7c 21 81 15  adde.  r11, r2, r11 *)
let adde rt ra rb =
  let rt = find_gpr rt in
  let ra = find_gpr ra in
  let rb = find_gpr rb in
  Dsl.[
    rt := var ra + var rb + cast unsigned gpr_bitwidth (var ca);
    set_ca (var ra) (var rt);
    set_ca32 (var rt);
  ]

let adde_dot mode rt ra rb =
  let rt = find_gpr rt in
  let ra = find_gpr ra in
  let rb = find_gpr rb in
  Dsl.[
    rt := var ra + var rb + cast unsigned gpr_bitwidth (var ca);
    set_ca (var ra) (var rt);
    set_ca32 (var rt);
    set_cond_reg0 mode (var rt);
  ]

(** Fixed-Point Arithmetic Instructions - Add to Minus One Extend
    Page 71 of IBM Power ISATM Version 3.0 B
    examples:
    7c 22 01 d4  addme  r1,r2
    7c 22 01 d5  addme. r1,r2  *)
let addme rt ra =
  let rt = find_gpr rt in
  let ra = find_gpr ra in
  let minus_one = Word.ones gpr_bitwidth in
  Dsl.[
    rt := var ra + cast unsigned gpr_bitwidth (var ca) + int minus_one;
    set_ca (var ra) (var rt);
    set_ca32 (var rt);
  ]

let addme_dot mode rt ra =
  let rt = find_gpr rt in
  let ra = find_gpr ra in
  let minus_one = Word.ones gpr_bitwidth in
  Dsl.[
    rt := var ra + cast unsigned gpr_bitwidth (var ca) + int minus_one;
    set_ca (var ra) (var rt);
    set_ca32 (var rt);
    set_cond_reg0 mode (var rt);
  ]

(** Fixed-Point Arithmetic Instructions - Add Extended using alternate carry bit
    Page 72 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 19 54   addex r1,r2,r3
    llvm doesn't recognize this instruction *)
let addex rt ra rb = failwith "unimplemented"

(** Fixed-Point Arithmetic Instructions -
    Page 72 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 01 94   addze r1,r2
    7c 22 01 95   addze. r1,r2 *)
let addze rt ra =
  let rt = find_gpr rt in
  let ra = find_gpr ra in
  Dsl.[
    rt := var ra + cast unsigned gpr_bitwidth (var ca);
    set_ca (var ra) (var rt);
    set_ca32 (var rt);
  ]

let addze_dot mode rt ra =
  let rt = find_gpr rt in
  let ra = find_gpr ra in
  Dsl.[
    rt := var ra + cast unsigned gpr_bitwidth (var ca);
    set_ca (var ra) (var rt);
    set_ca32 (var rt);
    set_cond_reg0 mode (var rt);
  ]

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
] [@@deriving sexp]

let lift mode endian mem opcode ops = match opcode, ops with
  | `ADD4,   [| Reg rt; Reg ra; Reg rb  |] -> add rt ra rb
  | `ADD4o,  [| Reg rt; Reg ra; Reg rb  |] ->
    let add = match mode with
      | `r32 -> add_dot_32
      | `r64 -> add_dot_64 in
    add rt ra rb
  | `ADDI,   [| Reg rt; Reg ra; Imm imm |] -> addi mode rt ra imm
  | `ADDIS,  [| Reg rt; Reg ra; Imm imm |] -> addis mode rt ra imm
  | `ADDIC,  [| Reg rt; Reg ra; Imm imm |] -> addic rt ra imm
  | `ADDICo, [| Reg rt; Reg ra; Imm imm |] -> addic_dot mode rt ra imm
  | `ADDC,   [| Reg rt; Reg ra; Reg rb  |] -> addc rt ra rb
  | `ADDCo,  [| Reg rt; Reg ra; Reg rb  |] -> addc_dot mode rt ra rb
  | `ADDE,   [| Reg rt; Reg ra; Reg rb  |] -> adde rt ra rb
  | `ADDEo,  [| Reg rt; Reg ra; Reg rb  |] -> adde_dot mode rt ra rb
  | `ADDME,  [| Reg rt; Reg ra;         |] -> addme rt ra
  | `ADDMEo, [| Reg rt; Reg ra;         |] -> addme_dot mode rt ra
  | `ADDZE,  [| Reg rt; Reg ra;         |] -> addze rt ra
  | `ADDZEo, [| Reg rt; Reg ra;         |] -> addze_dot mode rt ra
  | _ -> failwith "unimplemented"
