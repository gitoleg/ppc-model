open Core_kernel.Std
open Bap.Std
open Op

open Ppc_model.Hardware
open Ppc_rtl

(** TODO: we still need to invent a way to write dot instructions  *)

(** Fixed-point AND Immediate
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    71 2a 00 20     andi.   r10,r9,32 *)
let andi_dot mode ra rs imm =
  let zero48 = Word.zero 48 in
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let imm = Word.of_int64 ~width:16 (Imm.to_int64 imm) in
  Dsl.[
    ra := var rs land (int zero48 ^ int imm);
    set_cond_reg0 mode (var ra);
  ]

(** Fixed-point AND Immediate Shifted
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    75 2a 08 00     andis.  r10,r9,2048 *)
let andis_dot mode rs ra ui =
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let zero16 = Word.zero 16 in
  let zero32 = Word.zero 32 in
  let ui = Word.of_int64 ~width:16 (Imm.to_int64 ui) in
  Dsl.[
    ra := var rs land (int zero32 ^ int ui ^ int zero16);
    set_cond_reg0 mode (var ra);
  ]

(** Fixed-point AND
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7f 39 e8 38     and     r25,r25,r29
    7d 49 30 39     and.    r9,r10,r6 *)
let and_ rs ra rb =
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let rb = find_gpr rb in
  Dsl.[
    ra := var rs land var rb;
  ]

let and_dot mode rs ra rb =
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let rb = find_gpr rb in
  Dsl.[
    ra := var rs land var rb;
    set_cond_reg0 mode (var ra);
  ]

(** Fixed-point AND with Complement
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c ea 50 78     andc    r10,r7,r10
    7e 09 18 79     andc.   r9,r16,r3  *)
let andc rs ra rb =
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let rb = find_gpr rb in
  Dsl.[
    ra := var rs land (lnot (var rb));
  ]

let andc_dot mode rs ra rb =
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let rb = find_gpr rb in
  Dsl.[
    ra := var rs land (lnot (var rb));
    set_cond_reg0 mode (var ra);
  ]


(** Fixed-point OR Immediate
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    60 c6 51 c1     ori     r6,r6,20929 *)
let ori rs ra imm =
  let zero48 = Word.zero 48 in
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let imm = Word.of_int64 ~width:16 (Imm.to_int64 imm) in
  Dsl.[
    ra := var rs lor (int zero48 ^ int imm);
  ]

(** Fixed-point OR Immediate Shifted
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    65 4a 00 10     oris    r10,r10,16 *)
let oris rs ra ui =
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let zero16 = Word.zero 16 in
  let zero32 = Word.zero 32 in
  let ui = Word.of_int64 ~width:16 (Imm.to_int64 ui) in
  Dsl.[
    ra := var rs lor (int zero32 ^ int ui ^ int zero16);
  ]

(** Fixed-point OR
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7f 38 c3 78     or      r24,r25,r24
    7d 0a 4b 79     or.     r10,r8,r9  *)
let or_ rs ra rb =
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let rb = find_gpr rb in
  Dsl.[
    ra := var rs lor var rb;
  ]

let or_dot mode rs ra rb =
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let rb = find_gpr rb in
  Dsl.[
    ra := var rs lor var rb;
    set_cond_reg0 mode (var ra);
  ]

(** Fixed-point OR with Complement
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c 8a 53 38     orc     r10,r4,r10
    7c 8a 53 39     orc.    r10,r4,r10 *)
let orc rs ra rb =
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let rb = find_gpr rb in
  Dsl.[
    ra := var rs lor (lnot (var rb));
  ]

let orc_dot mode rs ra rb =
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let rb = find_gpr rb in
  Dsl.[
    ra := var rs lor (lnot (var rb));
    set_cond_reg0 mode (var ra);
  ]

(** Fixed-point XOR Immediate
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    68 63 00 01     xori    r3,r3,1 *)
let xori rs ra imm =
  let zero48 = Word.zero 48 in
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let imm = Word.of_int64 ~width:16 (Imm.to_int64 imm) in
  Dsl.[
    ra := var rs lxor (int zero48 ^ int imm);
  ]

(** Fixed-point XOR Immediate Shifted
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    6d 2a 04 00     xoris   r10,r9,1024
 *)
let xoris rs ra ui =
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let zero16 = Word.zero 16 in
  let zero32 = Word.zero 32 in
  let ui = Word.of_int64 ~width:16 (Imm.to_int64 ui) in
  Dsl.[
    ra := var rs lxor (int zero32 ^ int ui ^ int zero16);
  ]

(** Fixed-point XOR
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c 6a 52 78     xor     r10,r3,r10
    7d 4a 4a 79     xor.    r10,r10,r9 *)
let xor_ rs ra rb =
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let rb = find_gpr rb in
  Dsl.[
    ra := var rs lxor var rb;
  ]

let xor_dot mode rs ra rb =
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let rb = find_gpr rb in
  Dsl.[
    ra := var rs lxor var rb;
    set_cond_reg0 mode (var ra);
  ]

(** Fixed-point NAND
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c 63 1b b8     nand    r3,r3,r3
    7c 63 1b b9     nand.   r3,r3,r3 *)
let nand rs ra rb =
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let rb = find_gpr rb in
  Dsl.[
    ra := lnot (var rs land var rb);
  ]

let nand_dot mode rs ra rb =
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let rb = find_gpr rb in
  Dsl.[
    ra := lnot (var rs land var rb);
    set_cond_reg0 mode (var ra);
  ]

(** Fixed-point NOR
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7d 09 48 f8     nor     r9,r8,r9
    7d 09 48 f9     nor.    r9,r8,r9  *)
let nor rs ra rb =
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let rb = find_gpr rb in
  Dsl.[
    ra := lnot (var rs lor var rb);
  ]

let nor_dot mode rs ra rb =
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let rb = find_gpr rb in
  Dsl.[
    ra := lnot (var rs lor var rb);
    set_cond_reg0 mode (var ra);
  ]

(** Fixed-point Equivalent
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7d 09 4a 38     eqv     r9,r8,r9
    7d 09 4a 39     eqv.    r9,r8,r9 *)
let eqv rs ra rb =
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let rb = find_gpr rb in
  Dsl.[
    ra := lnot (var rs lxor (var rb));
  ]

let eqv_dot mode rs ra rb =
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let rb = find_gpr rb in
  Dsl.[
    ra := lnot (var rs lxor (var rb));
    set_cond_reg0 mode (var ra);
  ]

(** Fixed-point Equivalent
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7d 4a 07 74     extsb   r10,r10
    7d 48 07 75     extsb.  r8,r10
    7d 25 07 34     extsh   r5,r9
    7d 25 07 35     extsh.  r5,r9 *)
let exts rs ra size =
  let bits = Size.in_bits size in
  let sign_pos = Size.in_bits size - 1 in
  let zero_tail = Word.zero bits in
  let zeros = Word.concat (Word.zero (gpr_bitwidth - bits)) zero_tail in
  let ones = Word.concat (Word.ones (gpr_bitwidth - bits)) zero_tail in
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let sign = Var.create ~fresh:true "sign" (Type.imm 1) in
  Dsl.[
    sign := extract sign_pos sign_pos (var rs);
    ra := cast unsigned gpr_bitwidth (extract sign_pos 0 (var rs));
    if_ (var sign) [
      ra := var ra lor (int ones)
    ] [
      ra := var ra lor (int zeros)
    ]
  ]

let exts_dot mode rs ra size =
  let bits = Size.in_bits size in
  let sign_pos = Size.in_bits size - 1 in
  let zero_tail = Word.zero bits in
  let zeros = Word.concat (Word.zero (gpr_bitwidth - bits)) zero_tail in
  let ones = Word.concat (Word.ones (gpr_bitwidth - bits)) zero_tail in
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let sign = Var.create ~fresh:true "sign" (Type.imm 1) in
  Dsl.[
    sign := extract sign_pos sign_pos (var rs);
    ra := cast unsigned gpr_bitwidth (extract sign_pos 0 (var rs));
    if_ (var sign) [
      ra := var ra lor (int ones)
    ] [
      ra := var ra lor (int zeros)
    ];
    set_cond_reg0 mode (var ra);
  ]

(** Fixed-point Count Leading Zeros Word
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c 63 00 34     cntlzw   r3,r3
    7c 63 00 35     cntlzw.  r3,r3 *)
let cntlzw rs ra =
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let bits = 32 in
  let one = Word.one bits in
  let probe = Word.of_int ~width:bits (1 lsl 31) in
  let zero = Word.zero bits in
  let xv = Var.create ~fresh:true "x" (Type.imm bits) in
  let mask = Var.create ~fresh:true "mask" (Type.imm bits) in
  let cnt = Var.create ~fresh:true "cnt" (Type.imm bits) in
  let has_no_ones = Var.create ~fresh:true "has_no_ones" (Type.imm 1) in
  let init = Dsl.[
      xv := extract 31 0 (var rs);
      mask := int probe;
      cnt := int zero;
      has_no_ones := int Word.b1;
    ] in
  let foreach_bit = Dsl.[
      if_ (var has_no_ones land ((var xv land var mask) = int zero)) [
        cnt := var cnt + int one;
        mask := var mask lsr int one;
      ] [
        has_no_ones := int Word.b0;
      ];
    ] in
  let loop = List.concat @@ List.init bits ~f:(fun _ -> foreach_bit) in
  let finish = Dsl.[ra := cast unsigned gpr_bitwidth (var cnt)] in
  init @ loop @ finish

let cntlzw_dot mode rs ra =
  let rav = find_gpr ra in
  cntlzw rs ra @
  Dsl.[set_cond_reg0 mode (var rav)]

(** Fixed-point Count Trailing Zeros Word
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c 63 04 34     cnttzw   r3,r3
    7c 63 04 35     cnttzw.  r3,r3 *)
let cnttzw rs ra =
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let bits = 32 in
  let one = Word.one bits in
  let probe = Word.one bits in
  let zero = Word.zero bits in
  let xv = Var.create ~fresh:true "x" (Type.imm bits) in
  let mask = Var.create ~fresh:true "mask" (Type.imm bits) in
  let cnt = Var.create ~fresh:true "cnt" (Type.imm bits) in
  let has_no_ones = Var.create ~fresh:true "has_no_ones" (Type.imm 1) in
  let init = Dsl.[
      xv := extract 31 0 (var rs);
      mask := int probe;
      cnt := int zero;
      has_no_ones := int Word.b1;
    ] in
  let foreach_bit = Dsl.[
      if_ (var has_no_ones land ((var xv land var mask) = int zero)) [
        cnt := var cnt + int one;
        mask := var mask lsl int one;
      ] [
        has_no_ones := int Word.b0;
      ];
    ] in
  let loop = List.concat @@ List.init bits ~f:(fun _ -> foreach_bit) in
  let finish = Dsl.[ra := cast unsigned gpr_bitwidth (var cnt)] in
  init @ loop @ finish

let cnttzw_dot mode rs ra =
  let rav = find_gpr ra in
  cnttzw rs ra @
  Dsl.[set_cond_reg0 mode (var rav)]

(** Fixed-point Compare Bytes
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c 8a 53 f8   cmpb r10, r4, r10 *)
let cmpb ra rs rb =
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let rb = find_gpr rb in
  let byte = 8 in
  let rs_byte = Var.create ~fresh:true "rs_byte" (Type.imm byte) in
  let rb_byte = Var.create ~fresh:true "rb_byte" (Type.imm byte) in
  let res = Var.create ~fresh:true "res" (Type.imm gpr_bitwidth) in
  let zero = Word.zero gpr_bitwidth in
  let get_byte reg n =
    let hi = (n + 1) * byte - 1 in
    let lo = n * byte in
    Dsl.extract hi lo (Dsl.var reg) in
  let set_byte reg n value =
    let shift = Word.of_int ~width:gpr_bitwidth (n * byte) in
    let value = Word.of_int ~width:gpr_bitwidth value in
    let value = Word.(value lsl shift) in
    Dsl.(var reg lor int value) in
  let foreach_byte index =
    Dsl.[
      rs_byte := get_byte rs index;
      rb_byte := get_byte rb index;
      if_ (var rs_byte = var rb_byte) [
        res := set_byte res index 0xFF
      ] [ ]
    ] in
  let init = Dsl.[res := int zero ] in
  let loop = List.concat (List.init 8 ~f:foreach_byte) in
  let finish = Dsl.[ra := var res] in
  List.concat [
    init;
    loop;
    finish;
  ]

(** Fixed-point Population Count Bytes/Words
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c 84 00 f4       popcntb r4, r4 (not working in llvm)
    7c 84 02 f4       popcntw r4, r4 *)
let popcnt ra rs size =
  let ra = find_gpr ra in
  let rs = find_gpr rs in
  let bits = Size.in_bits size in
  let steps = gpr_bitwidth / bits in
  let one = Word.one gpr_bitwidth in
  let zero = Word.zero gpr_bitwidth in
  let res = Var.create ~fresh:true "res" (Type.imm gpr_bitwidth) in
  let x = Var.create ~fresh:true "x" (Type.imm bits) in
  let cnt = Var.create ~fresh:true "cnt" (Type.imm gpr_bitwidth) in
  let foreach_bit reg index =
    let index = Word.of_int ~width:bits index in
    Dsl.[
      if_ (extract 0 0 (var reg lsr int index) = int Word.b1 ) [
        cnt := var cnt + int one;
      ] [];
    ] in
  let foreach_size index =
    let lo = index * bits in
    let hi = (index + 1) * bits - 1 in
    let shift = Word.of_int ~width:gpr_bitwidth (index * bits) in
    let init = Dsl.[
      cnt := int zero;
      x := extract hi lo (var rs);
    ] in
    let loop = List.concat @@ List.init bits (foreach_bit x) in
    let finish = Dsl.[
      res := var res lor (var cnt lsl int shift);
    ] in
    List.concat [
      init;
      loop;
      finish;
    ] in
  let init = Dsl.[
    res := int zero;
  ] in
  let loop = List.concat @@ List.init steps ~f:foreach_size in
  let finish = Dsl.[ra := var res] in
  List.concat [
    init;
    loop;
    finish;
  ]

(** Fixed-point Parity Doubleword/Word
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c 84 01 74     prtyd r4, r4 (not working in llvm)
    7c 84 01 34     prtyw r4, r4 (not working in llvm)    *)
let parity rs ra size = failwith "llvm doens't now about this insn"

type and_ = [
  | `ANDIo
  | `ANDISo
  | `AND
  | `ANDo
  | `ANDC
  | `ANDCo
  | `NAND
  | `NANDo
] [@@deriving sexp,enumerate]

type or_ = [
  | `ORI
  | `ORIS
  | `OR
  | `ORo
  | `ORC
  | `ORCo
  | `NOR
  | `NORo
] [@@deriving sexp,enumerate]

type xor = [
  | `XORI
  | `XORIS
  | `XOR
  | `XORo
] [@@deriving sexp,enumerate]

type eqv = [
  | `EQV
  | `EQVo
] [@@deriving sexp,enumerate]

type exts = [
  | `EXTSB
  | `EXTSBo
  | `EXTSH
  | `EXTSHo
] [@@deriving sexp,enumerate]

type cntz = [
  | `CNTLZW
  | `CNTLZWo
  | `CNTTZW
  | `CNTTZWo
] [@@deriving sexp,enumerate]

type cmpb = [ `CMPB ] [@@deriving sexp,enumerate]

type popcnt = [
  | `POPCNTB
  | `POPCNTW
] [@@deriving sexp,enumerate]

type parity = [
  | `PRTYD
  | `PRTYW
] [@@deriving sexp,enumerate]

type t = [ and_ | or_ | xor | eqv | exts | cntz | cmpb | popcnt | parity ] [@@deriving sexp,enumerate]

let lift t mode endian mem ops =
  match t, ops with
  | `ANDIo,   [| Reg ra; Reg rs; Imm ui |] -> andi_dot mode ra rs ui
  | `ANDISo,  [| Reg rs; Reg ra; Imm ui |] -> andis_dot mode rs ra ui
  | `AND,     [| Reg rs; Reg ra; Reg rb |] -> and_ rs ra rb
  | `ANDo,    [| Reg rs; Reg ra; Reg rb |] -> and_dot mode rs ra rb
  | `ANDC,    [| Reg rs; Reg ra; Reg rb |] -> andc rs ra rb
  | `ANDCo,   [| Reg rs; Reg ra; Reg rb |] -> andc_dot mode rs ra rb
  | `ORI,     [| Reg rs; Reg ra; Imm ui |] -> ori rs ra ui
  | `ORIS,    [| Reg rs; Reg ra; Imm ui |] -> oris rs ra ui
  | `OR,      [| Reg rs; Reg ra; Reg rb |] -> or_ rs ra rb
  | `ORo,     [| Reg rs; Reg ra; Reg rb |] -> or_dot mode rs ra rb
  | `ORC,     [| Reg rs; Reg ra; Reg rb |] -> orc rs ra rb
  | `ORCo,    [| Reg rs; Reg ra; Reg rb |] -> orc_dot mode rs ra rb
  | `XORI,    [| Reg rs; Reg ra; Imm ui |] -> xori rs ra ui
  | `XORIS,   [| Reg rs; Reg ra; Imm ui |] -> xoris rs ra ui
  | `XOR,     [| Reg rs; Reg ra; Reg rb |] -> xor_ rs ra rb
  | `XORo,    [| Reg rs; Reg ra; Reg rb |] -> xor_dot mode rs ra rb
  | `NAND,    [| Reg rs; Reg ra; Reg rb |] -> nand rs ra rb
  | `NANDo,   [| Reg rs; Reg ra; Reg rb |] -> nand_dot mode rs ra rb
  | `NOR,     [| Reg rs; Reg ra; Reg rb |] -> nor rs ra rb
  | `NORo,    [| Reg rs; Reg ra; Reg rb |] -> nor_dot mode rs ra rb
  | `EQV,     [| Reg rs; Reg ra; Reg rb |] -> eqv rs ra rb
  | `EQVo,    [| Reg rs; Reg ra; Reg rb |] -> eqv_dot mode rs ra rb
  | `EXTSB,   [| Reg rs; Reg ra; |] -> exts rs ra `r8
  | `EXTSBo,  [| Reg rs; Reg ra; |] -> exts_dot mode rs ra `r8
  | `EXTSH,   [| Reg rs; Reg ra; |] -> exts rs ra `r16
  | `EXTSHo,  [| Reg rs; Reg ra; |] -> exts_dot mode rs ra `r16
  | `CNTLZW,  [| Reg rs; Reg ra; |] -> cntlzw rs ra
  | `CNTLZWo, [| Reg rs; Reg ra; |] -> cntlzw_dot mode rs ra
  | `CNTTZW,  [| Reg rs; Reg ra; |] -> cnttzw rs ra
  | `CNTTZWo, [| Reg rs; Reg ra; |] -> cnttzw_dot mode rs ra
  | `CMPB,    [| Reg ra; Reg rs; Reg rb; |] -> cmpb ra rs rb
  | `POPCNTW, [| Reg ra; Reg rs; |] -> popcnt ra rs `r32
  | `POPCNTB, [| Reg ra; Reg rs; |] -> popcnt ra rs `r8
  (* | `PRTYD,  [| Reg rs; Reg ra; |] -> parity rs ra `r64 *)
  (* | `PRTYB,  [| Reg rs; Reg ra; |] -> parity rs ra `r8 *)
  | _ -> failwith "unexpected operand set"
