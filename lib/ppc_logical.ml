open Core_kernel.Std
open Bap.Std

open Ppc_types
open Hardware

(** Extended mnemonics:

    nop         = ori 0, 0, 0
    xnop        = xri 0, 0, 0
    mr  rx, ry  = or rx, ry, ry
    not   rx, ry  = nor rx, ry, ry *)

(** Fixed-point AND Immediate
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    71 2a 00 20     andi.   r10,r9,32 *)
let andi_dot addr_size ra rs imm =
  let zero48 = Word.zero 48 in
  let rs = Dsl.find_gpr rs in
  let ra = Dsl.find_gpr ra in
  let imm = Word.of_int64 ~width:16 (Imm.to_int64 imm) in
  Dsl.[ ra := var rs land (int zero48 ^ int imm);
    cr_bit' 0 := is_negative addr_size (var ra);
    cr_bit' 1 := is_positive addr_size (var ra);
    cr_bit' 2 := is_zero addr_size (var ra); ]

(** Fixed-point AND Immediate Shifted
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    75 2a 08 00     andis.  r10,r9,2048 *)
let andis_dot addr_size ra rs ui =
  let rs = Dsl.find_gpr rs in
  let ra = Dsl.find_gpr ra in
  let zero16 = Word.zero 16 in
  let zero32 = Word.zero 32 in
  let ui = Word.of_int64 ~width:16 (Imm.to_int64 ui) in
  Dsl.[ ra := var rs land (int zero32 ^ int ui ^ int zero16);
    cr_bit' 0 := is_negative addr_size (var ra);
    cr_bit' 1 := is_positive addr_size (var ra);
    cr_bit' 2 := is_zero addr_size (var ra); ]

(** Fixed-point AND
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7f 39 e8 38     and     r25,r25,r29
    7d 49 30 39     and.    r9,r10,r6 *)
let and_ ra rs rb =
  let rs = Dsl.find_gpr rs in
  let ra = Dsl.find_gpr ra in
  let rb = Dsl.find_gpr rb in
  Dsl.[ ra := var rs land var rb; ]

let and_dot addr_size ra rs rb =
  and_ ra rs rb @ Dsl.write_fixpoint_result addr_size (Dsl.find_gpr ra)

(** Fixed-point AND with Complement
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c ea 50 78     andc    r10,r7,r10
    7e 09 18 79     andc.   r9,r16,r3  *)
let andc ra rs rb =
  let rs = Dsl.find_gpr rs in
  let ra = Dsl.find_gpr ra in
  let rb = Dsl.find_gpr rb in
  Dsl.[ ra := var rs land (lnot (var rb)); ]

let andc_dot addr_size ra rs rb =
  andc ra rs rb @ Dsl.write_fixpoint_result addr_size (Dsl.find_gpr ra)

(** Fixed-point OR Immediate
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    60 c6 51 c1     ori     r6,r6,20929 *)
let ori ra rs imm =
  let zero48 = Word.zero 48 in
  let rs = Dsl.find_gpr rs in
  let ra = Dsl.find_gpr ra in
  let imm = Word.of_int64 ~width:16 (Imm.to_int64 imm) in
  Dsl.[ ra := var rs lor (int zero48 ^ int imm); ]

(** Fixed-point OR Immediate Shifted
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    65 4a 00 10     oris    r10,r10,16 *)
let oris ra rs ui =
  let rs = Dsl.find_gpr rs in
  let ra = Dsl.find_gpr ra in
  let zero16 = Word.zero 16 in
  let zero32 = Word.zero 32 in
  let ui = Word.of_int64 ~width:16 (Imm.to_int64 ui) in
  Dsl.[ ra := var rs lor (int zero32 ^ int ui ^ int zero16); ]

(** Fixed-point OR
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7f 38 c3 78     or      r24,r25,r24
    7d 0a 4b 79     or.     r10,r8,r9  *)
let or_ ra rs rb =
  let rs = Dsl.find_gpr rs in
  let ra = Dsl.find_gpr ra in
  let rb = Dsl.find_gpr rb in
  Dsl.[ ra := var rs lor var rb; ]

let or_dot addr_size ra rs rb =
  or_ ra rs rb @ Dsl.write_fixpoint_result addr_size (Dsl.find_gpr ra)

(** Fixed-point OR with Complement
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c 8a 53 38     orc     r10,r4,r10
    7c 8a 53 39     orc.    r10,r4,r10 *)
let orc ra rs rb =
  let rs = Dsl.find_gpr rs in
  let ra = Dsl.find_gpr ra in
  let rb = Dsl.find_gpr rb in
  Dsl.[ ra := var rs lor (lnot (var rb)); ]

let orc_dot addr_size ra rs rb =
  orc ra rs rb @ Dsl.write_fixpoint_result addr_size (Dsl.find_gpr ra)

(** Fixed-point XOR Immediate
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    68 63 00 01     xori    r3,r3,1 *)
let xori ra rs imm =
  let zero48 = Word.zero 48 in
  let rs = Dsl.find_gpr rs in
  let ra = Dsl.find_gpr ra in
  let imm = Word.of_int64 ~width:16 (Imm.to_int64 imm) in
  Dsl.[ ra := var rs lxor (int zero48 ^ int imm); ]

(** Fixed-point XOR Immediate Shifted
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    6d 2a 04 00     xoris   r10,r9,1024
 *)
let xoris ra rs ui =
  let rs = Dsl.find_gpr rs in
  let ra = Dsl.find_gpr ra in
  let zero16 = Word.zero 16 in
  let zero32 = Word.zero 32 in
  let ui = Word.of_int64 ~width:16 (Imm.to_int64 ui) in
  Dsl.[ ra := var rs lxor (int zero32 ^ int ui ^ int zero16); ]

(** Fixed-point XOR
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c 6a 52 78     xor     r10,r3,r10
    7d 4a 4a 79     xor.    r10,r10,r9 *)
let xor_ ra rs rb =
  let rs = Dsl.find_gpr rs in
  let ra = Dsl.find_gpr ra in
  let rb = Dsl.find_gpr rb in
  Dsl.[ ra := var rs lxor var rb; ]

let xor_dot addr_size ra rs rb =
  xor_ ra rs rb @ Dsl.write_fixpoint_result addr_size (Dsl.find_gpr ra)

(** Fixed-point NAND
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c 63 1b b8     nand    r3,r3,r3
    7c 63 1b b9     nand.   r3,r3,r3 *)
let nand ra rs rb =
  let rs = Dsl.find_gpr rs in
  let ra = Dsl.find_gpr ra in
  let rb = Dsl.find_gpr rb in
  Dsl.[ ra := lnot (var rs land var rb); ]

let nand_dot addr_size ra rs rb =
  nand ra rs rb @ Dsl.write_fixpoint_result addr_size (Dsl.find_gpr ra)

(** Fixed-point NOR
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7d 09 48 f8     nor     r9,r8,r9
    7d 09 48 f9     nor.    r9,r8,r9  *)
let nor ra rs rb =
  let rs = Dsl.find_gpr rs in
  let ra = Dsl.find_gpr ra in
  let rb = Dsl.find_gpr rb in
  Dsl.[ ra := lnot (var rs lor var rb); ]

let nor_dot addr_size ra rs rb =
  nor ra rs rb @ Dsl.write_fixpoint_result addr_size (Dsl.find_gpr ra)

(** Fixed-point Equivalent
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7d 09 4a 38     eqv     r9,r8,r9
    7d 09 4a 39     eqv.    r9,r8,r9 *)
let eqv ra rs rb =
  let rs = Dsl.find_gpr rs in
  let ra = Dsl.find_gpr ra in
  let rb = Dsl.find_gpr rb in
  Dsl.[ ra := lnot (var rs lxor (var rb)); ]

let eqv_dot addr_size ra rs rb =
  eqv ra rs rb @ Dsl.write_fixpoint_result addr_size (Dsl.find_gpr ra)

(** Fixed-point Extend Sign Byte/Halfword/Word
    Pages 92-99 of IBM Power ISATM Version 3.0 B
    examples:
    7d 4a 07 74     extsb   r10,r10
    7d 48 07 75     extsb.  r8,r10
    7d 25 07 34     extsh   r5,r9
    7d 25 07 35     extsh.  r5,r9
    7d 25 07 b4     extsw   r5,r9
    7d 25 07 b5     extsw.  r5,r9 *)
let exts ra rs size =
  let bits = Size.in_bits size in
  let sign_pos = Size.in_bits size - 1 in
  let zero_tail = Word.zero bits in
  let zeros = Word.concat (Word.zero (gpr_bitwidth - bits)) zero_tail in
  let ones = Word.concat (Word.ones (gpr_bitwidth - bits)) zero_tail in
  let rs = Dsl.find_gpr rs in
  let ra = Dsl.find_gpr ra in
  let sign = Dsl.fresh "sign" (Type.imm 1) in
  Dsl.[ sign := extract sign_pos sign_pos (var rs);
    ra := cast unsigned gpr_bitwidth (extract sign_pos 0 (var rs));
    if_ (var sign) Dsl.[
      ra := var ra lor (int ones)
    ] Dsl.[
      ra := var ra lor (int zeros)
    ]
  ]

let exts_dot addr_size ra rs size =
  exts ra rs size @ Dsl.write_fixpoint_result addr_size (Dsl.find_gpr ra)

(** Fixed-point Count Leading Zeros Word/Doubleword
    Pages 92-99 of IBM Power ISATM Version 3.0 B
    examples:
    7c 63 00 34     cntlzw   r3,r3
    7c 63 00 35     cntlzw.  r3,r3
    7c 63 00 74     cntlzd   r3,r3
    7c 63 00 75     cntlzd.  r3,r3 *)
let cntlz ra rs size =
  let rs = Dsl.find_gpr rs in
  let ra = Dsl.find_gpr ra in
  let bits = Size.in_bits size in
  let high_bit = bits - 1 in
  let one = Word.one bits in
  let probe =
    let one_ = Word.one bits in
    let shift = Word.of_int ~width:bits (bits -1) in
    Word.(one_ lsl shift) in
  let zero = Word.zero bits in
  let xv = Dsl.fresh "x" (Type.imm bits) in
  let mask = Dsl.fresh "mask" (Type.imm bits) in
  let cnt = Dsl.fresh "cnt" (Type.imm bits) in
  let has_no_ones = Dsl.fresh "has_no_ones" (Type.imm 1) in
  let init = Dsl.[
      xv := extract high_bit 0 (var rs);
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

let cntlz_dot addr_size ra rs size =
  cntlz ra rs size @ Dsl.write_fixpoint_result addr_size (Dsl.find_gpr ra)

(** Fixed-point Count Trailing Zeros Word
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c 63 04 34     cnttzw   r3,r3
    7c 63 04 35     cnttzw.  r3,r3
    7c 63 04 74     cnttzd   r3,r3
    7c 63 04 75     cnttzd.  r3,r3 *)
let cnttz ra rs size =
  let rs = Dsl.find_gpr rs in
  let ra = Dsl.find_gpr ra in
  let bits = Size.in_bits size in
  let high_bit = bits - 1 in
  let one = Word.one bits in
  let probe = Word.one bits in
  let zero = Word.zero bits in
  let xv = Dsl.fresh "x" (Type.imm bits) in
  let mask = Dsl.fresh "mask" (Type.imm bits) in
  let cnt = Dsl.fresh "cnt" (Type.imm bits) in
  let has_no_ones = Dsl.fresh "has_no_ones" (Type.imm 1) in
  let init = Dsl.[
      xv := extract high_bit 0 (var rs);
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

let cnttz_dot addr_size ra rs size =
  cnttz ra rs size @ Dsl.write_fixpoint_result addr_size (Dsl.find_gpr ra)

(** Fixed-point Compare Bytes
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c 8a 53 f8   cmpb r10, r4, r10 *)
let cmpb ra rs rb =
  let rs = Dsl.find_gpr rs in
  let ra = Dsl.find_gpr ra in
  let rb = Dsl.find_gpr rb in
  let byte = 8 in
  let rs_byte = Dsl.fresh "rs_byte" (Type.imm byte) in
  let rb_byte = Dsl.fresh "rb_byte" (Type.imm byte) in
  let res = Dsl.fresh "res" (Type.imm gpr_bitwidth) in
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

(** Fixed-point Population Count Bytes/Words/Doubleword
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c 84 00 f4       popcntb r4, r4 (not working in llvm)
    7c 84 02 f4       popcntw r4, r4
    7c 84 03 f4       popcntd r4, r4 *)
let popcnt ra rs size =
  let ra = Dsl.find_gpr ra in
  let rs = Dsl.find_gpr rs in
  let bits = Size.in_bits size in
  let steps = gpr_bitwidth / bits in
  let one = Word.one gpr_bitwidth in
  let zero = Word.zero gpr_bitwidth in
  let res = Dsl.fresh "res" (Type.imm gpr_bitwidth) in
  let x = Dsl.fresh "x" (Type.imm bits) in
  let cnt = Dsl.fresh "cnt" (Type.imm gpr_bitwidth) in
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
let parity ra rs size = Dsl.ppc_fail "llvm doens't now about this insn"

(** Fixed-point Bit Permute Doubleword
    Pages 100 of IBM Power ISATM Version 3.0 B
    examples:
    7c a1 49 f8    bperm r1, r5, r9 *)
let bpermd ra rs rb =
  let ra = Dsl.find_gpr ra in
  let rb = Dsl.find_gpr rb in
  let rs = Dsl.find_gpr rs in
  let max_ind = Word.of_int ~width:8 64 in
  let index = Dsl.fresh "index" (Type.imm 8) in
  let iv = Dsl.fresh "iv" (Type.imm 8) in
  let tmp = Dsl.fresh "tmp" (Type.imm gpr_bitwidth) in
  let bit = Dsl.fresh "bit" (Type.imm 1) in
  let zero_bit = Word.zero 1 in
  let zero = Word.zero gpr_bitwidth in
  let init = Dsl.[
      tmp := int zero;
    ] in
  let foreach_byte i =
    let hi = (i + 1) * 8 - 1 in
    let lo = i * 8 in
    let i = Word.of_int ~width:8 i in
    Dsl.[
      iv := int i;
      index := extract hi lo (var rs);
      if_ (var index < int max_ind) [
        bit := extract 0 0 (var rb lsr var index);
      ] [
        bit := int zero_bit;
      ];
      tmp := var tmp lor ((cast unsigned gpr_bitwidth (var bit)) lsl var iv);
    ] in
  let loop = List.concat @@ List.init 8 ~f:foreach_byte in
  let finish = Dsl.[
      ra := cast unsigned gpr_bitwidth (extract 7 0 (var tmp));
    ] in
  List.concat [
    init;
    loop;
    finish;
  ]

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
  | `EXTSW
  | `EXTSWo
] [@@deriving sexp,enumerate]

type cntz = [
  | `CNTLZW
  | `CNTLZWo
  | `CNTLZD
  | `CNTLZDo
  | `CNTTZW
  | `CNTTZWo
  | `CNTTZD
  | `CNTTZDo
] [@@deriving sexp,enumerate]

type cmpb = [ `CMPB ] [@@deriving sexp,enumerate]

type popcnt = [
  | `POPCNTB
  | `POPCNTW
  | `POPCNTD
] [@@deriving sexp,enumerate]

type bperm = [ `BPERMD ] [@@deriving sexp,enumerate]

type t = [ and_ | or_ | xor | eqv | exts | cntz | cmpb | popcnt | bperm ] [@@deriving sexp,enumerate]

let lift t addr_size endian mem ops =
  match t, ops with
  | `ANDIo,   [| Reg ra; Reg rs; Imm ui |] -> andi_dot addr_size ra rs ui
  | `ANDISo,  [| Reg ra; Reg rs; Imm ui |] -> andis_dot addr_size ra rs ui
  | `AND,     [| Reg ra; Reg rs; Reg rb |] -> and_ ra rs rb
  | `ANDo,    [| Reg ra; Reg rs; Reg rb |] -> and_dot addr_size ra rs rb
  | `ANDC,    [| Reg ra; Reg rs; Reg rb |] -> andc ra rs rb
  | `ANDCo,   [| Reg ra; Reg rs; Reg rb |] -> andc_dot addr_size ra rs rb
  | `ORI,     [| Reg ra; Reg rs; Imm ui |] -> ori ra rs ui
  | `ORIS,    [| Reg ra; Reg rs; Imm ui |] -> oris ra rs ui
  | `OR,      [| Reg ra; Reg rs; Reg rb |] -> or_ ra rs rb
  | `ORo,     [| Reg ra; Reg rs; Reg rb |] -> or_dot addr_size ra rs rb
  | `ORC,     [| Reg ra; Reg rs; Reg rb |] -> orc ra rs rb
  | `ORCo,    [| Reg ra; Reg rs; Reg rb |] -> orc_dot addr_size ra rs rb
  | `XORI,    [| Reg ra; Reg rs; Imm ui |] -> xori ra rs ui
  | `XORIS,   [| Reg ra; Reg rs; Imm ui |] -> xoris ra rs ui
  | `XOR,     [| Reg ra; Reg rs; Reg rb |] -> xor_ ra rs rb
  | `XORo,    [| Reg ra; Reg rs; Reg rb |] -> xor_dot addr_size ra rs rb
  | `NAND,    [| Reg ra; Reg rs; Reg rb |] -> nand ra rs rb
  | `NANDo,   [| Reg ra; Reg rs; Reg rb |] -> nand_dot addr_size ra rs rb
  | `NOR,     [| Reg ra; Reg rs; Reg rb |] -> nor ra rs rb
  | `NORo,    [| Reg ra; Reg rs; Reg rb |] -> nor_dot addr_size ra rs rb
  | `EQV,     [| Reg ra; Reg rs; Reg rb |] -> eqv ra rs rb
  | `EQVo,    [| Reg ra; Reg rs; Reg rb |] -> eqv_dot addr_size ra rs rb
  | `EXTSB,   [| Reg ra; Reg rs; |] -> exts ra rs `r8
  | `EXTSBo,  [| Reg ra; Reg rs; |] -> exts_dot addr_size ra rs `r8
  | `EXTSH,   [| Reg ra; Reg rs; |] -> exts ra rs `r16
  | `EXTSHo,  [| Reg ra; Reg rs; |] -> exts_dot addr_size ra rs `r16
  | `EXTSW,   [| Reg ra; Reg rs; |] -> exts ra rs `r32
  | `EXTSWo,  [| Reg ra; Reg rs; |] -> exts_dot addr_size ra rs `r32
  | `CNTLZW,  [| Reg ra; Reg rs; |] -> cntlz rs ra `r32
  | `CNTLZWo, [| Reg ra; Reg rs; |] -> cntlz_dot addr_size rs ra `r32
  | `CNTLZD,  [| Reg ra; Reg rs; |] -> cntlz rs ra `r64
  | `CNTLZDo, [| Reg ra; Reg rs; |] -> cntlz_dot addr_size rs ra `r64
  | `CNTTZW,  [| Reg ra; Reg rs; |] -> cnttz rs ra `r32
  | `CNTTZWo, [| Reg ra; Reg rs; |] -> cnttz_dot addr_size rs ra `r32
  | `CNTTZD,  [| Reg ra; Reg rs; |] -> cnttz rs ra `r64
  | `CNTTZDo, [| Reg ra; Reg rs; |] -> cnttz_dot addr_size rs ra `r64
  | `CMPB,    [| Reg ra; Reg rs; Reg rb; |] -> cmpb ra rs rb
  | `POPCNTW, [| Reg ra; Reg rs; |] -> popcnt ra rs `r32
  | `POPCNTB, [| Reg ra; Reg rs; |] -> popcnt ra rs `r8
  | `POPCNTD, [| Reg ra; Reg rs; |] -> popcnt ra rs `r64
  | `BPERMD,  [| Reg ra; Reg rs; Reg rb |] -> bpermd ra rs rb
  | opcode, _ ->
    let opcode = Sexp.to_string (sexp_of_t opcode) in
    Dsl.ppc_fail "%s: unexpected operand set" opcode
