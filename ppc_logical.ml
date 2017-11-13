open Core_kernel.Std
open Bap.Std
open Op

open Ppc_model.Hardware
open Ppc_rtl

(** Fixed-point AND Immediate
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    71 2a 00 20     andi.   r10,r9,32 *)
let andi_dot mode rs ra imm =
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
  let w_one = Word.one gpr_bitwidth in
  let w_31 = Word.of_int ~width:gpr_bitwidth 31 in
  let probe = Word.(w_one lsl w_31) in
  let zero = Word.zero gpr_bitwidth in
  let xv = Var.create ~fresh:true "x" (Type.imm gpr_bitwidth) in
  let nv = Var.create ~fresh:true "n" (Type.imm gpr_bitwidth) in
  let iv = Var.create ~fresh:true "i" (Type.imm gpr_bitwidth) in
  let is_zero = Var.create ~fresh:true "is_zero" (Type.imm 1) in
  let init = Dsl.[
      xv := var rs;
      nv := int probe;
      iv := int zero;
      is_zero := int Word.b1;
    ] in
  let code = Dsl.[
      if_ (var is_zero land (var xv land var nv = int zero)) [
        iv := var iv + int w_one;
        is_zero := int Word.b1;
        nv := var nv lsr int w_one;
      ] [ is_zero := int Word.b0 ];
    ] in
  let code = List.concat @@ List.init 32 ~f:(fun _ -> code) in
  let finish = Dsl.[ra := var iv] in
  init @ code @ finish

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


type t = [ and_ | or_ | xor | eqv | exts | cntz ] [@@deriving sexp,enumerate]

let lift t mode endian mem ops =
  match t, ops with
  | `ANDIo,  [| Reg rs; Reg ra; Imm ui |] -> andi_dot mode rs ra ui
  | `ANDISo, [| Reg rs; Reg ra; Imm ui |] -> andis_dot mode rs ra ui
  | `AND,    [| Reg rs; Reg ra; Reg rb |] -> and_ rs ra rb
  | `ANDo,   [| Reg rs; Reg ra; Reg rb |] -> and_dot mode rs ra rb
  | `ANDC,   [| Reg rs; Reg ra; Reg rb |] -> andc rs ra rb
  | `ANDCo,  [| Reg rs; Reg ra; Reg rb |] -> andc_dot mode rs ra rb
  | `ORI,    [| Reg rs; Reg ra; Imm ui |] -> ori rs ra ui
  | `ORIS,   [| Reg rs; Reg ra; Imm ui |] -> oris rs ra ui
  | `OR,     [| Reg rs; Reg ra; Reg rb |] -> or_ rs ra rb
  | `ORo,    [| Reg rs; Reg ra; Reg rb |] -> or_dot mode rs ra rb
  | `ORC,    [| Reg rs; Reg ra; Reg rb |] -> orc rs ra rb
  | `ORCo,   [| Reg rs; Reg ra; Reg rb |] -> orc_dot mode rs ra rb
  | `XORI,   [| Reg rs; Reg ra; Imm ui |] -> xori rs ra ui
  | `XORIS,  [| Reg rs; Reg ra; Imm ui |] -> xoris rs ra ui
  | `XOR,    [| Reg rs; Reg ra; Reg rb |] -> xor_ rs ra rb
  | `XORo,   [| Reg rs; Reg ra; Reg rb |] -> xor_dot mode rs ra rb
  | `NAND,   [| Reg rs; Reg ra; Reg rb |] -> nand rs ra rb
  | `NANDo,  [| Reg rs; Reg ra; Reg rb |] -> nand_dot mode rs ra rb
  | `NOR,    [| Reg rs; Reg ra; Reg rb |] -> nor rs ra rb
  | `NORo,   [| Reg rs; Reg ra; Reg rb |] -> nor_dot mode rs ra rb
  | `EQV,    [| Reg rs; Reg ra; Reg rb |] -> eqv rs ra rb
  | `EQVo,   [| Reg rs; Reg ra; Reg rb |] -> eqv_dot mode rs ra rb
  | `EXTSB,  [| Reg rs; Reg ra; |] -> exts rs ra `r8
  | `EXTSBo, [| Reg rs; Reg ra; |] -> exts_dot mode rs ra `r8
  | `EXTSH,  [| Reg rs; Reg ra; |] -> exts rs ra `r16
  | `EXTSHo, [| Reg rs; Reg ra; |] -> exts_dot mode rs ra `r16
  | `CNTLZW, [| Reg rs; Reg ra; |] -> cntlzw rs ra
  | _ -> failwith "unexpected operand set"
