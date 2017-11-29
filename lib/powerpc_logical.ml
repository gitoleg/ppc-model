open Core_kernel.Std
open Bap.Std

open Powerpc_types
open Model
open Hardware
open Dsl

(** Extended mnemonics:

    nop         = ori 0, 0, 0
    xnop        = xri 0, 0, 0
    mr  rx, ry  = or rx, ry, ry
    not   rx, ry  = nor rx, ry, ry *)

(** Fixed-point AND Immediate
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    71 2a 00 20     andi.   r10,r9,32 *)
let andi_dot addr_size ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let im = unsigned imm ops.(2) in
  RTL.[
    ra := rs land im;
    nbit cr 0 := low addr_size ra <$ zero;
    nbit cr 1 := low addr_size ra >$ zero;
    nbit cr 2 := low addr_size ra = zero
  ]

(** Fixed-point AND Immediate Shifted
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    75 2a 08 00     andis.  r10,r9,2048 *)
let andis_dot addr_size ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let im = unsigned imm ops.(2) in
  let sh = unsigned int 16 in
  RTL.[
    ra := rs land (im lsl sh);
    nbit cr 0 := low addr_size ra <$ zero;
    nbit cr 1 := low addr_size ra >$ zero;
    nbit cr 2 := low addr_size ra = zero;
  ]

(** Fixed-point AND
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7f 39 e8 38     and     r25,r25,r29
    7d 49 30 39     and.    r9,r10,r6 *)
let and_ ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[ ra := rs land rb; ]

let write_fixpoint_result addr_size res =
  let res = signed reg res in
  RTL.[
    nbit cr 0 := low addr_size res <$ zero;
    nbit cr 1 := low addr_size res >$ zero;
    nbit cr 2 := low addr_size res = zero;
  ]

let and_dot addr_size ops =
  and_ ops @ write_fixpoint_result addr_size ops.(0)

(** Fixed-point AND with Complement
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c ea 50 78     andc    r10,r7,r10
    7e 09 18 79     andc.   r9,r16,r3  *)
let andc ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[ ra := rs land (lnot rb); ]

let andc_dot addr_size ops =
  andc ops @ write_fixpoint_result addr_size ops.(0)

(** Fixed-point OR Immediate
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    60 c6 51 c1     ori     r6,r6,20929 *)
let ori ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let im = unsigned imm ops.(2) in
  RTL.[ ra := rs lor im; ]

(** Fixed-point OR Immediate Shifted
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    65 4a 00 10     oris    r10,r10,16 *)
let oris ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let im = unsigned imm ops.(2) in
  let sh = unsigned int 16 in
  RTL.[ ra := rs lor (im lsl sh); ]

(** Fixed-point OR
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7f 38 c3 78     or      r24,r25,r24
    7d 0a 4b 79     or.     r10,r8,r9  *)
let or_ ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[ ra := rs lor rb; ]

let or_dot addr_size ops =
  or_ ops @ write_fixpoint_result addr_size ops.(0)

(** Fixed-point OR with Complement
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c 8a 53 38     orc     r10,r4,r10
    7c 8a 53 39     orc.    r10,r4,r10 *)
let orc ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[ ra := rs lor (lnot rb); ]

let orc_dot addr_size ops =
  orc ops @ write_fixpoint_result addr_size ops.(0)

(** Fixed-point XOR Immediate
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    68 63 00 01     xori    r3,r3,1 *)
let xori ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let im = unsigned imm ops.(2) in
  RTL.[ ra := rs lxor im; ]

(** Fixed-point XOR Immediate Shifted
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    6d 2a 04 00     xoris   r10,r9,1024
 *)
let xoris ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let im = unsigned imm ops.(2) in
  let sh = unsigned int 16 in
  RTL.[ ra := rs lxor (im lsl sh); ]

(** Fixed-point XOR
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c 6a 52 78     xor     r10,r3,r10
    7d 4a 4a 79     xor.    r10,r10,r9 *)
let xor_ ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[ ra := rs lxor rb; ]

let xor_dot addr_size ops =
  xor_ ops @ write_fixpoint_result addr_size ops.(0)

(** Fixed-point NAND
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c 63 1b b8     nand    r3,r3,r3
    7c 63 1b b9     nand.   r3,r3,r3 *)
let nand ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[ ra := lnot (rs land rb); ]

let nand_dot addr_size ops =
  nand ops @ write_fixpoint_result addr_size ops.(0)

(** Fixed-point NOR
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7d 09 48 f8     nor     r9,r8,r9
    7d 09 48 f9     nor.    r9,r8,r9  *)
let nor ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[ ra := lnot (rs lor rb); ]

let nor_dot addr_size ops =
  nor ops @ write_fixpoint_result addr_size ops.(0)

(** Fixed-point Equivalent
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7d 09 4a 38     eqv     r9,r8,r9
    7d 09 4a 39     eqv.    r9,r8,r9 *)
let eqv ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[ ra := lnot (rs lxor rb); ]

let eqv_dot addr_size ops =
  eqv ops @ write_fixpoint_result addr_size ops.(0)

(** Fixed-point Extend Sign Byte/Halfword/Word
    Pages 92-99 of IBM Power ISATM Version 3.0 B
    examples:
    7d 4a 07 74     extsb   r10,r10
    7d 48 07 75     extsb.  r8,r10
    7d 25 07 34     extsh   r5,r9
    7d 25 07 35     extsh.  r5,r9
    7d 25 07 b4     extsw   r5,r9
    7d 25 07 b5     extsw.  r5,r9 *)
let exts ops size =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  RTL.[ ra := low size rs;]

let exts_dot addr_size ops size =
  exts ops size @ write_fixpoint_result addr_size ops.(0)

(** Fixed-point Count Leading Zeros Word/Doubleword
    Pages 92-99 of IBM Power ISATM Version 3.0 B
    examples:
    7c 63 00 34     cntlzw   r3,r3
    7c 63 00 35     cntlzw.  r3,r3
    7c 63 00 74     cntlzd   r3,r3
    7c 63 00 75     cntlzd.  r3,r3 *)
let cntlz ops size =
  let bits = Size.in_bits size in
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let xv = unsigned var in
  let cnt = unsigned var in
  let has_no_ones = unsigned var in
  let init = RTL.[
      xv := low size rs;
      cnt := zero;
      has_no_ones := one;
    ] in
  let foreach_bit = RTL.[
      if_ (has_no_ones land (msb (low size xv) = zero)) [
        cnt := cnt + one;
        xv := xv lsl one;
      ] [
        has_no_ones := zero;
      ];
    ] in
  let loop = List.concat @@ List.init bits ~f:(fun _ -> foreach_bit) in
  let finish = RTL.[ra := cnt] in
  init @ loop @ finish

let cntlz_dot addr_size ops size =
  cntlz ops size @ write_fixpoint_result addr_size ops.(0)

(** Fixed-point Count Trailing Zeros Word
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c 63 04 34     cnttzw   r3,r3
    7c 63 04 35     cnttzw.  r3,r3
    7c 63 04 74     cnttzd   r3,r3
    7c 63 04 75     cnttzd.  r3,r3 *)
let cnttz ops size =
  let bits = Size.in_bits size in
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let xv = unsigned var in
  let cnt = unsigned var in
  let has_no_ones = unsigned var in
  let init = RTL.[
      xv := low size rs;
      cnt := zero;
      has_no_ones := one;
    ] in
  let foreach_bit = RTL.[
      if_ (has_no_ones land (lsb xv = zero)) [
        cnt := cnt + one;
        xv := xv lsr one
      ] [
        has_no_ones := zero;
      ];
    ] in
  let loop = List.concat @@ List.init bits ~f:(fun _ -> foreach_bit) in
  let finish = RTL.[ra := cnt] in
  init @ loop @ finish

let cnttz_dot addr_size ops size =
  cnttz ops size @ write_fixpoint_result addr_size ops.(0)

(** Fixed-point Compare Bytes
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c 8a 53 f8   cmpb r10, r4, r10 *)
let cmpb ops =
  let ra = unsigned reg ops.(0) in
  let rs = unsigned reg ops.(1) in
  let rb = unsigned reg ops.(2) in
  let tm = unsigned var in
  let xb = unsigned int 0xFF in
  let foreach_byte index =
    let sh = unsigned int (index * 8) in
    RTL.[
      if_ (nbyte rs index = nbyte rb index) [
        tm := tm lor (xb lsl sh);
      ] [ ]
    ] in
  let init = RTL.[tm := zero ] in
  let loop = List.concat @@ List.init 8 ~f:foreach_byte in
  let finish = RTL.[ra := tm] in
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
let popcnt ops size =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let bits = Size.in_bits size in
  let steps = gpr_bitwidth / bits in
  let cnt = unsigned var in
  let tmp = unsigned var in
  let res = unsigned var in
  let foreach_bit reg index =
    RTL.[
      if_ (nbit reg index = one) [
        cnt := cnt + one;
      ] [];
    ] in
  let foreach_size index =
    let lo = index * bits in
    let hi = (index + 1) * bits - 1 in
    let shift = unsigned int (index * bits) in
    let init = RTL.[
      cnt := zero;
      (* tmp := extract hi lo rs; *)
    ] in
    let loop = List.concat @@ List.init bits (foreach_bit tmp) in
    let finish = RTL.[
      res := res lor (cnt lsl shift);
    ] in
    List.concat [
      init;
      loop;
      finish;
    ] in
  let init = RTL.[
    res := zero;
  ] in
  let loop = List.concat @@ List.init steps ~f:foreach_size in
  let finish = RTL.[ra := res] in
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
let parity ops size = ppc_fail "llvm doens't now about this insn"

(** Fixed-point Bit Permute Doubleword
    Pages 100 of IBM Power ISATM Version 3.0 B
    examples:
    7c a1 49 f8    bperm r1, r5, r9 *)
let bpermd ops =
  let ra = unsigned reg ops.(0) in
  let rs = unsigned reg ops.(1) in
  let rb = unsigned reg ops.(2) in
  let max_ind = unsigned int 64 in
  let index = unsigned var in
  let iv = unsigned var in
  let tmp = unsigned var in
  let bit = unsigned var in
  let init = RTL.[
      tmp := zero;
    ] in
  let foreach_byte i =
    let hi = (i + 1) * 8 - 1 in
    let lo = i * 8 in
    let i = unsigned int i in
    RTL.[
      iv := i;
      (* index := extract hi lo rs; *)
      if_ (index < max_ind) [
        bit := lsb (rb lsr index);
      ] [
        bit := zero;
      ];
      tmp := tmp lor (bit lsl iv);
    ] in
  let loop = List.concat @@ List.init 8 ~f:foreach_byte in
  let finish = RTL.[
      ra := low byte tmp;
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

let lift t cpu ops = match t with
  | `ANDIo   -> andi_dot cpu.addr_size ops
  | `ANDISo  -> andis_dot cpu.addr_size ops
  | `AND     -> and_ ops
  | `ANDo    -> and_dot cpu.addr_size ops
  | `ANDC    -> andc ops
  | `ANDCo   -> andc_dot cpu.addr_size ops
  | `ORI     -> ori ops
  | `ORIS    -> oris ops
  | `OR      -> or_ ops
  | `ORo     -> or_dot cpu.addr_size ops
  | `ORC     -> orc ops
  | `ORCo    -> orc_dot cpu.addr_size ops
  | `XORI    -> xori ops
  | `XORIS   -> xoris ops
  | `XOR     -> xor_ ops
  | `XORo    -> xor_dot cpu.addr_size ops
  | `NAND    -> nand ops
  | `NANDo   -> nand_dot cpu.addr_size ops
  | `NOR     -> nor ops
  | `NORo    -> nor_dot cpu.addr_size ops
  | `EQV     -> eqv ops
  | `EQVo    -> eqv_dot cpu.addr_size ops
  | `EXTSB   -> exts ops `r8
  | `EXTSBo  -> exts_dot cpu.addr_size ops `r8
  | `EXTSH   -> exts ops `r16
  | `EXTSHo  -> exts_dot cpu.addr_size ops `r16
  | `EXTSW   -> exts ops `r32
  | `EXTSWo  -> exts_dot cpu.addr_size ops `r32
  | `CNTLZW  -> cntlz ops `r32
  | `CNTLZWo -> cntlz_dot cpu.addr_size ops `r32
  | `CNTLZD  -> cntlz ops `r64
  | `CNTLZDo -> cntlz_dot cpu.addr_size ops `r64
  | `CNTTZW  -> cnttz ops `r32
  | `CNTTZWo -> cnttz_dot cpu.addr_size ops `r32
  | `CNTTZD  -> cnttz ops `r64
  | `CNTTZDo -> cnttz_dot cpu.addr_size ops `r64
  | `CMPB    -> cmpb ops
  | `POPCNTW -> popcnt ops `r32
  | `POPCNTB -> popcnt ops `r8
  | `POPCNTD -> popcnt ops `r64
  | `BPERMD  -> bpermd ops
