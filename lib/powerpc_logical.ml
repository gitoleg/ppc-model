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
let andi_dot cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let im = unsigned imm ops.(2) in
  RTL.[
    ra := rs land im;
    nth bit cr 0 := low cpu.addr_size ra <$ zero;
    nth bit cr 1 := low cpu.addr_size ra >$ zero;
    nth bit cr 2 := low cpu.addr_size ra = zero
  ]

(** Fixed-point AND Immediate Shifted
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    75 2a 08 00     andis.  r10,r9,2048 *)
let andis_dot cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let im = unsigned imm ops.(2) in
  let sh = unsigned const byte 16 in
  RTL.[
    ra := rs land (im lsl sh);
    nth bit cr 0 := low cpu.addr_size ra <$ zero;
    nth bit cr 1 := low cpu.addr_size ra >$ zero;
    nth bit cr 2 := low cpu.addr_size ra = zero;
  ]

(** Fixed-point AND
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7f 39 e8 38     and     r25,r25,r29
    7d 49 30 39     and.    r9,r10,r6 *)
let and_ cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[ ra := rs land rb; ]

let and_dot cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[
    ra := rs land rb;
    nth bit cr 0 := low cpu.addr_size ra <$ zero;
    nth bit cr 1 := low cpu.addr_size ra >$ zero;
    nth bit cr 2 := low cpu.addr_size ra = zero;
  ]

(** Fixed-point AND with Complement
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c ea 50 78     andc    r10,r7,r10
    7e 09 18 79     andc.   r9,r16,r3  *)
let andc cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[ ra := rs land (lnot rb); ]

let andc_dot cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[
    ra := rs land (lnot rb);
    nth bit cr 0 := low cpu.addr_size ra <$ zero;
    nth bit cr 1 := low cpu.addr_size ra >$ zero;
    nth bit cr 2 := low cpu.addr_size ra = zero;
  ]

(** Fixed-point OR Immediate
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    60 c6 51 c1     ori     r6,r6,20929 *)
let ori cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let im = unsigned imm ops.(2) in
  RTL.[ ra := rs lor im; ]

(** Fixed-point OR Immediate Shifted
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    65 4a 00 10     oris    r10,r10,16 *)
let oris cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let im = unsigned imm ops.(2) in
  let sh = unsigned const byte 16 in
  RTL.[ ra := rs lor (im lsl sh); ]

(** Fixed-point OR
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7f 38 c3 78     or      r24,r25,r24
    7d 0a 4b 79     or.     r10,r8,r9  *)
let or_ cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[ ra := rs lor rb; ]

let or_dot cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[
    ra := rs lor rb;
    nth bit cr 0 := low cpu.addr_size ra <$ zero;
    nth bit cr 1 := low cpu.addr_size ra >$ zero;
    nth bit cr 2 := low cpu.addr_size ra = zero;
  ]

(** Fixed-point OR with Complement
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c 8a 53 38     orc     r10,r4,r10
    7c 8a 53 39     orc.    r10,r4,r10 *)
let orc cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[ ra := rs lor (lnot rb); ]

let orc_dot cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[
    ra := rs lor (lnot rb);
    nth bit cr 0 := low cpu.addr_size ra <$ zero;
    nth bit cr 1 := low cpu.addr_size ra >$ zero;
    nth bit cr 2 := low cpu.addr_size ra = zero;
  ]


(** Fixed-point XOR Immediate
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    68 63 00 01     xori    r3,r3,1 *)
let xori cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let im = unsigned imm ops.(2) in
  RTL.[ ra := rs lxor im; ]

(** Fixed-point XOR Immediate Shifted
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    6d 2a 04 00     xoris   r10,r9,1024
 *)
let xoris cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let im = unsigned imm ops.(2) in
  let sh = unsigned const byte 16 in
  RTL.[ ra := rs lxor (im lsl sh); ]

(** Fixed-point XOR
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c 6a 52 78     xor     r10,r3,r10
    7d 4a 4a 79     xor.    r10,r10,r9 *)
let xor_ cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[ ra := rs lxor rb; ]

let xor_dot cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[
    ra := rs lxor rb;
    nth bit cr 0 := low cpu.addr_size ra <$ zero;
    nth bit cr 1 := low cpu.addr_size ra >$ zero;
    nth bit cr 2 := low cpu.addr_size ra = zero;
  ]

(** Fixed-point NAND
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c 63 1b b8     nand    r3,r3,r3
    7c 63 1b b9     nand.   r3,r3,r3 *)
let nand cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[ ra := lnot (rs land rb); ]

let nand_dot cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[
    ra := lnot (rs land rb);
    nth bit cr 0 := low cpu.addr_size ra <$ zero;
    nth bit cr 1 := low cpu.addr_size ra >$ zero;
    nth bit cr 2 := low cpu.addr_size ra = zero;
  ]


(** Fixed-point NOR
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7d 09 48 f8     nor     r9,r8,r9
    7d 09 48 f9     nor.    r9,r8,r9  *)
let nor cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[ ra := lnot (rs lor rb); ]

let nor_dot cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[
    ra := lnot (rs lor rb);
    nth bit cr 0 := low cpu.addr_size ra <$ zero;
    nth bit cr 1 := low cpu.addr_size ra >$ zero;
    nth bit cr 2 := low cpu.addr_size ra = zero;
  ]

(** Fixed-point Equivalent
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7d 09 4a 38     eqv     r9,r8,r9
    7d 09 4a 39     eqv.    r9,r8,r9 *)
let eqv cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[ ra := lnot (rs lxor rb); ]

let eqv_dot cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[
    ra := lnot (rs lxor rb);
    nth bit cr 0 := low cpu.addr_size ra <$ zero;
    nth bit cr 1 := low cpu.addr_size ra >$ zero;
    nth bit cr 2 := low cpu.addr_size ra = zero;
  ]

(** Fixed-point Extend Sign Byte/Halfword/Word
    Pages 92-99 of IBM Power ISATM Version 3.0 B
    examples:
    7d 4a 07 74     extsb   r10,r10
    7d 48 07 75     extsb.  r8,r10
    7d 25 07 34     extsh   r5,r9
    7d 25 07 35     extsh.  r5,r9
    7d 25 07 b4     extsw   r5,r9
    7d 25 07 b5     extsw.  r5,r9 *)
let extsb cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  RTL.[ ra := low byte rs;]

let extsb_dot cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  RTL.[
    ra := low byte rs;
    nth bit cr 0 := low cpu.addr_size ra <$ zero;
    nth bit cr 1 := low cpu.addr_size ra >$ zero;
    nth bit cr 2 := low cpu.addr_size ra = zero;
  ]

let extsh cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  RTL.[ ra := low halfword rs;]

let extsh_dot cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  RTL.[
    ra := low halfword rs;
    nth bit cr 0 := low cpu.addr_size ra <$ zero;
    nth bit cr 1 := low cpu.addr_size ra >$ zero;
    nth bit cr 2 := low cpu.addr_size ra = zero;
  ]

let extsw cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  RTL.[ ra := low word rs;]

let extsw_dot cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  RTL.[
    ra := low word rs;
    nth bit cr 0 := low cpu.addr_size ra <$ zero;
    nth bit cr 1 := low cpu.addr_size ra >$ zero;
    nth bit cr 2 := low cpu.addr_size ra = zero;
  ]

(** Fixed-point Count Leading Zeros Word/Doubleword
    Pages 92-99 of IBM Power ISATM Version 3.0 B
    examples:
    7c 63 00 34     cntlzw   r3,r3
    7c 63 00 35     cntlzw.  r3,r3
    7c 63 00 74     cntlzd   r3,r3
    7c 63 00 75     cntlzd.  r3,r3 *)
let cntlzw cpu ops =
  let ra = unsigned reg ops.(0) in
  let rs = unsigned reg ops.(1) in
  let xv = unsigned var word in
  let cnt = unsigned var byte in
  let has_no_ones = unsigned var bit in
  let biti = unsigned var bit in
  RTL.[
    xv := low word rs;
    cnt := zero;
    has_no_ones := one;
    foreach biti xv [
          if_ (has_no_ones land (biti = zero)) [
            cnt := cnt + one;
          ] [
            has_no_ones := zero;
          ]
        ];
    ra := cnt;
  ]

let cntlzw_dot cpu ops =
  let ra = unsigned reg ops.(0) in
  let rs = unsigned reg ops.(1) in
  let xv = unsigned var word in
  let cnt = unsigned var byte in
  let has_no_ones = unsigned var bit in
  let biti = unsigned var bit in
  RTL.[
    xv := low word rs;
    cnt := zero;
    has_no_ones := one;
    foreach biti xv [
          if_ (has_no_ones land (biti = zero)) [
            cnt := cnt + one;
          ] [
            has_no_ones := zero;
          ]
        ];
    ra := cnt;
    nth bit cr 0 := low cpu.addr_size ra <$ zero;
    nth bit cr 1 := low cpu.addr_size ra >$ zero;
    nth bit cr 2 := low cpu.addr_size ra = zero;
  ]

let cntlzd cpu ops =
  let ra = unsigned reg ops.(0) in
  let rs = unsigned reg ops.(1) in
  let xv = unsigned var doubleword in
  let cnt = unsigned var byte in
  let has_no_ones = unsigned var bit in
  let biti = unsigned var bit in
  RTL.[
    xv := rs;
    cnt := zero;
    has_no_ones := one;
    foreach biti xv [
          if_ (has_no_ones land (biti = zero)) [
            cnt := cnt + one;
          ] [
            has_no_ones := zero;
          ]
        ];
    ra := cnt;
  ]

let cntlzd_dot cpu ops =
  let ra = unsigned reg ops.(0) in
  let rs = unsigned reg ops.(1) in
  let xv = unsigned var doubleword in
  let cnt = unsigned var byte in
  let has_no_ones = unsigned var bit in
  let biti = unsigned var bit in
  RTL.[
    xv := rs;
    cnt := zero;
    has_no_ones := one;
    foreach biti xv [
          if_ (has_no_ones land (biti = zero)) [
            cnt := cnt + one;
          ] [
            has_no_ones := zero;
          ]
        ];
    ra := cnt;
    nth bit cr 0 := low cpu.addr_size ra <$ zero;
    nth bit cr 1 := low cpu.addr_size ra >$ zero;
    nth bit cr 2 := low cpu.addr_size ra = zero;
  ]

(** Fixed-point Count Trailing Zeros Word
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c 63 04 34     cnttzw   r3,r3
    7c 63 04 35     cnttzw.  r3,r3
    7c 63 04 74     cnttzd   r3,r3
    7c 63 04 75     cnttzd.  r3,r3 *)
let cnttzw cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let xv = unsigned var word in
  let cnt = unsigned var word in
  let biti = unsigned var bit in
  RTL.[
    xv := low word rs;
    cnt := zero;
    foreach biti xv [
      if_ (biti = zero) [
        cnt := cnt + one;
      ] [
        cnt := zero;
      ]
    ];
    ra := cnt;
  ]

let cnttzw_dot cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let xv = unsigned var word in
  let cnt = unsigned var word in
  let biti = unsigned var bit in
  RTL.[
    xv := low word rs;
    cnt := zero;
    foreach biti xv [
      if_ (biti = zero) [
        cnt := cnt + one;
      ] [
        cnt := zero;
      ]
    ];
    ra := cnt;
    nth bit cr 0 := low cpu.addr_size ra <$ zero;
    nth bit cr 1 := low cpu.addr_size ra >$ zero;
    nth bit cr 2 := low cpu.addr_size ra = zero;
  ]

let cnttzd cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let xv = unsigned var doubleword in
  let cnt = unsigned var word in
  let biti = unsigned var bit in
  RTL.[
    xv := rs;
    cnt := zero;
    foreach biti xv [
      if_ (biti = zero) [
        cnt := cnt + one;
      ] [
        cnt := zero;
      ]
    ];
    ra := cnt;
  ]

let cnttzd_dot cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let xv = unsigned var doubleword in
  let cnt = unsigned var word in
  let biti = unsigned var bit in
  RTL.[
    xv := rs;
    cnt := zero;
    foreach biti xv [
      if_ (biti = zero) [
        cnt := cnt + one;
      ] [
        cnt := zero;
      ]
    ];
    ra := cnt;
    nth bit cr 0 := low cpu.addr_size ra <$ zero;
    nth bit cr 1 := low cpu.addr_size ra >$ zero;
    nth bit cr 2 := low cpu.addr_size ra = zero;
  ]

(** Fixed-point Compare Bytes
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c 8a 53 f8   cmpb r10, r4, r10 *)
let cmpb cpu ops =
  let ra = unsigned reg ops.(0) in
  let rs = unsigned reg ops.(1) in
  let rb = unsigned reg ops.(2) in
  let xb = unsigned const doubleword 0xFF in
  let ind = unsigned var byte in
  let byte_i = unsigned var byte in
  let byte_j = unsigned var byte in
  let max = unsigned const byte 7 in
  let sh = unsigned const byte 8 in
  let tmp = unsigned var doubleword in
  RTL.[
    ind := zero;
    tmp := zero;
    foreach byte_i rs [
      byte_j := nth byte (rb lsl (ind * sh)) 0;
      when_ (byte_i = byte_j) [
        tmp := tmp lor (xb lsl ((max - ind) * sh));
      ];
      ind := ind + one;
    ];
    ra := tmp;
  ]

(** Fixed-point Population Count Bytes/Words/Doubleword
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c 84 00 f4       popcntb r4, r4 (not working in llvm)
    7c 84 02 f4       popcntw r4, r4
    7c 84 03 f4       popcntd r4, r4 *)
let popcntw cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let cnt = unsigned var doubleword in
  let res = unsigned var doubleword in
  let word_i = unsigned var word in
  let bit_i = unsigned var bit in
  let ind = unsigned var word in
  let x = unsigned const byte 32 in
  RTL.[
    res := zero;
    ind := one;
    foreach word_i rs [
      cnt := zero;
      foreach bit_i word_i [
        when_ (bit_i = one) [
          cnt := cnt + one;
        ];
      ];
      res := res lor (cnt lsl (ind * x));
      ind := ind - one;
    ];
    ra := res;
  ]

let popcntd cpu ops =
  let ra = signed reg ops.(0) in
  let rs = signed reg ops.(1) in
  let cnt = unsigned var doubleword in
  let bit_i = unsigned var bit in
  RTL.[
    cnt := zero;
    foreach bit_i rs [
      when_ (bit_i = one) [
        cnt := cnt + one;
      ];
    ];
    ra := cnt;
  ]

(** Fixed-point Parity Doubleword/Word
    Pages 92-98 of IBM Power ISATM Version 3.0 B
    examples:
    7c 84 01 74     prtyd r4, r4 (not working in llvm)
    7c 84 01 34     prtyw r4, r4 (not working in llvm)    *)
let parity ops size = ppc_fail "llvm disasm failed"


(** Fixed-point Bit Permute Doubleword
    Pages 100 of IBM Power ISATM Version 3.0 B
    examples:
    7c a1 49 f8    bperm r1, r5, r9 *)
let bpermd cpu ops =
  let ra = unsigned reg ops.(0) in
  let rs = unsigned reg ops.(1) in
  let rb = unsigned reg ops.(2) in
  let max_ind = unsigned const byte 64 in
  let tmp = unsigned var byte in
  let bit_index = unsigned var byte in
  let ind = unsigned var byte in
  let x = unsigned var doubleword in
  let max = unsigned const byte 7 in
  RTL.[
    tmp := zero;
    ind := max;
    foreach bit_index rs [
      if_ (bit_index < max_ind) [
        x := msb (rb lsl bit_index);
      ] [
        x := zero;
      ];
      tmp := tmp lor (x lsl ind);
      ind := ind - one;
    ];
    ra := tmp;
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
  | `POPCNTW
  | `POPCNTD
] [@@deriving sexp,enumerate]

type bperm = [ `BPERMD ] [@@deriving sexp,enumerate]

type t = [ and_ | or_ | xor | eqv | exts | cntz | cmpb | popcnt | bperm ] [@@deriving sexp,enumerate]

let lift t cpu ops = match t with
  | `ANDIo   -> andi_dot cpu ops
  | `ANDISo  -> andis_dot cpu ops
  | `AND     -> and_ cpu ops
  | `ANDo    -> and_dot cpu ops
  | `ANDC    -> andc cpu ops
  | `ANDCo   -> andc_dot cpu ops
  | `ORI     -> ori cpu ops
  | `ORIS    -> oris cpu ops
  | `OR      -> or_ cpu ops
  | `ORo     -> or_dot cpu ops
  | `ORC     -> orc cpu ops
  | `ORCo    -> orc_dot cpu ops
  | `XORI    -> xori cpu ops
  | `XORIS   -> xoris cpu ops
  | `XOR     -> xor_ cpu ops
  | `XORo    -> xor_dot cpu ops
  | `NAND    -> nand cpu ops
  | `NANDo   -> nand_dot cpu ops
  | `NOR     -> nor cpu ops
  | `NORo    -> nor_dot cpu ops
  | `EQV     -> eqv cpu ops
  | `EQVo    -> eqv_dot cpu ops
  | `EXTSB   -> extsb cpu ops
  | `EXTSBo  -> extsb_dot cpu ops
  | `EXTSH   -> extsh cpu ops
  | `EXTSHo  -> extsh_dot cpu ops
  | `EXTSW   -> extsw cpu ops
  | `EXTSWo  -> extsw_dot cpu ops
  | `CNTLZW  -> cntlzw cpu ops
  | `CNTLZWo -> cntlzw_dot cpu ops
  | `CNTLZD  -> cntlzd cpu ops
  | `CNTLZDo -> cntlzd_dot cpu ops
  | `CNTTZW  -> cnttzw cpu ops
  | `CNTTZWo -> cnttzw_dot cpu ops
  | `CNTTZD  -> cnttzd cpu ops
  | `CNTTZDo -> cnttzd_dot cpu ops
  | `CMPB    -> cmpb cpu ops
  | `POPCNTW -> popcntw cpu ops
  | `POPCNTD -> popcntd cpu ops
  | `BPERMD  -> bpermd cpu ops
  | _ -> ppc_fail "oops"
