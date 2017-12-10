
open Powerpc

(** Fixed-Point Arithmetic Instructions - Substract From
    Page 69 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 18 50    subf r1, r2, r3 *)
let subf cpu ops =
  let rt = unsigned reg ops.(0) in
  let ra = unsigned reg ops.(1) in
  let rb = unsigned reg ops.(2) in
  RTL.[rt := (lnot ra) + rb + one]

(** Fixed-Point Arithmetic Instructions - Substract From Immediate Carrying
    Page 70 of IBM Power ISATM Version 3.0 B
    example:
    20 22 10 92    subfic r1, r2, 4242 *)
(** TODO: carry flags  *)
let subfic cpu ops =
  let rt = unsigned reg ops.(0) in
  let ra = unsigned reg ops.(1) in
  let si = unsigned imm ops.(2) in
  RTL.[rt := (lnot ra) + si + one]

(** Fixed-Point Arithmetic Instructions - Substract From Carrying
    Page 70 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 18 10    subfc r1, r2, r3 *)
(** TODO: carry flags  *)
let subfc cpu ops =
  let rt = signed reg ops.(0) in
  let ra = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[rt := (lnot ra) + rb + one]

(** Fixed-Point Arithmetic Instructions - Substract From Extended
    Page 71 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 19 10    subfe r1, r2, r3 *)
(** TODO: carry flags  *)
let subfe cpu ops =
  let rt = signed reg ops.(0) in
  let ra = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[rt := (lnot ra) + rb + cpu.ca]

(** Fixed-Point Arithmetic Instructions - Substract From Minus One Extended
    Page 71 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 01 d0    subfme r1, r2 *)
(** TODO: carry flags  *)
let subfme cpu ops =
  let rt = signed reg ops.(0) in
  let ra = signed reg ops.(1) in
  RTL.[rt := (lnot ra) + cpu.ca - one]

(** Fixed-Point Arithmetic Instructions - Substract From Zero Extended
    Page 71 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 01 90    subfze r1, r2 *)
(** TODO: carry flags  *)
let subfze cpu ops =
  let rt = signed reg ops.(0) in
  let ra = signed reg ops.(1) in
  RTL.[rt := (lnot ra) + cpu.ca]

(** Fixed-Point Arithmetic Instructions - Multiply Low Immediate
    Page 73 of IBM Power ISATM Version 3.0 B
    example:
    1c 22 00 2a    mulli r1, r2, 42 *)
let mulli cpu ops =
  let rt = signed reg ops.(0) in
  let ra = signed reg ops.(1) in
  let si = signed imm ops.(2) in
  RTL.[rt :=  ra *  si]

(** Fixed-Point Arithmetic Instructions - Multiply High Word
    Page 73 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 18 96    mulhw r1, r2, r3 *)
let mulhw cpu ops =
  let rt = signed reg ops.(0) in
  let ra = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  let tmp1 = signed var doubleword in
  let tmp2 = signed var doubleword in
  RTL.[
    tmp1 := low word ra;
    tmp2 := low word rb;
    low word rt :=  high word (tmp1 * tmp2);
  ]

(** Fixed-Point Arithmetic Instructions - Multiply High Word Unsigned
    Page 73 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 18 16    mulhwu r1, r2, r3 *)
let mulhwu cpu ops =
  let rt = unsigned reg ops.(0) in
  let ra = unsigned reg ops.(1) in
  let rb = unsigned reg ops.(2) in
  let tmp1 = unsigned var doubleword in
  let tmp2 = unsigned var doubleword in
  RTL.[
    tmp1 := low word ra;
    tmp2 := low word rb;
    low word rt :=  high word (tmp1 * tmp2);
  ]

(** Fixed-Point Arithmetic Instructions - Multiply Low Word
    Page 73 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 19 d6   mullw r1, r2, r3 *)
let mullw cpu ops =
  let rt = signed reg ops.(0) in
  let ra = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  let tmp1 = unsigned var doubleword in
  let tmp2 = unsigned var doubleword in
  RTL.[
    tmp1 := low word ra;
    tmp2 := low word rb;
    rt :=  tmp1 *  tmp2;
  ]

(** Fixed-Point Arithmetic Instructions - Divide Word
    Page 74 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 1b d6    divw r1, r2, r3 *)
let divw cpu ops =
  let rt = signed reg ops.(0) in
  let ra = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[
    low word rt := low word ra /$ low word rb;
  ]

(** Fixed-Point Arithmetic Instructions - Divide Word Unsigned
    Page 74 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 1b 96    divwu r1, r2, r3 *)
let divwu cpu ops =
  let rt = unsigned reg ops.(0) in
  let ra = unsigned reg ops.(1) in
  let rb = unsigned reg ops.(2) in
  RTL.[
    low word rt :=  low word ra / low word rb;
  ]

(** Fixed-Point Arithmetic Instructions - Divide Word Extended
    Page 75 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 1b 56    divwe r1, r2, r3 *)
let divwe cpu ops =
  let rt = signed reg ops.(0) in
  let ra = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  let x = signed var doubleword in
  RTL.[
    x := zero;
    high word x := low word ra;
    low word rt :=  x /$ low word rb;
  ]

(** Fixed-Point Arithmetic Instructions - Divide Word Extended Unsigned
    Page 75 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 1b 16    divweu r1, r2, r3 *)
let divweu cpu ops =
  let rt = unsigned reg ops.(0) in
  let ra = unsigned reg ops.(1) in
  let rb = unsigned reg ops.(2) in
  let x = unsigned var doubleword in
  RTL.[
    x := zero;
    high word x := low word ra;
    low word rt :=  x / low word rb;
  ]

type t = [
  | `SUBF
  | `SUBFIC
  | `SUBFC
  | `SUBFE
  | `SUBFME
  | `SUBFZE
  | `MULLI
  | `MULHW
  | `MULHWU
  | `MULLW
  | `DIVW
  | `DIVWU
  | `DIVWE
  | `DIVWEU
] [@@deriving sexp, enumerate]

let lift t cpu ops = match t with
  | `SUBF   -> subf   cpu ops
  | `SUBFIC -> subfic cpu ops
  | `SUBFC  -> subfc  cpu ops
  | `SUBFE  -> subfe  cpu ops
  | `SUBFME -> subfme cpu ops
  | `SUBFZE -> subfze cpu ops
  | `MULLI -> mulli cpu ops
  | `MULHW -> mulhw cpu ops
  | `MULHWU-> mulhwu cpu ops
  | `MULLW -> mullw cpu ops
  | `DIVW -> divw cpu ops
  | `DIVWU -> divwu cpu ops
  | `DIVWE -> divwe cpu ops
  | `DIVWEU -> divweu cpu ops
