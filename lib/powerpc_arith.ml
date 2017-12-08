
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

type t = [
  | `SUBF
  | `SUBFIC
  | `SUBFC
  | `SUBFE
  | `SUBFME
  | `SUBFZE
] [@@deriving sexp, enumerate]

let lift t cpu ops = match t with
  | `SUBF   -> subf   cpu ops
  | `SUBFIC -> subfic cpu ops
  | `SUBFC  -> subfc  cpu ops
  | `SUBFE  -> subfe  cpu ops
  | `SUBFME -> subfme cpu ops
  | `SUBFZE -> subfze cpu ops
