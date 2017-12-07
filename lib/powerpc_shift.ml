open Powerpc

(** Fix-point Shift Left Word
    Page 107 of IBM Power ISATM Version 3.0 B
    example:
    7d 2a 58 30     slw r10, r9, r11 *)
let slw cpu ops =
  let ra = unsigned reg ops.(0) in
  let rs = unsigned reg ops.(1) in
  let rb = unsigned reg ops.(2) in
  RTL.[
    if_ (nth bit rb 58 = zero) [
      ra := nth word rs 1 lsl last rb 5;
    ] [
      ra := zero;
    ]
  ]

(** Fix-point Shift Right Word
    Page 107 of IBM Power ISATM Version 3.0 B
    example:
    7d 2a 5c 30     srw r10, r9, r11 *)
let srw cpu ops =
  let ra = unsigned reg ops.(0) in
  let rs = unsigned reg ops.(1) in
  let rb = unsigned reg ops.(2) in
  RTL.[
    if_ (nth bit rb 58 = zero) [
      ra := nth word rs 1 lsr last rb 5;
    ] [
      ra := zero;
    ]
  ]

(** Fix-point Shift Right Algebraic Word Immediate
    Page 107 of IBM Power ISATM Version 3.0 B
    example:
    7d 2a 5e 70     srawi r10, r9, 11 *)
let srawi cpu ops =
  let ra = signed reg ops.(0) in
  let rs = unsigned reg ops.(1) in
  let sh = unsigned imm ops.(2) in
  RTL.[
    if_ (nth bit rs 31 = zero) [
      ra := nth word rs 1 lsr last sh 5;
    ] [

    ]
    (* cpu.ca := ; *)
    (* cpu.ca32 := ; *)
  ]

type t = [
  | `SLW
  | `SRW
  | `SRAWI
] [@@deriving sexp, enumerate]

let lift opcode cpu ops =
  match opcode with
  | `SLW   -> slw cpu ops
  | `SRW   -> srw cpu ops
  | `SRAWI -> srawi cpu ops
