open Core_kernel.Std
open Bap.Std

open Powerpc_types
open Hardware
open Dsl

let lt = unsigned const byte 4
let gt = unsigned const byte 2
let eq = unsigned const byte 1

(** Fix-point Compare Immediate
    Page 85 of IBM Power ISATM Version 3.0 B
    examples:
    2f 89 ff ff     cmpwi cr7, r9, -1
    2f a9 ff ff     cmpdi cr7, r9, -1 *)
let cmpwi cpu ops =
  let bf = unsigned reg ops.(0) in
  let ra = signed reg ops.(1) in
  let si = signed imm ops.(2) in
  let tm = unsigned var byte in
  RTL.[
    if_ (low word ra <$ si) [
      tm := lt;
    ] [
      if_ (low word ra >$ si) [
        tm := gt;
      ] [
        tm := eq;
      ]
    ];
    extract bf 0 2 := tm;
    nth bit bf 3 := so;
  ]

let cmpdi cpu ops =
  let bf = unsigned reg ops.(0) in
  let ra = signed reg ops.(1) in
  let si = signed imm ops.(2) in
  let tm = unsigned var byte in
  RTL.[
    if_ (ra <$ si) [
      tm := lt;
    ] [
      if_ (ra >$ si) [
        tm := gt;
      ] [
        tm := eq;
      ]
    ];
    extract bf 0 2 := tm;
    nth bit bf 3 := so;
  ]

(** Fix-point Compare
    Page 85 of IBM Power ISATM Version 3.0 B
    examples:
    7f 86 38 00     cmpw cr7, r6, r7
    7f a6 38 00     cmpd cr7, r6, r7 *)
let cmpw cpu ops =
  let bf = unsigned reg ops.(0) in
  let ra = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  let tm = unsigned var byte in
  RTL.[
    if_ (low word ra <$ low word rb) [
      tm := lt;
    ] [
      if_ (low word ra >$ low word rb) [
        tm := gt;
      ] [
        tm := eq;
      ]
    ];
    extract bf 0 2 := tm;
    nth bit bf 3 := so;
  ]

let cmpd cpu ops =
  let bf = unsigned reg ops.(0) in
  let ra = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  let tm = unsigned var byte in
  RTL.[
    if_ (ra <$ rb) [
      tm := lt;
    ] [
      if_ (ra >$ rb) [
        tm := gt;
      ] [
        tm := eq;
      ]
    ];
    extract bf 0 2 := tm;
    nth bit bf 3 := so;
  ]

(** Fix-point Compare Logical Immediate
    Page 86 of IBM Power ISATM Version 3.0 B
    examples:
    2b 89 00 01     cmplwi cr7, r9, 1
    2b a9 00 01     cmpldi cr7, r9, 1 *)
let cmplwi cpu ops =
  let bf = unsigned reg ops.(0) in
  let ra = unsigned reg ops.(1) in
  let ui = unsigned imm ops.(2) in
  let tm = unsigned var byte in
  RTL.[
    if_ (low word ra < ui) [
      tm := lt;
    ] [
      if_ (low word ra > ui) [
        tm := gt;
      ] [
        tm := eq;
      ]
    ];
    extract bf 0 2 := tm;
    nth bit bf 3 := so;
  ]

let cmpldi cpu ops =
  let bf = unsigned reg ops.(0) in
  let ra = unsigned reg ops.(1) in
  let ui = unsigned imm ops.(2) in
  let tm = unsigned var byte in
  RTL.[
    if_ (ra < ui) [
      tm := lt;
    ] [
      if_ (ra > ui) [
        tm := gt;
      ] [
        tm := eq;
      ]
    ];
    extract bf 0 2 := tm;
    nth bit bf 3 := so;
  ]

(** Fix-point Compare Logical
    Page 86 of IBM Power ISATM Version 3.0 B
    examples:
    7f 86 38 40     cmplw cr7, r6, r7
    7f a6 38 40     cmpld cr7, r6, r7 *)
let cmplw cpu ops =
  let bf = unsigned reg ops.(0) in
  let ra = unsigned reg ops.(1) in
  let rb = unsigned reg ops.(2) in
  let tm = unsigned var byte in
  RTL.[
    if_ (low word ra < low word rb) [
      tm := lt;
    ] [
      if_ (low word ra > low word rb) [
        tm := gt;
      ] [
        tm := eq;
      ]
    ];
    extract bf 0 2 := tm;
    nth bit bf 3 := so;
  ]

let cmpld cpu ops =
  let bf = unsigned reg ops.(0) in
  let ra = unsigned reg ops.(1) in
  let rb = unsigned reg ops.(2) in
  let tm = unsigned var byte in
  RTL.[
    if_ (ra < rb) [
      tm := lt;
    ] [
      if_ (ra > rb) [
        tm := gt;
      ] [
        tm := eq;
      ]
    ];
    extract bf 0 2 := tm;
    nth bit bf 3 := so;
  ]

type t = [
  | `CMPWI
  | `CMPDI
  | `CMPW
  | `CMPD
  | `CMPLWI
  | `CMPLDI
  | `CMPLW
  | `CMPLD
] [@@deriving sexp, enumerate]

let lift opcode cpu ops =
  let open Op in
  match opcode with
  | `CMPWI  -> cmpwi cpu ops
  | `CMPDI  -> cmpdi cpu ops
  | `CMPW   -> cmpw cpu ops
  | `CMPD   -> cmpd cpu ops
  | `CMPLWI -> cmplwi cpu ops
  | `CMPLDI -> cmpldi cpu ops
  | `CMPLW  -> cmplw cpu ops
  | `CMPLD  -> cmpld cpu ops
