open Core_kernel.Std
open Bap.Std
open Op

open Powerpc_types
open Hardware
open Dsl

(** Fixed-point Store Byte/Halfword/Word
    Pages 54-56 of IBM Power ISATM Version 3.0 B
    examples:
    99 3c 01 6c     stb r9,364(r28)
    99 20 01 6C     stb r9,364(0)
    b1 3c 01 6c     sth r9,364(r28)
    91 28 ff d4     stw r9,-44(r8) *)
let stb cpu ops =
  let rs = signed reg ops.(0) in
  let im = signed imm ops.(1) in
  let ra = signed reg ops.(2) in
  RTL.[ cpu.store (ra + im) rs byte; ]

let sth cpu ops =
  let rs = signed reg ops.(0) in
  let im = signed imm ops.(1) in
  let ra = signed reg ops.(2) in
  RTL.[ cpu.store (ra + im) rs halfword; ]

let stw cpu ops =
  let rs = signed reg ops.(0) in
  let im = signed imm ops.(1) in
  let ra = signed reg ops.(2) in
  RTL.[ cpu.store (ra + im) rs word; ]

(** Fixed-point Store Byte/Halfword/Word/Doubleword Indexed
    Pages 54-57 of IBM Power ISATM Version 3.0 B
    examples:
    7d 2e f9 ae     stbx    r9,r14,r31
    7d 3e eb 2e     sthx    r9,r30,r29
    7f b6 f9 2e     stwx    r29,r22,r31
    7c 28 49 2a     stdx    r1, r8, r9  *)
let stbx cpu ops =
  let rs = signed reg ops.(0) in
  let ra = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[ cpu.store (ra + rb) rs byte; ]

let sthx cpu ops =
  let rs = signed reg ops.(0) in
  let ra = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[ cpu.store (ra + rb) rs halfword; ]

let stwx cpu ops =
  let rs = signed reg ops.(0) in
  let ra = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[ cpu.store (ra + rb) rs word; ]

let stdx cpu ops =
  let rs = signed reg ops.(0) in
  let ra = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[ cpu.store (ra + rb) rs doubleword; ]

(** fixed-point Store Byte/Halfword/Word with Update
    Pages 54-56 of IBM Power ISATM Version 3.0 B
    examples:
    9c 9d ff ff     stbu r4,-1(r29)
    b5 3d ff ff     sthu r9,-1(r29)
    94 21 ff f0     stwu r1,-16(r1)  *)
let stbu cpu ops =
  (* if Reg.equal rs ra then ppc_fail "Invalid instruction szu: same operands"; *)
  let rs = signed reg ops.(1) in
  let im = signed imm ops.(2) in
  let ra = signed reg ops.(3) in
  RTL.[
    cpu.store (ra + im) rs byte;
    ra := ra + im;
  ]

let sthu cpu ops =
  (* if Reg.equal rs ra then ppc_fail "Invalid instruction szu: same operands"; *)
  let rs = signed reg ops.(1) in
  let im = signed imm ops.(2) in
  let ra = signed reg ops.(3) in
  RTL.[
    cpu.store (ra + im) rs halfword;
    ra := ra + im;
  ]

let stwu cpu ops =
  (* if Reg.equal rs ra then ppc_fail "Invalid instruction szu: same operands"; *)
  let rs = signed reg ops.(1) in
  let im = signed imm ops.(2) in
  let ra = signed reg ops.(3) in
  RTL.[
    cpu.store (ra + im) rs word;
    ra := ra + im;
  ]

(** Fixed-point Store Byte/Halfword/Word/Doubleword with Update Indexed
    Pages 54-57 of IBM Power ISATM Version 3.0 B
    examples:
    7d 3f c9 ee     stbux r9,r31,r25
    7d 3f cb 6e     sthux r9,r31,r25
    7d 41 49 6e     stwux r10,r1,r9
    7c 28 49 6a     stdux r1,r8,r9   *)
let stbux cpu ops =
  (* if Reg.equal rs ra then ppc_fail "Invalid instruction szux: same operands"; *)
  let rs = signed reg ops.(1) in
  let ra = signed reg ops.(2) in
  let rb = signed reg ops.(3) in
  RTL.[
    cpu.store (ra + rb) rs byte;
    ra := ra + rb;
  ]

let sthux cpu ops =
  (* if Reg.equal rs ra then ppc_fail "Invalid instruction szux: same operands"; *)
  let rs = signed reg ops.(1) in
  let ra = signed reg ops.(2) in
  let rb = signed reg ops.(3) in
  RTL.[
    cpu.store (ra + rb) rs halfword;
    ra := ra + rb;
  ]

let stwux cpu ops =
  (* if Reg.equal rs ra then ppc_fail "Invalid instruction szux: same operands"; *)
  let rs = signed reg ops.(1) in
  let ra = signed reg ops.(2) in
  let rb = signed reg ops.(3) in
  RTL.[
    cpu.store (ra + rb) rs word;
    ra := ra + rb;
  ]

let stdux cpu ops =
  (* if Reg.equal rs ra then ppc_fail "Invalid instruction szux: same operands"; *)
  let rs = signed reg ops.(1) in
  let ra = signed reg ops.(2) in
  let rb = signed reg ops.(3) in
  RTL.[
    cpu.store (ra + rb) rs doubleword;
    ra := ra + rb;
  ]

(** Fixed-point Store Doubleword
    Page 57 of IBM Power ISATM Version 3.0 B
    examples:
    f8 29 00 08   std r1, 8(r9) *)
let std cpu ops =
  let rs = signed reg ops.(0) in
  let im = signed imm ops.(1) in
  let ra = signed reg ops.(2) in
  let sh = unsigned const byte 2 in
  RTL.[
    cpu.store (ra + (im lsl sh)) rs doubleword;
  ]

(** Fixed-point Store Doubleword with Update
    Page 57 of IBM Power ISATM Version 3.0 B
    examples:
    f8 29 00 09   stdu r1, 8(r9) *)
let stdu cpu ops =
  let rs = signed reg ops.(1) in
  let im = signed imm ops.(2) in
  let ra = signed reg ops.(3) in
  let sh = unsigned const byte 2 in
  let ea = unsigned var doubleword in
  RTL.[
    ea := ra + (im lsl sh);
    cpu.store ea rs doubleword;
    ra := ea;
  ]

type st = [
  | `STB
  | `STH
  | `STW
] [@@deriving sexp, enumerate ]

type stx = [
  | `STBX
  | `STHX
  | `STWX
  | `STDX
] [@@deriving sexp, enumerate ]

type stu = [
  | `STBU
  | `STHU
  | `STWU
] [@@deriving sexp, enumerate ]

type stux = [
  | `STBUX
  | `STHUX
  | `STWUX
  | `STDUX
] [@@deriving sexp, enumerate]

type std = [
  | `STD
  | `STDU
] [@@deriving sexp, enumerate]

type t = [ st | stx | stu | stux | std ] [@@deriving sexp, enumerate]

let size_of_t = function
  | `STB | `STBX | `STBU | `STBUX -> `r8
  | `STH | `STHX | `STHU | `STHUX -> `r16
  | `STW | `STWX | `STWU | `STWUX -> `r32
  | `STD | `STDX | `STDU | `STDUX -> `r64

let lift opcode cpu ops = match opcode with
  | `STB -> stb cpu ops
  | `STH -> sth cpu ops
  | `STW -> stw cpu ops
  | `STBX -> stbx cpu ops
  | `STHX -> sthx cpu ops
  | `STWX -> stwx cpu ops
  | `STDX -> stdx cpu ops
  | `STBU -> stbu cpu ops
  | `STHU -> sthu cpu ops
  | `STWU -> stwu cpu ops
  | `STBUX -> stbux cpu ops
  | `STHUX -> sthux cpu ops
  | `STWUX -> stwux cpu ops
  | `STDUX -> stdux cpu ops
  | `STD  -> std cpu ops
  | `STDU -> stdu cpu ops
