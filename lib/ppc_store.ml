open Core_kernel.Std
open Bap.Std
open Op

open Ppc_model.Hardware
open Ppc_rtl

let addr_of_exp addr_size exp = match addr_size with
  | `r32 -> extract_low_32 exp
  | `r64 -> exp

(** Fixed-point Store Byte/Halfword/Word
    Pages 54-56 of IBM Power ISATM Version 3.0 B
    examples:
    99 3c 01 6c     stb r9,364(r28)
    99 20 01 6C     stb r9,364(0)
    b1 3c 01 6c     sth r9,364(r28)
    91 28 ff d4     stw r9,-44(r8) *)
let st addr_size endian size rs imm ra =
  let rs = find_gpr rs in
  let ra = match find_gpr_opt ra with
    | None -> Dsl.int (Word.zero 64)
    | Some ra -> Dsl.var ra in
  let bits = Size.in_bits addr_size in
  let ea = Var.create ~fresh:true "ea" (Type.imm bits) in
  let imm = Word.of_int64 (Imm.to_int64 imm) in
  Dsl.[
    ea := addr_of_exp addr_size (ra + cast signed gpr_bitwidth (int imm));
    store addr_size ~addr:(var ea) endian size (var rs);
  ]

(** Fixed-point Store Byte/Halfword/Word/Doubleword Indexed
    Pages 54-57 of IBM Power ISATM Version 3.0 B
    examples:
    7d 2e f9 ae     stbx    r9,r14,r31
    7d 3e eb 2e     sthx    r9,r30,r29
    7f b6 f9 2e     stwx    r29,r22,r31
    7c 28 49 2a     stdx    r1, r8, r9  *)
let stx addr_size endian size rs ra rb =
  let rs = find_gpr rs in
  let ra = match find_gpr_opt ra with
    | None -> Dsl.int (Word.zero 64)
    | Some ra -> Dsl.var ra in
  let rb = find_gpr rb in
  let bits = Size.in_bits addr_size in
  let ea = Var.create ~fresh:true "ea" (Type.imm bits) in
  Dsl.[
    ea := addr_of_exp addr_size (ra + var rb);
    store addr_size ~addr:(var ea) endian size (var rs);
  ]

(** Fixed-point Store Byte/Halfword/Word with Update
    Pages 54-56 of IBM Power ISATM Version 3.0 B
    examples:
    9c 9d ff ff     stbu r4,-1(r29)
    b5 3d ff ff     sthu r9,-1(r29)
    94 21 ff f0     stwu r1,-16(r1)  *)
let stu addr_size endian size rs imm ra =
  if Reg.equal rs ra then ppc_fail "Invalid instruction szu: same operands";
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let imm = Word.of_int64 (Imm.to_int64 imm) in
  let bits = Size.in_bits addr_size in
  let ea = Var.create ~fresh:true "ea" (Type.imm bits) in
  Dsl.[
    ea := addr_of_exp addr_size (var ra + cast signed gpr_bitwidth (int imm));
    store addr_size ~addr:(var ea) endian size (var rs);
    ra := cast unsigned 64 (var ea);
  ]

(** Fixed-point Store Byte/Halfword/Word/Doubleword with Update Indexed
    Pages 54-57 of IBM Power ISATM Version 3.0 B
    examples:
    7d 3f c9 ee     stbux r9,r31,r25
    7d 3f cb 6e     sthux r9,r31,r25
    7d 41 49 6e     stwux r10,r1,r9
    7c 28 49 6a     stdux r1,r8,r9   *)
let stux addr_size endian size rs ra rb =
  if Reg.equal rs ra then ppc_fail "Invalid instruction szux: same operands";
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let rb = find_gpr rb in
  let bits = Size.in_bits addr_size in
  let ea = Var.create ~fresh:true "ea" (Type.imm bits) in
  Dsl.[
    ea := addr_of_exp addr_size (var ra + var rb);
    store addr_size ~addr:(var ea) endian size (var rs);
    ra := cast unsigned 64 (var ea);
  ]

(** Fixed-point Store Doubleword
    Page 57 of IBM Power ISATM Version 3.0 B
    examples:
    f8 29 00 08   std r1, 8(r9) *)
let std addr_size endian rs imm ra =
  let rs = find_gpr rs in
  let ra = match find_gpr_opt ra with
    | None -> Dsl.int (Word.zero 64)
    | Some ra -> Dsl.var ra in
  let addr_bits = Size.in_bits addr_size in
  let imm =
    let x = Imm.to_int64 imm in
    Word.of_int64 Int64.(x lsl 2)  in
  let ea = Var.create ~fresh:true "ea" (Type.imm addr_bits) in
  Dsl.[
    ea := addr_of_exp addr_size (ra + cast signed gpr_bitwidth (int imm));
    store addr_size ~addr:(var ea) endian `r64 (var rs);
  ]

(** Fixed-point Store Doubleword with Update
    Page 57 of IBM Power ISATM Version 3.0 B
    examples:
    f8 29 00 09   stdu r1, 8(r9) *)
let stdu addr_size endian rs imm ra =
  let rs = find_gpr rs in
  let ra = find_gpr ra in
  let imm =
    let x = Imm.to_int64 imm in
    Word.of_int64 Int64.(x lsl 2)  in
  let bits = Size.in_bits addr_size in
  let ea = Var.create ~fresh:true "ea" (Type.imm bits) in
  Dsl.[
    ea := addr_of_exp addr_size (var ra + int imm);
    store addr_size ~addr:(var ea) endian `r64 (var rs);
    ra := cast unsigned gpr_bitwidth (var ea);
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

let lift opcode addr_size endian mem ops =
  let size = size_of_t opcode in
  match opcode, ops with
  | #st,   [| Reg rs; Imm imm; Reg ra; |] -> st addr_size endian size rs imm ra
  | #stx,  [| Reg rs; Reg ra; Reg rb; |] -> stx addr_size endian size rs ra rb
  | #stu,  [| Reg _; Reg rs; Imm imm; Reg ra; |] -> stu addr_size endian size rs imm ra
  | #stux, [| Reg _; Reg rs; Reg ra; Reg rb |] -> stux addr_size endian size rs ra rb
  | `STD, [| Reg rs; Imm imm; Reg ra; |] -> std addr_size endian rs imm ra
  | `STDU,[| Reg _; Reg rs; Imm imm; Reg ra |] -> stdu addr_size endian rs imm ra
  | opcode, _ ->
    let opcode = Sexp.to_string (sexp_of_t opcode) in
    ppc_fail "%s: unexpected operand set" opcode
