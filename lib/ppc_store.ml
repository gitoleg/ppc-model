open Core_kernel.Std
open Bap.Std
open Op

open Ppc_model.Hardware
open Ppc_rtl

(** Fixed-point Store Byte/Halfword/Word
    Pages 54-56 of IBM Power ISATM Version 3.0 B

    examples:
    99 3c 01 6c     stb     r9,364(r28)
    99 20 01 6C     stb     r9,364(0)
    91 28 ff d4     stw     r9,-44(r8) *)
module St = struct

  let st32 endian size rs imm ra =
    let rs = find_gpr rs in
    let ra = match find_gpr_opt ra with
      | None -> Dsl.int (Word.zero 64)
      | Some ra -> Dsl.var ra in
    let ea = Var.create ~fresh:true "ea" (Type.imm 32) in
    let imm = Word.of_int64 ~width:gpr_bitwidth (Imm.to_int64 imm) in
    Dsl.[
      ea := extract 31 0 (ra + cast signed gpr_bitwidth (int imm));
      store32 ~addr:(var ea) endian size (var rs);
    ]

  let st64 endian size rs imm ra =
    let rs = find_gpr rs in
    let ra = match find_gpr_opt ra with
      | None -> Dsl.int (Word.zero 64)
      | Some ra -> Dsl.var ra in
    let ea = Var.create ~fresh:true "ea" (Type.imm 64) in
    let imm = Word.of_int64 ~width:gpr_bitwidth (Imm.to_int64 imm) in
    Dsl.[
      ea := ra + cast signed gpr_bitwidth (int imm);
      store64 ~addr:(var ea) endian size (var rs);
    ]
end

(** Fixed-point Store Byte/Halfword/Word Indexed
    Pages 54-56 of IBM Power ISATM Version 3.0 B

    examples:
    7d 2e f9 ae     stbx    r9,r14,r31
    7d 3e eb 2e     sthx    r9,r30,r29
    7f b6 f9 2e     stwx    r29,r22,r31  *)
module Stx = struct

  let stx32 endian size rs ra rb =
    let rs = find_gpr rs in
    let ra = find_gpr ra in
    let rb = find_gpr rb in
    let ea = Var.create ~fresh:true "ea" (Type.imm 32) in
    Dsl.[
      ea := extract 31 0 (var ra + var rb);
      store32 ~addr:(var ea) endian size (var rs);
    ]

  let stx64 endian size rs ra rb =
    let rs = find_gpr rs in
    let ra = find_gpr ra in
    let rb = find_gpr rb in
    let ea = Var.create ~fresh:true "ea" (Type.imm 64) in
    Dsl.[
      ea := var ra + var rb;
      store64 ~addr:(var ea) endian size (var rs);
    ]
end


(** Fixed-point Store Byte/Halfword/Word with Update
    Pages 54-56 of IBM Power ISATM Version 3.0 B

    examples:
    9c 9d ff ff     stbu    r4,-1(r29)
    94 21 ff f0     stwu    r1,-16(r1)  *)
module Stu = struct

  let stu32 endian size rs imm ra =
    if Reg.equal rs ra then failwith "Invalid instruction szu: same operands";
    let rs = find_gpr rs in
    let ra = find_gpr ra in
    let imm = Word.of_int64 ~width:gpr_bitwidth (Imm.to_int64 imm) in
    let ea = Var.create ~fresh:true "ea" (Type.imm 32) in
    Dsl.[
      ea := extract 31 0 (var ra + cast signed gpr_bitwidth (int imm));
      store32 ~addr:(var ea) endian size (var rs);
      ra := cast unsigned 64 (var ea);
    ]

  let stu64 endian size rs imm ra =
    if Reg.equal rs ra then failwith "Invalid instruction szu: same operands";
    let rs = find_gpr rs in
    let ra = find_gpr ra in
    let imm = Word.of_int64 ~width:gpr_bitwidth (Imm.to_int64 imm) in
    let ea = Var.create ~fresh:true "ea" (Type.imm 64) in
    Dsl.[
      ea := var ra + cast signed gpr_bitwidth (int imm);
      store64 ~addr:(var ea) endian size (var rs);
      ra := var ea;
    ]
end


(** Fixed-point Store Byte/Halfword/Word with Update Indexed
    Pages 48-54 of IBM Power ISATM Version 3.0 B

    examples:
    7d 3f c9 ee     stbux   r9,r31,r25
    7d 41 49 6e     stwux   r10,r1,r9 *)
module Stux = struct

  let stux32 endian size rs ra rb =
    if Reg.equal rs ra then failwith "Invalid instruction szux: same operands";
    let rs = find_gpr rs in
    let ra = find_gpr ra in
    let rb = find_gpr rb in
    let ea = Var.create ~fresh:true "ea" (Type.imm 32) in
    Dsl.[
      ea := extract 31 0 (var ra + var rb);
      store32 ~addr:(var ea) endian size (var rs);
      ra := cast unsigned 64 (var ea);
    ]

  let stux64 endian size rs ra rb =
    if Reg.equal rs ra then failwith "Invalid instruction szux: same operands";
    let rs = find_gpr rs in
    let ra = find_gpr ra in
    let rb = find_gpr rb in
    let ea = Var.create ~fresh:true "ea" (Type.imm 64) in
    Dsl.[
      ea := var ra + var rb;
      store64 ~addr:(var ea) endian size (var rs);
      ra := var ea;
    ]
end

type st = [
  | `STB
  | `STH
  | `STW
] [@@deriving sexp, enumerate ]

type stx = [
  | `STBX
  | `STHX
  | `STWX
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
] [@@deriving sexp, enumerate]

type t = [ st | stx | stu | stux ] [@@deriving sexp, enumerate]

let size_of_t = function
  | `STB | `STBX | `STBU | `STBUX -> `r8
  | `STH | `STHX | `STHU | `STHUX -> `r16
  | `STW | `STWX | `STWU | `STWUX -> `r32

let lift opcode mode endian mem ops =
  let size = size_of_t opcode in
  match opcode, ops with
  | #st, [| Reg rs; Imm imm; Reg ra; |] ->
    let st = match mode with
      | `r32 -> St.st32
      | `r64 -> St.st64 in
    st endian size rs imm ra
  | #stx, [| Reg rs; Reg ra; Reg rb; |] ->
    let stx = match mode with
      | `r32 -> Stx.stx32
      | `r64 -> Stx.stx64 in
    stx endian size rs ra rb
  | #stu, [| Reg rs; Reg _; Imm imm; Reg ra; |] ->
    let stu = match mode with
      | `r32 -> Stu.stu32
      | `r64 -> Stu.stu64 in
    stu endian size rs imm ra
  | #stux, [| Reg rs; Reg _; Reg ra; Reg rb |] ->
    let stux = match mode with
      | `r32 -> Stux.stux32
      | `r64 -> Stux.stux64 in
    stux endian size rs ra rb
  | opcode, _ ->
    let opcode = Sexp.to_string (sexp_of_t opcode) in
    failwith @@ sprintf "%s: unexpected operand set" opcode
