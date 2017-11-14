open Core_kernel.Std
open Bap.Std
open Op

open Ppc_model.Hardware
open Ppc_rtl

(** Fixed-point Load Byte/Halfword/Word and Zero
    Pages 48-54 of IBM Power ISATM Version 3.0 B

    examples:
    89 3c 00 14 - lbz r9, 20(r28)
    89 20 00 14 - lbz r9, 20(0)
    83 eb ff fc - lwz r31, -4(r11) *)
module Lz = struct

  let lz32 endian size rt imm ra =
    let rt = find_gpr rt in
    let ra = match find_gpr_opt ra with
      | None -> Dsl.int (Word.zero 64)
      | Some ra -> Dsl.var ra in
    let ea = Var.create ~fresh:true "ea" (Type.imm 32) in
    let imm = Word.of_int64 ~width:gpr_bitwidth (Imm.to_int64 imm) in
    Dsl.[
      ea := extract 31 0 (ra + cast signed gpr_bitwidth (int imm));
      rt := cast unsigned 64 (load32 ~addr:(var ea) endian size);
    ]

  let lz64 endian size rt imm ra =
    let rt = find_gpr rt in
    let ra = match find_gpr_opt ra with
      | None -> Dsl.int (Word.zero 64)
      | Some ra -> Dsl.var ra in
    let ea = Var.create ~fresh:true "ea" (Type.imm 64) in
    let imm = Word.of_int64 ~width:gpr_bitwidth (Imm.to_int64 imm) in
    Dsl.[
      ea := ra + cast signed gpr_bitwidth (int imm);
      rt := cast unsigned 64 (load64 ~addr:(var ea) endian size);
    ]
end

(** Fixed-point Load Byte/Halfword/Word and Zero Indexed
    Pages 48-54 of IBM Power ISATM Version 3.0 B

    examples:
    lbzx  7d 3d 50 ae
    lwzx  7d 3d 50 2e *)
module Lzx = struct

  let lzx32 endian size rt ra rb =
    let rt = find_gpr rt in
    let ra = match find_gpr_opt ra with
      | None -> Dsl.int (Word.zero 64)
      | Some ra -> Dsl.var ra in
    let rb = find_gpr rb in
    let ea = Var.create ~fresh:true "ea" (Type.imm 32) in
    Dsl.[
      ea := extract 31 0 (ra + var rb);
      rt := cast unsigned 64 (load32 ~addr:(var ea) endian size);
    ]

  let lzx64 endian size rt ra rb =
    let rt = find_gpr rt in
    let ra = match find_gpr_opt ra with
      | None -> Dsl.int (Word.zero 64)
      | Some ra -> Dsl.var ra in
    let rb = find_gpr rb in
    let ea = Var.create ~fresh:true "ea" (Type.imm 64) in
    Dsl.[
      ea := ra + var rb;
      rt := cast unsigned 64 (load64 ~addr:(var ea) endian size);
    ]
end


(** Fixed-point Load Byte/Halfword/Word and Zero with Update
    Pages 48-54 of IBM Power ISATM Version 3.0 B

    examples:
    lbzu  8d 3c 00 14
    lwzu  85 3f ff fc *)
module Lzu = struct

  let lzu32 endian size rt imm ra =
    if Reg.equal rt ra then failwith "Invalid instruction lzu: same operands";
    let rt = find_gpr rt in
    let ra = find_gpr ra in
    let imm = Word.of_int64 ~width:gpr_bitwidth (Imm.to_int64 imm) in
    let ea = Var.create ~fresh:true "ea" (Type.imm 32) in
    Dsl.[
      ea := extract 31 0 (var ra + cast signed gpr_bitwidth (int imm));
      rt := cast unsigned 64 (load32 ~addr:(var ea) endian size);
      ra := cast unsigned 64 (var ea);
    ]

  let lzu64 endian size rt imm ra =
    if Reg.equal rt ra then failwith "Invalid instruction lzu: same operands";
    let rt = find_gpr rt in
    let ra = find_gpr ra in
    let imm = Word.of_int64 ~width:gpr_bitwidth (Imm.to_int64 imm) in
    let ea = Var.create ~fresh:true "ea" (Type.imm 64) in
    Dsl.[
      ea := var ra + cast signed gpr_bitwidth (int imm);
      rt := cast unsigned 64 (load64 ~addr:(var ea) endian size);
      ra := var ea;
    ]
end


(** Fixed-point Load Byte/Halfword/Word and Zero with Update Indexed
    Pages 48-54 of IBM Power ISATM Version 3.0 B

    examples:
    lbzux 7d 3d 50 ee *)
module Lzux = struct

  let lzux32 endian size rt ra rb =
    if Reg.equal rt ra then failwith "Invalid instruction lzux: same operands";
    let rt = find_gpr rt in
    let ra = find_gpr ra in
    let rb = find_gpr rb in
    let ea = Var.create ~fresh:true "ea" (Type.imm 32) in
    Dsl.[
      ea := extract 31 0 (var ra + var rb);
      rt := cast unsigned 64 (load32 ~addr:(var ea) endian size);
      ra := cast unsigned 64 (var ea);
    ]

  let lzux64 endian size rt ra rb =
    if Reg.equal rt ra then failwith "Invalid instruction lzux: same operands";
    let rt = find_gpr rt in
    let ra = find_gpr ra in
    let rb = find_gpr rb in
    let ea = Var.create ~fresh:true "ea" (Type.imm 64) in
    Dsl.[
      ea := var ra + var rb;
      rt := cast unsigned 64 (load64 ~addr:(var ea) endian size);
      ra := var ea;
    ]
end

type lz = [
  | `LBZ
  | `LHZ
  | `LWZ
] [@@deriving sexp, enumerate ]

type lzx = [
  | `LBZX
  | `LHZX
  | `LWZX
] [@@deriving sexp, enumerate ]

type lzu = [
  | `LBZU
  | `LHZU
  | `LWZU
] [@@deriving sexp, enumerate ]

type lzux = [
  | `LBZUX
  | `LHZUX
  | `LWZUX
] [@@deriving sexp, enumerate]

type t = [ lz | lzx | lzu | lzux ] [@@deriving sexp, enumerate]

let size_of_t = function
  | `LBZ | `LBZX | `LBZU | `LBZUX -> `r8
  | `LHZ | `LHZX | `LHZU | `LHZUX -> `r16
  | `LWZ | `LWZX | `LWZU | `LWZUX -> `r32

let lift opcode mode endian mem ops =
  let size = size_of_t opcode in
  match opcode, ops with
  | #lz, [| Reg rt; Imm imm; Reg ra; |] ->
    let lz = match mode with
      | `r32 -> Lz.lz32
      | `r64 -> Lz.lz64 in
    lz endian size rt imm ra
  | #lzx, [| Reg rt; Reg ra; Reg rb; |] ->
    let lzx = match mode with
      | `r32 -> Lzx.lzx32
      | `r64 -> Lzx.lzx64 in
    lzx endian size rt ra rb
  | #lzu, [| Reg rt; Reg _; Imm imm; Reg ra; |] ->
    let lzu = match mode with
      | `r32 -> Lzu.lzu32
      | `r64 -> Lzu.lzu64 in
    lzu endian size rt imm ra
  | #lzux, [| Reg rt; Reg _; Reg ra; Reg rb |] ->
    let lzux = match mode with
      | `r32 -> Lzux.lzux32
      | `r64 -> Lzux.lzux64 in
    lzux endian size rt ra rb
  | opcode, _ ->
    let opcode = Sexp.to_string (sexp_of_t opcode) in
    failwith @@ sprintf "%s: unexpected operand set" opcode
