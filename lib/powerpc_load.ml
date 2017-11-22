open Core_kernel.Std
open Bap.Std
open Op

open Powerpc_types
open Dsl
open Hardware

(** Fixed-point Load Byte/Halfword/Word and Zero
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    89 3c 00 14 - lbz r9, 20(r28)
    89 20 00 14 - lbz r9, 20(0)
    a1 3c 00 14 - lhz r9, 20(r28)
    83 eb ff fc - lwz r31, -4(r11) *)
let lz cpu size ops =
  let rt = unsigned reg ops.(0) in
  let im = signed imm ops.(1) in
  let ra = unsigned reg ops.(2) in
  RTL.[ rt := cpu.load (ra + im) size; ]

(** Fixed-point Load Byte/Halfword/Word and Zero Indexed
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    7d 3d 50 ae   lbzx r9, r29, r10
    7d 3d 52 2e   lhzx r9, r29, r10
    7d 3d 50 2e   lwzx r9, r29, r10  *)
let lzx cpu size ops =
  let rt = signed reg ops.(0) in
  let ra = signed reg ops.(1) in
  let rb = signed reg ops.(2) in
  RTL.[ rt := cpu.load (ra + rb) size; ]

(** Fixed-point Load Byte/Halfword/Word and Zero with Update
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    8d 3c 00 14  lbzu r9, 20(r28)
    a5 3c 00 14  lhzu r9, 20(r28)
    85 3f ff fc  lwzu r9, -4(r31)  *)
let lzu cpu size ops =
  (* if Reg.equal rt ra then Dsl.ppc_fail "Invalid instruction lzu: same operands"; *)
  let rt = signed reg ops.(0) in
  let im = signed imm ops.(2) in
  let ra = signed reg ops.(3) in
  RTL.[
    rt := cpu.load (ra + im) size;
    ra := ra + im;
  ]

(** Fixed-point Load Byte/Halfword/Word and Zero with Update Indexed
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    7d 3d 50 ee  lbzux r9, r29, r10
    7d 3d 52 6e  lhzux r9, r29, r10
    7d 3d 50 6e  lwzux r9, r29, r10  *)
let lzux cpu size ops =
  (* if Reg.equal rt ra then Dsl.ppc_fail "Invalid instruction lzux: same operands"; *)
  let rt = signed reg ops.(0) in
  let ra = signed reg ops.(2) in
  let rb = signed reg ops.(3) in
  RTL.[
    rt := cpu.load (ra + rb) size;
    ra := ra + rb;
  ]

(** Fixed-point Load Halfword Algebraic
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    a8 29 00 05    lha r1, 5(r9) *)
let lha cpu ops =
  let rt = signed reg ops.(0) in
  let im = signed imm ops.(1) in
  let ra = signed reg ops.(2) in
  RTL.[
    rt := cpu.load (ra + im) halfword;
  ]

(* (\** Fixed-point Load Word Algebraic *)
(*     Pages 48-54 of IBM Power ISATM Version 3.0 B *)
(*     examples: *)
(*     eb eb 01 16    lwa r31, 276(r11) *\) *)
(* let lwa cpu ops = *)
(*   let rt = reg ops.(0) in *)
(*   let ra = reg0 ops.(1) in *)
(*   let im = imms 14 ops.(2) in *)
(*   let zero = Word.b0 in *)
(*   let data = fresh "data" word_t in *)
(*   RTL.[ *)
(*     var data := cpu.load (ra + im) word; *)
(*     if_ (extract 31 31 (var data) = int zero) [ *)
(*       rt := cast unsigned 64 (var data); *)
(*     ] [ *)
(*       rt := cast signed 64 (var data); *)
(*     ] *)
(*   ] *)

(* (\** Fixed-point Load Halfword/Word Algebraic Indexed *)
(*     Pages 48-54 of IBM Power ISATM Version 3.0 B *)
(*     examples: *)
(*     7c 25 4a ae    lhax r1, r5, r9 *)
(*     7c 25 4a aa    lwax r1, r5, r9  *\) *)
(* (\** TODO: check lha - if we may to unify with it  *\) *)
(* let lax cpu size ops = *)
(*   let rt = reg ops.(0) in *)
(*   let ra = reg0 ops.(1) in *)
(*   let rb = reg ops.(2) in *)
(*   let zero = Word.b0 in *)
(*   let data_bits = Size.in_bits size in *)
(*   let hbit = Size.in_bits size - 1 in *)
(*   let data = fresh "data" (Type.imm data_bits) in *)
(*   RTL.[ *)
(*     var data := cpu.load (ra + rb) size; *)
(*     if_ (extract hbit hbit (var data) = int zero) [ *)
(*       rt := cast unsigned 64 (var data); *)
(*     ] [ *)
(*       rt := cast signed 64 (var data); *)
(*     ] *)
(*   ] *)

(* (\** Fixed-point Load Halfword Algebraic with Update *)
(*     Pages 48-54 of IBM Power ISATM Version 3.0 B *)
(*     examples: *)
(*     ac 29 00 05    lhau r1, 5(r9) *\) *)
(* let lhau cpu size ops = *)
(*   (\* if Reg.equal rt ra then RTL.ppc_fail "Invalid instruction lhau: same operands"; *\) *)
(*   let size = `r16 in *)
(*   let rt = reg ops.(0) in *)
(*   let ra = reg ops.(1) in *)
(*   let im = imms 16 ops.(2) in *)
(*   let zero = Word.b0 in *)
(*   let data_bits = Size.in_bits size in *)
(*   let hbit = Size.in_bits size - 1 in *)
(*   let data = fresh "data" (Type.imm data_bits) in *)
(*   RTL.[ *)
(*     var data := cpu.load (ra + im) size; *)
(*     if_ (extract hbit hbit (var data) = int zero) [ *)
(*       rt := cast unsigned 64 (var data); *)
(*     ] [ *)
(*       rt := cast signed 64 (var data); *)
(*     ]; *)
(*     ra := cast unsigned 64 (ra + im) *)
(*   ] *)

(* (\** Fixed-point Load Data/Word Algebraic with Update Indexed *)
(*     Pages 48-54 of IBM Power ISATM Version 3.0 B *)
(*     examples: *)
(*     7c 25 4a ee    lhaux r1, r5, r9 *)
(*     7c 25 4a ea    lwaux r1, r5, r9 *\) *)
(* let laux cpu size ops = *)
(*   (\* if Reg.equal rt ra then RTL.ppc_fail "Invalid instruction lhaux: same operands"; *\) *)
(*   let rt = reg ops.(0) in *)
(*   let ra = reg ops.(1) in *)
(*   let rb = reg ops.(2) in *)
(*   let zero = Word.b0 in *)
(*   let data_bits = Size.in_bits size in *)
(*   let hbit = Size.in_bits size - 1 in *)
(*   let data = fresh "data" (Type.imm data_bits) in *)
(*   RTL.[ *)
(*     var data := cpu.load (ra + rb) size; *)
(*     if_ (extract hbit hbit (var data) = int zero) [ *)
(*       rt := cast unsigned 64 (var data); *)
(*     ] [ *)
(*       rt := cast signed 64 (var data); *)
(*     ]; *)
(*     ra := cast unsigned 64 (ra + rb) *)
(*   ] *)

(* (\** Fixed-point Load Dobuleword *)
(*     Pages 48-54 of IBM Power ISATM Version 3.0 B *)
(*     examples: *)
(*     e8 29 00 08    ld r1, 8(r9) *\) *)
(* let ld cpu size ops = (\* addr_size endian rt ra imm = *\) *)
(*   let rt = RTL.find rt in *)
(*   let ra = if RTL.exists ra then RTL.(var @@ find ra) *)
(*     else RTL.int (Word.zero 64) in *)
(*   let bits = Size.in_bits addr_size in *)
(*   let imm = *)
(*     let x = Imm.to_int64 imm in *)
(*     Word.of_int64 Int64.(x lsl 2)  in *)
(*   let ea = RTL.fresh "ea" (Type.imm bits) in *)
(*   RTL.[ *)
(*     ea := addr_of_exp addr_size (ra + cast signed gpr_bitwidth (int imm)); *)
(*     rt := cast unsigned gpr_bitwidth (load addr_size ~addr:(var ea) endian `r64); *)
(*   ] *)

(* (\** Fixed-point Load Dobuleword Indexed *)
(*     Pages 48-54 of IBM Power ISATM Version 3.0 B *)
(*     examples: *)
(*     7c 28 48 2a    ldx r1, r8, r9 *\) *)
(* let ldx cpu size ops = (\* addr_size endian rt ra rb = *\) *)
(*   let rt = RTL.find rt in *)
(*   let ra = if RTL.exists ra then RTL.(var @@ find ra) *)
(*     else RTL.int (Word.zero 64) in *)
(*   let rb = RTL.find rb in *)
(*   let bits = Size.in_bits addr_size in *)
(*   let ea = RTL.fresh "ea" (Type.imm bits) in *)
(*   RTL.[ *)
(*     ea := addr_of_exp addr_size (ra + var rb); *)
(*     rt := cast unsigned gpr_bitwidth (load addr_size ~addr:(var ea) endian `r64); *)
(*   ] *)

(* (\** Fixed-point Load Dobuleword with Update *)
(*     Pages 48-54 of IBM Power ISATM Version 3.0 B *)
(*     examples: *)
(*     e8 29 00 09    ldu r1, 8(r9) *\) *)
(* let ldu cpu size ops = (\* addr_size endian rt ra imm = *\) *)
(*   if Reg.equal rt ra then RTL.ppc_fail "Invalid instruction ldu: same operands"; *)
(*   let rt = RTL.find rt in *)
(*   let ra = RTL.find ra in *)
(*   let bits = Size.in_bits addr_size in *)
(*   let imm = *)
(*     let x = Imm.to_int64 imm in *)
(*     Word.of_int64 Int64.(x lsl 2)  in *)
(*   let ea = RTL.fresh "ea" (Type.imm bits) in *)
(*   RTL.[ *)
(*     ea := addr_of_exp addr_size (var ra + cast signed gpr_bitwidth (int imm)); *)
(*     rt := cast unsigned gpr_bitwidth (load addr_size ~addr:(var ea) endian `r64); *)
(*     ra := cast unsigned gpr_bitwidth (var ea); *)
(*   ] *)

(* (\** Fixed-point Load Dobuleword with Update Indexed *)
(*     Pages 48-54 of IBM Power ISATM Version 3.0 B *)
(*     examples: *)
(*     7c 28 48 6a    ldux r1, r8, r9 *\) *)
(* let ldux cpu size ops = (\* addr_size endian rt ra rb = *\) *)
(*   if Reg.equal rt ra then RTL.ppc_fail "Invalid instruction ldux: same operands"; *)
(*   let rt = RTL.find rt in *)
(*   let ra = RTL.find ra in *)
(*   let rb = RTL.find rb in *)
(*   let bits = Size.in_bits addr_size in *)
(*   let ea = RTL.fresh "ea" (Type.imm bits) in *)
(*   RTL.[ *)
(*     ea := addr_of_exp addr_size (var ra + var rb); *)
(*     rt := cast unsigned gpr_bitwidth (load addr_size ~addr:(var ea) endian `r64); *)
(*     ra := cast unsigned gpr_bitwidth (var ea); *)
(*   ] *)

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

type la = [
  | `LHA
  | `LWA
] [@@deriving sexp, enumerate]

type lax = [
  | `LHAX
  | `LWAX
] [@@deriving sexp, enumerate]

type lhau = [
  | `LHAU
] [@@deriving sexp, enumerate]

type laux = [
  | `LHAUX
  | `LWAUX
] [@@deriving sexp, enumerate]

type ld = [
  | `LD
  | `LDX
  | `LDU
  | `LDUX
] [@@deriving sexp, enumerate]

type t = [ lz | lzx | lzu | lzux | la | lax | lhau | laux | ld ] [@@deriving sexp, enumerate]

let size_of_t = function
  | `LBZ | `LBZX | `LBZU | `LBZUX -> byte
  | `LHZ | `LHZX | `LHZU | `LHZUX
  | `LHA | `LHAX | `LHAU | `LHAUX -> halfword
  | `LWZ | `LWZX | `LWZU | `LWZUX
  | `LWA | `LWAX | `LWAUX         -> word
  | `LD  | `LDX  | `LDU  | `LDUX  -> doubleword

let lift opcode cpu ops =
  let size = size_of_t opcode in
  match opcode with
  | #lz   -> lz cpu size ops
  | #lzx  -> lzx cpu size ops
  | #lzu  -> lzu cpu size ops
  | #lzux -> lzux cpu size ops
  | `LHA  -> lha cpu ops
  (* | `LWA  -> lwa cpu ops *)
  (* | #lax  -> lax cpu size ops *)
  (* | `LHAU -> lhau cpu size ops *)
  (* | #laux -> laux cpu size ops *)
  (* | `LD   -> ld cpu size ops *)
  (* | `LDX  -> ldx cpu size ops *)
  (* | `LDU  -> ldu cpu size ops *)
  (* | `LDUX -> ldux cpu size ops *)
  | _ -> failwith "oops"