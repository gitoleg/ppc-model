open Core_kernel.Std
open Bap.Std
open Op

open Ppc_model
open Ppc_rtl

let ov x y res =
  DSL.(Registers.oF := cast high 1 ((x lxor y) land (x lxor res)))

let ov32 x y res =
  DSL.(Registers.oF := cast high 1
           (extract 31 0 ((x lxor y) land (x lxor res))))

(** condition register, field 0  *)
let cr0 res so zero =
  let open DSL in
  let zero = int zero in
  let bit0 = res < zero in
  let bit1 = res > zero in
  let bit2 = res = zero in
  let field0 = so ^ bit2 ^ bit1 ^ bit0 in
  DSL.(Registers.cr := extract 31 4 (var Registers.cr) ^ field0)

(** condition register, field 0  *)
let cr0_64 res so =
  let zero = Word.zero 64 in
  cr0 res so zero

(** condition register, field 0  *)
let cr0_32 res so =
  let zero = Word.zero 32 in
  let res = DSL.extract 31 0 res in
  cr0 res so zero

(**
7d 62 5e 15
7d 62 5a 14 *)
let add32 mem endian rt ra rb =
  let opcode = Memory.get ~scale:`r32 mem in
  let opcode = Or_error.ok_exn opcode in
  let bits = Word.enum_bits opcode endian in
  let oe = Seq.nth_exn bits 21 in
  let rc = Seq.nth_exn bits 31 in
  let rt = find_gpr rt in
  let ra = find_gpr ra in
  let rb = find_gpr rb in
  let oe = if oe then
      DSL.[ov32 (var ra) (var rb) (var rt); Registers.so := var Registers.of32]
      else [] in
  let rc = if rc then
      DSL.[cr0_32 (var rt) (var Registers.so)]
      else [] in
  DSL.[rt := var ra + var rb] @ oe @ rc

(** Fixed-Point Arithmetic Instructions - Add Immediate
    Pages 67-68 of IBM Power ISATM Version 3.0 B
    examples:
    38 21 00 10     addi    r1,r1,16
    3b de fd 28     addi    r30,r30,-728 *)
let addi rt ra imm =
  let width = Registers.gpr_width in
  let rt = find_gpr rt in
  let imm = Word.of_int64 ~width (Imm.to_int64 imm) in
  match find_gpr_opt ra with
  | None -> DSL.[rt := cast signed width (int imm)]
  | Some ra -> DSL.[rt := var ra + cast signed width (int imm)]

(** Fixed-Point Arithmetic Instructions - Add Immediate Shifted
    Pages 67-68 of IBM Power ISATM Version 3.0 B
    examples:
    3f de 00 02     addis   r30,r30,2
    3d 6b f0 00     addis   r11,r11,-4096 *)
let addis rt ra imm =
  let width = Registers.gpr_width in
  let rt = find_gpr rt in
  let imm =
    Int64.(Imm.to_int64 imm lsl 16) |> Word.of_int64 ~width in
  match find_gpr_opt ra with
  | None -> DSL.[rt := cast signed width (int imm)]
  | Some ra ->
    DSL.[ rt := var ra + cast signed width (int imm) ]

(** Fixed-Point Arithmetic Instructions - Add PC Immediate Shifted
    Pages 67-68 of IBM Power ISATM Version 3.0 B
    example:
    4c 22 00 04     addpcis r1,4 *)
(** llvm doesn't recognize this instruction  *)
let addpcis rt imm0 imm1 imm2 = failwith "unimplemented"

type t = [
  | `ADD4
  | `ADD4o
  | `ADDI
  | `ADDIS
  | `ADDPCIS
] [@@deriving sexp]

let lift mode endian mem opcode ops = match opcode, ops with
  | `ADD4,  [| Reg rt; Reg ra; Reg rb  |] -> add32 mem endian rt ra rb
  | `ADD4o, [| Reg rt; Reg ra; Reg rb  |] -> add32 mem endian rt ra rb
  | `ADDI,  [| Reg rt; Reg ra; Imm imm |] -> addi rt ra imm
  | `ADDIS, [| Reg rt; Reg ra; Imm imm |] -> addis rt ra imm
  | _ -> failwith "unimplemented"
