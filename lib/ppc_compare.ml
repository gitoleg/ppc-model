open Core_kernel.Std
open Bap.Std

open Ppc_model.Hardware
open Ppc_rtl

let sign_extend64 op_size exp = match op_size with
  | `r32 -> Dsl.(cast signed 64 (extract_low_32 exp))
  | `r64 -> exp

let unsign_extend64 op_size exp = match op_size with
  | `r32 -> Dsl.(cast unsigned 64 (extract_low_32 exp))
  | `r64 -> exp

let lt_word = Word.of_int ~width:3 0b100
let gt_word = Word.of_int ~width:3 0b010
let eq_word = Word.of_int ~width:3 0b001

let write_cr_field n exp =
  let beg = n * 4 in
  let lt = condition_register_bit (beg + 3) in
  let gt = condition_register_bit (beg + 2) in
  let eq = condition_register_bit (beg + 1) in
  let sb = condition_register_bit beg in
  Dsl.[
    lt := extract 2 2 exp;
    gt := extract 1 1 exp;
    eq := extract 0 0 exp;
    sb := var so;
  ]

(** Fix-point Compare Immediate
    Page 85 of IBM Power ISATM Version 3.0 B
    examples:
    2f 89 ff ff     cmpwi cr7, r9, -1
    2f a9 ff ff     cmpdi cr7, r9, -1 *)
let cmpi op_size bf ra si =
  let ra = find_gpr ra in
  let ra = sign_extend64 op_size (Dsl.var ra) in
  let fn = condition_register_field bf in
  let si = Word.of_int64 (Imm.to_int64 si) in
  let c = Var.create ~fresh:true "res" (Type.imm 3) in
  Dsl.[
    if_ (ra <$ int si) [
      c := int lt_word;
    ] [
      if_ (ra >$ int si) [
        c := int gt_word;
      ] [
        c := int eq_word;
      ]
    ];
  ] @ write_cr_field fn (Dsl.var c)

(** Fix-point Compare
    Page 85 of IBM Power ISATM Version 3.0 B
    examples:
    7f 86 38 00     cmpw cr7, r6, r7
    7f a6 38 00     cmpd cr7, r6, r7 *)
let cmp op_size bf ra rb =
  let ra = find_gpr ra in
  let rb = find_gpr rb in
  let ra = sign_extend64 op_size (Dsl.var ra) in
  let rb = sign_extend64 op_size (Dsl.var rb) in
  let fn = condition_register_field bf in
  let c = Var.create ~fresh:true "res" (Type.imm 3) in
  Dsl.[
    if_ (ra <$ rb) [
      c := int lt_word;
    ] [
      if_ (ra >$ rb) [
        c := int gt_word;
      ] [
        c := int eq_word;
      ]
    ];
  ] @ write_cr_field fn (Dsl.var c)

(** Fix-point Compare Logical Immediate
    Page 86 of IBM Power ISATM Version 3.0 B
    examples:
    2b 89 00 01     cmplwi cr7, r9, 1
    2b a9 00 01     cmpldi cr7, r9, 1 *)
let cmpli op_size bf ra si =
  let ra = find_gpr ra in
  let ra = unsign_extend64 op_size (Dsl.var ra) in
  let fn = condition_register_field bf in
  let si = Word.of_int64 (Imm.to_int64 si) in
  let c = Var.create ~fresh:true "res" (Type.imm 3) in
  Dsl.[
    if_ (ra < int si) [
      c := int lt_word;
    ] [
      if_ (ra > int si) [
        c := int gt_word;
      ] [
        c := int eq_word;
      ]
    ];
  ] @ write_cr_field fn (Dsl.var c)

(** Fix-point Compare Logical
    Page 86 of IBM Power ISATM Version 3.0 B
    examples:
    7f 86 38 40     cmplw cr7, r6, r7
    7f a6 38 40     cmpld cr7, r6, r7 *)
let cmpl op_size bf ra rb =
  let ra = find_gpr ra in
  let rb = find_gpr rb in
  let ra = unsign_extend64 op_size (Dsl.var ra) in
  let rb = unsign_extend64 op_size (Dsl.var rb) in
  let fn = condition_register_field bf in
  let c = Var.create ~fresh:true "res" (Type.imm 3) in
  Dsl.[
    if_ (ra < rb) [
      c := int lt_word;
    ] [
      if_ (ra > rb) [
        c := int gt_word;
      ] [
        c := int eq_word;
      ]
    ];
  ] @ write_cr_field fn (Dsl.var c)

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

let lift opcode addr_size endian mem ops =
  let open Op in
  match opcode, ops with
  | `CMPWI, [| Reg bf; Reg ra; Imm si |] -> cmpi `r32 bf ra si
  | `CMPDI, [| Reg bf; Reg ra; Imm si |] -> cmpi `r64 bf ra si
  | `CMPW,  [| Reg bf; Reg ra; Reg rb |] -> cmp `r32 bf ra rb
  | `CMPD,  [| Reg bf; Reg ra; Reg rb |] -> cmp `r64 bf ra rb
  | `CMPLWI, [| Reg bf; Reg ra; Imm si |] -> cmpli `r32 bf ra si
  | `CMPLDI, [| Reg bf; Reg ra; Imm si |] -> cmpli `r64 bf ra si
  | `CMPLW,  [| Reg bf; Reg ra; Reg rb |] -> cmpl `r32 bf ra rb
  | `CMPLD,  [| Reg bf; Reg ra; Reg rb |] -> cmpl `r64 bf ra rb
  | _ -> failwith "unimplemented"
