open Core_kernel.Std
open Bap.Std
open OUnit2

open Powerpc
open Powerpc_tests_helpers

let subf arch ctxt =
  let name = "SUBF" in
  let bytes = make_insn ~name `XO [31; 1; 2; 3; 0; 40; 0] in
  let x = Word.of_int64 0xABCDEF42_12345678L in
  let y = Word.of_int64 0x42L in
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let r3 = find_gpr "R3" in
  let init = Bil.[
      r3 := int x;
      r2 := int y;
    ] in
  let expected = Word.(x - y) in
  check_gpr init bytes r1 expected arch ctxt

(** TODO: add carry flags testing *)
let subfic arch ctxt =
  let name = "SUBFIC" in
  let x = 4242 in
  let bytes = make_insn ~name `D [8; 1; 2; x] in
  let x = Word.of_int ~width:64 x in
  let y = Word.of_int64 42L in
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let init = Bil.[
      r2 := int y;
  ] in
  let expected = Word.(x - y) in
  check_gpr init bytes r1 expected arch ctxt

(** TODO: add carry flags testing *)
let subfc arch ctxt =
  let name = "SUBFC" in
  let bytes = make_insn ~name `XO [31; 1; 2; 3; 0; 8; 0] in
  let x = Word.of_int64 0xABCDEF42_12345678L in
  let y = Word.of_int64 0x42L in
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let r3 = find_gpr "R3" in
  let init = Bil.[
      r3 := int x;
      r2 := int y;
    ] in
  let expected = Word.(x - y) in
  check_gpr init bytes r1 expected arch ctxt

let subfe arch ctxt =
  let name = "SUBFE" in
  let bytes = make_insn ~name `XO [31; 1; 2; 3; 0; 136; 0] in
  let x = Word.of_int64 0xABCDEF42_12345678L in
  let y = Word.of_int64 0x42L in
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let r3 = find_gpr "R3" in
  let init = Bil.[
      r3 := int x;
      r2 := int y;
      ca := int Word.b1
    ] in
  let expected = Word.((lnot y) + x + b1) in
  check_gpr init bytes r1 expected arch ctxt

let subfme arch ctxt =
  let name = "SUBFME" in
  let bytes = make_insn ~name `XO [31; 1; 2; 0; 0; 232; 0] in
  let x = Word.of_int64 0xABCDEF42_12345678L in
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let init = Bil.[
      r2 := int x;
      ca := int Word.b1
    ] in
  let expected = Word.(lnot x) in
  check_gpr init bytes r1 expected arch ctxt

let subfze arch ctxt =
  let name = "SUBFZE" in
  let bytes = make_insn ~name `XO [31; 1; 2; 0; 0; 200; 0] in
  let x = Word.of_int64 0xABCDEF42_12345678L in
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let init = Bil.[
      r2 := int x;
      ca := int Word.b1
    ] in
  let expected = Word.((lnot x) + b1) in
  check_gpr init bytes r1 expected arch ctxt

let suite = "arith" >::: [
    "subf"    >:: subf `ppc;
    "subfic"  >:: subfic `ppc;
    "subfc"   >:: subfc `ppc;
    "subfe"   >:: subfe `ppc;
    "subme"   >:: subfme `ppc;
    (* "subze"   >:: subfze `ppc; *)
  ]
