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

let extend x = Word.extract_exn ~hi:63 x
let low x = Word.extract_exn ~hi:31 x
let high x = Word.extract_exn ~hi:63 ~lo:32 x

let  mulli arch ctxt =
  let name = "MULLI" in
  let y = 0b1_1111_1111_1111 in
  let bytes = make_insn ~name `D [7; 1; 2; y] in
  let x = Word.of_int64 0xABCDEFAA_BBCDEDAAL in
  let y = Word.of_int ~width:32 y in
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let init = Bil.[
      r2 := int x;
    ] in
  let expected = Word.(x * y) in
  check_gpr init bytes r1 expected arch ctxt

let mulhw arch ctxt =
  let name = "MULHW" in
  let bytes = make_insn ~name `XO [31; 1; 2; 3; 0; 75; 0] in
  let x = Word.of_int64 0xABCDEFAA_BBCDEDAAL in
  let y = Word.of_int64 0x00000000_00004242L in
  let z = Word.of_int64 0xFFFFFFFF_ABCDEFAAL in
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let r3 = find_gpr "R3" in
  let init = Bil.[
      r1 := int x;
      r2 := int y;
      r3 := int z;
    ] in
  let expected = Word.concat (high x) (high Word.(y * z)) in
  check_gpr init bytes r1 expected arch ctxt

let mulhwu  arch ctxt =
  let name = "MULHWU" in
  let bytes = make_insn ~name `XO [31; 1; 2; 3; 0; 11; 0] in
 let x = Word.of_int64 0xABCDEFAA_BBCDEDAAL in
  let y = Word.of_int64 0x4242L in
  let z = Word.of_int64 0xABCDEFAAL in
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let r3 = find_gpr "R3" in
  let init = Bil.[
      r1 := int x;
      r2 := int y;
      r3 := int z;
    ] in
  let expected = Word.concat (high x) (high Word.(y * z)) in
  check_gpr init bytes r1 expected arch ctxt

let mullw  arch ctxt =
  let name = "MULLW" in
  let bytes = make_insn ~name `XO [31; 1; 2; 3; 0; 235; 0] in
  let x = Word.of_int64 0xABCDEFAA_BBCDEDAAL in
  let y = Word.of_int64 0x12345678_42424242L in
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let r3 = find_gpr "R3" in
  let init = Bil.[
      r2 := int x;
      r3 := int y;
    ] in
  let low x = extend (low x) in
  let expected = Word.(low x * low y) in
  check_gpr init bytes r1 expected arch ctxt

let divw arch ctxt =
  let name = "DIVW" in
  let bytes = make_insn ~name `XO [31; 1; 2; 3; 0; 491; 0] in
  let x = Word.of_int64 0xABCDEFAA_88888888L in
  let y = Word.of_int64 0x12345678_44444444L in
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let r3 = find_gpr "R3" in
  let init = Bil.[
      r1 := int @@ Word.zero 64;
      r2 := int x;
      r3 := int y;
    ] in
  let x,y = Word.signed (low x), Word.signed (low y) in
  let z = Word.(x / y) in
  let expected = extend z in
  check_gpr init bytes r1 expected arch ctxt

let divwu  arch ctxt =
  let name = "DIVWU" in
  let bytes = make_insn ~name `XO [31; 1; 2; 3; 0; 459; 0] in
  let x = Word.of_int64 0xABCDEFAA_88888888L in
  let y = Word.of_int64 0x12345678_44444444L in
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let r3 = find_gpr "R3" in
  let init = Bil.[
      r1 := int @@ Word.zero 64;
      r2 := int x;
      r3 := int y;
    ] in
  let z = Word.(low x / low y) in
  let expected = extend z in
  check_gpr init bytes r1 expected arch ctxt

(** TODO: write test, it's just a stub  *)
let divwe arch ctxt =
  let name = "DIVWE" in
  let bytes = make_insn ~name `XO [31; 1; 2; 3; 0; 427; 0] in
  let x = Word.of_int64 0xABCDEFAA_88888888L in
  let y = Word.of_int64 0x12345678_44444444L in
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let r3 = find_gpr "R3" in
  let init = Bil.[
      r1 := int @@ Word.zero 64;
      r2 := int x;
      r3 := int y;
    ] in
  let shift = Word.of_int64 32L in
  let x = Word.(signed (x lsl shift)) in
  let y = Word.signed (low y) in
  let expected = extend (low Word.(x/y)) in
  check_gpr init bytes r1 expected arch ctxt

let divweu arch ctxt =
  let name = "DIVWEU" in
  let bytes = make_insn ~name `XO [31; 1; 2; 3; 0; 395; 0] in
  let x = Word.of_int64 0xABCDEFAA_88888888L in
  let y = Word.of_int64 0x12345678_44444444L in
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let r3 = find_gpr "R3" in
  let init = Bil.[
      r1 := int @@ Word.zero 64;
      r2 := int x;
      r3 := int y;
    ] in
  let shift = Word.of_int64 32L in
  let x = Word.(x lsl shift) in
  let y = low y in
  let expected = extend (low Word.(x/y)) in
  check_gpr init bytes r1 expected arch ctxt

let suite = "arith" >::: [
    "subf"      >:: subf `ppc;
    "subfic"   >:: subfic `ppc;
    "subfc"    >:: subfc `ppc;
    "subfe"    >:: subfe `ppc;
    "subme"  >:: subfme `ppc;
    "subze"   >:: subfze `ppc;

    "mulli"      >:: mulli `ppc;
    "mulhw"   >:: mulhw `ppc;
    "mulhwu" >:: mulhwu `ppc;
    "mullw"     >:: mullw `ppc;

    "divw"       >:: divw `ppc;
    "divwu"     >:: divwu `ppc;
    "divwe"     >:: divwe `ppc;
    "divweu"   >:: divweu `ppc;
  ]
