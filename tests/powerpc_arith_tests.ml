open Core_kernel.Std
open Bap.Std
open OUnit2

open Powerpc
open Powerpc_tests_helpers

(** TODO: add ca flags testing  *)

let extend ?(upto=64) x = Word.extract_exn ~hi:(upto - 1) x
let low ?(len=32) x = Word.extract_exn ~hi:(len - 1) x

let high ?(len=32) x =
  let width = Word.bitwidth x in
  let hi = width - 1 in
  let lo = width - len in
  Word.extract_exn ~hi ~lo x

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

let subfic arch ctxt =
  let name = "SUBFIC" in
  let x = 42 in
  let bytes = make_insn ~name `D [8; 1; 2; x] in
  let x = Word.of_int ~width:64 x in
  let y = Word.of_int64 43L in
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let init = Bil.[
      r2 := int y;
  ] in
  let expected = Word.(x - y) in
  let ctxt = eval init bytes arch in
  let r1_val = lookup_var ctxt r1 in
  let ca_val = lookup_var ctxt ca in
  let ca32_val = lookup_var ctxt ca32 in
  let ca_exp = Word.b1 in
  let ca32_exp = Word.b1 in
  assert_bool "subfic failed, result" (is_equal_words expected r1_val);
  assert_bool "subfic failed, ca" (is_equal_words ca_exp ca_val);
  assert_bool "subfic failed, ca32" (is_equal_words ca32_exp ca32_val)

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

let mulli arch ctxt =
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
  let left = Word.of_int64 ~width:32 0xFFFFFFFFL in
  let expected = Word.concat left (high Word.(y * z)) in
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
  let left = Word.zero 32 in
  let expected = Word.concat left (high Word.(y * z)) in
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
  let x = Word.signed (low x) in
  let y = Word.signed (low y) in
  let expected = extend Word.(signed (x / y)) in
  check_gpr init bytes r1 expected arch ctxt

let divwu arch ctxt =
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
  let expected = extend @@ Word.signed (low Word.(x/y)) in
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

let modsw arch ctxt =
  let name = "MODSW" in
  let bytes = make_insn ~name `X [31; 1; 2; 3; 779; 0] in
  let x = Word.of_int64 0xABCDEFAB_CDEFABCDL in
  let y = Word.of_int64 0x12345678_91234567L in
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let r3 = find_gpr "R3" in
  let init = Bil.[
      r2 := int x;
      r3 := int y;
    ] in
  let x = Word.signed (low x) in
  let y = Word.signed (low y) in
  let expected = extend (Word.signed Word.(x mod y)) in
  check_gpr init bytes r1 expected arch ctxt

let moduw arch ctxt =
  let name = "MODUW" in
  let bytes = make_insn ~name `X [31; 1; 2; 3; 267; 0] in
  let x = Word.of_int64 0xABCDEFAB_CDEFABCDL in
  let y = Word.of_int64 0x12345678_91234567L in
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let r3 = find_gpr "R3" in
  let init = Bil.[
      r2 := int x;
      r3 := int y;
    ] in
  let expected = extend Word.(low x mod low y) in
  check_gpr init bytes r1 expected arch ctxt

let mulld arch ctxt =
  let name = "MULLD" in
  let bytes = make_insn ~name `XO [31; 1; 2; 3; 0; 233; 0] in
  let x = Word.of_int64 0xABCDEFAB_CDEFABCDL in
  let y = Word.of_int64 0x12345678_91234567L in
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let r3 = find_gpr "R3" in
  let init = Bil.[
      r2 := int x;
      r3 := int y;
    ] in
  let expected = Word.(x * y) in
  check_gpr init bytes r1 expected arch ctxt

let mulhd arch ctxt =
  let name = "MULHD" in
  let bytes = make_insn ~name `XO [31; 1; 2; 3; 0; 73; 0] in
  let x = Word.of_int64 0xABCDEFAB_CDEFABCDL in
  let y = Word.of_int64 0x12345678_91234567L in
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let r3 = find_gpr "R3" in
  let init = Bil.[
      r2 := int x;
      r3 := int y;
    ] in
  let x = Word.signed x in
  let y = Word.signed y in
  let x = extend ~upto:128 x in
  let y = extend ~upto:128 y in
  let expected = high ~len:64 Word.(x * y) in
  check_gpr init bytes r1 expected arch ctxt

let mulhdu arch ctxt =
  let name = "MULHDU" in
  let bytes = make_insn ~name `XO [31; 1; 2; 3; 0; 9; 0] in
  let x = Word.of_int64 0xABCDEFAB_CDEFABCDL in
  let y = Word.of_int64 0x12345678_91234567L in
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let r3 = find_gpr "R3" in
  let init = Bil.[
      r2 := int x;
      r3 := int y;
    ] in
  let x = extend ~upto:128 x in
  let y = extend ~upto:128 y in
  let expected = high ~len:64 Word.(x * y) in
  check_gpr init bytes r1 expected arch ctxt

let divd arch ctxt =
  let name = "DIVD" in
  let bytes = make_insn ~name `XO [31; 1; 2; 3; 0; 489; 0] in
  let x = Word.of_int64 0xABCDEFAB_CDEFABCDL in
  let y = Word.of_int64 0x12345678_91234567L in
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let r3 = find_gpr "R3" in
  let init = Bil.[
      r2 := int x;
      r3 := int y;
    ] in
  let x = Word.signed x in
  let y = Word.signed y in
  let expected = Word.(x / y) in
  check_gpr init bytes r1 expected arch ctxt

let divdu arch ctxt =
  let name = "DIVDU" in
  let bytes = make_insn ~name `XO [31; 1; 2; 3; 0; 457; 0] in
  let x = Word.of_int64 0xABCDEFAB_CDEFABCDL in
  let y = Word.of_int64 0x12345678_91234567L in
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let r3 = find_gpr "R3" in
  let init = Bil.[
      r2 := int x;
      r3 := int y;
    ] in
  let expected = Word.(x / y) in
  check_gpr init bytes r1 expected arch ctxt

let divde arch ctxt =
  let name = "DIVDE" in
  let bytes = make_insn ~name `XO [31; 1; 2; 3; 0; 425; 0] in
  let x = Word.of_int64 0xABCDEFAB_CDEFABCDL in
  let y = Word.of_int64 0x12345678_91234567L in
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let r3 = find_gpr "R3" in
  let init = Bil.[
      r2 := int x;
      r3 := int y;
    ] in
  let shift = Word.of_int64 64L in
  let x = extend ~upto:128 x in
  let x = Word.(x lsl shift) in
  let x = Word.signed x in
  let expected = low ~len:64 Word.(x / y) in
  check_gpr init bytes r1 expected arch ctxt

let divdeu arch ctxt =
  let name = "DIVDEU" in
  let bytes = make_insn ~name `XO [31; 1; 2; 3; 0; 393; 0] in
  let x = Word.of_int64 0xABCDEFAB_CDEFABCDL in
  let y = Word.of_int64 0x12345678_91234567L in
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let r3 = find_gpr "R3" in
  let init = Bil.[
      r2 := int x;
      r3 := int y;
    ] in
  let shift = Word.of_int64 64L in
  let x = extend ~upto:128 x in
  let x = Word.(x lsl shift) in
  let expected = low ~len:64 Word.(x / y) in
  check_gpr init bytes r1 expected arch ctxt

let modsd arch ctxt =
  let name = "MODSD" in
  let bytes = make_insn ~name `X [31; 1; 2; 3; 777; 0] in
  let x = Word.of_int64 0xABCDEFAB_CDEFABCDL in
  let y = Word.of_int64 0x12345678_91234567L in
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let r3 = find_gpr "R3" in
  let init = Bil.[
      r2 := int x;
      r3 := int y;
    ] in
  let x = Word.signed x in
  let y = Word.signed y in
  let expected = Word.(x mod y) in
  check_gpr init bytes r1 expected arch ctxt

let modud arch ctxt =
  let name = "MODUD" in
  let bytes = make_insn ~name `X [31; 1; 2; 3; 265; 0] in
  let x = Word.of_int64 0xABCDEFAB_CDEFABCDL in
  let y = Word.of_int64 0x12345678_91234567L in
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let r3 = find_gpr "R3" in
  let init = Bil.[
      r2 := int x;
      r3 := int y;
    ] in
  let expected = Word.(x mod y) in
  check_gpr init bytes r1 expected arch ctxt

let suite = "arith" >::: [
    "subf"     >:: subf `ppc;
    "subfic"   >:: subfic `ppc;
    "subfc"    >:: subfc `ppc;
    "subfe"    >:: subfe `ppc;
    "subme"    >:: subfme `ppc;
    "subze"    >:: subfze `ppc;
    "mulli"    >:: mulli `ppc;
    "mulhw"    >:: mulhw `ppc;
    "mulhwu"   >:: mulhwu `ppc;
    "mullw"    >:: mullw `ppc;
    "divw"     >:: divw `ppc;
    "divwu"    >:: divwu `ppc;
    "divwe"    >:: divwe `ppc;
    "divweu"   >:: divweu `ppc;
    "modsw"    >:: modsw `ppc;
    "moduw"    >:: moduw `ppc;
    "mulld"    >:: mulld `ppc;
    "mulhd"    >:: mulhd `ppc;
    "mulhdu"   >:: mulhdu `ppc;
    "divd"     >:: divd `ppc;
    "divdu"    >:: divdu `ppc;
    "divde"    >:: divde `ppc;
    "divdeu"   >:: divdeu `ppc;
    "modsd"    >:: modsd `ppc;
    "modud"    >:: modud `ppc;

    "subf 64"     >:: subf `ppc64;
    "subfic 64"   >:: subfic `ppc64;
    "subfc 64"    >:: subfc `ppc64;
    "subfe 64"    >:: subfe `ppc64;
    "subme 64"    >:: subfme `ppc64;
    "subze 64"    >:: subfze `ppc64;
    "mulli 64"    >:: mulli `ppc64;
    "mulhw 64"    >:: mulhw `ppc64;
    "mulhwu 64"   >:: mulhwu `ppc64;
    "mullw 64"    >:: mullw `ppc64;
    "divw 64"     >:: divw `ppc64;
    "divwu 64"    >:: divwu `ppc64;
    "divwe 64"    >:: divwe `ppc64;
    "divweu 64"   >:: divweu `ppc64;
    "modsw 64"    >:: modsw `ppc64;
    "moduw 64"    >:: moduw `ppc64;
    "mulld 64"    >:: mulld `ppc64;
    "mulhd 64"    >:: mulhd `ppc64;
    "mulhdu 64"   >:: mulhdu `ppc64;
    "divd 64"     >:: divd `ppc64;
    "divdu 64"    >:: divdu `ppc64;
    "divde 64"    >:: divde `ppc64;
    "divdeu 64"   >:: divdeu `ppc64;
    "modsd 64"    >:: modsd `ppc64;
    "modud 64"    >:: modud `ppc64;



  ]
