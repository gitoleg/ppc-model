open Core_kernel.Std
open Bap.Std
open OUnit2

open Powerpc_tests_helpers

let print_insn name bytes =
  printf "\n%s %s\n" name (string_of_bytes bytes)

let crand arch ctxt =
  let name = "CRAND" in
  let bytes = make_insn ~name `XL [19; 1; 2; 3; 257; 0] in
  let cr1 = cr_bit 1 in
  let cr2 = cr_bit 2 in
  let cr3 = cr_bit 3 in
  let init = Bil.[
      cr2 := int Word.b1;
      cr3 := int Word.b1;
    ] in
  check_gpr init bytes cr1 Word.b1 arch ctxt

let crnand arch ctxt =
  let name = "CRNAND" in
  let bytes = make_insn ~name `XL [19; 1; 2; 3; 225; 0] in
  let cr1 = cr_bit 1 in
  let cr2 = cr_bit 2 in
  let cr3 = cr_bit 3 in
  let init = Bil.[
      cr2 := int Word.b1;
      cr3 := int Word.b1;
    ] in
  check_gpr init bytes cr1 Word.b0 arch ctxt

let cror arch ctxt =
  let name = "CROR" in
  let bytes = make_insn ~name `XL [19; 1; 2; 3; 449; 0] in
  let cr1 = cr_bit 1 in
  let cr2 = cr_bit 2 in
  let cr3 = cr_bit 3 in
  let init = Bil.[
      cr2 := int Word.b1;
      cr3 := int Word.b0;
    ] in
  check_gpr init bytes cr1 Word.b1 arch ctxt

let crxor arch ctxt =
  let name = "CRXOR" in
  let bytes = make_insn ~name `XL [19; 1; 2; 3; 193; 0] in
  let cr1 = cr_bit 1 in
  let cr2 = cr_bit 2 in
  let cr3 = cr_bit 3 in
  let init = Bil.[
      cr2 := int Word.b1;
      cr3 := int Word.b1;
    ] in
  check_gpr init bytes cr1 Word.b0 arch ctxt

let crnor arch ctxt =
  let name = "CRNOR" in
  let bytes = make_insn ~name `XL [19; 1; 2; 3; 33; 0] in
  let cr1 = cr_bit 1 in
  let cr2 = cr_bit 2 in
  let cr3 = cr_bit 3 in
  let init = Bil.[
      cr2 := int Word.b0;
      cr3 := int Word.b0;
    ] in
  check_gpr init bytes cr1 Word.b1 arch ctxt

let creqv arch ctxt =
  let name = "CREQV" in
  let bytes = make_insn ~name `XL [19; 1; 2; 3; 289; 0] in
  let cr1 = cr_bit 1 in
  let cr2 = cr_bit 2 in
  let cr3 = cr_bit 3 in
  let init = Bil.[
      cr2 := int Word.b1;
      cr3 := int Word.b1;
    ] in
  check_gpr init bytes cr1 Word.b1 arch ctxt

let crandc arch ctxt =
  let name = "CRANDC" in
  let bytes = make_insn ~name `XL [19; 1; 2; 3; 129; 0] in
  let cr1 = cr_bit 1 in
  let cr2 = cr_bit 2 in
  let cr3 = cr_bit 3 in
  let init = Bil.[
      cr2 := int Word.b1;
      cr3 := int Word.b0;
    ] in
  check_gpr init bytes cr1 Word.b1 arch ctxt

let crorc arch ctxt =
  let name = "CRORC" in
  let bytes = make_insn ~name `XL [19; 1; 2; 3; 417; 0] in
  let cr1 = cr_bit 1 in
  let cr2 = cr_bit 2 in
  let cr3 = cr_bit 3 in
  let init = Bil.[
      cr2 := int Word.b0;
      cr3 := int Word.b0;
    ] in
  check_gpr init bytes cr1 Word.b1 arch ctxt

let mcrf arch ctxt =
  let name = "MCRF" in
  let bytes = make_insn ~name `XL [19; 1; 2; 3; 257; 0] in
  let cr1 = cr_bit 1 in
  let cr2 = cr_bit 2 in
  let cr3 = cr_bit 3 in
  let init = Bil.[
      cr2 := int Word.b1;
      cr3 := int Word.b1;
    ] in
  check_gpr init bytes cr1 Word.b1 arch ctxt

let suite  = "CR" >::: [
    "crand"   >:: crand `ppc;
    "crnand"  >:: crnand `ppc;
    "cror"    >:: cror `ppc;
    "crxor"   >:: crxor `ppc;
    "crnor"   >:: crnor `ppc;
    "creqv"   >:: creqv `ppc;
    "crandc"  >:: crandc `ppc;
    "crorc"   >:: crorc `ppc;
  ]
