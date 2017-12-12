open Core_kernel.Std
open Bap.Std
open OUnit2

open Powerpc_rtl

module RTL = struct
  include Powerpc_rtl
  include Infix
end

module Dsl = Powerpc_dsl
open Dsl

open Powerpc_tests_helpers
module Model = Powerpc_model
open Model

let mtspr arch ctxt =
  let name = "MTSPR" in
  let anybit = 0 in
  let bytes = make_insn ~name `XFX [31; 1; 9; 467; anybit] in
  let r1 = find_gpr "R1" in
  let x = Word.of_int64 0xABCDEFAB_12345678L in
  let init = Bil.[
      r1 := int x;
    ] in
  check_gpr init bytes ctr x arch ctxt

let mfspr arch ctxt =
  let name = "MFSPR" in
  let anybit = 0 in
  let bytes = make_insn ~name `XFX [31; 1; 8; 339; anybit] in
  let r1 = find_gpr "R1" in
  let x = Word.of_int64 0xABCDEFAB_12345678L in
  let init = Bil.[
      lr := int x;
    ] in
  check_gpr init bytes r1 x arch ctxt

let mtcrf arch ctxt =
  let name = "MTCRF" in
  let anybit = 0 in
  let mask = 0b01101001 in
  let set_bits = [1;2;4;7] in
  let bytes = make_insn ~name `XFX [31; 1; 0; mask; anybit; 144; anybit] in
  let x = Word.of_int64 0xABCDEFAB_12345678L in
  let r1 = find_gpr "R1" in
  let init = Bil.[
      r1 := int x;
    ] in
  let init_rtl = RTL.[ Hardware.cr := zero] in
  let init = init @ bil_of_t init_rtl in
  let ctxt = eval init bytes arch in
  let crs = Model.Hardware_vars.cri in
  let range = List.range 0 32 in
  let r1_bits = Word.enum_bits (Word.extract_exn ~hi:31 x) BigEndian in
  let r1_bits = Seq.to_array r1_bits in
  List.iter range ~f:(fun i ->
      if List.exists set_bits ~f:(fun j ->
          j*4 <= i && i < (j + 1) * 4) then
        let expected =
          if r1_bits.(i) then Word.b1 else Word.b0 in
        let cr_bit = Int.Map.find_exn crs i in
        let value = lookup_var ctxt cr_bit in
        let err = sprintf "mtcrf assign to bit %d failed\n" i in
        assert_bool err (is_equal_words expected value))

let mfcr arch ctxt =
  let name = "MFCR" in
  let bytes = make_insn ~name `XFX [31; 1; 0; 0;0; 19; 0] in
  let r1 = find_gpr "R1" in
  let x = Word.of_int64 ~width:32 0x12345678L in
  let xw = Exp.of_word x in
  let init_rtl = RTL.[
      Hardware.cr := xw;
    ] in
  let init = bil_of_t init_rtl in
  let expected = Word.extract_exn ~hi:63 x in
  check_gpr init bytes r1 expected arch ctxt

let suite = "move" >::: [
    "mtspr"   >:: mtspr `ppc;
    "mfspr"   >:: mfspr `ppc;
    "mtcrf"   >:: mtcrf `ppc;
    "mfcr"    >:: mfcr  `ppc;

    "mtspr 64"   >:: mtspr `ppc64;
    "mfspr 64"   >:: mfspr `ppc64;
    "mtcrf 64"   >:: mtcrf `ppc64;
    "mfcr  64"   >:: mfcr  `ppc64
  ]
