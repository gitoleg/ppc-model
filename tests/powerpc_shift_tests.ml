open Core_kernel.Std
open OUnit2
open Bap.Std

open Powerpc_tests_helpers

let slw arch sh ?(set_to_zero=false) ctxt =
  let name = "SLW" in
  let bytes = make_insn ~name `X [31; 9; 10; 11; 24; 0] in
  let x = Word.of_int64 0xFFEEAA42_AABBCCDDL in
  let shw = Word.of_int ~width:64 sh in
  let shw = if set_to_zero then
      let s = Word.of_int ~width:64 32 in
      Word.(shw lor s)
    else shw in
  let r9  = find_gpr "R9" in
  let r10 = find_gpr "R10" in
  let r11 = find_gpr "R11" in
  let init = Bil.[
      r9 := int x;
      r11 := int shw;
    ] in
  if set_to_zero then
    check_gpr init bytes r10 (Word.zero 64) arch ctxt
  else
    let x = Word.extract_exn ~hi:31 Word.(x lsl shw) in
    let expected = Word.concat (Word.zero 32) x in
    check_gpr init bytes r10 expected arch ctxt

let srw arch sh ?(set_to_zero=false) ctxt =
  let name = "SRW" in
  let bytes = make_insn ~name `X [31; 9; 10; 11; 536; 0] in
  let x = Word.of_int64 0xFFEEAA42_AABBCCDDL in
  let shw = Word.of_int ~width:64 sh in
  let shw = if set_to_zero then
      let s = Word.of_int ~width:64 32 in
      Word.(shw lor s)
    else shw in
  let r9  = find_gpr "R9" in
  let r10 = find_gpr "R10" in
  let r11 = find_gpr "R11" in
  let init = Bil.[
      r9 := int x;
      r11 := int shw;
    ] in
  if set_to_zero then
    check_gpr init bytes r10 (Word.zero 64) arch ctxt
  else
    let x = Word.((extract_exn ~hi:31 x) lsr shw) in
    let expected = Word.concat (Word.zero 32) x in
    check_gpr init bytes r10 expected arch ctxt

let srawi arch sh x ctxt =
  let name = "SRAWI" in
  let bytes = make_insn ~name `X [31; 9; 10; sh; 824; 0] in
  let x = Word.of_int64 x in
  let shw = Word.of_int ~width:64 sh in
  let r9  = find_gpr "R9" in
  let r10 = find_gpr "R10" in
  let init = Bil.[
      r9 := int x;
    ] in
  let left_bits = 32 + sh in
  let left =
    if Word.(equal (extract_exn ~hi:31 ~lo:31 x) b1) then
      Word.ones left_bits
    else
      Word.zero left_bits in
  let right_bits = 31 - sh in
  let y = Word.((extract_exn ~hi:31 x) lsr shw) in
  let right = Word.extract_exn ~hi:right_bits y in
  let expected = Word.concat left right in
  check_gpr init bytes r10 expected arch ctxt

let suite = "shift" >::: [
    (* "slw 4"          >:: slw `ppc 4; *)
    (* "slw 0"          >:: slw `ppc 0; *)
    (* "slw zero res"   >:: slw `ppc 4 ~set_to_zero:true; *)

    (* "srw 4"          >:: srw `ppc 4; *)
    (* "srw 0"          >:: srw `ppc 0; *)
    (* "srw zero res"   >:: srw `ppc 4 ~set_to_zero:true; *)

    "srawi 4, pos"   >:: srawi `ppc 4 0xFFFFFFFF_0ABBCCDDL;
    "srawi 4, neg"   >:: srawi `ppc 4 0x00000000_EABBCCDDL;
  ]
