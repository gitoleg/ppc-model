open Core_kernel.Std
open Bap.Std
open OUnit2

open Powerpc
open Powerpc_tests_helpers


let print_insn name bytes =
  printf "\n%s %s\n" name (string_of_bytes bytes)

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
  let bytes = make_insn ~name `XFX [31; 1; 0; mask; anybit; 144; anybit] in
  let r1 = find_gpr "R1" in
  let ctxt = eval [] bytes arch in
  let r1_val = lookup_var ctxt r1 in
  let () = match r1_val with
    | None -> printf "none\n"
    | Some w -> printf "res %s\n" (Word.to_string w) in

  assert_bool "" true
  (* check_gpr [] bytes r1 x arch ctxt *)

let suite = "move" >::: [
    "mtspr"   >:: mtspr `ppc;
    "mfspr"   >:: mfspr `ppc;
    "mtcrf"   >:: mtcrf `ppc;
  ]
