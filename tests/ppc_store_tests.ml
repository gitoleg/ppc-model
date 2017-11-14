open Core_kernel.Std
open Bap.Std
open OUnit2

open Ppc_model
open Ppc_tests_helpers

let stb32 ctxt =
  let bytes = "\x99\x3c\x01\x6c" in (** stb r9, 364(r28) *)
  let r9 = find_gpr "R9" in
  let r28 = find_gpr "R28" in
  let mem = PPC32.mem in
  let addr = 0xABCDEF00 in
  let ea = Word.of_int ~width:32 (addr + 364) in
  let init = Bil.[
    r9 := int (Word.of_int ~width:64 0xFFFFFF42);
    r28 := int (Word.of_int ~width:64 addr);
  ] in
  let expected = Word.of_int ~width:8 0x42 in
  check_mem init bytes mem ~addr:ea ~size:`r8 expected `ppc ctxt

let stb32_zero_op ctxt =
  let bytes = "\x99\x20\x01\x6C" in (** stb r9, 364(0)  *)
  let r9 = find_gpr "R9" in
  let mem = PPC32.mem in
  let ea = Word.of_int ~width:32 364 in
  let init = Bil.[
    r9 := int (Word.of_int ~width:64 0xFFFFFF42);
  ] in
  let expected = Word.of_int ~width:8 0x42 in
  check_mem init bytes mem ~addr:ea ~size:`r8 expected `ppc ctxt

let stw32 ctxt =
  let bytes = "\x91\x28\xff\xd4" in (** stw r9, -44(r8)  *)
  let r8 = find_gpr "R8" in
  let r9 = find_gpr "R9" in
  let mem = PPC32.mem in
  let addr = 0xABCDEF44 in
  let ea = Word.of_int ~width:32 (addr - 44) in
  let data = Word.of_int64 0xFFFFFFFFABCDEF42L in
  let init = Bil.[
    r9 := int data;
    r8 := int (Word.of_int ~width:64 addr);
  ] in
  let expected = Word.of_int ~width:32 0xABCDEF42 in
  check_mem init bytes mem ~addr:ea ~size:`r32 expected `ppc ctxt

let stbx32 ctxt =
  let bytes = "\x7d\x2e\xf9\xae" in (** stbx r9, r14, r31 *)
  let r9 = find_gpr "R9" in
  let r14 = find_gpr "R14" in
  let r31 = find_gpr "R31" in
  let mem = PPC32.mem in
  let x = Word.of_int ~width:32 0xABCD0000 in
  let y = Word.of_int ~width:32 0x0000EF42 in
  let ea = Word.(x + y) in
  let data = Word.of_int64 0xFFFFFFFFABCDEF42L in
  let init = Bil.[
      r9  := int data;
      r14 := int x;
      r31 := int y;
    ] in
  let expected = Word.of_int ~width:8 0x42 in
  check_mem init bytes mem ~addr:ea ~size:`r8 expected `ppc ctxt

let stbx32_zero_op ctxt =
  let bytes = "\x7d\x20\xf9\xae" in (**	stbx r9, 0, r31  *)
  let r9 = find_gpr "R9" in
  let r31 = find_gpr "R31" in
  let mem = PPC32.mem in
  let ea = Word.of_int ~width:32 0xABCDEF42 in
  let data = Word.of_int64 0xFFFFFFFFABCDEF42L in
  let init = Bil.[
      r9  := int data;
      r31 := int ea;
    ] in
  let expected = Word.of_int ~width:8 0x42 in
  check_mem init bytes mem ~addr:ea ~size:`r8 expected `ppc ctxt

let sthx32 ctxt =
  let bytes = "\x7d\x3e\xeb\x2e" in (** sthx r9, r30, r29  *)
  let r9 = find_gpr "R9" in
  let r29 = find_gpr "R29" in
  let r30 = find_gpr "R30" in
  let mem = PPC32.mem in
  let x = Word.of_int ~width:32 0xABCD0000 in
  let y = Word.of_int ~width:32 0x0000EF42 in
  let ea = Word.(x + y) in
  let data = Word.of_int64 0xFFFFFFFFABCDEF42L in
  let init = Bil.[
      r9  := int data;
      r29 := int x;
      r30 := int y;
    ] in
  let expected = Word.of_int ~width:16 0xEF42 in
  check_mem init bytes mem ~addr:ea ~size:`r16 expected `ppc ctxt

let stbu32 ctxt =
  let bytes = "\x9c\x9d\xff\xff" in (**  stbu r4, -1(r29) *)
  let r4 = find_gpr "R4" in
  let r29 = find_gpr "R29" in
  let mem = PPC32.mem in
  let addr = 0xABCDEF01 in
  let ea = Word.of_int ~width:32 (addr - 1) in
  let init = Bil.[
    r4 := int (Word.of_int ~width:64 0xFFFFFF42);
    r29 := int (Word.of_int ~width:64 addr);
  ] in
  let expected = Word.of_int ~width:8 0x42 in
  check_mem init bytes mem ~addr:ea ~size:`r8 expected `ppc ctxt;
  let expected_addr = Word.of_int ~width:64 (addr - 1) in
  check_gpr init bytes r29 expected_addr `ppc ctxt

let stwu32 ctxt =
  let bytes = "\x94\x21\xff\xf0" in (** stwu r1, -16(r1) *)
  let r1 = find_gpr "R1" in
  let mem = PPC32.mem in
  let addr = 0xABCDEF42 in
  let ea = Word.of_int ~width:32 (addr - 16) in
  let init = Bil.[
      r1 := int (Word.of_int ~width:64 addr);
  ] in
  let expected = Word.of_int ~width:32 addr in
  check_mem init bytes mem ~addr:ea ~size:`r32 expected `ppc ctxt;
  let expected_addr = Word.of_int ~width:64 (addr - 16) in
  check_gpr init bytes r1 expected_addr `ppc ctxt

let stbux32 ctxt =
  let bytes = "\x7d\x3f\xc9\xee" in (** stbux r9, r31, r25  *)
  let r9 = find_gpr "R9" in
  let r25 = find_gpr "R25" in
  let r31 = find_gpr "R31" in
  let mem = PPC32.mem in
  let x = Word.of_int ~width:32 0xABCD0000 in
  let y = Word.of_int ~width:32 0x0000EF42 in
  let ea = Word.(x + y) in
  let data = Word.of_int64 0xFFFFFFFFABCDEF42L in
  let init = Bil.[
      r31 := int x;
      r25 := int y;
      r9 := int data;
    ] in
  let expected = Word.of_int ~width:8 0x42 in
  check_mem init bytes mem ~addr:ea ~size:`r8 expected `ppc ctxt;
  let expected_addr = Word.of_int ~width:64 0xABCDEF42 in
  check_gpr init bytes r31 expected_addr `ppc ctxt

let stwux32 ctxt =
  let bytes = "\x7d\x41\x49\x6e" in (** stwux r10, r1, r9  *)
  let r1 = find_gpr "R1" in
  let r9 = find_gpr "R9" in
  let r10 = find_gpr "R10" in
  let mem = PPC32.mem in
  let x = Word.of_int ~width:32 0xABCD0000 in
  let y = Word.of_int ~width:32 0x00000001 in
  let ea = Word.(x + y) in
  let data = Word.of_int64 0xFFFFFFFFABCDEF42L in
  let init = Bil.[
      r1 := int x;
      r9 := int y;
      r10 := int data;
    ] in
  let expected = Word.of_int ~width:32 0xABCDEF42 in
  check_mem init bytes mem ~addr:ea ~size:`r32 expected `ppc ctxt;
  let expected_addr = Word.of_int ~width:64 0xABCD0001 in
  check_gpr init bytes r1 expected_addr `ppc ctxt

let suite = "load" >::: [
    "stb32"    >:: stb32;
    "stb32_0"  >:: stb32_zero_op;
    "stw32"    >:: stw32;
    "stbx32"   >:: stbx32;
    "stbx32_0" >:: stbx32_zero_op;
    "sthx32"   >:: sthx32;
    "stbu32"   >:: stbu32;
    "stwu32"   >:: stwu32;
    "stbux32"  >:: stbux32;
    "stwux32"  >:: stwux32;
  ]
