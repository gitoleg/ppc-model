open Core_kernel.Std
open Bap.Std
open OUnit2

open Powerpc_model
open Powerpc_tests_helpers

let env_of_arch = function
  | `ppc   -> mem32, 32
  | `ppc64 -> mem64, 64
  | _ -> failwith "ppc OR ppc64 arch only"

let stb arch ctxt =
  let bytes = "\x99\x3c\x01\x6c" in (** stb r9, 364(r28) *)
  let mem, addr_size = env_of_arch arch in
  let r9 = find_gpr "R9" in
  let r28 = find_gpr "R28" in
  let addr = 0xABCDEF00 in
  let ea = Word.of_int ~width:addr_size (addr + 364) in
  let data = Word.of_int64 0xFFFFFFFFABCDEF42L in
  let init = Bil.[
    r9 := int data;
    r28 := int (Word.of_int ~width:64 addr);
  ] in
  let expected = Word.of_int ~width:8 0x42 in
  check_mem init bytes mem ~addr:ea ~size:`r8 expected arch ctxt

let stb_zero_op arch ctxt =
  let bytes = "\x99\x20\x01\x6C" in (** stb r9, 364(0)  *)
  let mem, addr_size = env_of_arch arch in
  let r9 = find_gpr "R9" in
  let ea = Word.of_int ~width:addr_size 364 in
  let data = Word.of_int64 0xFFFFFFFFABCDEF42L in
  let init = Bil.[
      r9 := int data;
  ] in
  let expected = Word.of_int ~width:8 0x42 in
  check_mem init bytes mem ~addr:ea ~size:`r8 expected arch ctxt

let stw arch ctxt =
  let bytes = "\x91\x28\xff\xd4" in (** stw r9, -44(r8)  *)
  let mem, addr_size = env_of_arch arch in
  let r8 = find_gpr "R8" in
  let r9 = find_gpr "R9" in
  let addr = 0xABCDEF44 in
  let ea = Word.of_int ~width:addr_size (addr - 44) in
  let data = Word.of_int64 0xFFFFFFFFABCDEF42L in
  let init = Bil.[
    r9 := int data;
    r8 := int (Word.of_int ~width:64 addr);
  ] in
  let expected = Word.of_int ~width:32 0xABCDEF42 in
  check_mem init bytes mem ~addr:ea ~size:`r32 expected arch ctxt

let stbx arch ctxt =
  let bytes = "\x7d\x2e\xf9\xae" in (** stbx r9, r14, r31 *)
  let mem, addr_size = env_of_arch arch in
  let r9 = find_gpr "R9" in
  let r14 = find_gpr "R14" in
  let r31 = find_gpr "R31" in
  let x = Word.of_int ~width:addr_size 0xABCD0000 in
  let y = Word.of_int ~width:addr_size 0x0000EF42 in
  let ea = Word.(x + y) in
  let data = Word.of_int64 0xFFFFFFFFABCDEF42L in
  let init = Bil.[
      r9  := int data;
      r14 := int x;
      r31 := int y;
    ] in
  let expected = Word.of_int ~width:8 0x42 in
  check_mem init bytes mem ~addr:ea ~size:`r8 expected arch ctxt

let stbx_zero_op arch ctxt =
  let bytes = "\x7d\x20\xf9\xae" in (**	stbx r9, 0, r31  *)
  let mem, addr_size = env_of_arch arch in
  let r9 = find_gpr "R9" in
  let r31 = find_gpr "R31" in
  let ea = Word.of_int ~width:addr_size 0xABCDEF42 in
  let data = Word.of_int64 0xFFFFFFFFABCDEF42L in
  let init = Bil.[
      r9  := int data;
      r31 := int ea;
    ] in
  let expected = Word.of_int ~width:8 0x42 in
  check_mem init bytes mem ~addr:ea ~size:`r8 expected arch ctxt

let sthx arch ctxt =
  let bytes = "\x7d\x3e\xeb\x2e" in (** sthx r9, r30, r29  *)
  let mem, addr_size = env_of_arch arch in
  let r9 = find_gpr "R9" in
  let r29 = find_gpr "R29" in
  let r30 = find_gpr "R30" in
  let x = Word.of_int ~width:addr_size 0xABCD0000 in
  let y = Word.of_int ~width:addr_size 0x0000EF42 in
  let ea = Word.(x + y) in
  let data = Word.of_int64 0xFFFFFFFFABCDEF42L in
  let init = Bil.[
      r9  := int data;
      r29 := int x;
      r30 := int y;
    ] in
  let expected = Word.of_int ~width:16 0xEF42 in
  check_mem init bytes mem ~addr:ea ~size:`r16 expected arch ctxt

let stbu arch ctxt =
  let bytes = "\x9c\x9d\xff\xff" in (**  stbu r4, -1(r29) *)
  let mem, addr_size = env_of_arch arch in
  let r4 = find_gpr "R4" in
  let r29 = find_gpr "R29" in
  let addr = 0xABCDEF01 in
  let ea = Word.of_int ~width:addr_size (addr - 1) in
  let init = Bil.[
    r4 := int (Word.of_int ~width:64 0xFFFFFF42);
    r29 := int (Word.of_int ~width:64 addr);
  ] in
  let expected = Word.of_int ~width:8 0x42 in
  check_mem init bytes mem ~addr:ea ~size:`r8 expected arch ctxt;
  let expected_addr = Word.of_int ~width:64 (addr - 1) in
  check_gpr init bytes r29 expected_addr arch ctxt

let stwu arch ctxt =
  let bytes = "\x94\x21\xff\xf0" in (** stwu r1, -16(r1) *)
  let mem, addr_size = env_of_arch arch in
  let r1 = find_gpr "R1" in
  let addr = 0xABCDEF42 in
  let ea = Word.of_int ~width:addr_size (addr - 16) in
  let init = Bil.[
      r1 := int (Word.of_int ~width:64 addr);
  ] in
  let expected = Word.of_int ~width:32 addr in
  check_mem init bytes mem ~addr:ea ~size:`r32 expected arch ctxt;
  let expected_addr = Word.of_int ~width:64 (addr - 16) in
  check_gpr init bytes r1 expected_addr arch ctxt

let stbux arch ctxt =
  let bytes = "\x7d\x3f\xc9\xee" in (** stbux r9, r31, r25  *)
  let mem, addr_size = env_of_arch arch in
  let r9 = find_gpr "R9" in
  let r25 = find_gpr "R25" in
  let r31 = find_gpr "R31" in
  let x = 0xABCD0000 in
  let y = 0x0000EF42 in
  let ea = Word.of_int ~width:addr_size (x + y) in
  let data = Word.of_int64 0xFFFFFFFFABCDEF42L in
  let init = Bil.[
      r31 := int (Word.of_int ~width:64 x);
      r25 := int (Word.of_int ~width:64 y);
      r9 := int data;
    ] in
  let expected = Word.of_int ~width:8 0x42 in
  check_mem init bytes mem ~addr:ea ~size:`r8 expected arch ctxt;
  let expected_addr = Word.of_int ~width:64 0xABCDEF42 in
  check_gpr init bytes r31 expected_addr arch ctxt

let stwux arch ctxt =
  let bytes = "\x7d\x41\x49\x6e" in (** stwux r10, r1, r9  *)
  let mem, addr_size = env_of_arch arch in
  let r1 = find_gpr "R1" in
  let r9 = find_gpr "R9" in
  let r10 = find_gpr "R10" in
  let x = 0xABCD0000 in
  let y = 0x00000001 in
  let ea = Word.of_int ~width:addr_size (x + y) in
  let data = Word.of_int64 0xFFFFFFFFABCDEF42L in
  let init = Bil.[
      r1 := int (Word.of_int ~width:64 x);
      r9 := int (Word.of_int ~width:64 y);
      r10 := int data;
    ] in
  let expected = Word.of_int ~width:32 0xABCDEF42 in
  check_mem init bytes mem ~addr:ea ~size:`r32 expected arch ctxt;
  let expected_addr = Word.of_int ~width:64 0xABCD0001 in
  check_gpr init bytes r1 expected_addr arch ctxt

let std arch ctxt =
  let bytes = "\xf8\x29\x00\x08" in (** std r1, 8(r9) *)
  let mem, addr_size = env_of_arch arch in
  let r1 = find_gpr "R1" in
  let r9 = find_gpr "R9" in
  let addr = 0xABCDEF44 in
  let disp = 8 lsl 2 in
  let ea = Word.of_int ~width:addr_size (addr + disp) in
  let data = Word.of_int64 0xCCABDD42_ABCDEF42L in
  let init = Bil.[
    r9 := int (Word.of_int ~width:64 addr);
    r1 := int data;
  ] in
  let expected = data in
  check_mem init bytes mem ~addr:ea ~size:`r64 expected arch ctxt

let stdx arch ctxt =
  let bytes = "\x7c\x28\x49\x2a" in (** stdx r1, r8, r9 *)
  let mem, addr_size = env_of_arch arch in
  let r1 = find_gpr "R1" in
  let r8 = find_gpr "R8" in
  let r9 = find_gpr "R9" in
  let x = Word.of_int ~width:addr_size 0xABCD0000 in
  let y = Word.of_int ~width:addr_size 0x0000EF42 in
  let ea = Word.(x + y) in
  let data = Word.of_int64 0xCCABDD42_ABCDEF42L in
  let init = Bil.[
      r1 := int data;
      r8 := int x;
      r9 := int y;
    ] in
  let expected = data in
  check_mem init bytes mem ~addr:ea ~size:`r64 expected arch ctxt

let stdu arch ctxt =
  let bytes = "\xf8\x29\x00\x09" in (**  stdu r1, 8(r9) *)
  let mem, addr_size = env_of_arch arch in
  let r1 = find_gpr "R1" in
  let r9 = find_gpr "R9" in
  let addr = 0xABCDEF42 in
  let disp = 8 lsl 2 in
  let ea = Word.of_int ~width:addr_size (addr + disp) in
  let data = Word.of_int64 0xCCABDD42_ABCDEF42L in
  let init = Bil.[
      r9 := int (Word.of_int ~width:64 addr);
      r1 := int data;
  ] in
  let expected = data in
  check_mem init bytes mem ~addr:ea ~size:`r64 expected arch ctxt;
  let expected_addr = Word.of_int ~width:64 (addr + disp) in
  check_gpr init bytes r9 expected_addr arch ctxt

let stdux arch ctxt =
  let bytes = "\x7c\x28\x49\x6a" in (** stdux r1, r8, r9 *)
  let mem, addr_size = env_of_arch arch in
  let r1 = find_gpr "R1" in
  let r8 = find_gpr "R8" in
  let r9 = find_gpr "R9" in
  let x = 0xABCD0000 in
  let y = 0x0000EF42 in
  let ea = Word.of_int ~width:addr_size (x + y) in
  let data = Word.of_int64 0xCCABDD42_ABCDEF42L in
  let init = Bil.[
      r1 := int data;
      r8 := int (Word.of_int ~width:64 x);
      r9 := int (Word.of_int ~width:64 y);
    ] in
  let expected = data in
  check_mem init bytes mem ~addr:ea ~size:`r64 expected arch ctxt;
  let expected_addr = Word.of_int ~width:64 0xABCDEF42 in
  check_gpr init bytes r8 expected_addr arch ctxt

let stdux_big_addr ctxt =
  let bytes = "\x7c\x28\x49\x6a" in (** stdux r1, r8, r9 *)
  let mem, addr_size = env_of_arch `ppc64 in
  let r1 = find_gpr "R1" in
  let r8 = find_gpr "R8" in
  let r9 = find_gpr "R9" in
  let x = Word.of_int64 0xCABCEF42_ABCD0000L in
  let y = Word.of_int64 0x0000EF42L in
  let ea = Word.(x + y) in
  let data = Word.of_int64 0xCCABDD42_ABCDEF42L in
  let init = Bil.[
      r1 := int data;
      r8 := int x;
      r9 := int y;
    ] in
  let expected = data in
  check_mem init bytes mem ~addr:ea ~size:`r64 expected `ppc64 ctxt;
  let expected_addr = ea in
  check_gpr init bytes r8 expected_addr `ppc64 ctxt

let suite = "store" >::: [
    "stb32"    >:: stb `ppc;
    "stb32_0"  >:: stb_zero_op `ppc;
    "stw32"    >:: stw `ppc;
    "stbx32"   >:: stbx `ppc;
    "stbx32_0" >:: stbx_zero_op `ppc;
    "sthx32"   >:: sthx `ppc;
    "stbu32"   >:: stbu `ppc;
    "stwu32"   >:: stwu `ppc;
    "stbux32"  >:: stbux `ppc;
    "stwux32"  >:: stwux `ppc;
    "std32"    >:: std `ppc;
    "stdx32"   >:: stdx `ppc;
    "stdu32"   >:: stdu `ppc;
    "stdux32"  >:: stdux `ppc;

    "stb64"    >:: stb `ppc64;
    "stb64_0"  >:: stb_zero_op `ppc64;
    "stw64"    >:: stw `ppc64;
    "stbx64"   >:: stbx `ppc64;
    "stbx64_0" >:: stbx_zero_op `ppc64;
    "sthx64"   >:: sthx `ppc64;
    "stbu64"   >:: stbu `ppc64;
    "stwu64"   >:: stwu `ppc64;
    "stbux64"  >:: stbux `ppc64;
    "stwux64"  >:: stwux `ppc64;
    "std64"    >:: std `ppc64;
    "stdx64"   >:: stdx `ppc64;
    "stdu64"   >:: stdu `ppc64;
    "stdux64"  >:: stdux `ppc64;
    "stdux64a" >:: stdux_big_addr;
  ]
