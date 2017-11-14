open Core_kernel.Std
open Bap.Std
open OUnit2

open Ppc_model
open Ppc_tests_helpers

let endian = BigEndian

let lbz32 ctxt =
  let bytes = "\x89\x3c\x00\x14" in (** lbz r9, 20(r28) *)
  let r9 = find_gpr "R9" in
  let r28 = find_gpr "R28" in
  let addr = 0xABCDEF00 in
  let ea = Word.of_int ~width:32 (addr + 20) in
  let mem = PPC32.mem in
  let value = 0x42 in
  let data = Word.of_int ~width:32 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r8;
      r28 := int (Word.of_int ~width:64 addr);
      r9 := int (Word.of_int64 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width:64 value in
  check_gpr init bytes r9 expected `ppc ctxt

let lbz32_zero_op ctxt =
  let bytes = "\x89\x20\x00\x14" in (** lbz r9, 20(0) *)
  let r9 = find_gpr "R9" in
  let ea = Word.of_int ~width:32 20 in
  let mem = PPC32.mem in
  let value = 0x42 in
  let data = Word.of_int ~width:32 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r8;
      r9 := int (Word.of_int64 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width:64 value in
  check_gpr init bytes r9 expected `ppc ctxt

let lwz32 ctxt =
  let bytes = "\x83\xeb\xff\xfc" in (** lwz r31, -4(r11) *)
  let r31 = find_gpr "R31" in
  let r11 = find_gpr "R11" in
  let addr = 0xABCDEF04 in
  let ea = Word.of_int ~width:32 (addr - 4) in
  let mem = PPC32.mem in
  let value = 0xAA42FF42 in
  let data = Word.of_int ~width:32 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r32;
      r11 := int (Word.of_int ~width:64 addr);
      r31 := int (Word.of_int64 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width:64 value in
  check_gpr init bytes r31 expected `ppc ctxt

let lbzx32 ctxt =
  let bytes = "\x7d\x3d\x50\xae" in (** lbzx r9, r29, r10 *)
  let r9 = find_gpr "R9" in
  let r10 = find_gpr "R10" in
  let r29 = find_gpr "R29" in
  let x = Word.of_int ~width:32 0xABCD0000 in
  let y = Word.of_int ~width:32 0x0000FF42 in
  let ea = Word.(x + y) in
  let mem = PPC32.mem in
  let value = 0x42 in
  let data = Word.of_int ~width:32 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r8;
      r29 := int x;
      r10 := int y;
      r9 := int (Word.of_int64 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width:64 value in
  check_gpr init bytes r9 expected `ppc ctxt

let lbzx32_zero_op ctxt =
  let bytes = "\x7d\x20\x50\xae" in (** lbzx r9, 0, r10 *)
  let r9 = find_gpr "R9" in
  let r10 = find_gpr "R10" in
  let ea = Word.of_int ~width:32 0xABCD0000 in
  let mem = PPC32.mem in
  let value = 0x42 in
  let data = Word.of_int ~width:32 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r8;
      r10 := int ea;
      r9 := int (Word.of_int64 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width:64 value in
  check_gpr init bytes r9 expected `ppc ctxt

let lwzx32 ctxt =
  let bytes = "\x7d\x3d\x50\x2e" in (** lwzx r9, r29, r10 *)
  let r9 = find_gpr "R9" in
  let r10 = find_gpr "R10" in
  let r29 = find_gpr "R29" in
  let x = Word.of_int ~width:32 0xABCD0000 in
  let y = Word.of_int ~width:32 0x0000FF42 in
  let ea = Word.(x + y) in
  let mem = PPC32.mem in
  let value = 0xAA42FF42 in
  let data = Word.of_int ~width:32 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r32;
      r29 := int x;
      r10 := int y;
      r9 := int (Word.of_int64 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width:64 value in
  check_gpr init bytes r9 expected `ppc ctxt

let lbzu32 ctxt =
  let bytes = "\x8d\x3c\x00\x14" in (** lbzu r9, 20(r28)  *)
  let r9 = find_gpr "R9" in
  let r28 = find_gpr "R28" in
  let addr = 0xABCDEF04 in
  let ea = Word.of_int ~width:32 (addr + 20) in
  let mem = PPC32.mem in
  let value = 0x42 in
  let data = Word.of_int ~width:32 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r8;
      r28 := int (Word.of_int ~width:64 addr);
      r9 := int (Word.of_int64 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width:64 value in
  check_gpr init bytes r9 expected `ppc ctxt;
  let expected_addr = Word.of_int ~width:64 (addr + 20) in
  check_gpr init bytes r28 expected_addr `ppc ctxt

let lwzu32 ctxt =
  let bytes = "\x85\x3f\xff\xfc" in (** lwzu r9, -4(r31)  *)
  let r9 = find_gpr "R9" in
  let r31 = find_gpr "R31" in
  let addr = 0xABCDEF04 in
  let ea = Word.of_int ~width:32 (addr - 4) in
  let mem = PPC32.mem in
  let value = 0xAA42FF42 in
  let data = Word.of_int ~width:32 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r32;
      r31 := int (Word.of_int ~width:64 addr);
      r9 := int (Word.of_int64 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width:64 value in
  check_gpr init bytes r9 expected `ppc ctxt;
  let expected_addr = Word.of_int ~width:64 (addr - 4) in
  check_gpr init bytes r31 expected_addr `ppc ctxt

let lbzux32 ctxt =
  let bytes = "\x7d\x3d\x50\xee" in (** lbzux r9, r29, r10 *)
  let r9 = find_gpr "R9" in
  let r10 = find_gpr "R10" in
  let r29 = find_gpr "R29" in
  let x = Word.of_int ~width:32 0xABCD0000 in
  let y = Word.of_int ~width:32 0x0000EF42 in
  let ea = Word.(x + y) in
  let mem = PPC32.mem in
  let value = 0x42 in
  let data = Word.of_int ~width:32 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r8;
      r29 := int x;
      r10 := int y;
      r9 := int (Word.of_int64 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width:64 value in
  check_gpr init bytes r9 expected `ppc ctxt;
  let expected_addr = Word.of_int ~width:64 0xABCDEF42 in
  check_gpr init bytes r29 expected_addr `ppc ctxt

let lbz64 ctxt =
  let bytes = "\x89\x3c\x00\x14" in (** lbz r9, 20(r28) *)
  let r9 = find_gpr "R9" in
  let r28 = find_gpr "R28" in
  let addr = 0xABCDEFABCDEF00 in
  let ea = Word.of_int ~width:64 (addr + 20) in
  let mem = PPC64.mem in
  let value = 0x42 in
  let data = Word.of_int ~width:64 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r8;
      r28 := int (Word.of_int ~width:64 addr);
      r9 := int (Word.of_int64 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width:64 value in
  check_gpr init bytes r9 expected `ppc64 ctxt

let lbz64_zero_op ctxt =
  let bytes = "\x89\x20\x00\x14" in (** lbz r9, 20(0) *)
  let r9 = find_gpr "R9" in
  let ea = Word.of_int ~width:64 20 in
  let mem = PPC64.mem in
  let value = 0x42 in
  let data = Word.of_int ~width:64 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r8;
      r9 := int (Word.of_int64 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width:64 value in
  check_gpr init bytes r9 expected `ppc64 ctxt

let lwz64 ctxt =
  let bytes = "\x83\xeb\xff\xfc" in (** lwz r31, -4(r11) *)
  let r31 = find_gpr "R31" in
  let r11 = find_gpr "R11" in
  let addr = 0xABCDEFABCDEF04 in
  let ea = Word.of_int ~width:64 (addr - 4) in
  let mem = PPC64.mem in
  let value = 0xAA42FF42 in
  let data = Word.of_int ~width:64 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r32;
      r11 := int (Word.of_int ~width:64 addr);
      r31 := int (Word.of_int64 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width:64 value in
  check_gpr init bytes r31 expected `ppc64 ctxt

let lbzx64 ctxt =
  let bytes = "\x7d\x3d\x50\xae" in (** lbzx r9, r29, r10 *)
  let r9 = find_gpr "R9" in
  let r10 = find_gpr "R10" in
  let r29 = find_gpr "R29" in
  let x = Word.of_int ~width:64 0xABCDEFABCD0000 in
  let y = Word.of_int ~width:64 0x0000000000FF42 in
  let ea = Word.(x + y) in
  let mem = PPC64.mem in
  let value = 0x42 in
  let data = Word.of_int ~width:64 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r8;
      r29 := int x;
      r10 := int y;
      r9 := int (Word.of_int64 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width:64 value in
  check_gpr init bytes r9 expected `ppc64 ctxt

let lbzx64_zero_op ctxt =
  let bytes = "\x7d\x20\x50\xae" in (** lbzx r9, 0, r10 *)
  let r9 = find_gpr "R9" in
  let r10 = find_gpr "R10" in
  let ea = Word.of_int ~width:64 0xABCDEFABCD0000 in
  let mem = PPC64.mem in
  let value = 0x42 in
  let data = Word.of_int ~width:64 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r8;
      r10 := int ea;
      r9 := int (Word.of_int64 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width:64 value in
  check_gpr init bytes r9 expected `ppc64 ctxt

let lwzx64 ctxt =
  let bytes = "\x7d\x3d\x50\x2e" in (** lwzx r9, r29, r10 *)
  let r9 = find_gpr "R9" in
  let r10 = find_gpr "R10" in
  let r29 = find_gpr "R29" in
  let x = Word.of_int ~width:64 0xABCDEFABCD0000 in
  let y = Word.of_int ~width:64 0x0000000000FF42 in
  let ea = Word.(x + y) in
  let mem = PPC64.mem in
  let value = 0xAA42FF42 in
  let data = Word.of_int ~width:64 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r32;
      r29 := int x;
      r10 := int y;
      r9 := int (Word.of_int64 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width:64 value in
  check_gpr init bytes r9 expected `ppc64 ctxt

let lbzu64 ctxt =
  let bytes = "\x8d\x3c\x00\x14" in (** lbzu r9, 20(r28)  *)
  let r9 = find_gpr "R9" in
  let r28 = find_gpr "R28" in
  let addr = 0xABCDEFABCDEF04 in
  let ea = Word.of_int ~width:64 (addr + 20) in
  let mem = PPC64.mem in
  let value = 0x42 in
  let data = Word.of_int ~width:64 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r8;
      r28 := int (Word.of_int ~width:64 addr);
      r9 := int (Word.of_int64 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width:64 value in
  check_gpr init bytes r9 expected `ppc64 ctxt;
  let expected_addr = Word.of_int ~width:64 (addr + 20) in
  check_gpr init bytes r28 expected_addr `ppc64 ctxt

let lwzu64 ctxt =
  let bytes = "\x85\x3f\xff\xfc" in (** lwzu r9, -4(r31)  *)
  let r9 = find_gpr "R9" in
  let r31 = find_gpr "R31" in
  let addr = 0xABCDEFABCDEF04 in
  let ea = Word.of_int ~width:64 (addr - 4) in
  let mem = PPC64.mem in
  let value = 0xAA42FF42 in
  let data = Word.of_int ~width:64 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r32;
      r31 := int (Word.of_int ~width:64 addr);
      r9 := int (Word.of_int64 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width:64 value in
  check_gpr init bytes r9 expected `ppc64 ctxt;
  let expected_addr = Word.of_int ~width:64 (addr - 4) in
  check_gpr init bytes r31 expected_addr `ppc64 ctxt

let lbzux64 ctxt =
  let bytes = "\x7d\x3d\x50\xee" in (** lbzux r9, r29, r10 *)
  let r9 = find_gpr "R9" in
  let r10 = find_gpr "R10" in
  let r29 = find_gpr "R29" in
  let x = Word.of_int ~width:64 0xABCDEFABCD0000 in
  let y = Word.of_int ~width:64 0x0000000000EF42 in
  let ea = Word.(x + y) in
  let mem = PPC64.mem in
  let value = 0x42 in
  let data = Word.of_int ~width:64 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r8;
      r29 := int x;
      r10 := int y;
      r9 := int (Word.of_int64 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width:64 value in
  check_gpr init bytes r9 expected `ppc64 ctxt;
  let expected_addr = Word.of_int ~width:64 0xABCDEFABCDEF42 in
  check_gpr init bytes r29 expected_addr `ppc64 ctxt

let suite = "load" >::: [
    "lbz32"     >:: lbz32;
    "lbz32_0"   >:: lbz32_zero_op;
    "lwz32"     >:: lwz32;
    "lbzx32"    >:: lbzx32;
    "lbzx32_0"  >:: lbzx32_zero_op;
    "lwzx32"    >:: lwzx32;
    "lbzu32"    >:: lbzu32;
    "lwzu32"    >:: lwzu32;
    "lbzux32"   >:: lbzux32;
    "lbz64"     >:: lbz64;
    "lbz64_0"   >:: lbz64_zero_op;
    "lwz64"     >:: lwz64;
    "lbzx64"    >:: lbzx64;
    "lbzx64_0"  >:: lbzx64_zero_op;
    "lwzx64"    >:: lwzx64;
    "lbzu64"    >:: lbzu64;
    "lwzu64"    >:: lwzu64;
    "lbzux64"   >:: lbzux64;
  ]
