open Core_kernel.Std
open Bap.Std
open OUnit2

open Ppc_model
open Ppc_tests_helpers

let endian = BigEndian

let env_of_arch = function
  | `ppc   -> PPC32.mem, 32
  | `ppc64 -> PPC64.mem, 64
  | _ -> failwith "ppc OR ppc64 arch only"

let lbz arch ctxt =
  let bytes = "\x89\x3c\x00\x14" in (** lbz r9, 20(r28) *)
  let mem, addr_size = env_of_arch arch in
  let r9 = find_gpr "R9" in
  let r28 = find_gpr "R28" in
  let addr = 0xABCDEF00 in
  let ea = Word.of_int ~width:addr_size (addr + 20) in
  let value = 0x42 in
  let data = Word.of_int ~width:32 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r8;
      r28 := int (Word.of_int ~width:64 addr);
      r9 := int (Word.of_int64 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width:64 value in
  check_gpr init bytes r9 expected arch ctxt

let lbz_zero_op arch ctxt =
  let bytes = "\x89\x20\x00\x14" in (** lbz r9, 20(0) *)
  let mem, addr_size = env_of_arch arch in
  let r9 = find_gpr "R9" in
  let ea = Word.of_int ~width:addr_size 20 in
  let value = 0x42 in
  let data = Word.of_int ~width:32 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r8;
      r9 := int (Word.of_int64 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width:64 value in
  check_gpr init bytes r9 expected arch ctxt

let lwz arch ctxt =
  let bytes = "\x83\xeb\xff\xfc" in (** lwz r31, -4(r11) *)
  let mem, addr_size = env_of_arch arch in
  let r31 = find_gpr "R31" in
  let r11 = find_gpr "R11" in
  let addr = 0xABCDEF04 in
  let ea = Word.of_int ~width:addr_size (addr - 4) in
  let value = 0xAA42FF42 in
  let data = Word.of_int ~width:32 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r32;
      r11 := int (Word.of_int ~width:64 addr);
      r31 := int (Word.of_int64 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width:64 value in
  check_gpr init bytes r31 expected arch ctxt

let lbzx arch ctxt =
  let bytes = "\x7d\x3d\x50\xae" in (** lbzx r9, r29, r10 *)
  let mem, addr_size = env_of_arch arch in
  let r9 = find_gpr "R9" in
  let r10 = find_gpr "R10" in
  let r29 = find_gpr "R29" in
  let x = Word.of_int ~width:addr_size 0xABCD0000 in
  let y = Word.of_int ~width:addr_size 0x0000FF42 in
  let ea = Word.(x + y) in
  let value = 0x42 in
  let data = Word.of_int ~width:32 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r8;
      r29 := int x;
      r10 := int y;
      r9 := int (Word.of_int64 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width:64 value in
  check_gpr init bytes r9 expected arch ctxt

let lbzx_zero_op arch ctxt =
  let bytes = "\x7d\x20\x50\xae" in (** lbzx r9, 0, r10 *)
  let mem, addr_size = env_of_arch arch in
  let r9 = find_gpr "R9" in
  let r10 = find_gpr "R10" in
  let ea = Word.of_int ~width:addr_size 0xABCD0000 in
  let value = 0x42 in
  let data = Word.of_int ~width:32 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r8;
      r10 := int ea;
      r9 := int (Word.of_int64 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width:64 value in
  check_gpr init bytes r9 expected arch ctxt

let lwzx arch ctxt =
  let bytes = "\x7d\x3d\x50\x2e" in (** lwzx r9, r29, r10 *)
  let mem, addr_size = env_of_arch arch in
  let r9 = find_gpr "R9" in
  let r10 = find_gpr "R10" in
  let r29 = find_gpr "R29" in
  let x = Word.of_int ~width:addr_size 0xABCD0000 in
  let y = Word.of_int ~width:addr_size 0x0000FF42 in
  let ea = Word.(x + y) in
  let value = 0xAA42FF42 in
  let data = Word.of_int ~width:32 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r32;
      r29 := int x;
      r10 := int y;
      r9 := int (Word.of_int64 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width:64 value in
  check_gpr init bytes r9 expected arch ctxt

let lbzu arch ctxt =
  let bytes = "\x8d\x3c\x00\x14" in (** lbzu r9, 20(r28)  *)
  let mem, addr_size = env_of_arch arch in
  let r9 = find_gpr "R9" in
  let r28 = find_gpr "R28" in
  let addr = 0xABCDEF04 in
  let ea = Word.of_int ~width:addr_size (addr + 20) in
  let value = 0x42 in
  let data = Word.of_int ~width:32 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r8;
      r28 := int (Word.of_int ~width:64 addr);
      r9 := int (Word.of_int64 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width:64 value in
  check_gpr init bytes r9 expected arch ctxt;
  let expected_addr = Word.of_int ~width:64 (addr + 20) in
  check_gpr init bytes r28 expected_addr arch ctxt

let lwzu arch ctxt =
  let bytes = "\x85\x3f\xff\xfc" in (** lwzu r9, -4(r31)  *)
  let mem, addr_size = env_of_arch arch in
  let r9 = find_gpr "R9" in
  let r31 = find_gpr "R31" in
  let addr = 0xABCDEF04 in
  let ea = Word.of_int ~width:addr_size (addr - 4) in
  let value = 0xAA42FF42 in
  let data = Word.of_int ~width:32 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r32;
      r31 := int (Word.of_int ~width:64 addr);
      r9 := int (Word.of_int64 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width:64 value in
  check_gpr init bytes r9 expected arch ctxt;
  let expected_addr = Word.of_int ~width:64 (addr - 4) in
  check_gpr init bytes r31 expected_addr arch ctxt

let lbzux arch ctxt =
  let bytes = "\x7d\x3d\x50\xee" in (** lbzux r9, r29, r10 *)
  let mem, addr_size = env_of_arch arch in
  let r9 = find_gpr "R9" in
  let r10 = find_gpr "R10" in
  let r29 = find_gpr "R29" in
  let x = Word.of_int ~width:addr_size 0xABCD0000 in
  let y = Word.of_int ~width:addr_size 0x0000EF42 in
  let ea = Word.(x + y) in
  let value = 0x42 in
  let data = Word.of_int ~width:32 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r8;
      r29 := int x;
      r10 := int y;
      r9 := int (Word.of_int64 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width:64 value in
  check_gpr init bytes r9 expected arch ctxt;
  let expected_addr = Word.of_int ~width:64 0xABCDEF42 in
  check_gpr init bytes r29 expected_addr arch ctxt

let suite = "load" >::: [
    "lbz32"     >:: lbz `ppc;
    "lbz32_0"   >:: lbz_zero_op `ppc;
    "lwz32"     >:: lwz `ppc;
    "lbzx32"    >:: lbzx `ppc;
    "lbzx32_0"  >:: lbzx_zero_op `ppc;
    "lwzx32"    >:: lwzx `ppc;
    "lbzu32"    >:: lbzu `ppc;
    "lwzu32"    >:: lwzu `ppc;
    "lbzux32"   >:: lbzux `ppc;

    "lbz64"     >:: lbz `ppc64;
    "lbz64_0"   >:: lbz_zero_op `ppc64;
    "lwz64"     >:: lwz `ppc64;
    "lbzx64"    >:: lbz `ppc64;
    "lbzx64_0"  >:: lbzx_zero_op `ppc64;
    "lwzx64"    >:: lwzx `ppc64;
    "lbzu64"    >:: lbzu `ppc64;
    "lwzu64"    >:: lwzu `ppc64;
    "lbzux64"   >:: lbzux `ppc64;
  ]
