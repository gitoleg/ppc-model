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

let lha arch ctxt =
  let bytes = "\xa8\x29\x00\x05" in (** lha r1, 5(r9)  *)
  let r1 = find_gpr "R1" in
  let r9 = find_gpr "R9" in
  let mem, addr_size = env_of_arch arch in
  let addr = 0x00ABCDEF in
  let ea = Word.of_int ~width:addr_size (addr + 5) in
  let data = Word.of_int ~width:64 0xffab in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r16;
      r9 := int (Word.of_int ~width:64 addr);
    ] in
  let expected = Word.of_int64 0xffffffff_ffffffabL in
  check_gpr init bytes r1 expected arch ctxt

let lhax arch ctxt =
  let bytes = "\x7c\x25\x4a\xae" in (** lhax r1, r5, r9 *)
  let r1 = find_gpr "R1" in
  let r5 = find_gpr "R5" in
  let r9 = find_gpr "R9" in
  let mem, addr_size = env_of_arch arch in
  let addr = 0x00ABCDEF in
  let disp = 5 in
  let ea = Word.of_int ~width:addr_size (addr + disp) in
  let data = Word.of_int ~width:64 0xffab in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r16;
      r5 := int (Word.of_int ~width:64 disp);
      r9 := int (Word.of_int ~width:64 addr);
    ] in
  let expected = Word.of_int64 0xffffffff_ffffffabL in
  check_gpr init bytes r1 expected arch ctxt

let lhau arch ctxt =
  let bytes = "\xac\x29\x00\x05" in (** lhau r1, 5(r9)  *)
  let r1 = find_gpr "R1" in
  let r9 = find_gpr "R9" in
  let mem, addr_size = env_of_arch arch in
  let addr = 0x00ABCDEF in
  let ea = Word.of_int ~width:addr_size (addr + 5) in
  let data = Word.of_int ~width:64 0xffab in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r16;
      r9 := int (Word.of_int ~width:64 addr);
    ] in
  let expected = Word.of_int64 0xffffffff_ffffffabL in
  check_gpr init bytes r1 expected arch ctxt;
  let expected_addr = Word.of_int ~width:64 (addr + 5) in
  check_gpr init bytes r9 expected_addr arch ctxt

let lhaux arch ctxt =
  let bytes = "\x7c\x25\x4a\xee" in (** lhaux r1, r5, r9 *)
  let r1 = find_gpr "R1" in
  let r5 = find_gpr "R5" in
  let r9 = find_gpr "R9" in
  let mem, addr_size = env_of_arch arch in
  let addr = 0x00ABCDEF in
  let disp = 5 in
  let ea = Word.of_int ~width:addr_size (addr + disp) in
  let data = Word.of_int ~width:64 0x0fab in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r16;
      r5 := int (Word.of_int ~width:64 disp);
      r9 := int (Word.of_int ~width:64 addr);
    ] in
  let expected = Word.of_int64 0x00000000_000000fabL in
  check_gpr init bytes r1 expected arch ctxt;
  let expected_addr = Word.of_int ~width:64 (addr + disp) in
  check_gpr init bytes r5 expected_addr arch ctxt

let lwa arch ctxt =
  let bytes = "\xeb\xeb\x01\x16" in (** lwa r31, 276(r11)  *)
  let r31 = find_gpr "R31" in
  let r11 = find_gpr "R11" in
  let mem, addr_size = env_of_arch arch in
  let addr = 0x00ABCDEF in
  let disp = 276 lsl 2 in
  let ea = Word.of_int ~width:addr_size (addr + disp) in
  let data = Word.of_int64 0xffacffabL in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r32;
      r11 := int (Word.of_int ~width:64 addr);
    ] in
  let expected = Word.of_int64 0xffffffff_ffacffabL in
  check_gpr init bytes r31 expected arch ctxt

let lwax arch ctxt =
  let bytes = "\x7c\x25\x4a\xaa" in (** lwax r1, r5, r9 *)
  let r1 = find_gpr "R1" in
  let r5 = find_gpr "R5" in
  let r9 = find_gpr "R9" in
  let mem, addr_size = env_of_arch arch in
  let addr = 0x00ABCDEF in
  let disp = 5 in
  let ea = Word.of_int ~width:addr_size (addr + disp) in
  let data = Word.of_int64 0x0facffabL in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r32;
      r5 := int (Word.of_int ~width:64 disp);
      r9 := int (Word.of_int ~width:64 addr);
    ] in
  let expected = Word.of_int64 0x00000000_0facffabL in
  check_gpr init bytes r1 expected arch ctxt

let lwaux arch ctxt =
  let bytes = "\x7c\x25\x4a\xea" in (** lwaux r1, r5, r9 *)
  let r1 = find_gpr "R1" in
  let r5 = find_gpr "R5" in
  let r9 = find_gpr "R9" in
  let mem, addr_size = env_of_arch arch in
  let addr = 0x00ABCDEF in
  let disp = 5 in
  let ea = Word.of_int ~width:addr_size (addr + disp) in
  let data = Word.of_int64 0xffacffabL in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r32;
      r5 := int (Word.of_int ~width:64 disp);
      r9 := int (Word.of_int ~width:64 addr);
    ] in
  let expected = Word.of_int64 0xffffffff_ffacffabL in
  check_gpr init bytes r1 expected arch ctxt;
  let expected_addr = Word.of_int ~width:64 (addr + disp) in
  check_gpr init bytes r5 expected_addr arch ctxt

let ld arch ctxt =
  let bytes = "\xe8\x29\x00\x08" in  (** ld r1, 8(r9) *)
  let r1 = find_gpr "R1" in
  let r9 = find_gpr "R9" in
  let mem, addr_size = env_of_arch arch in
  let addr = 0x00ABCDEF in
  let disp = 8 lsl 2 in
  let ea = Word.of_int ~width:addr_size (addr + disp) in
  let data = Word.of_int64 0xffacbbdd_ffacffabL in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r64;
      r9 := int (Word.of_int ~width:64 addr);
    ] in
  let expected = data in
  check_gpr init bytes r1 expected arch ctxt

let ldx arch ctxt =
  let bytes = "\x7c\x28\x48\x2a" in  (** ldx r1, r8, r9 *)
  let r1 = find_gpr "R1" in
  let r8 = find_gpr "R8" in
  let r9 = find_gpr "R9" in
  let mem, addr_size = env_of_arch arch in
  let addr = 0x00ABCDEF in
  let disp = 5 in
  let ea = Word.of_int ~width:addr_size (addr + disp) in
  let data = Word.of_int64 0xffacbbdd_ffacffabL in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r64;
      r9 := int (Word.of_int ~width:64 addr);
      r8 := int (Word.of_int ~width:64 disp);
    ] in
  let expected = data in
  check_gpr init bytes r1 expected arch ctxt

let ldu arch ctxt =
  let bytes = "\xe8\x29\x00\x09" in  (** ldu r1, 8(r9) *)
  let r1 = find_gpr "R1" in
  let r9 = find_gpr "R9" in
  let mem, addr_size = env_of_arch arch in
  let addr = 0x00ABCDEF in
  let disp = 8 lsl 2 in
  let ea = Word.of_int ~width:addr_size (addr + disp) in
  let data = Word.of_int64 0xffacbbdd_ffacffabL in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r64;
      r9 := int (Word.of_int ~width:64 addr);
    ] in
  let expected = data in
  check_gpr init bytes r1 expected arch ctxt;
  let expected_addr = Word.of_int ~width:64 (addr + disp) in
  check_gpr init bytes r9 expected_addr arch ctxt

let ldux arch ctxt =
  let bytes = "\x7c\x28\x48\x6a" in  (** ldux r1, r8, r9  *)
  let r1 = find_gpr "R1" in
  let r8 = find_gpr "R8" in
  let r9 = find_gpr "R9" in
  let mem, addr_size = env_of_arch arch in
  let addr = 0x00ABCDEF in
  let disp = 5 in
  let ea = Word.of_int ~width:addr_size (addr + disp) in
  let data = Word.of_int64 0xffacbbdd_ffacffabL in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r64;
      r9 := int (Word.of_int ~width:64 addr);
      r8 := int (Word.of_int ~width:64 disp);
    ] in
  let expected = data in
  check_gpr init bytes r1 expected arch ctxt;
  let expected_addr = Word.of_int ~width:64 (addr + disp) in
  check_gpr init bytes r8 expected_addr arch ctxt

let ldux_big_addr ctxt =
  let bytes = "\x7c\x28\x48\x6a" in  (** ldux r1, r8, r9  *)
  let arch = `ppc64 in
  let r1 = find_gpr "R1" in
  let r8 = find_gpr "R8" in
  let r9 = find_gpr "R9" in
  let mem, addr_size = env_of_arch arch in
  let addr = Word.of_int64 0xCDEF4234_CDABCDEFL in
  let disp = Word.of_int64 5L in
  let ea = Word.(addr + disp) in
  let data = Word.of_int64 0xffacbbdd_ffacffabL in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r64;
      r9 := int addr;
      r8 := int disp;
    ] in
  let expected = data in
  check_gpr init bytes r1 expected arch ctxt;
  let expected_addr = ea in
  check_gpr init bytes r8 expected_addr arch ctxt

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
    "lha32"     >:: lha `ppc;
    "lhax32"    >:: lhax `ppc;
    "lhau32"    >:: lhau `ppc;
    "lhaux32"   >:: lhaux `ppc;
    "lwa32"     >:: lwa `ppc;
    "lwax32"    >:: lwax `ppc;
    "lwaux32"   >:: lwaux `ppc;
    "ld32"      >:: ld `ppc;
    "ldx32"     >:: ldx `ppc;
    "ldu32"     >:: ldu `ppc;
    "ldux32"    >:: ldux `ppc;

    "lbz64"     >:: lbz `ppc64;
    "lbz64_0"   >:: lbz_zero_op `ppc64;
    "lwz64"     >:: lwz `ppc64;
    "lbzx64"    >:: lbz `ppc64;
    "lbzx64_0"  >:: lbzx_zero_op `ppc64;
    "lwzx64"    >:: lwzx `ppc64;
    "lbzu64"    >:: lbzu `ppc64;
    "lwzu64"    >:: lwzu `ppc64;
    "lbzux64"   >:: lbzux `ppc64;
    "lha64"     >:: lha `ppc64;
    "lhax64"    >:: lhax `ppc64;
    "lhau64"    >:: lhau `ppc64;
    "lhaux64"   >:: lhaux `ppc64;
    "lwa64"     >:: lwa `ppc64;
    "lwax64"    >:: lwax `ppc64;
    "lwaux64"   >:: lwaux `ppc64;
    "ld64"      >:: ld `ppc64;
    "ldx64"     >:: ldx `ppc64;
    "ldu64"     >:: ldu `ppc64;
    "ldux64"    >:: ldux `ppc64;
    "ldux64a"   >:: ldux_big_addr;
  ]
