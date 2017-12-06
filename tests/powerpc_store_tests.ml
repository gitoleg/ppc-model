open Core_kernel.Std
open Bap.Std
open OUnit2

open Powerpc_model
open Powerpc_tests_helpers

let env_of_arch = function
  | `ppc   -> mem32, 32
  | `ppc64 -> mem64, 64
  | _ -> failwith "ppc OR ppc64 arch only"

let st name opcode size ~d_addr arch ctxt =
  let bytes = make_insn ~name `D [opcode; 9; 28; d_addr] in
  let mem, addr_size = env_of_arch arch in
  let r9 = find_gpr "R9" in
  let r28 = find_gpr "R28" in
  let addr = 0xABCDEF00 in
  let ea = Word.of_int ~width:addr_size (addr + d_addr) in
  let data = Word.of_int64 0xFFFFFFFFABCDEF42L in
  let init = Bil.[
    r9 := int data;
    r28 := int (Word.of_int ~width:64 addr);
  ] in
  let bits = Size.in_bits size in
  let expected = Or_error.ok_exn @@ Word.extract ~hi:(bits - 1) ~lo:0 data in
  check_mem init bytes mem ~addr:ea ~size expected arch ctxt

let stb = st "STB" 38 `r8
let sth = st "STH" 44 `r16
let stw = st "STW" 36 `r32

let st_zero_reg name opcode size ~d_addr arch ctxt =
  let bytes = make_insn ~name `D [opcode; 9; 0; d_addr] in
  let mem, addr_size = env_of_arch arch in
  let r9 = find_gpr "R9" in
  let ea = Word.of_int ~width:addr_size d_addr in
  let data = Word.of_int64 0xFFFFFFFFABCDEF42L in
  let init = Bil.[
    r9 := int data;
  ] in
  let bits = Size.in_bits size in
  let expected = Or_error.ok_exn @@ Word.extract ~hi:(bits - 1) ~lo:0 data in
  check_mem init bytes mem ~addr:ea ~size expected arch ctxt

let stb_zero_reg = st_zero_reg "STB" 38 `r8
let sth_zero_reg = st_zero_reg "STH" 44 `r16
let stw_zero_reg = st_zero_reg "STW" 36 `r32

let stx name opt_opcode size arch ctxt =
  let bytes = make_insn ~name `X [31; 9; 14; 31; opt_opcode] in
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
  let bits = Size.in_bits size in
  let expected = Or_error.ok_exn @@ Word.extract ~hi:(bits - 1) ~lo:0 data in
  check_mem init bytes mem ~addr:ea ~size expected arch ctxt

let stbx = stx "STBX" 215 `r8
let sthx = stx "STHX" 407 `r16
let stwx = stx "STWX" 151 `r32

let stx_zero_reg name opt_opcode size arch ctxt =
  let bytes = make_insn ~name `X [31; 9; 0; 31; opt_opcode] in
  let mem, addr_size = env_of_arch arch in
  let r9 = find_gpr "R9" in
  let r31 = find_gpr "R31" in
  let ea = Word.of_int ~width:addr_size 0xABCD0000 in
  let data = Word.of_int64 0xFFFFFFFFABCDEF42L in
  let init = Bil.[
      r9  := int data;
      r31 := int ea;
    ] in
  let bits = Size.in_bits size in
  let expected = Or_error.ok_exn @@ Word.extract ~hi:(bits - 1) ~lo:0 data in
  check_mem init bytes mem ~addr:ea ~size expected arch ctxt

let stbx_zero_reg = stx_zero_reg "STBX" 215 `r8
let sthx_zero_reg = stx_zero_reg "STHX" 407 `r16
let stwx_zero_reg = stx_zero_reg "STWX" 151 `r32

let stu name opcode size arch ~d_addr ctxt =
  let bytes = make_insn ~name `D [opcode; 4; 29; d_addr] in
  let mem, addr_size = env_of_arch arch in
  let r4 = find_gpr "R4" in
  let r29 = find_gpr "R29" in
  let addr = 0xABCDEF01 in
  let ea = Word.of_int ~width:addr_size (addr + d_addr) in
  let data = Word.of_int64 0xFFFFFFFFABCDEF42L in
  let init = Bil.[
    r4 := int data;
    r29 := int (Word.of_int ~width:64 addr);
  ] in
  let bits = Size.in_bits size in
  let expected = Or_error.ok_exn @@ Word.extract ~hi:(bits - 1) ~lo:0 data in
  check_mem init bytes mem ~addr:ea ~size expected arch ctxt;
  let expected_addr = Word.of_int ~width:64 (addr + d_addr) in
  check_gpr init bytes r29 expected_addr arch ctxt

let stbu = stu "STBU" 39 `r8
let sthu = stu "STHU" 45 `r16
let stwu = stu "STWU" 37 `r32

let stux name opt_opcode size arch ctxt =
  let bytes = make_insn ~name `X [31; 9; 31; 25; opt_opcode] in
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
  let bits = Size.in_bits size in
  let expected = Or_error.ok_exn @@ Word.extract ~hi:(bits - 1) ~lo:0 data in
  check_mem init bytes mem ~addr:ea ~size expected arch ctxt;
  let expected_addr = Word.of_int ~width:64 0xABCDEF42 in
  check_gpr init bytes r31 expected_addr arch ctxt

let stbux = stux "STBUX" 247 `r8
let sthux = stux "STHUX" 439 `r16
let stwux = stux "STWUX" 183 `r32

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
    "stb32 +imm"    >:: stb `ppc ~d_addr:16;
    "stb32 -imm"    >:: stb `ppc ~d_addr:(-16);
    "stb32 0reg"    >:: stb_zero_reg `ppc ~d_addr:16;
    "sth32 +imm"    >:: sth `ppc ~d_addr:16;
    "sth32 -imm"    >:: sth `ppc ~d_addr:(-16);
    "sth32 0reg"    >:: sth_zero_reg `ppc ~d_addr:16;
    "stw32 +imm"    >:: stw `ppc ~d_addr:16;
    "stw32 -imm"    >:: stw `ppc ~d_addr:(-16);
    "stw32 0reg"    >:: stw_zero_reg `ppc ~d_addr:16;

    "stbx32"        >:: stbx `ppc;
    "stbx32 0reg"   >:: stbx_zero_reg `ppc;
    "sthx32"        >:: sthx `ppc;
    "sthx32 0reg"   >:: sthx_zero_reg `ppc;
    "stwx32"        >:: stwx `ppc;
    "stwx32 0reg"   >:: stwx_zero_reg `ppc;

    "stbu32 +imm"    >:: stbu `ppc ~d_addr:16;
    "stbu32 -imm"    >:: stbu `ppc ~d_addr:(-16);
    "sthu32 +imm"    >:: sthu `ppc ~d_addr:16;
    "sthu32 -imm"    >:: sthu `ppc ~d_addr:(-16);
    "stwu32 +imm"    >:: stwu `ppc ~d_addr:16;
    "stwu32 -imm"    >:: stwu `ppc ~d_addr:(-16);

    "stbux32"  >:: stbux `ppc;
    "sthux32"  >:: sthux `ppc;
    "stwux32"  >:: stwux `ppc;

    "std32"    >:: std `ppc;
    "stdx32"   >:: stdx `ppc;
    "stdu32"   >:: stdu `ppc;
    "stdux32"  >:: stdux `ppc;

    "stb64 +imm"    >:: stb `ppc64 ~d_addr:16;
    "stb64 -imm"    >:: stb `ppc64 ~d_addr:(-16);
    "stb64 0reg"    >:: stb_zero_reg `ppc64 ~d_addr:16;
    "sth64 +imm"    >:: sth `ppc64 ~d_addr:16;
    "sth64 -imm"    >:: sth `ppc64 ~d_addr:(-16);
    "sth64 0reg"    >:: sth_zero_reg `ppc64 ~d_addr:16;
    "stw64 +imm"    >:: stw `ppc64 ~d_addr:16;
    "stw64 -imm"    >:: stw `ppc64 ~d_addr:(-16);
    "stw64 0reg"    >:: stw_zero_reg `ppc64 ~d_addr:16;

    "stbx64"        >:: stbx `ppc64;
    "stbx64 0reg"   >:: stbx_zero_reg `ppc64;
    "sthx64"        >:: sthx `ppc64;
    "sthx64 0reg"   >:: sthx_zero_reg `ppc64;
    "stwx64"        >:: stwx `ppc64;
    "stwx64 0reg"   >:: stwx_zero_reg `ppc64;

    "stbu64 +imm"    >:: stbu `ppc64 ~d_addr:16;
    "stbu64 -imm"    >:: stbu `ppc64 ~d_addr:(-16);
    "sthu64 +imm"    >:: sthu `ppc64 ~d_addr:16;
    "sthu64 -imm"    >:: sthu `ppc64 ~d_addr:(-16);
    "stwu64 +imm"    >:: stwu `ppc64 ~d_addr:16;
    "stwu64 -imm"    >:: stwu `ppc64 ~d_addr:(-16);

    "stbux64"  >:: stbux `ppc64;
    "sthux64"  >:: sthux `ppc64;
    "stwux64"  >:: stwux `ppc64;

    "std64"    >:: std `ppc64;
    "stdx64"   >:: stdx `ppc64;
    "stdu64"   >:: stdu `ppc64;
    "stdux64"  >:: stdux `ppc64;
    "stdux64a" >:: stdux_big_addr;
  ]
