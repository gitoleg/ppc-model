open Core_kernel.Std
open Bap.Std
open OUnit2

open Powerpc_types
open Hardware
open Powerpc_tests_helpers

let addi arch ctxt =
  let bytes = "\x38\x21\x00\x10" in (** addi r1, r1, 16  *)
  let r1 = find_gpr "R1" in
  let init = Bil.[
    r1 := int (Word.of_int ~width:64 26);
  ] in
  let expected = Word.of_int ~width:64 42 in
  check_gpr init bytes r1 expected arch ctxt

let addi_zero_op arch ctxt =
  let bytes = "\x38\x20\x00\x10" in (** addi r1, 0, 16 OR li r1, 16 *)
  let r1 = find_gpr "R1" in
  let expected = Word.of_int ~width:64 16 in
  check_gpr [] bytes r1 expected arch ctxt

let addis arch ctxt =
  let bytes = "\x3f\xde\x00\x02" in (** addis r30, r30, 2   *)
  let r30 = find_gpr "R30" in
  let init = Bil.[
      r30 := int (Word.of_int ~width:64 42);
  ] in
  let r = 42 + (2 lsl 16) in
  let expected = Word.of_int ~width:64 r in
  check_gpr init bytes r30 expected arch ctxt

let addis_zero_op arch ctxt =
  let bytes = "\x3f\xc0\x00\x02" in (** addis r30, 0, 2 OR lis r30,1 *)
  let r30 = find_gpr "R30" in
  let r = 2 lsl 16 in
  let expected = Word.of_int ~width:64 r in
  check_gpr [] bytes r30 expected arch ctxt

let add arch ctxt =
  let bytes = "\x7d\x62\x5a\x14" in (** add r11, r2, r11  *)
  let r2 = find_gpr "R2" in
  let r11 = find_gpr "R11" in
  let x = Word.of_int ~width:64 34 in
  let y = Word.of_int ~width:64 8 in
  let init = Bil.[
    r2 := int x;
    r11 := int y;
  ] in
  let expected = Word.of_int ~width:64 42 in
  check_gpr init bytes r11 expected arch ctxt

let addic arch ctxt =
  let bytes = "\x30\x21\x00\x10" in (** addic r1, r1, 16  *)
  let r1 = find_gpr "R1" in
  let x = Word.of_int ~width:64 0x00000000_fffffff0 in
  let init = Bil.[
    r1 := int x;
  ] in
  let expected = Word.of_int ~width:64 0x00000001_00000000 in
  let ctxt = eval init bytes arch in
  let r1_value = lookup_var ctxt r1 in
  let ca_value = lookup_var ctxt ca in
  let ca32_value = lookup_var ctxt ca32 in
  let ca_expected = match arch with
    | `ppc -> Word.b1
    | `ppc64 -> Word.b0
    | _ -> failwith "PowerPC only expected" in
  assert_bool "addic failed: result" (is_equal_words expected r1_value);
  assert_bool "addic failed: ca32" (is_equal_words Word.b1 ca32_value);
  assert_bool "addic failed: ca" (is_equal_words ca_expected ca_value)

let addc arch ctxt =
  let bytes = "\x7d\x62\x58\x14" in (** addc r11, r2, r11  *)
  let r2 = find_gpr "R2" in
  let r11 = find_gpr "R11" in
  let x = Word.of_int ~width:64 0x00000000_fffffff0 in
  let init = Bil.[
      r2 := int x;
      r11 := int (Word.of_int ~width:64 16);
  ] in
  let expected = Word.of_int ~width:64 0x00000001_00000000 in
  let ctxt = eval init bytes arch in
  let r11_value = lookup_var ctxt r11 in
  let ca_value = lookup_var ctxt ca in
  let ca32_value = lookup_var ctxt ca32 in
  let ca_expected = match arch with
    | `ppc -> Word.b1
    | `ppc64 -> Word.b0
    | _ -> failwith "PowerPC only expected" in
  assert_bool "addc failed: result" (is_equal_words expected r11_value);
  assert_bool "addc failed: ca32" (is_equal_words Word.b1 ca32_value);
  assert_bool "addc failed: ca" (is_equal_words ca_expected ca_value)

let adde arch ctxt =
  let bytes = "\x7c\x21\x81\x14" in (** adde r1, r1, r16  *)
  let r1 = find_gpr "R1" in
  let r16 = find_gpr "R16" in
  let x = Word.of_int ~width:64 0x00000000_fffffff0 in
  let init = Bil.[
      r1 := int x;
      r16 := int (Word.of_int ~width:64 15);
      ca := int Word.b1;
    ] in
  let ctxt = eval init bytes arch in
  let r1_value = lookup_var ctxt r1 in
  let ca_value = lookup_var ctxt ca in
  let ca32_value = lookup_var ctxt ca32 in
  let expected = Word.of_int ~width:64 0x00000001_00000000 in
  let ca_expected = match arch with
    | `ppc -> Word.b1
    | `ppc64 -> Word.b0
    | _ -> failwith "PowerPC only expected" in
  assert_bool "adde failed: result" (is_equal_words expected r1_value);
  assert_bool "adde failed: ca32" (is_equal_words Word.b1 ca32_value);
  assert_bool "adde failed: ca" (is_equal_words ca_expected ca_value)

let addme arch ctxt =
  let bytes = "\x7c\x22\x01\xd4" in (** addme r1, r2   *)
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let x = Word.of_int ~width:64 0x00000000_ffffffff in
  let init = Bil.[
      r2 := int x;
      ca := int Word.b1;
    ] in
  let ctxt = eval init bytes arch in
  let r1_value = lookup_var ctxt r1 in
  let ca_value = lookup_var ctxt ca in
  let ca32_value = lookup_var ctxt ca32 in
  let expected = Word.of_int ~width:64 0x00000000_ffffffff in
  assert_bool "addme failed: result" (is_equal_words expected r1_value);
  assert_bool "addme failed: ca32" (is_equal_words Word.b0 ca32_value);
  assert_bool "addme failed: ca" (is_equal_words Word.b0 ca_value)

let addze arch ctxt =
  let bytes = "\x7c\x22\x01\x94" in (** addze r1, r2  *)
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let x = Word.of_int64 0xffffffff_ffffffffL in
  let init = Bil.[
      r2 := int x;
      ca := int Word.b1;
    ] in
  let ctxt = eval init bytes arch in
  let r1_value = lookup_var ctxt r1 in
  let ca_value = lookup_var ctxt ca in
  let ca32_value = lookup_var ctxt ca32 in
  let expected = Word.of_int ~width:64 0x0 in
  assert_bool "addze failed: result" (is_equal_words expected r1_value);
  assert_bool "addze failed: ca32" (is_equal_words Word.b1 ca32_value);
  assert_bool "addze failed: ca" (is_equal_words Word.b1 ca_value)

let add_dot arch ctxt =
  let bytes = "\x7d\x62\x5a\x15" in (** add. r11, r2, r11  *)
  let r2 = find_gpr "R2" in
  let r11 = find_gpr "R11" in
  let x = Word.of_int ~width:64 34 in
  let y = Word.of_int ~width:64 8 in
  let init = Bil.[
    r2 := int x;
    r11 := int y;
  ] in
  let expected = Word.of_int ~width:64 42 in
  let ctxt = eval init bytes arch in
  let r11_value = lookup_var ctxt r11 in
  let nf_value = lookup_var ctxt nf in
  let pf_value = lookup_var ctxt pf in
  let zf_value = lookup_var ctxt zf in
  assert_bool "add. failed: result" (is_equal_words expected r11_value);
  assert_bool "add. failed: nf" (is_equal_words Word.b0 nf_value);
  assert_bool "add. failed: pf" (is_equal_words Word.b1 pf_value);
  assert_bool "add. failed: zf" (is_equal_words Word.b0 zf_value)

let addic_dot arch ctxt =
  let bytes = "\x34\x21\x00\x10" in (** addic. r1, r1, 16   *)
  let r1 = find_gpr "R1" in
  let x = Word.of_int64 0xffffffff_fffffff0L in
  let init = Bil.[
    r1 := int x;
  ] in
  let expected = Word.of_int ~width:64 0 in
  let ctxt = eval init bytes arch in
  let r1_value = lookup_var ctxt r1 in
  let ca_value = lookup_var ctxt ca in
  let ca32_value = lookup_var ctxt ca32 in
  let nf_value = lookup_var ctxt nf in
  let pf_value = lookup_var ctxt pf in
  let zf_value = lookup_var ctxt zf in
  assert_bool "addic. failed: result" (is_equal_words expected r1_value);
  assert_bool "addic. failed: ca32" (is_equal_words Word.b1 ca32_value);
  assert_bool "addic. failed: ca" (is_equal_words Word.b1 ca_value);
  assert_bool "addic. failed: nf" (is_equal_words Word.b0 nf_value);
  assert_bool "addic. failed: pf" (is_equal_words Word.b0 pf_value);
  assert_bool "addic. failed: zf" (is_equal_words Word.b1 zf_value)

let addc_dot arch ctxt =
  let bytes = "\x7d\x62\x58\x15" in (** addc. r11, r2, r11  *)
  let r2 = find_gpr "R2" in
  let r11 = find_gpr "R11" in
  let x = Word.of_int ~width:64 0x0fffffff_fffffff0 in
  let y = Word.of_int64 0xf0000000_0000000fL in
  let init = Bil.[
      r2 := int x;
      r11 := int y;
  ] in
  let expected = Word.of_int64 0xffffffff_ffffffffL in
  let ctxt = eval init bytes arch in
  let r11_value = lookup_var ctxt r11 in
  let ca_value = lookup_var ctxt ca in
  let ca32_value = lookup_var ctxt ca32 in
  let nf_value = lookup_var ctxt nf in
  let pf_value = lookup_var ctxt pf in
  let zf_value = lookup_var ctxt zf in
  assert_bool "addc. failed: result" (is_equal_words expected r11_value);
  assert_bool "addc. failed: ca32" (is_equal_words Word.b0 ca32_value);
  assert_bool "addc. failed: ca" (is_equal_words Word.b0 ca_value);
  assert_bool "addc. failed: nf" (is_equal_words Word.b1 nf_value);
  assert_bool "addc. failed: pf" (is_equal_words Word.b0 pf_value);
  assert_bool "addc. failed: zf" (is_equal_words Word.b0 zf_value)

let adde_dot arch ctxt =
  let bytes = "\x7c\x21\x81\x15" in (** adde. r1, r1, r16  *)
  let r1 = find_gpr "R1" in
  let r16 = find_gpr "R16" in
  let x = Word.of_int ~width:64 0x00000000_0ffffff0 in
  let y = Word.of_int ~width:64 0x00000000_f000000e in
  let init = Bil.[
      r1 := int x;
      r16 := int y;
      ca := int Word.b1;
    ] in
  let ctxt = eval init bytes arch in
  let r1_value = lookup_var ctxt r1 in
  let ca_value = lookup_var ctxt ca in
  let ca32_value = lookup_var ctxt ca32 in
  let expected = Word.of_int ~width:64 0x00000000_ffffffff in
  let nf_expected, pf_expected = match arch with
    | `ppc -> Word.b1, Word.b0
    | `ppc64 -> Word.b0, Word.b1
    | _ -> failwith "PowerPC only expected" in
  let nf_value = lookup_var ctxt nf in
  let pf_value = lookup_var ctxt pf in
  let zf_value = lookup_var ctxt zf in
  assert_bool "adde. failed: result" (is_equal_words expected r1_value);
  assert_bool "adde. failed: ca32" (is_equal_words Word.b0 ca32_value);
  assert_bool "adde. failed: ca" (is_equal_words Word.b0 ca_value);
  assert_bool "adde. failed: nf" (is_equal_words nf_expected nf_value);
  assert_bool "adde. failed: pf" (is_equal_words pf_expected pf_value);
  assert_bool "adde. failed: zf" (is_equal_words Word.b0 zf_value)

let addme_dot arch ctxt =
  let bytes = "\x7c\x22\x01\xd5" in (** addme. r1, r2  *)
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let x = Word.of_int ~width:64 0x00000001_00000001 in
  let init = Bil.[
      r2 := int x;
      ca := int Word.b0;
    ] in
  let ctxt = eval init bytes arch in
  let r1_value = lookup_var ctxt r1 in
  let ca_value = lookup_var ctxt ca in
  let ca32_value = lookup_var ctxt ca32 in
  let expected = Word.of_int ~width:64 0x00000001_00000000 in
  let cf_expected, zf_expected, pf_expected = match arch with
    | `ppc -> Word.b1, Word.b1, Word.b0
    | `ppc64 -> Word.b1, Word.b0, Word.b1
    | _ -> failwith "PowerPC only expected" in
  let nf_value = lookup_var ctxt nf in
  let pf_value = lookup_var ctxt pf in
  let zf_value = lookup_var ctxt zf in
  assert_bool "addme. failed: result" (is_equal_words expected r1_value);
  assert_bool "addme. failed: ca32" (is_equal_words Word.b1 ca32_value);
  assert_bool "addme. failed: ca" (is_equal_words cf_expected ca_value);
  assert_bool "addme. failed: nf" (is_equal_words Word.b0 nf_value);
  assert_bool "addme. failed: pf" (is_equal_words pf_expected pf_value);
  assert_bool "addme. failed: zf" (is_equal_words zf_expected zf_value)

let addze_dot arch ctxt =
  let bytes = "\x7c\x22\x01\x95" in (** addze. r1, r2  *)
  let r1 = find_gpr "R1" in
  let r2 = find_gpr "R2" in
  let x = Word.of_int64 0xffffffff_ffffffffL in
  let init = Bil.[
      r2 := int x;
      ca := int Word.b1;
    ] in
  let ctxt = eval init bytes arch in
  let r1_value = lookup_var ctxt r1 in
  let ca_value = lookup_var ctxt ca in
  let ca32_value = lookup_var ctxt ca32 in
  let expected = Word.of_int ~width:64 0x0 in
  assert_bool "addze. failed: result" (is_equal_words expected r1_value);
  assert_bool "addze. failed: ca32" (is_equal_words Word.b1 ca32_value);
  assert_bool "addze. failed: ca" (is_equal_words Word.b1 ca_value)

let suite = "add" >::: [
    "addi32"    >:: addi `ppc;
    "addi32_0"  >:: addi_zero_op `ppc;
    "addis32"   >:: addis `ppc;
    "addis32_0" >:: addis_zero_op `ppc;
    "add32"     >:: add `ppc;
    "addic32"   >:: addic `ppc;
    "addc32"    >:: addc `ppc;
    "adde32"    >:: adde `ppc;
    "addme32"   >:: addme `ppc;
    "addze32"   >:: addze `ppc;
    "add.32"    >:: add_dot `ppc;
    "addic.32"  >:: addic_dot `ppc;
    "addc.32"   >:: addc_dot `ppc;
    "adde.32"   >:: adde_dot `ppc;
    "addme.32"  >:: addme_dot `ppc;
    "addze.32"  >:: addze_dot `ppc;

    "addi32"    >:: addi `ppc64;
    "addi32_0"  >:: addi_zero_op `ppc64;
    "addis32"   >:: addis `ppc64;
    "addis32_0" >:: addis_zero_op `ppc64;
    "add32"     >:: add `ppc64;
    "addic32"   >:: addic `ppc64;
    "addc32"    >:: addc `ppc64;
    "adde32"    >:: adde `ppc64;
    "addme32"   >:: addme `ppc64;
    "addze32"   >:: addze `ppc64;
    "add.64"    >:: add_dot `ppc64;
    "addic.64"  >:: addic_dot `ppc64;
    "addc.64"   >:: addc_dot `ppc64;
    "adde.64"   >:: adde_dot `ppc64;
    "addme.64"  >:: addme_dot `ppc64;
    "addze.64"  >:: addze_dot `ppc64;
  ]
