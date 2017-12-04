open Core_kernel.Std
open Bap.Std
open OUnit2

open Powerpc_types
open Powerpc_model.Hardware
open Powerpc_tests_helpers
open Dsl

let eval_rtl rtl =
  let bil = RTL.bil_of_t rtl in
  Stmt.eval bil (new Bili.context)

let plain_assign ctxt =
  let v = Var.create "v" (Type.Imm 16) in
  let e = Exp.of_var v in
  let w = Exp.of_word (Word.of_int ~width:8 0x42) in
  let expected = Word.of_int ~width:16 0x42 in
  let rtl = RTL.[ e := w; ] in
  let ctxt = eval_rtl rtl in
  let value = lookup_var ctxt v in
  assert_bool "plain := failed" (is_equal_words expected value)

let concat_assign ctxt =
  let v1 = Var.create "v1" (Type.Imm 2) in
  let v2 = Var.create "v2" (Type.Imm 1) in
  let v3 = Var.create "v3" (Type.Imm 4) in
  let e1,e2,e3 = Exp.(of_var v1, of_var v2, of_var v3) in
  let w = Exp.of_word (Word.of_int ~width:6 0b111010) in
  let x = RTL.(e1 ^ e2 ^ e3) in
  let rtl = RTL.[
      x := w;
    ] in
  let ctxt = eval_rtl rtl in
  let expected_val1 = Word.of_int ~width:2 0b01 in
  let expected_val2 = Word.of_int ~width:1 0b1 in
  let expected_val3 = Word.of_int ~width:4 0b1010 in
  let val1 = lookup_var ctxt v1 in
  let val2 = lookup_var ctxt v2 in
  let val3 = lookup_var ctxt v3 in
  assert_bool "concated exp := exp : failed at v1"
    (is_equal_words expected_val1 val1);
  assert_bool "concated exp := exp : failed at v2"
    (is_equal_words expected_val2 val2);
  assert_bool "concated exp := exp : failed at v3"
    (is_equal_words expected_val3 val3)

let complex_assign ctxt =
  let v1 = Var.create "v1" (Type.Imm 1) in
  let v2 = Var.create "v2" (Type.Imm 2) in
  let v3 = Var.create "v3" (Type.Imm 3) in
  let v4 = Var.create "v4" (Type.Imm 3) in
  let v5 = Var.create "v5" (Type.Imm 2) in
  let e1,e2,e3,e4,e5 =
    Exp.(of_var v1, of_var v2, of_var v3, of_var v4, of_var v5) in
  let w = Exp.of_word (Word.of_int ~width:6 0b111010) in
  let x = RTL.(e1 ^ e2 ^ e3 ^ e4 ^ e5) in
  let rtl = RTL.[
      e1 := zero;
      e2 := zero;
      e3 := zero;
      e4 := zero;
      e5 := zero;
      extract x 2 7 := w;
    ] in
  let ctxt = eval_rtl rtl in
  let expected_val1 = Word.of_int ~width:1 0b0 in
  let expected_val2 = Word.of_int ~width:2 0b01 in
  let expected_val3 = Word.of_int ~width:3 0b110 in
  let expected_val4 = Word.of_int ~width:3 0b100 in
  let expected_val5 = Word.of_int ~width:2 0b00 in
  let val1 = lookup_var ctxt v1 in
  let val2 = lookup_var ctxt v2 in
  let val3 = lookup_var ctxt v3 in
  let val4 = lookup_var ctxt v4 in
  let val5 = lookup_var ctxt v5 in
  assert_bool "complex left := exp : failed at v1"
    (is_equal_words expected_val1 val1);
  assert_bool "complex left := exp : failed at v2"
    (is_equal_words expected_val2 val2);
  assert_bool "complex left := exp : failed at v3"
    (is_equal_words expected_val3 val3);
  assert_bool "complex left := exp : failed at v4"
    (is_equal_words expected_val4 val4);
  assert_bool "complex left := exp : failed at v5"
    (is_equal_words expected_val5 val5)

let nth_byte ctxt =
  let var = Var.create "tmp" (Type.Imm 64) in
  let e = Exp.of_var var in
  let x1 = Exp.of_word (Word.of_int64 0xAAAAAAAA_AAAAAAAAL) in
  let x2 = Exp.of_word (Word.of_int ~width:16 0xFF) in
  let expected = Word.of_int64 0xAAAAAAFF_AAAAAAAAL in
  let rtl = RTL.[
      e := x1;
      nth byte e 3 := x2;
    ] in
  let ctxt = eval_rtl rtl in
  let value = lookup_var ctxt var in
  assert_bool "nsize failed" (is_equal_words expected value)

let nth_hw ctxt =
  let var = Var.create "tmp" (Type.Imm 64) in
  let e = Exp.of_var var in
  let x1 = Exp.of_word (Word.of_int64 0xAAAAAAAA_AAAAAAAAL) in
  let x2 = Exp.of_word (Word.of_int ~width:16 0xFFFF) in
  let expected = Word.of_int64 0xAAAAFFFF_AAAAAAAAL in
  let rtl = RTL.[
      e := x1;
      nth halfword e 1 := x2;
    ] in
  let ctxt = eval_rtl rtl in
  let value = lookup_var ctxt var in
  assert_bool "nsize failed" (is_equal_words expected value)

let nth_bit ctxt =
  let var = Var.create "tmp" (Type.Imm 64) in
  let e = Exp.of_var var in
  let x1 = Exp.of_word (Word.of_int64 0xAAAAAAAA_AAAAAAAAL) in
  let expected = Word.of_int64 0xEAAAAAAA_AAAAAAAAL in
  let rtl = RTL.[
      e := x1;
      nth bit e 1 := one;
    ] in
  let ctxt = eval_rtl rtl in
  let value = lookup_var ctxt var in
  assert_bool "nbit failed" (is_equal_words expected value)

let suite = "Dsl" >::: [
    "plain :="                               >:: plain_assign;
    "concated exp := exp"                    >:: concat_assign;
    "extract (concat [v1; v2; ...]) := exp"  >:: complex_assign;
    "nth byte"                               >:: nth_byte;
    "nth halfword"                           >:: nth_hw;
    "nth bit"                                >:: nth_bit;
  ]
