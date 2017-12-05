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
  let x1 = Exp.of_word (Word.of_int64 0xFFFFFFFF_FFFFFFFFL) in
  let x2 = Exp.of_word (Word.of_int ~width:8 0xAA) in
  let expected = Word.of_int64 0xFFFFFFAA_FFFFFFFFL in
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

let foreach ctxt =
  let w = 40 in
  let x1 = Exp.of_word @@ Word.of_int ~width:w 0xAA_CC_BB_CC_DD in
  let x2 = Exp.of_word @@ Word.of_int ~width:w 0xBB_CC_DD_CC_EE in
  let v1 = Exp.of_var @@ Var.create "tmp1" (Type.Imm w) in
  let v2 = Exp.of_var @@ Var.create "tmp2" (Type.Imm w) in
  let cnt' = Var.create "cnt" (Type.Imm 8) in
  let ind_i' = Var.create "ind_i" (Type.Imm 8) in
  let ind_j' = Var.create "ind_j" (Type.Imm 8) in
  let byte_i' = Var.create "byte_i" (Type.Imm 8) in
  let byte_j' = Var.create "byte_j" (Type.Imm 8) in
  let cnt = Exp.of_var cnt' in
  let ind_i = Exp.of_var ind_i' in
  let ind_j = Exp.of_var ind_j' in
  let byte_i = Exp.of_var byte_i' in
  let byte_j = Exp.of_var byte_j' in
  let rtl =
    RTL.[
      v1 := x1;
      v2 := x2;
      cnt := zero;
      ind_i := zero;
      foreach byte_i v1 [
        ind_j := zero;
        foreach byte_j v2 [
          when_ (ind_j = ind_i) [
            when_ (byte_i = byte_j) [
              cnt := cnt + one;
            ]
          ];
          ind_j := ind_j + one;
        ];
        ind_i := ind_i + one;
      ];
    ] in
  let expected = Word.of_int ~width:8 2 in
  let ctxt = eval_rtl rtl in
  let value = lookup_var ctxt cnt' in
  assert_bool "foreach failed" (is_equal_words expected value)

let foreach_with_assign ctxt =
  let w = 24 in
  let x = Exp.of_word @@ Word.of_int ~width:w 0xAA_CC_BB in
  let y = Exp.of_word @@ Word.of_int ~width:w 0xAA_BB_BB in
  let v1 = Exp.of_var @@ Var.create "tmp1" (Type.Imm w) in
  let v2 = Exp.of_var @@ Var.create "tmp2" (Type.Imm w) in
  let v3' = Var.create "tmp3" (Type.Imm w) in
  let v3 = Exp.of_var v3' in
  let z = unsigned const byte 0xFF in
  let p = unsigned const byte 0xAA in
  let byte_i' = Var.create "byte_i" (Type.Imm 8) in
  let byte_i = Exp.of_var byte_i' in
  let byte_j = unsigned var byte in
  let byte_k = unsigned var byte in
  let ind_i = unsigned var byte in
  let ind_j = unsigned var byte in
  let ind_k = unsigned var byte in
  let rtl =
    RTL.[
      v1 := x;
      v2 := y;
      v3 := x;
      ind_i := zero;
      foreach byte_i v1 [
        ind_j := zero;
        foreach byte_j v2 [
          ind_k := zero;
          foreach byte_k v3 [
            when_ ((ind_i = ind_k) land (ind_k = ind_j)) [
              if_ (byte_j = byte_i) [
                byte_k := z;
              ] [
                byte_k := p;
              ]
            ];
            ind_k := ind_k + one;
          ];
          ind_j := ind_j + one;
        ];
        ind_i := ind_i + one;
      ];
    ] in
  let expected = Word.of_int ~width:w 0xFF_AA_FF in
  let ctxt = eval_rtl rtl in
  let value = lookup_var ctxt v3' in
  assert_bool "foreach with assign failed" (is_equal_words expected value)


let suite = "Dsl" >::: [
    "plain :="                               >:: plain_assign;
    "concated exp := exp"                    >:: concat_assign;
    "extract (concat [v1; v2; ...]) := exp"  >:: complex_assign;
    "nth byte"                               >:: nth_byte;
    "nth halfword"                           >:: nth_hw;
    "nth bit"                                >:: nth_bit;
    "foreach"                                >:: foreach;
    "foreach with assign"                    >:: foreach_with_assign;
  ]
