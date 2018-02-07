open Core_kernel.Std
open Bap.Std
open OUnit2

open Powerpc_tests_helpers
open Powerpc_rtl

module Model = Powerpc_model
module Dsl = Powerpc_dsl
module Exp = Powerpc_rtl.Exp

module RTL = struct
  include Powerpc_rtl
  include Infix
end

open Dsl

module P = Model.PowerPC_32

open P.E

let eval_rtl rtl =
  let bil = bil_of_t rtl in
  Stmt.eval bil (new Bili.context)

let width_of_string str expected ctxt =
  let x = unsigned of_string str in
  let width = Exp.width x in
  let err = sprintf "Dsl.of_string failed, expected width %d <> %d\n"
      expected width in
  assert_bool err (expected = width)


let dsl_extract ctxt =
  let v = Exp.of_word @@ Word.of_int ~width:32 0xAABBCCDD in
  let x = unsigned var word in
  let y' = Var.create "y" (Type.Imm 32) in
  let y  = Exp.of_var y' in
  let rtl = RTL.[
    x := v;
    y := Dsl.extract x 3 6;
  ] in
  let ctxt = eval_rtl rtl in
  let expected = Word.of_int ~width:32 5 in
  let value = lookup_var ctxt y' in
  assert_bool "extract failed" (is_equal_words expected value)

let dsl_extract_signed ctxt =
  let v = Exp.of_word @@ Word.of_int ~width:8 0xFFFF in
  let x = signed var word in
  let y' = Var.create "y" (Type.Imm 32) in
  let y  = Exp.(unsigned @@ of_var y') in
  let rtl = RTL.[
    x := v;
    y := Dsl.extract x 0 31;
  ] in
  let ctxt = eval_rtl rtl in
  let expected = Word.of_int64 ~width:32 0xFFFFFFFFL in
  let value = lookup_var ctxt y' in
  assert_bool "extract signed failed" (is_equal_words expected value)

let dsl_extract_unsigned ctxt =
  let v = Exp.of_word @@ Word.of_int ~width:8 0xFFFF in
  let x = unsigned var word in
  let y' = Var.create "y" (Type.Imm 32) in
  let y  = Exp.(signed @@ of_var y') in
  let rtl = RTL.[
    x := v;
    y := Dsl.extract x 0 31;
  ] in
  let ctxt = eval_rtl rtl in
  let expected = Word.of_int ~width:32 0xFF in
  let value = lookup_var ctxt y' in
  assert_bool "extract signed failed" (is_equal_words expected value)

let first ctxt =
  let v = Exp.of_word @@ Word.of_int ~width:32 0xAABBCCDD in
  let x = unsigned var word in
  let y' = Var.create "y" (Type.Imm 32) in
  let y  = Exp.of_var y' in
  let rtl = RTL.[
    x := v;
    y := Dsl.first x 6;
  ] in
  let ctxt = eval_rtl rtl in
  let expected = Word.of_int ~width:32 42 in
  let value = lookup_var ctxt y' in
  assert_bool "first failed" (is_equal_words expected value)

let last ctxt =
  let v = Exp.of_word @@ Word.of_int ~width:32 0xAABBCCDD in
  let x = unsigned var word in
  let y' = Var.create "y" (Type.Imm 32) in
  let y  = Exp.of_var y' in
  let rtl = RTL.[
    x := v;
    y := Dsl.last x 6;
  ] in
  let ctxt = eval_rtl rtl in
  let expected = Word.of_int ~width:32 29 in
  let value = lookup_var ctxt y' in
  assert_bool "last failed" (is_equal_words expected value)

let msb ctxt =
  let v = Exp.of_word @@ Word.of_int ~width:32 0x80000000 in
  let x = unsigned var word in
  let y' = Var.create "y" (Type.Imm 1) in
  let y  = Exp.of_var y' in
  let rtl = RTL.[
    x := v;
    y := Dsl.msb x;
  ] in
  let ctxt = eval_rtl rtl in
  let expected = Word.of_int ~width:1 1 in
  let value = lookup_var ctxt y' in
  assert_bool "msb failed" (is_equal_words expected value)

let lsb ctxt =
  let v = Exp.of_word @@ Word.of_int ~width:32 1 in
  let x = unsigned var word in
  let y' = Var.create "y" (Type.Imm 1) in
  let y  = Exp.of_var y' in
  let rtl = RTL.[
    x := v;
    y := Dsl.lsb x;
  ] in
  let ctxt = eval_rtl rtl in
  let expected = Word.of_int ~width:1 1 in
  let value = lookup_var ctxt y' in
  assert_bool "lsb failed" (is_equal_words expected value)

let low ctxt =
  let v = Exp.of_word @@ Word.of_int ~width:32 0xAABBCCDD in
  let x = unsigned var word in
  let y' = Var.create "y" (Type.Imm 8) in
  let y  = Exp.of_var y' in
  let rtl = RTL.[
    x := v;
    y := Dsl.low byte x;
  ] in
  let ctxt = eval_rtl rtl in
  let expected = Word.of_int ~width:8 0xDD in
  let value = lookup_var ctxt y' in
  assert_bool "low failed" (is_equal_words expected value)

let high width bits ctxt =
  let data = Word.of_int64 0xAABBCCDD_EEFFAABBL in
  let v = Exp.of_word data in
  let x = unsigned var doubleword in
  let y' = Var.create "y" (Type.Imm bits) in
  let y  = Exp.of_var y' in
  let rtl = RTL.[
    x := v;
    y := Dsl.high width x;
  ] in
  let ctxt = eval_rtl rtl in
  let expected = Word.extract_exn ~lo:(64 - bits) data in
  let value = lookup_var ctxt y' in
  assert_bool "high failed" (is_equal_words expected value)

let plain_assign ctxt =
  let v = Var.create "v" (Type.Imm 16) in
  let e = Exp.of_var v in
  let w = Exp.of_word (Word.of_int ~width:8 0x42) in
  let expected = Word.of_int ~width:16 0x42 in
  let rtl = RTL.[ e := w; ] in
  let ctxt = eval_rtl rtl in
  let value = lookup_var ctxt v in
  assert_bool "plain := failed" (is_equal_words expected value)

(** Assign to concated variables:
    +----+---+----+---+---+---+--+
    | 0 | 1 |  2 | 3 | 4 | 5 | 6 |
    |---|---|----|---|---|---|---|
    |   | 1 |  1 | 1 | 0 | 1 | 0 |
    |-------|----|---------------|
    |  var1 |var2|       var3    |
    +----+---+---+---+---+---+---+ *)
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


(** Assign to concated variables with extract:
    +----+---+---+---+---+---+---+---+---+---+---+
    | 0  | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |10 |
    |----|---|---|---|---|---|---|---|---|---|---|
    |    |   | 1 | 1 | 1 | 0 | 1 | 0 |   |   |   |
    |----|-------|-----------|-----------|-------|
    |var1|  var2 |   var3    |   var4    | var5  |
    +----+---+---+---+---+---+---+---+---+---+---+ *)
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
  let x = Var.create "x" (Type.Imm 64) in
  let y = Var.create "y" (Type.Imm 8) in
  let ex = Exp.of_var x in
  let ey = Exp.of_var y in
  let ew = Exp.of_word (Word.of_int64 0xABCDEF34_FFAABB42L) in
  let expected = Word.of_int ~width:8 0xEF in
  let rtl = RTL.[
      ex := ew;
      ey := nth byte ex 2;
    ] in
  let ctxt = eval_rtl rtl in
  let value = lookup_var ctxt y in
  assert_bool "nth byte failed" (is_equal_words expected value)

let nth_hw ctxt =
  let x = Var.create "x" (Type.Imm 64) in
  let y = Var.create "y" (Type.Imm 16) in
  let ex = Exp.of_var x in
  let ey = Exp.of_var y in
  let ew = Exp.of_word (Word.of_int64 0xABCDEF34_FFAABB42L) in
  let expected = Word.of_int ~width:16 0xEF34 in
  let rtl = RTL.[
      ex := ew;
      ey := nth halfword ex 1;
    ] in
  let ctxt = eval_rtl rtl in
  let value = lookup_var ctxt y in
  assert_bool "nth halfword failed" (is_equal_words expected value)

let nth_dw ctxt =
  let x = Var.create "x" (Type.Imm 96) in
  let y = Var.create "y" (Type.Imm 64) in
  let ex = Exp.of_var x in
  let ey = Exp.of_var y in
  let ew1 = Exp.of_word (Word.of_int ~width:32 0xABCDEF42) in
  let ew2 = Exp.of_word (Word.of_int ~width:32 0xFFAABBCC) in
  let ew3 = Exp.of_word (Word.of_int ~width:32 0x12345678) in
  let expected = Word.of_int64 0xABCDEF42_FFAABBCCL in
  let rtl = RTL.[
      ex := ew1 ^ ew2 ^ ew3;
      ey := nth doubleword ex 0;
    ] in
  let ctxt = eval_rtl rtl in
  let value = lookup_var ctxt y in
  assert_bool "nth doubleword failed" (is_equal_words expected value)

let nth_bit_with_assign ctxt =
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
  assert_bool "nth bit failed" (is_equal_words expected value)

let nth_byte_with_assign ctxt =
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
  assert_bool "nth byte failed" (is_equal_words expected value)

let nth_hw_with_assign ctxt =
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
  assert_bool "nth halfword failed" (is_equal_words expected value)

let cr_assign ctxt =
  let ones = Exp.of_word @@ Word.ones 8 in
  let rtl = RTL.[
      cr := zero;
      nth byte cr 2 := ones;
    ] in
  let ctxt = eval_rtl rtl in
  let range = List.range 0 32 in
  List.iter range ~f:(fun i ->
      let expected = if i >= 16 && i <= 23 then
          Word.b1 else Word.b0 in
      let cr_bit = Int.Map.find_exn P.cri i in
      let value = lookup_var ctxt cr_bit in
      let err = sprintf "cr assign to bit %d failed\n" i in
      assert_bool err (is_equal_words expected value))

let cr_field_assign ctxt =
  let ones = Exp.of_word @@ Word.ones 4 in
  let crf = String.Map.find_exn cr_fields "CR2" in
  let rtl = RTL.[
      cr := zero;
      crf := ones;
    ] in
  let ctxt = eval_rtl rtl in
  let range = List.range 0 32 in
  List.iter range ~f:(fun i ->
      let expected = if i >= 8 && i <= 11 then
          Word.b1 else Word.b0 in
      let cr_bit = Int.Map.find_exn P.cri i in
      let value = lookup_var ctxt cr_bit in
      let err = sprintf "cr field assign to bit %d failed\n" i in
      assert_bool err (is_equal_words expected value))

let cr_shift ctxt =
  let ones = Exp.of_word (Word.of_int64 0xFFFFFFFF_FFFFFFFFL) in
  let sh = Exp.of_word (Word.of_int64 2L) in
  let rtl = RTL.[
      cr := ones;
      cr := cr << sh;
    ] in
  let ctxt = eval_rtl rtl in
  let range = List.range 0 32 in
  List.iter range ~f:(fun i ->
      let expected = if i >= 0 && i <= 29 then
          Word.b1 else Word.b0 in
      let cr_bit = Int.Map.find_exn P.cri i in
      let value = lookup_var ctxt cr_bit in
      let err = sprintf "cr assign to bit %d failed\n" i in
      assert_bool err (is_equal_words expected value))

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
  let foreach' = RTL.foreach ~inverse:true in
  let rtl =
    RTL.[
      v1 := x1;
      v2 := x2;
      cnt := zero;
      ind_i := zero;
      foreach' byte_i v1 [
        ind_j := zero;
        foreach' byte_j v2 [
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
  let foreach' = RTL.foreach ~inverse:true in
  let rtl =
    RTL.[
      v1 := x;
      v2 := y;
      v3 := x;
      ind_i := zero;
      foreach' byte_i v1 [
        ind_j := zero;
        foreach' byte_j v2 [
          ind_k := zero;
          foreach' byte_k v3 [
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

let foreach_test_inverse inverse ctxt =
  let x = Exp.of_word @@ Word.of_int64 ~width:32 0xAA_BB_CC_DDL in
  let v = unsigned var word in
  let ind = unsigned var byte in
  let ind2 = unsigned const byte 2 in
  let ind3 = unsigned const byte 3 in
  let var1' = Var.create "tmp1" (Type.Imm 8) in
  let var2' = Var.create "tmp2" (Type.Imm 8) in
  let var3' = Var.create "tmp3" (Type.Imm 8) in
  let var4' = Var.create "tmp4" (Type.Imm 8) in
  let var1 = Exp.of_var var1' in
  let var2 = Exp.of_var var2' in
  let var3 = Exp.of_var var3' in
  let var4 = Exp.of_var var4' in
  let byte_i = unsigned var byte in
  let foreach' = RTL.foreach ~inverse in
  let rtl =
    RTL.[
      v := x;
      ind := zero;
      foreach' byte_i v [
        when_ (ind = zero) [ var1 := byte_i];
        when_ (ind = one)  [ var2 := byte_i];
        when_ (ind = ind2) [ var3 := byte_i];
        when_ (ind = ind3) [ var4 := byte_i];
        ind := ind + one;
      ];
    ] in
  let expected_1 = Word.of_int ~width:8 0xDD in
  let expected_2 = Word.of_int ~width:8 0xCC in
  let expected_3 = Word.of_int ~width:8 0xBB in
  let expected_4 = Word.of_int ~width:8 0xAA in
  let ctxt = eval_rtl rtl in
  let check var expected =
    let name = Var.name var in
    let value = lookup_var ctxt var in
    assert_bool (sprintf "foreach inverse=%b failed for %s" inverse name)
      (is_equal_words expected value) in
  if inverse then
    let () = check var1' expected_4 in
    let () = check var2' expected_3 in
    let () = check var3' expected_2 in
    check var4' expected_1
  else
    let () = check var1' expected_1 in
    let () = check var2' expected_2 in
    let () = check var3' expected_3 in
    check var4' expected_4

(** check that rotate insns could be implemented like this  *)
let circ_shift_32 ctxt =
  let w = Word.of_int64 0xAABBCCDD_EEFF4242L in
  let e = Exp.of_word w in
  let x = unsigned var doubleword in
  let y = unsigned var word in
  let z' = Var.create "res" (Type.Imm 64) in
  let z = Exp.of_var z' in
  let sh = unsigned const byte 4 in
  let rtl =
    RTL.[
      x := e;
      y := nth word x 1;
      z := nth doubleword ((y ^ y ^ y) << sh) 0;
    ] in
  let expected = Word.of_int64 0xEFF4242E_EFF4242EL in
  let ctxt = eval_rtl rtl in
  let value = lookup_var ctxt z' in
  assert_bool "circ_shift failed" (is_equal_words expected value)

let lshift_signed ctxt =
  let w = Word.of_int64 ~width:32 0xEEFF4242L in
  let e = Exp.signed (Exp.of_word w) in
  let z' = Var.create "res" (Type.Imm 64) in
  let z = Exp.of_var z' |> Exp.signed in
  let sh = unsigned const byte 2 in
  let rtl = RTL.[
      z := e << sh;
    ] in
  let expected =
    let b = Word.of_int64 ~width:32 0xFFFFFFFFL in
    let sh = Word.of_int ~width:32 2 in
    Word.(concat b (w lsl sh)) in
  let ctxt = eval_rtl rtl in
  let value = lookup_var ctxt z' in
  assert_bool "lshift failed" (is_equal_words expected value)

let rshift_signed ctxt =
  let w = Word.of_int64 ~width:32 0xEEFF4242L in
  let e = Exp.signed (Exp.of_word w) in
  let z' = Var.create "res" (Type.Imm 32) in
  let z = Exp.of_var z' |> Exp.signed in
  let sh = unsigned const byte 8 in
  let rtl = RTL.[
      z := e >> sh;
    ] in
  let expected = Word.of_int64 ~width:32 0xFFEEFF42L in
  let ctxt = eval_rtl rtl in
  let value = lookup_var ctxt z' in
  assert_bool "rshift failed" (is_equal_words expected value)

let suite = "Dsl" >::: [
    "exp of string 1"                        >:: width_of_string "1" 1;
    "exp of string 42"                       >:: width_of_string "42" 6;
    "exp of string 0b00"                     >:: width_of_string "0b00" 2;
    "exp of string 0b101"                    >:: width_of_string "0b101" 3;
    "exp of string 0b0101"                   >:: width_of_string "0b0101" 4;
    "exp of string 0o474"                    >:: width_of_string "0o474" 9;
    "exp of string 0o0474"                   >:: width_of_string "0o0474" 12;
    "exp of string 0x3FA"                    >:: width_of_string "0x3FA" 12;
    "exp of string 0x03FA"                   >:: width_of_string "0x03FA" 16;
    "exp of big string"                      >:: width_of_string "0xFFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF" 112;
    "extract"                                >:: dsl_extract;
    "extract signed"                         >:: dsl_extract_signed;
    "extract unsigned"                       >:: dsl_extract_unsigned;
    "low"                                    >:: low;
    "high byte"                              >:: high byte 8;
    "high halfword"                          >:: high halfword 16;
    "high word"                              >:: high word 32;
    "high doubleword"                        >:: high doubleword 64;
    "first"                                  >:: first;
    "last"                                   >:: last;
    "msb"                                    >:: msb;
    "lsb"                                    >:: lsb;
    "plain :="                                >:: plain_assign;
    "concated vars := exp"                    >:: concat_assign;
    "extract (concat [v1; v2; ...]) := exp"   >:: complex_assign;
    "nth bit with assign"                    >:: nth_bit_with_assign;
    "nth byte with assign"                   >:: nth_byte_with_assign;
    "nth halfword with assign"               >:: nth_hw_with_assign;
    "nth byte"                               >:: nth_byte;
    "nth halfword"                           >:: nth_hw;
    "nth doubleword"                         >:: nth_dw;
    "cr assign"                              >:: cr_assign;
    "cr_field_assign"                        >:: cr_field_assign;
    "cr_shift"                               >:: cr_shift;
    "foreach"                                >:: foreach;
    "foreach with assign"                    >:: foreach_with_assign;
    "foreach with inverse"                   >:: foreach_test_inverse true;
    "foreach without inverse"                >:: foreach_test_inverse false;
    "circ shift"                             >:: circ_shift_32;
    "lshift signed"                          >:: lshift_signed;
    "rshift signed"                          >:: rshift_signed;
  ]
