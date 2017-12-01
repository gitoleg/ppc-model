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

let nsize ctxt =
  let var = Var.create "tmp" (Type.Imm 64) in
  let e = Exp.of_var var in
  let x1 = Exp.of_word (Word.of_int64 0xAAAAAAAA_AAAAAAAAL) in
  let x2 = Exp.of_word (Word.of_int ~width:16 0xFFFF) in
  let expected = Word.of_int64 0xAAAAFFFF_AAAAAAAAL in
  let rtl = RTL.[
      e := x1;
      nsize e `r16 1 := x2;
    ] in
  let ctxt = eval_rtl rtl in
  let value = lookup_var ctxt var in
  assert_bool "nsize failed" (is_equal_words expected value)

let nbit ctxt =
  let var = Var.create "tmp" (Type.Imm 64) in
  let e = Exp.of_var var in
  let x1 = Exp.of_word (Word.of_int64 0xAAAAAAAA_AAAAAAAAL) in
  let expected = Word.of_int64 0xEAAAAAAA_AAAAAAAAL in
  let rtl = RTL.[
      e := x1;
      nbit e 1 := one;
    ] in
  let ctxt = eval_rtl rtl in
  let value = lookup_var ctxt var in
  assert_bool "nbit failed" (is_equal_words expected value)


let suite = "Dsl" >::: [
    "nsize"     >:: nsize;
    "nbit"      >:: nbit;
  ]
