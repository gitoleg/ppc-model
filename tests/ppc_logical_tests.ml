open Core_kernel.Std
open Bap.Std
open OUnit2

open Ppc_model
open Ppc_tests_helpers

let andi_dot arch ctxt =
  let bytes = "\x71\x2a\x00\x1F" in  (** andi.   r10,r9,31 *)
  let r10 = find_gpr "R10" in
  let r9 = find_gpr "R9" in
  let init = Bil.[
      r9 := int (Word.of_int ~width:64 10);
    ] in
  let expected = Word.of_int ~width:64 10 in
  check_gpr init bytes r10 expected arch ctxt

let andis_dot arch ctxt =
  let bytes = "\x75\x2a\x0E\x00" in  (** andis.  r10,r9,2048 *)
  let r10 = find_gpr "R10" in
  let r9 = find_gpr "R9" in
  let value = Word.of_int ~width:64 0x0800_0000 in
  let init = Bil.[
      r9 := int value;
    ] in
  let expected = value in
  check_gpr init bytes r10 expected arch ctxt

let and_ arch ctxt =
  let bytes = "\x7f\x39\xe8\x38" in (** and r25 r25 r29 *)
  let r25 = find_gpr "R25" in
  let r29  = find_gpr "R29" in
  let x = 31 in
  let y = 10 in
  let r = x land y in
  let init = Bil.[
      r29  := int (Word.of_int ~width:64 x);
      r25 := int (Word.of_int ~width:64 y);
    ] in
  let expected = Word.of_int ~width:64 r in
  check_gpr init bytes r25 expected arch ctxt

let andc arch ctxt =
  let bytes = "\x7c\xea\x50\x78" in (** andc r10 r7 r10 *)
  let r10 = find_gpr "R10" in
  let r7  = find_gpr "R7" in
  let x = 21 in
  let y = 10 in
  let r = x land (lnot y) in
  let init = Bil.[
      r7 := int (Word.of_int ~width:64 x);
      r10  := int (Word.of_int ~width:64 y);
    ] in
  let expected = Word.of_int ~width:64 r in
  check_gpr init bytes r10 expected arch ctxt

let ori arch ctxt =
  let bytes = "\x60\xc6\x51\xc1" in  (** ori     r6,r6,20929 *)
  let r6 = find_gpr "R6" in
  let x = 62 in
  let r = x lor 20929 in
  let init = Bil.[
      r6 := int (Word.of_int ~width:64 x);
    ] in
  let expected = Word.of_int ~width:64 r in
  check_gpr init bytes r6 expected arch ctxt

let oris arch ctxt =
  let bytes = "\x65\x4a\x00\x0F" in (** oris    r10,r10,15  *)
  let r10 = find_gpr "R10" in
  let x = 61440 in
  let y = 15 in
  let r = x lxor (y lsl 16) in
  let init = Bil.[
      r10 := int (Word.of_int ~width:64 x);
    ] in
  let expected = Word.of_int ~width:64 r in
  check_gpr init bytes r10 expected arch ctxt

let or_  arch ctxt =
  let bytes = "\x7f\x38\xc3\x78" in (** or r24,r25,r24  *)
  let r24 = find_gpr "R24" in
  let r25 = find_gpr "R25" in
  let x = 24 in
  let y = 10 in
  let r = x lor y in
  let init = Bil.[
      r24 := int (Word.of_int ~width:64 x);
      r25 := int (Word.of_int ~width:64 y);
    ] in
  let expected = Word.of_int ~width:64 r in
  check_gpr init bytes r24 expected arch ctxt

let orc arch ctxt =
  let bytes = "\x7c\x8a\x53\x38" in (** orc     r10,r4,r10 *)
  let r4 = find_gpr "R4" in
  let r10 = find_gpr "R10" in
  let x = 42 in
  let y = 10 in
  let r = x lor (lnot y)  in
  let init = Bil.[
      r4 := int (Word.of_int ~width:64 x);
      r10 := int (Word.of_int ~width:64 y);
    ] in
  let expected = Word.of_int ~width:64 r in
  check_gpr init bytes r10 expected arch ctxt

let xori arch ctxt =
  let bytes = "\x68\x63\x00\x0B" in (** xori    r3,r3,11   *)
  let r3 = find_gpr "R3" in
  let x = 16 in
  let r = 16 lxor 11 in
  let init = Bil.[
      r3 := int (Word.of_int ~width:64 x);
    ] in
  let expected = Word.of_int ~width:64 r in
  check_gpr init bytes r3 expected arch ctxt

let xoris arch ctxt =
  let bytes = "\x6d\x2a\x00\x0f" in (** xoris r10,r9,15 *)
  let r9 = find_gpr "R9" in
  let r10 = find_gpr "R10" in
  let x = 0x1F0000 in
  let r = x lxor (15 lsl 16) in
  let init = Bil.[
      r9 := int (Word.of_int ~width:64 x);
    ] in
  let expected = Word.of_int ~width:64 r in
  check_gpr init bytes r10 expected arch ctxt

let xor_ arch ctxt =
  let bytes = "\x7c\x6a\x52\x78" in (** xor     r10,r3,r10 *)
  let r3 = find_gpr "R3" in
  let r10 = find_gpr "R10" in
  let x = 42 in
  let y = 15 in
  let r = 42 lxor 15 in
  let init = Bil.[
      r3 := int (Word.of_int ~width:64 x);
      r10 := int (Word.of_int ~width:64 y);
    ] in
  let expected = Word.of_int ~width:64 r in
  check_gpr init bytes r10 expected arch ctxt

let nand arch ctxt =
  let bytes = "\x7c\x63\x23\xb8" in (** nand    r3,r3,r4 *)
  let r3 = find_gpr "R3" in
  let r4 = find_gpr "R4" in
  let x = 42 in
  let y = 15 in
  let r = lnot (42 land 15) in
  let init = Bil.[
      r3 := int (Word.of_int ~width:64 x);
      r4 := int (Word.of_int ~width:64 y);
    ] in
  let expected = Word.of_int ~width:64 r in
  check_gpr init bytes r3 expected arch ctxt

let nor arch ctxt =
  let bytes = "\x7d\x09\x48\xf8" in (** nor     r9,r8,r9 *)
  let r8 = find_gpr "R8" in
  let r9 = find_gpr "R9" in
  let x = 42 in
  let y = 15 in
  let r = lnot (x lor y) in
  let init = Bil.[
      r8 := int (Word.of_int ~width:64 x);
      r9 := int (Word.of_int ~width:64 y);
    ] in
  let expected = Word.of_int ~width:64 r in
  check_gpr init bytes r9 expected arch ctxt

let eqv arch ctxt =
  let bytes = "\x7d\x09\x4a\x38" in (** eqv     r9,r8,r9 *)
  let r8 = find_gpr "R8" in
  let r9 = find_gpr "R9" in
  let x = 42 in
  let y = 15 in
  let r = lnot (x lxor y); in
  let init = Bil.[
      r8 := int (Word.of_int ~width:64 x);
      r9 := int (Word.of_int ~width:64 y);
    ] in
  let expected = Word.of_int ~width:64 r in
  check_gpr init bytes r9 expected arch ctxt

let extsb arch ctxt =
  let bytes = "\x7d\x6a\x07\x74" in   (** extsb   r10,r11  *)
  let r10 = find_gpr "R10" in
  let r11 = find_gpr "R11" in
  let init = Bil.[
      r11 := int (Word.of_int ~width:64 0xC0);
    ] in
  let expected = Word.of_int64 0xFFFFFFFFFFFFFFC0L in
  check_gpr init bytes r10 expected arch ctxt

let extsh arch ctxt =
  let bytes ="\x7d\x25\x07\x34" in   (** extsh   r5,r9 *)
  let r5 = find_gpr "R5" in
  let r9 = find_gpr "R9" in
  let init = Bil.[
      r9 := int (Word.of_int ~width:64 0xC000);
    ] in
  let expected = Word.of_int64 0xFFFFFFFFFFFFC000L in
  check_gpr init bytes r5 expected arch ctxt

let and_dot arch ctxt = failwith "unimplemented"
let andc_dot arch ctxt = failwith "unimplemented"
let or_dot arch ctxt = failwith "unimplemented"
let orc_dot arch ctxt = failwith "unimplemented"
let xor_dot arch ctxt = failwith "unimplemented"
let nor_dot arch ctxt = failwith "unimplemented"
let nand_dot arch ctxt = failwith "unimplemented"
let eqv_dot arch ctxt = failwith "unimplemented"
let exts_dot arch ctxt = failwith "unimplemented"

let cntlz arch value zeros ctxt =
  let bytes = "\x7c\x63\x00\x34" in
  let r = find_gpr "R3" in
  let init = Bil.[
      r := int (Word.of_int ~width:64 value);
    ] in
  let expected = Word.of_int ~width:64 zeros in
  check_gpr init bytes r expected arch ctxt

let cnttz arch value zeros ctxt =
  let bytes = "\x7c\x63\x04\x34" in
  let r = find_gpr "R3" in
  let init = Bil.[
      r := int (Word.of_int ~width:64 value);
    ] in
  let expected = Word.of_int ~width:64 zeros in
  check_gpr init bytes r expected arch ctxt

let cmpb arch ~bytes_cnt x y expected ctxt =
  let bytes = "\x7c\x8a\x53\xf8" in
  let x = Word.of_int ~width:64 x in
  let y = Word.of_int ~width:64 y in
  let r10 = find_gpr "R10" in
  let r4 = find_gpr "R4" in
  let init = Bil.[
      r10 := int x;
      r4  := int y;
    ] in
  let head = Word.ones (64 - bytes_cnt * 8) in
  let expected = Word.of_int ~width:(bytes_cnt * 8) expected in
  let expected = Word.concat head expected in
  check_gpr init bytes r10 expected arch ctxt

let popcntw arch ctxt =
  let bytes = "\x7c\x44\x02\xf4" in (** popcntw r4, r2 *)
  let r4 = find_gpr "R4" in
  let r2 = find_gpr "R2" in
  let value = Word.of_int64 0xA0200040_10000001L in
  let expected = Word.of_int64 0x400000002L in (** 4 bits set in first word, and 2 in second  *)
  let init = Bil.[
      r2 := int value;
    ] in
  check_gpr init bytes r4 expected arch ctxt

let suite = "logical" >::: [
    "andi."     >:: andi_dot `ppc;
    "andis."    >:: andis_dot `ppc;
    "and"       >:: and_ `ppc;
    "andc"      >:: andc `ppc;
    "ori"       >:: ori `ppc;
    "oris"      >:: oris `ppc;
    "or_"       >:: or_ `ppc;
    "orc"       >:: orc `ppc;
    "xori"      >:: xori `ppc;
    "xoris"     >:: xoris `ppc;
    "xor_"      >:: xor_ `ppc;
    "nand"      >:: nand `ppc;
    "nor"       >:: nor `ppc;
    "eqv"       >:: eqv `ppc;
    "extsb"     >:: extsb `ppc;
    "extsh"     >:: extsh `ppc;
    "cntlz: 1"  >:: cntlz `ppc 0x0 32;
    "cntlz: 2"  >:: cntlz `ppc 0x4000000 5;
    "cntlz: 3"  >:: cntlz `ppc 0x40000000 1;
    "cntlz: 4"  >:: cntlz `ppc 0x80000000 0;
    "cnttz: 1"  >:: cnttz `ppc 0x0 32;
    "cnttz: 2"  >:: cnttz `ppc 0x1 0;
    "cnttz: 3"  >:: cnttz `ppc 0x2 1;
    "cnttz: 4"  >:: cnttz `ppc 0x20 5;
    "cmpb: 1"   >:: cmpb `ppc ~bytes_cnt:3 0x31_42_45 0x34_42_AD 0x00_FF_00;
    "cmpb: 2"   >:: cmpb `ppc ~bytes_cnt:3 0 0x34_42_AD 0x0;
    "cmpb: 3"   >:: cmpb `ppc ~bytes_cnt:3 0x34_42_AD 0x34_42_AD 0xFF_FF_FF;
    "popcntw"   >:: popcntw `ppc;

    "andi."     >:: andi_dot `ppc64;
    "andis."    >:: andis_dot `ppc64;
    "and"       >:: and_ `ppc64;
    "andc"      >:: andc `ppc64;
    "ori"       >:: ori `ppc64;
    "oris"      >:: oris `ppc64;
    "or_"       >:: or_ `ppc64;
    "orc"       >:: orc `ppc64;
    "xori"      >:: xori `ppc64;
    "xoris"     >:: xoris `ppc64;
    "xor_"      >:: xor_ `ppc64;
    "nand"      >:: nand `ppc64;
    "nor"       >:: nor `ppc64;
    "eqv"       >:: eqv `ppc64;
    "extsb"     >:: extsb `ppc64;
    "extsh"     >:: extsh `ppc64;
    "cntlz: 1"  >:: cntlz `ppc64 0x0 32;
    "cntlz: 2"  >:: cntlz `ppc64 0x4000000 5;
    "cntlz: 3"  >:: cntlz `ppc64 0x40000000 1;
    "cntlz: 4"  >:: cntlz `ppc64 0x80000000 0;
    "cnttz: 1"  >:: cnttz `ppc64 0x0 32;
    "cnttz: 2"  >:: cnttz `ppc64 0x1 0;
    "cnttz: 3"  >:: cnttz `ppc64 0x2 1;
    "cnttz: 4"  >:: cnttz `ppc64 0x20 5;
    "cmpb: 1"   >:: cmpb `ppc64 ~bytes_cnt:3 0x31_42_45 0x34_42_AD 0x00_FF_00;
    "cmpb: 2"   >:: cmpb `ppc64 ~bytes_cnt:3 0 0x34_42_AD 0x0;
    "cmpb: 3"   >:: cmpb `ppc64 ~bytes_cnt:3 0x34_42_AD 0x34_42_AD 0xFF_FF_FF;
    "popcntw"   >:: popcntw `ppc64;
  ]
