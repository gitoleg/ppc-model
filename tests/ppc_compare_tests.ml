open Core_kernel.Std
open Bap.Std
open OUnit2

open Ppc_model.Hardware
open Ppc_rtl
open Ppc_tests_helpers

type expected = LT | GT | EQ

let cr_field_num = 7

let print_bits lt gt eq =
  let str = function
    | None -> "unknown"
    | Some x -> Word.to_string x in
  printf "lt %s; gt %s; eq %s\n" (str lt) (str gt) (str eq)

let check ctxt expected =
  let cr_lt = condition_register_bit 3 in
  let cr_gt = condition_register_bit 2 in
  let cr_eq = condition_register_bit 1 in
  let lt = lookup_var ctxt cr_lt in
  let gt = lookup_var ctxt cr_gt in
  let eq = lookup_var ctxt cr_eq in
  match expected with
  | LT ->
    is_equal_words Word.b1 lt &&
    is_equal_words Word.b0 gt &&
    is_equal_words Word.b0 eq
  | GT ->
    is_equal_words Word.b0 lt &&
    is_equal_words Word.b1 gt &&
    is_equal_words Word.b0 eq
  | EQ ->
    is_equal_words Word.b0 lt &&
    is_equal_words Word.b0 gt &&
    is_equal_words Word.b1 eq

let cmpi name ~opcode l_field reg_value value expected arch (ctxt : test_ctxt) =
  let reg_num = 6 in
  let opcode = Word.of_int ~width:6 opcode in
  let cr_field = Word.of_int ~width:3 cr_field_num in
  let l_field = Word.of_int ~width:1 l_field in
  let reg = Word.of_int ~width:5 reg_num in
  let value = Word.of_int64 ~width:16 value in
  let bytes =
    make_bytes [opcode; cr_field; Word.b0; l_field; reg; value] in
  let reg = find_gpr (sprintf "R%d" reg_num) in
  let init = Bil.[reg := int @@ Word.of_int64 reg_value] in
  let c = eval init bytes arch in
  assert_bool (sprintf "%s failed" name) @@ check c expected

let cmpwi = cmpi "cmpwi" ~opcode:11 0
let cmpdi = cmpi "cmpdi" ~opcode:11 1
let cmplwi = cmpi "cmplwi" ~opcode:10 0
let cmpldi = cmpi "cmpldi" ~opcode:10 1

let cmp name ~opt_opcode l_field reg1_value reg2_value expected arch (ctxt : test_ctxt) =
  let reg1_num = 6 in
  let reg2_num = 7 in
  let opcode = Word.of_int ~width:6 31 in
  let cr_field = Word.of_int ~width:3 cr_field_num in
  let l_field = Word.of_int ~width:1 l_field in
  let reg1 = Word.of_int ~width:5 reg1_num in
  let reg2 = Word.of_int ~width:5 reg2_num in
  let opt_opcode = Word.of_int ~width:10 opt_opcode in
  let fin = Word.b0 in
  let bytes =
    make_bytes [opcode; cr_field; Word.b0; l_field; reg1; reg2; opt_opcode; fin] in
  let reg1 = find_gpr (sprintf "R%d" reg1_num) in
  let reg2 = find_gpr (sprintf "R%d" reg2_num) in
  let init = Bil.[
      reg1 := int @@ Word.of_int64 reg1_value;
      reg2 := int @@ Word.of_int64 reg2_value;
    ] in
  let c = eval init bytes arch in
  assert_bool (sprintf "%s failed" name) @@ check c expected

let cmpw = cmp "cmpw" ~opt_opcode:0 0
let cmpd = cmp "cmpd" ~opt_opcode:0 1
let cmplw = cmp "cmplw" ~opt_opcode:32 0
let cmpld = cmp "cmpld" ~opt_opcode:32 1

let suite = "fixpoint compare" >::: [

    "cmpwi32: lt"            >:: cmpwi 42L 44L LT `ppc;
    "cmpwi32: gt"            >:: cmpwi 44L 42L GT `ppc;
    "cmpwi32: eq"            >:: cmpwi 42L 42L EQ `ppc;
    "cmpwi32: 32lt"          >:: cmpwi 0x0BCDEFAB_00000042L 0x44L LT `ppc;
    "cmpwi32: lt signed"     >:: cmpwi (-42L) (-41L) LT `ppc;
    "cmpwi32: gt signed"     >:: cmpwi 44L (-42L) GT `ppc;
    "cmpwi32: eq signed"     >:: cmpwi (-42L) (-42L) EQ `ppc;

    "cmpdi32: lt"            >:: cmpdi 42L 44L LT `ppc;
    "cmpdi32: gt"            >:: cmpdi 44L 42L GT `ppc;
    "cmpdi32: eq"            >:: cmpdi 42L 42L EQ `ppc;
    "cmpdi32: 64 operand, 1" >:: cmpdi 0x0BCDEFAB_00000042L 0x44L GT `ppc;
    "cmpdi32: 64 operand, 2" >:: cmpdi 0xFBCDEFAB_00000042L 0x44L LT `ppc;
    "cmpdi32: lt signed"     >:: cmpdi (-42L) (-41L) LT `ppc;
    "cmpdi32: gt signed"     >:: cmpdi 44L (-42L) GT `ppc;
    "cmpdi32: eq signed"     >:: cmpdi (-42L) (-42L) EQ `ppc;

    "cmpw32: lt"             >:: cmpw 42L 44L LT `ppc;
    "cmpw32: gt"             >:: cmpw 44L 42L GT `ppc;
    "cmpw32: eq"             >:: cmpw 42L 42L EQ `ppc;
    "cmpw32: 32lt"           >:: cmpw 0x0BCDEFAB_00000042L 0x44L LT `ppc;
    "cmpw32: lt signed"      >:: cmpw (-42L) (-41L) LT `ppc;
    "cmpw32: gt signed"      >:: cmpw 44L (-42L) GT `ppc;
    "cmpw32: eq signed"      >:: cmpw (-42L) (-42L) EQ `ppc;

    "cmpd32: lt"             >:: cmpd 42L 44L LT `ppc;
    "cmpd32: gt"             >:: cmpd 44L 42L GT `ppc;
    "cmpd32: eq"             >:: cmpd 42L 42L EQ `ppc;
    "cmpd32: 64 operand, 1"  >:: cmpd 0x0BCDEFAB_00000042L 0x44L GT `ppc;
    "cmpd32: 64 operand, 2"  >:: cmpd 0xFBCDEFAB_00000042L 0x44L LT `ppc;
    "cmpd32: lt signed"      >:: cmpd (-42L) (-41L) LT `ppc;
    "cmpd32: gt signed"      >:: cmpd 44L (-42L) GT `ppc;
    "cmpd32: eq signed"      >:: cmpd (-42L) (-42L) EQ `ppc;

    "cmplwi32: lt"            >:: cmplwi 42L 44L LT `ppc;
    "cmplwi32: gt"            >:: cmplwi 44L 42L GT `ppc;
    "cmplwi32: eq"            >:: cmplwi 42L 42L EQ `ppc;
    "cmplwi32: 32lt"          >:: cmplwi 0x0BCDEFAB_00000042L 0x44L LT `ppc;
    "cmplwi32: lt big"        >:: cmplwi 42L 0xFFFFFFFFL LT `ppc;
    "cmplwi32: gt big"        >:: cmplwi 0xFFFFFFFF_FFFFFFFFL 42L GT `ppc;

    "cmpldi32: lt"            >:: cmpldi 42L 44L LT `ppc;
    "cmpldi32: gt"            >:: cmpldi 44L 42L GT `ppc;
    "cmpldi32: eq"            >:: cmpldi 42L 42L EQ `ppc;
    "cmpldi32: 64 operand, 1" >:: cmpldi 0x0BCDEFAB_00000042L 0x44L GT `ppc;
    "cmpldi32: 64 operand, 2" >:: cmpldi 0xFBCDEFAB_00000042L 0x44L GT `ppc;
    "cmpldi32: lt big"        >:: cmpldi 42L 0xFFFFFFFFL LT `ppc;
    "cmpldi32: gt big"        >:: cmpldi 0xFFFFFFFF_FFFFFFFFL 42L GT `ppc;

    "cmplw32: lt"             >:: cmplw 42L 44L LT `ppc;
    "cmplw32: gt"             >:: cmplw 44L 42L GT `ppc;
    "cmplw32: eq"             >:: cmplw 42L 42L EQ `ppc;
    "cmplw32: 32lt"           >:: cmplw 0x0BCDEFAB_00000042L 0x44L LT `ppc;
    "cmplw32: lt big"         >:: cmplw 42L 0xFFFFFFFFL LT `ppc;
    "cmplw32: gt big"         >:: cmplw 0xFFFFFFFF_FFFFFFFFL 42L GT `ppc;

    "cmpld32: lt"             >:: cmpld 42L 44L LT `ppc;
    "cmpld32: gt"             >:: cmpld 44L 42L GT `ppc;
    "cmpld32: eq"             >:: cmpld 42L 42L EQ `ppc;
    "cmpld32: 64 operand, 1"  >:: cmpld 0x0BCDEFAB_00000042L 0x44L GT `ppc;
    "cmpld32: 64 operand, 2"  >:: cmpld 0xFBCDEFAB_00000042L 0x44L GT `ppc;
    "cmpld32: lt big"         >:: cmpld 42L 0xFFFFFFFFL LT `ppc;
    "cmpld32: gt big"         >:: cmpld 0xFFFFFFFF_FFFFFFFFL 42L GT `ppc;




    "cmpwi64: lt"             >:: cmpwi 42L 44L LT `ppc64;
    "cmpwi64: gt"             >:: cmpwi 44L 42L GT `ppc64;
    "cmpwi64: eq"             >:: cmpwi 42L 42L EQ `ppc64;
    "cmpwi64: 64lt"           >:: cmpwi 0x0BCDEFAB_00000042L 0x44L LT `ppc64;
    "cmpwi64: lt signed"      >:: cmpwi (-42L) (-41L) LT `ppc64;
    "cmpwi64: gt signed"      >:: cmpwi 44L (-42L) GT `ppc64;
    "cmpwi64: eq signed"      >:: cmpwi (-42L) (-42L) EQ `ppc64;

    "cmpdi64: lt"             >:: cmpdi 42L 44L LT `ppc64;
    "cmpdi64: gt"             >:: cmpdi 44L 42L GT `ppc64;
    "cmpdi64: eq"             >:: cmpdi 42L 42L EQ `ppc64;
    "cmpdi64: 64 operand, 1"  >:: cmpdi 0x0BCDEFAB_00000042L 0x44L GT `ppc64;
    "cmpdi64: 64 operand, 2"  >:: cmpdi 0xFBCDEFAB_00000042L 0x44L LT `ppc64;
    "cmpdi64: lt signed"      >:: cmpdi (-42L) (-41L) LT `ppc64;
    "cmpdi64: gt signed"      >:: cmpdi 44L (-42L) GT `ppc64;
    "cmpdi64: eq signed"      >:: cmpdi (-42L) (-42L) EQ `ppc64;

    "cmpw64: lt"              >:: cmpw 42L 44L LT `ppc64;
    "cmpw64: gt"              >:: cmpw 44L 42L GT `ppc64;
    "cmpw64: eq"              >:: cmpw 42L 42L EQ `ppc64;
    "cmpw64: 64lt"            >:: cmpw 0x0BCDEFAB_00000042L 0x44L LT `ppc64;
    "cmpw64: lt signed"       >:: cmpw (-42L) (-41L) LT `ppc64;
    "cmpw64: gt signed"       >:: cmpw 44L (-42L) GT `ppc64;
    "cmpw64: eq signed"       >:: cmpw (-42L) (-42L) EQ `ppc64;

    "cmpd64: lt"              >:: cmpd 42L 44L LT `ppc64;
    "cmpd64: gt"              >:: cmpd 44L 42L GT `ppc64;
    "cmpd64: eq"              >:: cmpd 42L 42L EQ `ppc64;
    "cmpd64: 64 operand, 1"   >:: cmpd 0x0BCDEFAB_00000042L 0x44L GT `ppc64;
    "cmpd64: 64 operand, 2"   >:: cmpd 0xFBCDEFAB_00000042L 0x44L LT `ppc64;
    "cmpd64: lt signed"       >:: cmpd (-42L) (-41L) LT `ppc64;
    "cmpd64: gt signed"       >:: cmpd 44L (-42L) GT `ppc64;
    "cmpd64: eq signed"       >:: cmpd (-42L) (-42L) EQ `ppc64;

    "cmplwi64: lt"            >:: cmplwi 42L 44L LT `ppc64;
    "cmplwi64: gt"            >:: cmplwi 44L 42L GT `ppc64;
    "cmplwi64: eq"            >:: cmplwi 42L 42L EQ `ppc64;
    "cmplwi64: 64lt"          >:: cmplwi 0x0BCDEFAB_00000042L 0x44L LT `ppc64;
    "cmplwi64: lt big"        >:: cmplwi 42L 0xFFFFFFFFL LT `ppc64;
    "cmplwi64: gt big"        >:: cmplwi 0xFFFFFFFF_FFFFFFFFL 42L GT `ppc64;

    "cmpldi64: lt"            >:: cmpldi 42L 44L LT `ppc64;
    "cmpldi64: gt"            >:: cmpldi 44L 42L GT `ppc64;
    "cmpldi64: eq"            >:: cmpldi 42L 42L EQ `ppc64;
    "cmpldi64: 64 operand, 1" >:: cmpldi 0x0BCDEFAB_00000042L 0x44L GT `ppc64;
    "cmpldi64: 64 operand, 2" >:: cmpldi 0xFBCDEFAB_00000042L 0x44L GT `ppc64;
    "cmpldi64: lt big"        >:: cmpldi 42L 0xFFFFFFFFL LT `ppc64;
    "cmpldi64: gt big"        >:: cmpldi 0xFFFFFFFF_FFFFFFFFL 42L GT `ppc64;

    "cmplw64: lt"             >:: cmplw 42L 44L LT `ppc64;
    "cmplw64: gt"             >:: cmplw 44L 42L GT `ppc64;
    "cmplw64: eq"             >:: cmplw 42L 42L EQ `ppc64;
    "cmplw64: 64lt"           >:: cmplw 0x0BCDEFAB_00000042L 0x44L LT `ppc64;
    "cmplw64: lt big"         >:: cmplw 42L 0xFFFFFFFFL LT `ppc64;
    "cmplw64: gt big"         >:: cmplw 0xFFFFFFFF_FFFFFFFFL 42L GT `ppc64;

    "cmpld64: lt"             >:: cmpld 42L 44L LT `ppc64;
    "cmpld64: gt"             >:: cmpld 44L 42L GT `ppc64;
    "cmpld64: eq"             >:: cmpld 42L 42L EQ `ppc64;
    "cmpld64: 64 operand, 1"  >:: cmpld 0x0BCDEFAB_00000042L 0x44L GT `ppc64;
    "cmpld64: 64 operand, 2"  >:: cmpld 0xFBCDEFAB_00000042L 0x44L GT `ppc64;
    "cmpld64: lt big"         >:: cmpld 42L 0xFFFFFFFFL LT `ppc64;
    "cmpld64: gt big"         >:: cmpld 0xFFFFFFFF_FFFFFFFFL 42L GT `ppc64;

  ]
