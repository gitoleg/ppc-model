open Core_kernel.Std
open Bap.Std
open OUnit2

open Powerpc_types
open Hardware
open Model
open Dsl
open Powerpc_tests_helpers

let typecheck bytes arch ctxt =
  let bil = get_bil arch bytes in
  assert_bool "typecheck failed" (Result.is_ok bil)

let check_pc name c addr =
  match c#pc with
  | Bil.Imm a ->
    assert_bool
      (sprintf "%s fail: addr is not equal to expected: %s <> %s"
         name
         (Addr.to_string a)
         (Addr.to_string addr))
      (Addr.equal a addr)
  | _ ->
    assert_bool (sprintf "%s fail: pc is not a word" name) false

let check_jmp_absence name c =
  assert_bool (sprintf "%s fail: jmp occured" name) (c#pc = Bil.Bot)

let addr_of_arch = function
  | `ppc -> Word.of_int64 ~width:32 0xABCD42AAL
  | `ppc64 -> Word.of_int64 0x0AAAAAAA_00000042L
  | _ -> failwith "PowerPC only"

let extract ~hi ~lo w = Word.extract_exn ~hi ~lo w

let b arch ctxt =
  let bits = Size.in_bits @@ Arch.addr_size arch in
  let bytes = "\x4b\xff\xfe\xf0" in
  let bytes'= Word.of_int64 ~width:bits 0x4bfffef0L in
  let imm =
    extract ~hi:25 ~lo:2 bytes' |>
    extract ~hi:bits ~lo:0 in
  let addr = addr_of_arch arch in
  let expected =
    extract ~hi:(bits - 1) ~lo:0 @@
    Word.(addr + (imm lsl Word.of_int ~width:bits 2)) in
  let c = eval ~addr [] bytes arch in
  check_pc "b" c expected

let ba arch ctxt =
  let bits = Size.in_bits @@ Arch.addr_size arch in
  let bytes = "\x4b\xff\xfe\xf2" in
  let bytes'= Word.of_int64 ~width:bits 0x4bfffef2L in
 let imm =
    extract ~hi:25 ~lo:2 bytes' |>
    extract ~hi:bits ~lo:0 in
  let addr = addr_of_arch arch in
  let expected =
    extract ~hi:(bits - 1) ~lo:0 @@
    Word.(imm lsl Word.of_int ~width:bits 2) in
  let c = eval ~addr [] bytes arch in
  check_pc "ba" c expected

let bl arch ctxt =
  let bits = Size.in_bits @@ Arch.addr_size arch in
  let bytes = "\x4b\xff\xfe\xf1" in
  let bytes'= Word.of_int64 ~width:bits 0x4bfffef1L in
  let imm =
    extract ~hi:25 ~lo:2 bytes' |>
    extract ~hi:bits ~lo:0 in
  let addr = addr_of_arch arch in
  let expected =
    Word.(addr + (imm lsl Word.of_int ~width:bits 2)) |>
    extract ~hi:(bits - 1) ~lo:0 in
  let c = eval ~addr [] bytes arch in
  check_pc "bl" c expected;
  let next = Word.(of_int ~width:bits 4 + addr) in
  let next = extract ~hi:(lr_bitwidth - 1) ~lo:0 next in
  assert_bool "bl failed" @@ is_equal_words next (lookup_var c lr)

let bla arch ctxt =
  let bits = Size.in_bits @@ Arch.addr_size arch in
  let bytes = "\x4b\xff\xfe\xf3" in
  let bytes'= Word.of_int64 ~width:bits 0x4bfffef3L in
  let imm =
    extract ~hi:25 ~lo:2 bytes' |>
    extract ~hi:bits ~lo:0 in
  let addr = addr_of_arch arch in
  let expected =
    extract ~hi:(bits - 1) ~lo:0 @@
    Word.(imm lsl Word.of_int ~width:bits 2) in
  let c = eval ~addr [] bytes arch in
  check_pc "bla" c expected;
  let next = Word.(of_int ~width:bits 4 + addr) in
  let next = extract ~hi:(lr_bitwidth - 1) ~lo:0 next in
  assert_bool "bla failed" @@ is_equal_words next (lookup_var c lr)

type bo = {
  bo_field    : int;
  ctr_before  : word;
  ctr_after   : word option;
  cond_reg0   : word;
  expect_jump : bool;
}

let make_bo ?ctr_before ?ctr_after ?cond_reg0 ?(expect_jump = false) bo_field =
  let ctr_before = match ctr_before with
    | None -> Word.zero ctr_bitwidth
    | Some x -> Word.of_int ~width:ctr_bitwidth x in
  let ctr_after =
    Option.map ~f:(fun x -> Word.of_int ~width:ctr_bitwidth x) ctr_after in
  let cond_reg0 = match cond_reg0 with
    | None -> Word.b0
    | Some x -> Word.of_bool (x = 1) in
  {bo_field; ctr_before; ctr_after; cond_reg0; expect_jump;}

let counter_unchanged = make_bo ~ctr_before:42 ~cond_reg0:1 ~ctr_after:42 0b00100
let counter_decremented = make_bo ~ctr_before:42 ~cond_reg0:1 ~ctr_after:41 0b00000
let jmp_anyway = make_bo ~expect_jump:true 0b10100

(** cond: bo3 = CR0 = 1; ctr: bo1 = 1 and ctr = 0 *)
let cond_ctr_ok_1 = make_bo ~ctr_before:1 ~cond_reg0:1 ~expect_jump:true 0b01010

(** cond: bo3 = CR0 = 0; ctr: bo1 = 0 and ctr <> 0 *)
let cond_ctr_ok_2 = make_bo ~ctr_before:42 ~cond_reg0:0 ~expect_jump:true 0b00000

(** cond: bo3 = CR0 = 1; ctr: bo1 = 0 and ctr <> 0 *)
let cond_ctr_ok_3 = make_bo ~ctr_before:42 ~cond_reg0:1 ~expect_jump:true 0b01000

(** cond: bo3 = CR0 = 0; ctr: bo1 = 1 and ctr = 0 *)
let cond_ctr_ok_4 = make_bo ~ctr_before:1 ~cond_reg0:0 ~expect_jump:true 0b00010

(** cond: bo3 = CR0 = 0; ctr: bo1 = 1 and ctr <> 0 *)
let cond_ok_ctr_not_1 = make_bo ~ctr_before:42 ~cond_reg0:0 0b00010

(** cond: bo3 = 1; CR0 = 0; ctr: bo1 = 1 and ctr <> 0 *)
let cond_not_ctr_ok_1 = make_bo ~ctr_before:42 ~cond_reg0:0 0b01010

(** cond: bo3 = CR0 = 0;  *)
let cond_ok = make_bo  ~cond_reg0:0 ~expect_jump:true 0b00010

(** cond: bo3 = 1; CR0 = 0;  *)
let cond_not = make_bo ~cond_reg0:0 0b01010

let bcx name fin arch case (ctxt : test_ctxt) =
  let imm = 42 in
  let aa_is_set = fin land 2 <> 0 in
  let lk_is_set = fin land 1 <> 0 in
  let bits = Size.in_bits @@ Arch.addr_size arch in
  let opcode = Word.of_int ~width:6 16 in
  let bo = Word.of_int ~width:5 case.bo_field in
  let bi = Word.of_int ~width:5 0 in
  let bd = Word.of_int ~width:14 imm in
  let fin = Word.of_int ~width:2 fin in
  let bytes = make_bytes [opcode; bo; bi; bd; fin] in
  let cr0 = bit cr 0 in
  let init = Bil.[
      cr0 := int case.cond_reg0;
      ctr := int case.ctr_before;
    ] in
  let addr = addr_of_arch arch in
  let c = eval ~addr init bytes arch in
  if case.expect_jump then
    let imm = Word.of_int ~width:bits (imm lsl 2) in
    let expected =
      if aa_is_set then imm
      else  Word.(addr + imm) in
    check_pc name c expected
  else check_jmp_absence name c;
  let () = match case.ctr_after with
    | None -> ()
    | Some x ->
      assert_bool (sprintf "%s failed" name) @@ is_equal_words x (lookup_var c ctr) in
  if lk_is_set then
    let next = Word.(of_int ~width:bits 4 + addr) in
    let next = extract ~hi:(lr_bitwidth - 1) ~lo:0 next in
    assert_bool (sprintf "%s failed to check LR value" name) @@ is_equal_words next (lookup_var c lr)

let bc = bcx "bc" 0
let bca = bcx "bca" 2
let bcl = bcx "bcl" 1
let bcla = bcx "bcla" 3

let bdnz ~jmp arch  (ctxt : test_ctxt) =
  let imm = 42 in
  let bits = Size.in_bits @@ Arch.addr_size arch in
  let opcode = Word.of_int ~width:6 16 in
  let bo = Word.of_int ~width:5 0b10000 in
  let bi = Word.of_int ~width:5 0 in
  let bd = Word.of_int ~width:14 imm in
  let fin = Word.of_int ~width:2 0 in
  let bytes = make_bytes [opcode; bo; bi; bd; fin] in
  let ctr_val =
    if jmp then Word.of_int ~width:ctr_bitwidth 42
    else Word.one ctr_bitwidth in
  let init =  Bil.[ctr := int ctr_val] in
  let addr = addr_of_arch arch in
  let c = eval ~addr init bytes arch in
  let imm = Word.of_int ~width:bits (imm lsl 2) in
  if jmp then
    let expected = Word.(addr + imm) in
    check_pc "bdnz" c expected
  else
    check_jmp_absence "bdzf" c;
  let x = Word.pred ctr_val in
  assert_bool "bdnz cnt check failed" @@ is_equal_words x (lookup_var c ctr)

let bcxrx name opt_opcode fin reg arch case (ctxt : test_ctxt) =
  let lk_is_set = fin land 1 <> 0 in
  let bits = Size.in_bits @@ Arch.addr_size arch in
  let opcode = Word.of_int ~width:6 19 in
  let bo = Word.of_int ~width:5 case.bo_field in
  let bi = Word.of_int ~width:5 31 in
  let no_matter = Word.zero 3 in
  let bh = Word.zero 2 in
  let opt_opcode = Word.of_int ~width:10 opt_opcode in
  let fin = Word.of_int ~width:1 fin in
  let insn = [opcode; bo; bi; no_matter; bh; opt_opcode; fin] in
  let bytes = make_bytes insn in
  let cr0 = bit cr 31 in
  let addr = addr_of_arch arch in
  let init = Bil.[
      cr0 := int case.cond_reg0;
      ctr := int case.ctr_before;
    ] in
  let init = init @ Bil.[reg := cast unsigned 64 (int addr)] in
  let c = eval ~addr init bytes arch in
  if case.expect_jump then
    let expected_addr =
      let x = Word.of_int64 2L in
      let addr = extract ~hi:(bits-1) ~lo:0 addr in
      Word.(addr lsl x) in
    check_pc name c expected_addr
  else check_jmp_absence name c;
  let () = match case.ctr_after with
    | None -> ()
    | Some x ->
      assert_bool (sprintf "%s ctr check failed" name) @@ is_equal_words x (lookup_var c ctr) in
  if lk_is_set then
    let next = Word.(of_int ~width:bits 4 + addr) in
    let next = extract ~hi:(lr_bitwidth - 1) ~lo:0 next in
    assert_bool (sprintf "%s failed to check LR value" name) @@ is_equal_words next (lookup_var c lr)

let bclr = bcxrx "bclr" 16 0 lr
let bclrl = bcxrx "bclrl" 16 1 lr
let bcctr = bcxrx "bcctr" 528 0 ctr
let bcctrl = bcxrx "bcctrl" 528 1 ctr
let bctar = bcxrx "bctar" 560 0 tar
let bctarl = bcxrx "bctarl" 560 1 tar

let bdnzlr ~jmp arch  (ctxt : test_ctxt) =
  let bits = Size.in_bits @@ Arch.addr_size arch in
  let opcode = Word.of_int ~width:6 19 in
  let bo = Word.of_int ~width:5 0b10000 in
  let bi = Word.of_int ~width:5 0 in
  let no_matter = Word.zero 3 in
  let bh = Word.zero 2 in
  let opt_opcode = Word.of_int ~width:10 16 in
  let fin = Word.of_int ~width:1 0 in
  let bytes = make_bytes [opcode; bo; bi; no_matter; bh; opt_opcode; fin] in
  let ctr_val =
    if jmp then Word.of_int ~width:ctr_bitwidth 42
    else Word.one ctr_bitwidth in
  let addr = addr_of_arch arch in
  let init =  Bil.[
      ctr := int ctr_val;
      lr := cast unsigned 64 (int addr);
    ] in
  let c = eval ~addr init bytes arch in
  if jmp then
    let expected = Word.(addr lsl Word.of_int ~width:bits 2) in
    check_pc "bdnzlr" c expected
  else
    check_jmp_absence "bdzf" c;
  let x = Word.pred ctr_val in
  assert_bool "bdnzlr cnt check failed" @@ is_equal_words x (lookup_var c ctr)

let suite  = "branch" >::: [
    "b32"                         >:: b `ppc;
    "ba32"                        >:: ba `ppc;
    "bl32"                        >:: bl `ppc;
    "bla32"                       >:: bla `ppc;

    "bc32 counter_unchanged "     >:: bc `ppc counter_unchanged;
    "bc32 counter_decremented"    >:: bc `ppc counter_decremented;
    "bc32 jmp_anyway"             >:: bc `ppc jmp_anyway;
    "bc32 cond_ctr_ok_1"          >:: bc `ppc cond_ctr_ok_1;
    "bc32 cond_ctr_ok_2"          >:: bc `ppc cond_ctr_ok_2;
    "bc32 cond_ctr_ok_3"          >:: bc `ppc cond_ctr_ok_3;
    "bc32 cond_ctr_ok_4"          >:: bc `ppc cond_ctr_ok_4;
    "bc32 cond_ok_ctr_not_1"      >:: bc `ppc cond_ok_ctr_not_1;
    "bc32 cond_not_ctr_ok_1"      >:: bc `ppc cond_not_ctr_ok_1;

    "bca32 counter_unchanged "    >:: bca `ppc counter_unchanged;
    "bca32 counter_decremented"   >:: bca `ppc counter_decremented;
    "bca32 jmp_anyway"            >:: bca `ppc jmp_anyway;
    "bca32 cond_ctr_ok_1"         >:: bca `ppc cond_ctr_ok_1;
    "bca32 cond_ctr_ok_2"         >:: bca `ppc cond_ctr_ok_2;
    "bca32 cond_ctr_ok_3"         >:: bca `ppc cond_ctr_ok_3;
    "bca32 cond_ctr_ok_4"         >:: bca `ppc cond_ctr_ok_4;
    "bca32 cond_ok_ctr_not_1"     >:: bca `ppc cond_ok_ctr_not_1;
    "bca32 cond_not_ctr_ok_1"     >:: bca `ppc cond_not_ctr_ok_1;

    "bcl32 counter_unchanged "    >:: bcl `ppc counter_unchanged;
    "bcl32 counter_decremented"   >:: bcl `ppc counter_decremented;
    "bcl32 jmp_anyway"            >:: bcl `ppc jmp_anyway;
    "bcl32 cond_ctr_ok_1"         >:: bcl `ppc cond_ctr_ok_1;
    "bcl32 cond_ctr_ok_2"         >:: bcl `ppc cond_ctr_ok_2;
    "bcl32 cond_ctr_ok_3"         >:: bcl `ppc cond_ctr_ok_3;
    "bcl32 cond_ctr_ok_4"         >:: bcl `ppc cond_ctr_ok_4;
    "bcl32 cond_ok_ctr_not_1"     >:: bcl `ppc cond_ok_ctr_not_1;
    "bcl32 cond_not_ctr_ok_1"     >:: bcl `ppc cond_not_ctr_ok_1;

    "bcla32 counter_unchanged "   >:: bcla `ppc counter_unchanged;
    "bcla32 counter_decremented"  >:: bcla `ppc counter_decremented;
    "bcla32 jmp_anyway"           >:: bcla `ppc jmp_anyway;
    "bcla32 cond_ctr_ok_1"        >:: bcla `ppc cond_ctr_ok_1;
    "bcla32 cond_ctr_ok_2"        >:: bcla `ppc cond_ctr_ok_2;
    "bcla32 cond_ctr_ok_3"        >:: bcla `ppc cond_ctr_ok_3;
    "bcla32 cond_ctr_ok_4"        >:: bcla `ppc cond_ctr_ok_4;
    "bcla32 cond_ok_ctr_not_1"    >:: bcla `ppc cond_ok_ctr_not_1;
    "bcla32 cond_not_ctr_ok_1"    >:: bcla `ppc cond_not_ctr_ok_1;

    "bclr32 counter_unchanged "   >:: bclr `ppc counter_unchanged;
    "bclr32 counter_decremented"  >:: bclr `ppc counter_decremented;
    "bclr32 jmp_anyway"           >:: bclr `ppc jmp_anyway;
    "bclr32 cond_ctr_ok_1"        >:: bclr `ppc cond_ctr_ok_1;
    "bclr32 cond_ctr_ok_2"        >:: bclr `ppc cond_ctr_ok_2;
    "bclr32 cond_ctr_ok_3"        >:: bclr `ppc cond_ctr_ok_3;
    "bclr32 cond_ctr_ok_4"        >:: bclr `ppc cond_ctr_ok_4;
    "bclr32 cond_ok_ctr_not_1"    >:: bclr `ppc cond_ok_ctr_not_1;
    "bclr32 cond_not_ctr_ok_1"    >:: bclr `ppc cond_not_ctr_ok_1;

    "bclrl32 counter_unchanged "  >:: bclrl `ppc counter_unchanged;
    "bclrl32 counter_decremented" >:: bclrl `ppc counter_decremented;
    "bclrl32 jmp_anyway"          >:: bclrl `ppc jmp_anyway;
    "bclrl32 cond_ctr_ok_1"       >:: bclrl `ppc cond_ctr_ok_1;
    "bclrl32 cond_ctr_ok_2"       >:: bclrl `ppc cond_ctr_ok_2;
    "bclrl32 cond_ctr_ok_3"       >:: bclrl `ppc cond_ctr_ok_3;
    "bclrl32 cond_ctr_ok_4"       >:: bclrl `ppc cond_ctr_ok_4;
    "bclrl32 cond_ok_ctr_not_1"   >:: bclrl `ppc cond_ok_ctr_not_1;
    "bclrl32 cond_not_ctr_ok_1"   >:: bclrl `ppc cond_not_ctr_ok_1;

    "bdnz32 jmp"                  >:: bdnz ~jmp:true `ppc;
    "bdnz32 no jmp"               >:: bdnz ~jmp:false `ppc;
    "bdnzlr32 jmp"                >:: bdnzlr ~jmp:true `ppc;
    "bdnzlr32 no jmp"             >:: bdnzlr ~jmp:false `ppc;

    "bcctr32 jmp_anyway"          >:: bcctr `ppc jmp_anyway;
    "bcctr32 cond_ok"             >:: bcctr `ppc cond_ok;
    "bcctr32 cond_not"            >:: bcctr `ppc cond_not;

    "bcctrl32 jmp_anyway"         >:: bcctrl `ppc jmp_anyway;
    "bcctrl32 cond_ok"            >:: bcctrl `ppc cond_ok;
    "bcctrl32 cond_not"           >:: bcctrl `ppc cond_not;

    "b64"                         >:: b `ppc64;
    "ba64"                        >:: ba `ppc64;
    "bl64"                        >:: bl `ppc64;
    "bla64"                       >:: bla `ppc64;

    "bc64 counter_unchanged "     >:: bc `ppc64 counter_unchanged;
    "bc64 counter_decremented"    >:: bc `ppc64 counter_decremented;
    "bc64 jmp_anyway"             >:: bc `ppc64 jmp_anyway;
    "bc64 cond_ctr_ok_1"          >:: bc `ppc64 cond_ctr_ok_1;
    "bc64 cond_ctr_ok_2"          >:: bc `ppc64 cond_ctr_ok_2;
    "bc64 cond_ctr_ok_3"          >:: bc `ppc64 cond_ctr_ok_3;
    "bc64 cond_ctr_ok_4"          >:: bc `ppc64 cond_ctr_ok_4;
    "bc64 cond_ok_ctr_not_1"      >:: bc `ppc64 cond_ok_ctr_not_1;
    "bc64 cond_not_ctr_ok_1"      >:: bc `ppc64 cond_not_ctr_ok_1;

    "bca64 counter_unchanged "    >:: bca `ppc64 counter_unchanged;
    "bca64 counter_decremented"   >:: bca `ppc64 counter_decremented;
    "bca64 jmp_anyway"            >:: bca `ppc64 jmp_anyway;
    "bca64 cond_ctr_ok_1"         >:: bca `ppc64 cond_ctr_ok_1;
    "bca64 cond_ctr_ok_2"         >:: bca `ppc64 cond_ctr_ok_2;
    "bca64 cond_ctr_ok_3"         >:: bca `ppc64 cond_ctr_ok_3;
    "bca64 cond_ctr_ok_4"         >:: bca `ppc64 cond_ctr_ok_4;
    "bca64 cond_ok_ctr_not_1"     >:: bca `ppc64 cond_ok_ctr_not_1;
    "bca64 cond_not_ctr_ok_1"     >:: bca `ppc64 cond_not_ctr_ok_1;

    "bcl64 counter_unchanged "    >:: bcl `ppc64 counter_unchanged;
    "bcl64 counter_decremented"   >:: bcl `ppc64 counter_decremented;
    "bcl64 jmp_anyway"            >:: bcl `ppc64 jmp_anyway;
    "bcl64 cond_ctr_ok_1"         >:: bcl `ppc64 cond_ctr_ok_1;
    "bcl64 cond_ctr_ok_2"         >:: bcl `ppc64 cond_ctr_ok_2;
    "bcl64 cond_ctr_ok_3"         >:: bcl `ppc64 cond_ctr_ok_3;
    "bcl64 cond_ctr_ok_4"         >:: bcl `ppc64 cond_ctr_ok_4;
    "bcl64 cond_ok_ctr_not_1"     >:: bcl `ppc64 cond_ok_ctr_not_1;
    "bcl64 cond_not_ctr_ok_1"     >:: bcl `ppc64 cond_not_ctr_ok_1;

    "bcla64 counter_unchanged "   >:: bcla `ppc64 counter_unchanged;
    "bcla64 counter_decremented"  >:: bcla `ppc64 counter_decremented;
    "bcla64 jmp_anyway"           >:: bcla `ppc64 jmp_anyway;
    "bcla64 cond_ctr_ok_1"        >:: bcla `ppc64 cond_ctr_ok_1;
    "bcla64 cond_ctr_ok_2"        >:: bcla `ppc64 cond_ctr_ok_2;
    "bcla64 cond_ctr_ok_3"        >:: bcla `ppc64 cond_ctr_ok_3;
    "bcla64 cond_ctr_ok_4"        >:: bcla `ppc64 cond_ctr_ok_4;
    "bcla64 cond_ok_ctr_not_1"    >:: bcla `ppc64 cond_ok_ctr_not_1;
    "bcla64 cond_not_ctr_ok_1"    >:: bcla `ppc64 cond_not_ctr_ok_1;

    "bclr64 counter_unchanged "   >:: bclr `ppc64 counter_unchanged;
    "bclr64 counter_decremented"  >:: bclr `ppc64 counter_decremented;
    "bclr64 jmp_anyway"           >:: bclr `ppc64 jmp_anyway;
    "bclr64 cond_ctr_ok_1"        >:: bclr `ppc64 cond_ctr_ok_1;
    "bclr64 cond_ctr_ok_2"        >:: bclr `ppc64 cond_ctr_ok_2;
    "bclr64 cond_ctr_ok_3"        >:: bclr `ppc64 cond_ctr_ok_3;
    "bclr64 cond_ctr_ok_4"        >:: bclr `ppc64 cond_ctr_ok_4;
    "bclr64 cond_ok_ctr_not_1"    >:: bclr `ppc64 cond_ok_ctr_not_1;
    "bclr64 cond_not_ctr_ok_1"    >:: bclr `ppc64 cond_not_ctr_ok_1;

    "bclrl64 counter_unchanged "  >:: bclrl `ppc64 counter_unchanged;
    "bclrl64 counter_decremented" >:: bclrl `ppc64 counter_decremented;
    "bclrl64 jmp_anyway"          >:: bclrl `ppc64 jmp_anyway;
    "bclrl64 cond_ctr_ok_1"       >:: bclrl `ppc64 cond_ctr_ok_1;
    "bclrl64 cond_ctr_ok_2"       >:: bclrl `ppc64 cond_ctr_ok_2;
    "bclrl64 cond_ctr_ok_3"       >:: bclrl `ppc64 cond_ctr_ok_3;
    "bclrl64 cond_ctr_ok_4"       >:: bclrl `ppc64 cond_ctr_ok_4;
    "bclrl64 cond_ok_ctr_not_1"   >:: bclrl `ppc64 cond_ok_ctr_not_1;
    "bclrl64 cond_not_ctr_ok_1"   >:: bclrl `ppc64 cond_not_ctr_ok_1;

    "bdnz64 jmp"                  >:: bdnz ~jmp:true `ppc64;
    "bdnz64 no jmp"               >:: bdnz ~jmp:false `ppc64;
    "bdnzlr64 jmp"                >:: bdnzlr ~jmp:true `ppc64;
    "bdnzlr64 no jmp"             >:: bdnzlr ~jmp:false `ppc64;

    "bcctr64 jmp_anyway"          >:: bcctr `ppc64 jmp_anyway;
    "bcctr64 cond_ok"             >:: bcctr `ppc64 cond_ok;
    "bcctr64 cond_not"            >:: bcctr `ppc64 cond_not;

    "bcctrl64 jmp_anyway"         >:: bcctrl `ppc64 jmp_anyway;
    "bcctrl64 cond_ok"            >:: bcctrl `ppc64 cond_ok;
    "bcctrl64 cond_not"           >:: bcctrl `ppc64 cond_not;

  ]
