open Core_kernel.Std
open Bap.Std
open OUnit2

open Ppc_model.Hardware
open Ppc_rtl
open Ppc_tests_helpers

let typecheck bytes arch ctxt =
  let bil = get_bil arch bytes in
  assert_bool "typecheck failed" (Result.is_ok bil)

let is_equal_words w = function
  | None -> false
  | Some w' -> Word.equal w w'

let check_pc name c addr =
  match c#pc with
  | Bil.Imm a ->
    assert_bool (sprintf "%s fail: addr is not equal to expected" name)
      (Addr.equal a addr)
  | _ ->
    assert_bool (sprintf "%s fail: pc is not a word" name) false

let check_jmp_absence name c =
  assert_bool (sprintf "%s fail: jmp occured" name) (c#pc = Bil.Bot)

let addr_of_arch = function
  | `ppc -> Word.of_int64 ~width:32 0xABCD42AAL
  | `ppc64 -> Word.of_int64 0x0AAAAAAA_00000042L
  | _ -> failwith "PowerPC only"

let b arch ctxt =
  let bits = Size.in_bits @@ Arch.addr_size arch in
  let bytes = "\x4b\xff\xfe\xf0" in
  let bytes'= Word.of_int64 ~width:bits 0x4bfffef0L in
  let imm = Or_error.(ok_exn @@
              (Word.extract ~hi:25 ~lo:2 bytes' >>= fun w ->
               Word.extract ~hi:bits ~lo:0 w)) in
  let addr = addr_of_arch arch in
  let expected =
    Or_error.ok_exn @@
    Word.extract ~hi:(bits - 1) ~lo:0 @@
    Word.(addr + (imm lsl Word.of_int ~width:bits 2)) in
  let c = eval ~addr [] bytes arch in
  check_pc "b" c expected

let ba arch ctxt =
  let bits = Size.in_bits @@ Arch.addr_size arch in
  let bytes = "\x4b\xff\xfe\xf2" in
  let bytes'= Word.of_int64 ~width:bits 0x4bfffef2L in
  let imm = Or_error.(ok_exn @@
                      (Word.extract ~hi:25 ~lo:2 bytes' >>= fun w ->
                       Word.extract ~hi:bits ~lo:0 w)) in
  let addr = addr_of_arch arch in
  let expected =
    Or_error.ok_exn @@
    Word.extract ~hi:(bits - 1) ~lo:0 @@
    Word.(imm lsl Word.of_int ~width:bits 2) in
  let c = eval ~addr [] bytes arch in
  check_pc "ba" c expected

let bl arch ctxt =
  let bits = Size.in_bits @@ Arch.addr_size arch in
  let bytes = "\x4b\xff\xfe\xf1" in
  let bytes'= Word.of_int64 ~width:bits 0x4bfffef1L in
  let imm = Or_error.(ok_exn @@
              (Word.extract ~hi:25 ~lo:2 bytes' >>= fun w ->
               Word.extract ~hi:bits ~lo:0 w)) in
  let addr = addr_of_arch arch in
  let expected =
    Or_error.ok_exn @@
    Word.extract ~hi:(bits - 1) ~lo:0 @@
    Word.(addr + (imm lsl Word.of_int ~width:bits 2)) in
  let c = eval ~addr [] bytes arch in
  check_pc "bl" c expected;
  let next = Word.(of_int ~width:bits 4 + addr) in
  let next = Or_error.ok_exn @@
    Word.extract ~hi:(lr_bitwidth - 1) ~lo:0 next in
  assert_bool "bl failed" @@ is_equal_words next (lookup_var c lr)

let bla arch ctxt =
  let bits = Size.in_bits @@ Arch.addr_size arch in
  let bytes = "\x4b\xff\xfe\xf3" in
  let bytes'= Word.of_int64 ~width:bits 0x4bfffef3L in
  let imm = Or_error.(ok_exn @@
                      (Word.extract ~hi:25 ~lo:2 bytes' >>= fun w ->
                       Word.extract ~hi:bits ~lo:0 w)) in
  let addr = addr_of_arch arch in
  let expected =
    Or_error.ok_exn @@
    Word.extract ~hi:(bits - 1) ~lo:0 @@
    Word.(imm lsl Word.of_int ~width:bits 2) in
  let c = eval ~addr [] bytes arch in
  check_pc "bla" c expected;
  let next = Word.(of_int ~width:bits 4 + addr) in
  let next = Or_error.ok_exn @@
    Word.extract ~hi:(lr_bitwidth - 1) ~lo:0 next in
  assert_bool "bla failed" @@ is_equal_words next (lookup_var c lr)

type bo = {
  bo_field    : int;
  ctr_before  : word option;
  ctr_after   : word option;
  cond_reg0   : word option;
  expect_jump : bool;
}

let make_bo ?ctr_before ?ctr_after ?cond_reg0 ?(expect_jump = false) bo_field =
  let ctr_before =
    Option.map ~f:(fun x -> Word.of_int ~width:ctr_bitwidth x) ctr_before in
  let ctr_after =
    Option.map ~f:(fun x -> Word.of_int ~width:ctr_bitwidth x) ctr_after in
  let cond_reg0 =
    Option.map ~f:(fun x -> Word.of_bool (x = 1)) cond_reg0 in
  {bo_field; ctr_before; ctr_after; cond_reg0; expect_jump;}

let counter_unchanged = make_bo ~ctr_before:42 ~ctr_after:42 0b00100
let counter_decremented = make_bo ~ctr_before:42 ~ctr_after:41 0b00000
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

let concat_words ws = match ws with
  | [] -> failwith "words list is empty!"
  | w :: ws ->
    List.fold ~init:w ~f:(fun ws w -> Word.concat ws w) ws

let make_bytes ws =
  let bytes = concat_words ws in
  let bytes = Seq.to_list @@ Word.enum_chars bytes BigEndian in
  String.of_char_list bytes

let bcx name fin arch case (ctxt : test_ctxt) =
  let imm = 42 in
  let aa_is_set = fin land 2 <> 0 in
  let lk_is_set = fin land 1 <> 0 in
  let bits = Size.in_bits @@ Arch.addr_size arch in
  let opcode = Word.of_int ~width:6 16 in
  let bo = Word.of_int ~width:5 case.bo_field in
  let bi = Word.of_int ~width:5 31 in
  let bd = Word.of_int ~width:14 imm in
  let fin = Word.of_int ~width:2 fin in
  let bytes = make_bytes [opcode; bo; bi; bd; fin] in
  let cr0 = condition_register_bit 0 in
  let init_cr = match case.cond_reg0 with
    | Some w -> Some Bil.(cr0 := int w)
    | None -> None in
  let init_ctr = match case.ctr_before with
    | Some x -> Some Bil.(ctr := int x)
    | None -> None in
  let init = List.filter_map ~f:ident [init_cr; init_ctr] in
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
    let next = Or_error.ok_exn @@
      Word.extract ~hi:(lr_bitwidth - 1) ~lo:0 next in
    assert_bool (sprintf "%s failed" name) @@ is_equal_words next (lookup_var c lr)

let bc = bcx "bc" 0
let bca = bcx "bca" 2
let bcl = bcx "bcl" 1
let bcla = bcx "bcla" 3

let suite = "branch" >::: [
    "b32"                       >:: b `ppc;
    "ba32"                      >:: ba `ppc;
    "bl32"                      >:: bl `ppc;
    "bla32"                     >:: bla `ppc;

    "bc32 counter_unchanged "   >:: bc `ppc counter_unchanged;
    "bc32 counter_decremented"  >:: bc `ppc counter_decremented;
    "bc32 jmp_anyway"           >:: bc `ppc jmp_anyway;
    "bc32 cond_ctr_ok_1"        >:: bc `ppc cond_ctr_ok_1;
    "bc32 cond_ctr_ok_2"        >:: bc `ppc cond_ctr_ok_2;
    "bc32 cond_ctr_ok_3"        >:: bc `ppc cond_ctr_ok_3;
    "bc32 cond_ctr_ok_4"        >:: bc `ppc cond_ctr_ok_4;
    "bc32 cond_ok_ctr_not_1"    >:: bc `ppc cond_ok_ctr_not_1;
    "bc32 cond_not_ctr_ok_1"    >:: bc `ppc cond_not_ctr_ok_1;

    "bca32 counter_unchanged "   >:: bca `ppc counter_unchanged;
    "bca32 counter_decremented"  >:: bca `ppc counter_decremented;
    "bca32 jmp_anyway"           >:: bca `ppc jmp_anyway;
    "bca32 cond_ctr_ok_1"        >:: bca `ppc cond_ctr_ok_1;
    "bca32 cond_ctr_ok_2"        >:: bca `ppc cond_ctr_ok_2;
    "bca32 cond_ctr_ok_3"        >:: bca `ppc cond_ctr_ok_3;
    "bca32 cond_ctr_ok_4"        >:: bca `ppc cond_ctr_ok_4;
    "bca32 cond_ok_ctr_not_1"    >:: bca `ppc cond_ok_ctr_not_1;
    "bca32 cond_not_ctr_ok_1"    >:: bca `ppc cond_not_ctr_ok_1;

    "bcl32 counter_unchanged "   >:: bcl `ppc counter_unchanged;
    "bcl32 counter_decremented"  >:: bcl `ppc counter_decremented;
    "bcl32 jmp_anyway"           >:: bcl `ppc jmp_anyway;
    "bcl32 cond_ctr_ok_1"        >:: bcl `ppc cond_ctr_ok_1;
    "bcl32 cond_ctr_ok_2"        >:: bcl `ppc cond_ctr_ok_2;
    "bcl32 cond_ctr_ok_3"        >:: bcl `ppc cond_ctr_ok_3;
    "bcl32 cond_ctr_ok_4"        >:: bcl `ppc cond_ctr_ok_4;
    "bcl32 cond_ok_ctr_not_1"    >:: bcl `ppc cond_ok_ctr_not_1;
    "bcl32 cond_not_ctr_ok_1"    >:: bcl `ppc cond_not_ctr_ok_1;

    "bcla32 counter_unchanged "   >:: bcla `ppc counter_unchanged;
    "bcla32 counter_decremented"  >:: bcla `ppc counter_decremented;
    "bcla32 jmp_anyway"           >:: bcla `ppc jmp_anyway;
    "bcla32 cond_ctr_ok_1"        >:: bcla `ppc cond_ctr_ok_1;
    "bcla32 cond_ctr_ok_2"        >:: bcla `ppc cond_ctr_ok_2;
    "bcla32 cond_ctr_ok_3"        >:: bcla `ppc cond_ctr_ok_3;
    "bcla32 cond_ctr_ok_4"        >:: bcla `ppc cond_ctr_ok_4;
    "bcla32 cond_ok_ctr_not_1"    >:: bcla `ppc cond_ok_ctr_not_1;
    "bcla32 cond_not_ctr_ok_1"    >:: bcla `ppc cond_not_ctr_ok_1;


    "b64"                       >:: b `ppc64;
    "ba64"                      >:: ba `ppc64;
    "bl64"                      >:: bl `ppc64;
    "bla64"                     >:: bla `ppc64;

    "bc64 counter_unchanged "   >:: bc `ppc64 counter_unchanged;
    "bc64 counter_decremented"  >:: bc `ppc64 counter_decremented;
    "bc64 jmp_anyway"           >:: bc `ppc64 jmp_anyway;
    "bc64 cond_ctr_ok_1"        >:: bc `ppc64 cond_ctr_ok_1;
    "bc64 cond_ctr_ok_2"        >:: bc `ppc64 cond_ctr_ok_2;
    "bc64 cond_ctr_ok_3"        >:: bc `ppc64 cond_ctr_ok_3;
    "bc64 cond_ctr_ok_4"        >:: bc `ppc64 cond_ctr_ok_4;
    "bc64 cond_ok_ctr_not_1"    >:: bc `ppc64 cond_ok_ctr_not_1;
    "bc64 cond_not_ctr_ok_1"    >:: bc `ppc64 cond_not_ctr_ok_1;

    "bca64 counter_unchanged "   >:: bca `ppc64 counter_unchanged;
    "bca64 counter_decremented"  >:: bca `ppc64 counter_decremented;
    "bca64 jmp_anyway"           >:: bca `ppc64 jmp_anyway;
    "bca64 cond_ctr_ok_1"        >:: bca `ppc64 cond_ctr_ok_1;
    "bca64 cond_ctr_ok_2"        >:: bca `ppc64 cond_ctr_ok_2;
    "bca64 cond_ctr_ok_3"        >:: bca `ppc64 cond_ctr_ok_3;
    "bca64 cond_ctr_ok_4"        >:: bca `ppc64 cond_ctr_ok_4;
    "bca64 cond_ok_ctr_not_1"    >:: bca `ppc64 cond_ok_ctr_not_1;
    "bca64 cond_not_ctr_ok_1"    >:: bca `ppc64 cond_not_ctr_ok_1;

    "bcl64 counter_unchanged "   >:: bcl `ppc64 counter_unchanged;
    "bcl64 counter_decremented"  >:: bcl `ppc64 counter_decremented;
    "bcl64 jmp_anyway"           >:: bcl `ppc64 jmp_anyway;
    "bcl64 cond_ctr_ok_1"        >:: bcl `ppc64 cond_ctr_ok_1;
    "bcl64 cond_ctr_ok_2"        >:: bcl `ppc64 cond_ctr_ok_2;
    "bcl64 cond_ctr_ok_3"        >:: bcl `ppc64 cond_ctr_ok_3;
    "bcl64 cond_ctr_ok_4"        >:: bcl `ppc64 cond_ctr_ok_4;
    "bcl64 cond_ok_ctr_not_1"    >:: bcl `ppc64 cond_ok_ctr_not_1;
    "bcl64 cond_not_ctr_ok_1"    >:: bcl `ppc64 cond_not_ctr_ok_1;

    "bcla64 counter_unchanged "   >:: bcla `ppc64 counter_unchanged;
    "bcla64 counter_decremented"  >:: bcla `ppc64 counter_decremented;
    "bcla64 jmp_anyway"           >:: bcla `ppc64 jmp_anyway;
    "bcla64 cond_ctr_ok_1"        >:: bcla `ppc64 cond_ctr_ok_1;
    "bcla64 cond_ctr_ok_2"        >:: bcla `ppc64 cond_ctr_ok_2;
    "bcla64 cond_ctr_ok_3"        >:: bcla `ppc64 cond_ctr_ok_3;
    "bcla64 cond_ctr_ok_4"        >:: bcla `ppc64 cond_ctr_ok_4;
    "bcla64 cond_ok_ctr_not_1"    >:: bcla `ppc64 cond_ok_ctr_not_1;
    "bcla64 cond_not_ctr_ok_1"    >:: bcla `ppc64 cond_not_ctr_ok_1;


  ]
