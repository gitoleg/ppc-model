open Core_kernel.Std
open Bap.Std
open OUnit2

open Ppc_model

module Dis = Disasm_expert.Basic

let create_dis arch =
  Dis.create ~backend:"llvm" (Arch.to_string arch) |>
  Or_error.ok_exn |>
  Dis.store_kinds |>
  Dis.store_asm

let create_memory arch s addr =
  let endian = Arch.endian arch in
  Memory.create endian addr @@
  Bigstring.of_string s |> function
  | Ok r -> r
  | Error e -> failwith "can't create memory"

let to_bil arch mem insn =
  let module T = (val (target_of_arch arch)) in
  T.lift mem insn

let get_insn arch bytes =
  let width = Arch.addr_size arch |> Size.in_bits in
  let addr = Addr.of_int ~width 0 in
  let mem = create_memory arch bytes addr in
  let dis = create_dis arch in
  match Dis.insn_of_mem dis mem with
  | Ok (mem, Some insn, _) -> mem, insn
  | _ -> failwith "disasm failed"

let get_var c var = match c#lookup var with
  | None -> None
  | Some r ->
    match Bil.Result.value r with
    | Bil.Imm word -> Some word
    | Bil.Bot | Bil.Mem _ -> None

let check_gpr init bytes var expected arch ctxt =
  let mem,insn = get_insn arch bytes in
  let bil = Or_error.ok_exn @@ to_bil arch mem insn in
  let c = Stmt.eval (init @ bil) (new Bili.context) in
  match get_var c var with
  | None -> assert_bool "var not found OR it's result not Imm" false
  | Some w -> assert_equal ~cmp:Word.equal w expected

let check_cntlz value zeros ctxt =
  let bytes = "\x7c\x63\x00\x34" in
  let r = Var.Set.find_exn
      Hardware.gpr ~f:(fun v -> Var.name v = "R3") in
  let init = Bil.[
      r := int (Word.of_int ~width:64 value);
    ] in
  let expected = Word.of_int ~width:64 zeros in
  check_gpr init bytes r expected `ppc ctxt;
  check_gpr init bytes r expected `ppc64 ctxt

let check_cnttz value zeros ctxt =
  let bytes = "\x7c\x63\x04\x34" in
  let r = Var.Set.find_exn
      Hardware.gpr ~f:(fun v -> Var.name v = "R3") in
  let init = Bil.[
      r := int (Word.of_int ~width:64 value);
    ] in
  let expected = Word.of_int ~width:64 zeros in
  check_gpr init bytes r expected `ppc ctxt;
  check_gpr init bytes r expected `ppc64 ctxt

let check_cmpb ~bytes_cnt x y expected ctxt =
  let bytes = "\x7c\x8a\x53\xf8" in
  let x = Word.of_int ~width:64 x in
  let y = Word.of_int ~width:64 y in
  let r10 = Var.Set.find_exn
      Hardware.gpr ~f:(fun v -> Var.name v = "R10") in
  let r4 = Var.Set.find_exn
      Hardware.gpr ~f:(fun v -> Var.name v = "R4") in
  let init = Bil.[
      r10 := int x;
      r4  := int y;
    ] in
  let head = Word.ones (64 - bytes_cnt * 8) in
  let expected = Word.of_int ~width:(bytes_cnt * 8) expected in
  let expected = Word.concat head expected in
  check_gpr init bytes r10 expected `ppc ctxt;
  check_gpr init bytes r10 expected `ppc64 ctxt

let suite = "result" >::: [
    "check cntlz: 32" >:: check_cntlz 0x0 32;
    "check cntlz: 5"  >:: check_cntlz 0x4000000 5;
    "check cntlz: 1"  >:: check_cntlz 0x40000000 1;
    "check cntlz: 0"  >:: check_cntlz 0x80000000 0;
    "check cnttz: 32" >:: check_cnttz 0x0 32;
    "check cnttz: 0"  >:: check_cnttz 0x1 0;
    "check cnttz: 1"  >:: check_cnttz 0x2 1;
    "check cnttz: 5"  >:: check_cnttz 0x20 5;
    "check cmpb"      >:: check_cmpb ~bytes_cnt:3 0x31_42_45 0x34_42_AD 0x00_FF_00;
    "check cmpb"      >:: check_cmpb ~bytes_cnt:3 0 0x34_42_AD 0x0;
    "check cmpb"      >:: check_cmpb ~bytes_cnt:3 0x34_42_AD 0x34_42_AD 0xFF_FF_FF;
  ]
