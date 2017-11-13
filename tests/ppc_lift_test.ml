open Core_kernel.Std
open Bap.Std
open OUnit2

open Ppc_model
open Ppc_rtl

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
  | Error e ->
    eprintf "something went wrong\n"; exit 1

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
  | _ ->  printf "disasm failed\n"; exit 1

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
  | Some w ->
    assert_equal ~cmp:Word.equal w expected

let check_cntlz ctxt =
  let bytes = "\x7c\x63\x00\x34" in
  let r = Var.Set.find_exn
      Hardware.gpr ~f:(fun v -> Var.name v = "R3") in
  let init = Bil.[
      r := int (Word.of_int ~width:64 0x4000020);
    ] in
  let expected = Word.of_int ~width:64 5 in
  check_gpr init bytes r expected `ppc ctxt;
  check_gpr init bytes r expected `ppc64 ctxt

let suite = "result" >::: [
    "check cntlz"     >:: check_cntlz;
  ]
