open Core_kernel.Std
open Bap.Std
open Bap_plugins.Std

module Lifter = Ppc_lifter
module Dis = Disasm_expert.Basic

open Ppc_model
open Ppc_rtl

let () =
  match Plugins.load () |> Result.all with
  | Ok plugins -> ()
  | Error (path, er) ->
    Printf.eprintf "failed to load plugin from %s: %s"
      path (Error.to_string_hum er)

let bytes = [
  "\x89\x3c\x00\x14", "lbz";
  "\x89\x20\x00\x14", "lbz";
  "\x83\xeb\xff\xfc", "lwz";
  "\x7d\x3d\x50\xae", "lbzx";
  "\x7d\x3d\x50\x2e", "lwzx";
  "\x85\x3f\xff\xfc", "lwzu";
  "\x8d\x3c\x00\x14", "lbzu";
  "\x7d\x3d\x50\xee", "lbzux";
  "\x38\x21\x00\x10", "addi";
  "\x3b\xde\xfd\x28", "addi";
  "\x3f\xde\x00\x02", "addis";
  "\x3d\x6b\xf0\x00", "addis";
]

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

let check arch name = function
  | Ok bil -> printf "%s %s ok\n" (Arch.to_string arch) name
  | Error er ->
    printf "test for %s %s failed: %s\n"
      (Arch.to_string arch) name
      (Error.to_string_hum er)

let check_bil arch (bytes, name) =
  let mem, insn = get_insn arch bytes in
  let bil = to_bil arch mem insn in
  check arch name bil

module Check_bil = struct

  let check_var c var =
    match c#lookup var with
    | None -> printf "var %s not found\n" (Var.name var)
    | Some r ->
      match Bil.Result.value r with
      | Bil.Imm word ->
        printf "%s := %s\n" (Var.name var) (Word.to_string word)
      | Bil.Bot | Bil.Mem _ -> printf "that result is not imm\n"

  let run arch =
    let bytes = "\x3f\xde\x00\x02" in
    let r = Var.Set.find_exn
        Registers.gpr ~f:(fun v -> Var.name v = "R30") in
    let mem,insn = get_insn arch bytes in
    let bil = Or_error.ok_exn @@ to_bil arch mem insn in
    let bil = DSL.[
        r := int (Word.of_int ~width:64 1);
      ] @ bil in
    let c = Stmt.eval bil (new Bili.context) in
    check_var c r
end

let () =
  List.iter ~f:(fun b -> check_bil `ppc64 b) bytes;
  List.iter ~f:(fun b -> check_bil `ppc b) bytes;
