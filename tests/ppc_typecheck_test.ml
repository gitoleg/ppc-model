open Core_kernel.Std
open Bap.Std
open OUnit2

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
  | _ -> failwith "disasm failed"

let check_bil arch bytes ctxt =
  let mem, insn = get_insn arch bytes in
  let bil = to_bil arch mem insn in
  assert_bool "typecheck failed" (Result.is_ok bil)

let arches = [`ppc; `ppc64;]

let run bytes ctxt =
  List.iter ~f:(fun a -> check_bil a bytes ctxt) arches

let suite = "typecheck" >::: [
    (** fix-point load  *)
    "lbz"     >:: run "\x89\x3c\x00\x14";
    "lbz"     >:: run "\x89\x20\x00\x14";
    "lwz"     >:: run "\x83\xeb\xff\xfc";
    "lbzx"    >:: run "\x7d\x3d\x50\xae";
    "lwzx"    >:: run "\x7d\x3d\x50\x2e";
    "lwzu"    >:: run "\x85\x3f\xff\xfc";
    "lbzu"    >:: run "\x8d\x3c\x00\x14";
    "lbzux"   >:: run "\x7d\x3d\x50\xee";
    (** fix-point store  *)
    "stb"     >:: run "\x99\x3c\x01\x6c";
    "stb"     >:: run "\x99\x20\x01\x6C";
    "stw"     >:: run "\x91\x28\xff\xd4";
    "stbx"    >:: run "\x7d\x2e\xf9\xae";
    "sthx"    >:: run "\x7d\x3e\xeb\x2e";
    "stwx"    >:: run "\x7f\xb6\xf9\x2e";
    "stbu"    >:: run "\x9c\x9d\xff\xff";
    "stwu"    >:: run "\x94\x21\xff\xf0";
    "stbux"   >:: run "\x7d\x3f\xc9\xee";
    "stwux"   >:: run "\x7d\x41\x49\x6e";
    (** fix-point add  *)
    "addi"    >:: run "\x38\x21\x00\x10";
    "addi"    >:: run "\x3b\xde\xfd\x28";
    "addis"   >:: run "\x3f\xde\x00\x02";
    "addis"   >:: run "\x3d\x6b\xf0\x00";
    "add"     >:: run "\x7d\x62\x5a\x14";
    "add."    >:: run "\x7d\x62\x5a\x15";
    "addic"   >:: run "\x30\x21\x00\x10";
    "addic"   >:: run "\x33\xde\xfd\x28";
    "addic."  >:: run "\x34\x21\x00\x10";
    "addic."  >:: run "\x37\xde\xfd\x28";
    "addc"    >:: run "\x7d\x62\x58\x14";
    "addc."   >:: run "\x7d\x62\x58\x15";
    "adde"    >:: run "\x7c\x21\x81\x14";
    "adde."   >:: run "\x7c\x21\x81\x15";
    "addme"   >:: run "\x7c\x22\x01\xd4";
    "addme."  >:: run "\x7c\x22\x01\xd5";
    "addze"   >:: run "\x7c\x22\x01\x94";
    "addze."  >:: run "\x7c\x22\x01\x95";
    (** fix-point logical  *)
    "andi."   >:: run "\x71\x2a\x00\x20";
    "andis."  >:: run "\x75\x2a\x08\x00";
    "and"     >:: run "\x7f\x39\xe8\x38";
    "and."    >:: run "\x7d\x49\x30\x39";
    "andc"    >:: run "\x7c\xea\x50\x78";
    "andc."   >:: run "\x7e\x09\x18\x79";
    "ori"     >:: run "\x60\xc6\x51\xc1";
    "oris"    >:: run "\x65\x4a\x00\x10";
    "or"      >:: run "\x7f\x38\xc3\x78";
    "or."     >:: run "\x7d\x0a\x4b\x79";
    "orc"     >:: run "\x7c\x8a\x53\x38";
    "orc."    >:: run "\x7c\x8a\x53\x39";
    "xori"    >:: run "\x68\x63\x00\x01";
    "xoris"   >:: run "\x6d\x2a\x04\x00";
    "xor"     >:: run "\x7c\x6a\x52\x78";
    "xor."    >:: run "\x7d\x4a\x4a\x79";
    "nand"    >:: run "\x7c\x63\x1b\xb8";
    "nand."   >:: run "\x7c\x63\x1b\xb9";
    "nor"     >:: run "\x7d\x09\x48\xf8";
    "nor."    >:: run "\x7d\x09\x48\xf9";
    "eqv"     >:: run "\x7d\x09\x4a\x38";
    "eqv."    >:: run "\x7d\x09\x4a\x39";
    "extsb"   >:: run "\x7d\x4a\x07\x74";
    "extsb."  >:: run "\x7d\x48\x07\x75";
    "extsh"   >:: run "\x7d\x25\x07\x34";
    "extsh."  >:: run "\x7d\x25\x07\x35";
    "cntlzw"  >:: run "\x7c\x63\x00\x34";
    "cntlzw." >:: run "\x7c\x63\x00\x35";
    "cnttzw"  >:: run "\x7c\x63\x04\x34";
    "cnttzw." >:: run "\x7c\x63\x04\x35";
    "cmpb"    >:: run "\x7c\x8a\x53\xf8";
    "popcntw" >:: run "\x7c\x84\x02\xf4";
  ]
