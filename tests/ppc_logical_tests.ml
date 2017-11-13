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

let find_gpr name =
  Var.Set.find_exn Hardware.gpr
    ~f:(fun v -> String.equal (Var.name v) name)

let check_gpr init bytes var expected arch ctxt =
  let mem,insn = get_insn arch bytes in
  let bil = Or_error.ok_exn @@ to_bil arch mem insn in
  let c = Stmt.eval (init @ bil) (new Bili.context) in
  match get_var c var with
  | None -> assert_bool "var not found OR it's result not Imm" false
  | Some w -> assert_equal ~cmp:Word.equal w expected

let andi_dot ctxt =
  let bytes = "\x71\x2a\x00\x20" in  (** andi.   r10,r9,32 *)
  let r10 = find_gpr "R10" in
  let r9 = find_gpr "R9" in
  let init = Bil.[
      r9 := int (Word.of_int ~width:64 10);
    ] in
  let expected = Word.of_int ~width:64 42 in
  check_gpr init bytes r10 expected `ppc ctxt;
  check_gpr init bytes r10 expected `ppc64 ctxt

let andis_dot ctxt =
  let bytes = "\x75\x2a\x0E\x00" in  (** andis.  r10,r9,2048 *)
  let r10 = find_gpr "R10" in
  let r9 = find_gpr "R9" in
  let value = Word.of_int ~width:64 0x0800_0000 in
  let init = Bil.[
      r9 := int value;
    ] in
  let expected = value in
  check_gpr init bytes r10 expected `ppc ctxt;
  check_gpr init bytes r10 expected `ppc64 ctxt

let and_ rs ra rb = failwith "unimplemented"
let and_dot mode rs ra rb = failwith "unimplemented"
let andc rs ra rb = failwith "unimplemented"
let andc_dot mode rs ra rb = failwith "unimplemented"

let ori rs ra imm = failwith "unimplemented"
let oris rs ra ui = failwith "unimplemented"
let or_ rs ra rb = failwith "unimplemented"
let or_dot mode rs ra rb = failwith "unimplemented"
let orc rs ra rb = failwith "unimplemented"
let orc_dot mode rs ra rb = failwith "unimplemented"

let xori rs ra imm = failwith "unimplemented"
let xoris rs ra ui = failwith "unimplemented"
let xor_ rs ra rb = failwith "unimplemented"
let xor_dot mode rs ra rb = failwith "unimplemented"

let nand rs ra rb = failwith "unimplemented"
let nand_dot mode rs ra rb = failwith "unimplemented"
let nor rs ra rb = failwith "unimplemented"
let nor_dot mode rs ra rb = failwith "unimplemented"

let eqv rs ra rb = failwith "unimplemented"
let eqv_dot mode rs ra rb = failwith "unimplemented"
let exts rs ra size = failwith "unimplemented"
let exts_dot mode rs ra size = failwith "unimplemented"


let cntlz value zeros ctxt =
  let bytes = "\x7c\x63\x00\x34" in
  let r = find_gpr "R3" in
  let init = Bil.[
      r := int (Word.of_int ~width:64 value);
    ] in
  let expected = Word.of_int ~width:64 zeros in
  check_gpr init bytes r expected `ppc ctxt;
  check_gpr init bytes r expected `ppc64 ctxt

let cnttz value zeros ctxt =
  let bytes = "\x7c\x63\x04\x34" in
  let r = find_gpr "R3" in
  let init = Bil.[
      r := int (Word.of_int ~width:64 value);
    ] in
  let expected = Word.of_int ~width:64 zeros in
  check_gpr init bytes r expected `ppc ctxt;
  check_gpr init bytes r expected `ppc64 ctxt

let cmpb ~bytes_cnt x y expected ctxt =
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
  check_gpr init bytes r10 expected `ppc ctxt;
  check_gpr init bytes r10 expected `ppc64 ctxt

let popcntw ctxt =
  let bytes = "\x7c\x44\x02\xf4" in (** popcntw r4, r2 *)
  let r4 = find_gpr "R4" in
  let r2 = find_gpr "R2" in
  let value = Word.of_int64 0xA0200040_10000001L in
  let expected = Word.of_int64 0x400000002L in (** 4 bits set in first word, and 2 in second  *)
  let init = Bil.[
    r2 := int value;
  ] in
  check_gpr init bytes r4 expected `ppc ctxt;
  check_gpr init bytes r4 expected `ppc64 ctxt

let suite = "result" >::: [
    "andi."     >:: andi_dot;
    "andis."    >:: andis_dot;
    "cntlz: 1"  >:: cntlz 0x0 32;
    "cntlz: 2"  >:: cntlz 0x4000000 5;
    "cntlz: 3"  >:: cntlz 0x40000000 1;
    "cntlz: 4"  >:: cntlz 0x80000000 0;
    "cnttz: 1"  >:: cnttz 0x0 32;
    "cnttz: 2"  >:: cnttz 0x1 0;
    "cnttz: 3"  >:: cnttz 0x2 1;
    "cnttz: 4"  >:: cnttz 0x20 5;
    "cmpb: 1"   >:: cmpb ~bytes_cnt:3 0x31_42_45 0x34_42_AD 0x00_FF_00;
    "cmpb: 2"   >:: cmpb ~bytes_cnt:3 0 0x34_42_AD 0x0;
    "cmpb: 3"   >:: cmpb ~bytes_cnt:3 0x34_42_AD 0x34_42_AD 0xFF_FF_FF;
    "popcntw"   >:: popcntw;
  ]
