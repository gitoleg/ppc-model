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
  let bytes = "\x71\x2a\x00\x1F" in  (** andi.   r10,r9,31 *)
  let r10 = find_gpr "R10" in
  let r9 = find_gpr "R9" in
  let init = Bil.[
      r9 := int (Word.of_int ~width:64 10);
    ] in
  let expected = Word.of_int ~width:64 10 in
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

let and_ ctxt =
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
  check_gpr init bytes r25 expected `ppc ctxt;
  check_gpr init bytes r25 expected `ppc64 ctxt

let andc ctxt =
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
  check_gpr init bytes r10 expected `ppc ctxt;
  check_gpr init bytes r10 expected `ppc64 ctxt

let ori ctxt =
  let bytes = "\x60\xc6\x51\xc1" in  (** ori     r6,r6,20929 *)
  let r6 = find_gpr "R6" in
  let x = 62 in
  let r = x lor 20929 in
  let init = Bil.[
      r6 := int (Word.of_int ~width:64 x);
    ] in
  let expected = Word.of_int ~width:64 r in
  check_gpr init bytes r6 expected `ppc ctxt;
  check_gpr init bytes r6 expected `ppc64 ctxt

let oris ctxt =
  let bytes = "\x65\x4a\x00\x0F" in (** oris    r10,r10,15  *)
  let r10 = find_gpr "R10" in
  let x = 61440 in
  let y = 15 in
  let r = x lxor (y lsl 16) in
  let init = Bil.[
      r10 := int (Word.of_int ~width:64 x);
    ] in
  let expected = Word.of_int ~width:64 r in
  check_gpr init bytes r10 expected `ppc ctxt;
  check_gpr init bytes r10 expected `ppc64 ctxt

let or_  ctxt =
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
  check_gpr init bytes r24 expected `ppc ctxt;
  check_gpr init bytes r24 expected `ppc64 ctxt

let orc ctxt =
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
  check_gpr init bytes r10 expected `ppc ctxt;
  check_gpr init bytes r10 expected `ppc64 ctxt

let xori ctxt =
  let bytes = "\x68\x63\x00\x0B" in (** xori    r3,r3,11   *)
  let r3 = find_gpr "R3" in
  let x = 16 in
  let r = 16 lxor 11 in
  let init = Bil.[
      r3 := int (Word.of_int ~width:64 x);
    ] in
  let expected = Word.of_int ~width:64 r in
  check_gpr init bytes r3 expected `ppc ctxt;
  check_gpr init bytes r3 expected `ppc64 ctxt

let xoris ctxt =
  let bytes = "\x6d\x2a\x00\x0f" in (** xoris r10,r9,15 *)
  let r9 = find_gpr "R9" in
  let r10 = find_gpr "R10" in
  let x = 0x1F0000 in
  let r = x lxor (15 lsl 16) in
  let init = Bil.[
      r9 := int (Word.of_int ~width:64 x);
    ] in
  let expected = Word.of_int ~width:64 r in
  check_gpr init bytes r10 expected `ppc ctxt;
  check_gpr init bytes r10 expected `ppc64 ctxt

let xor_ ctxt =
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
  check_gpr init bytes r10 expected `ppc ctxt;
  check_gpr init bytes r10 expected `ppc64 ctxt

let nand ctxt =
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
  check_gpr init bytes r3 expected `ppc ctxt;
  check_gpr init bytes r3 expected `ppc64 ctxt

let nor ctxt =
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
  check_gpr init bytes r9 expected `ppc ctxt;
  check_gpr init bytes r9 expected `ppc64 ctxt

let eqv ctxt =
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
  check_gpr init bytes r9 expected `ppc ctxt;
  check_gpr init bytes r9 expected `ppc64 ctxt

let extsb ctxt =
  let bytes = "\x7d\x6a\x07\x74" in   (** extsb   r10,r11  *)
  let r10 = find_gpr "R10" in
  let r11 = find_gpr "R11" in
  let init = Bil.[
      r11 := int (Word.of_int ~width:64 0xC0);
    ] in
  let expected = Word.of_int64 0xFFFFFFFFFFFFFFC0L in
  check_gpr init bytes r10 expected `ppc ctxt;
  check_gpr init bytes r10 expected `ppc64 ctxt

let extsh ctxt =
  let bytes ="\x7d\x25\x07\x34" in   (** extsh   r5,r9 *)
  let r5 = find_gpr "R5" in
  let r9 = find_gpr "R9" in
  let init = Bil.[
      r9 := int (Word.of_int ~width:64 0xC000);
    ] in
  let expected = Word.of_int64 0xFFFFFFFFFFFFC000L in
  check_gpr init bytes r5 expected `ppc ctxt;
  check_gpr init bytes r5 expected `ppc64 ctxt

let and_dot ctxt = failwith "unimplemented"
let andc_dot ctxt = failwith "unimplemented"
let or_dot ctxt = failwith "unimplemented"
let orc_dot ctxt = failwith "unimplemented"
let xor_dot ctxt = failwith "unimplemented"
let nor_dot ctxt = failwith "unimplemented"
let nand_dot ctxt = failwith "unimplemented"
let eqv_dot ctxt = failwith "unimplemented"
let exts_dot ctxt = failwith "unimplemented"

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
    "and"       >:: and_;
    "andc"      >:: andc;
    "ori"       >:: ori;
    "oris"      >:: oris;
    "or_"       >:: or_;
    "orc"       >:: orc;
    "xori"      >:: xori;
    "xoris"     >:: xoris;
    "xor_"      >:: xor_;
    "nand"      >:: nand;
    "nor"       >:: nor;
    "eqv"       >:: eqv;
    "extsb"     >:: extsb;
    "extsh"     >:: extsh;
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
