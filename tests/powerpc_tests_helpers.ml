open Core_kernel.Std
open Bap.Std
open OUnit2

module Dis = Disasm_expert.Basic

open Powerpc

open Powerpc_model

module P32 = PowerPC_32
module P64 = PowerPC_64

module Any_ppc = P32

let cr_bit n =
  try
    Int.Map.find_exn Any_ppc.cri n
  with _ ->
    sprintf "requested CR bit %d not found" n |>
    failwith

let nf = cr_bit 0
let pf = cr_bit 1
let zf = cr_bit 2
let ca = Any_ppc.ca
let ca32 = Any_ppc.ca32
let lr = Any_ppc.lr
let ctr = Any_ppc.ctr
let tar = Any_ppc.tar

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

let get_insn ?addr arch bytes =
  let width = Arch.addr_size arch |> Size.in_bits in
  let addr = match addr with
    | None -> Addr.of_int ~width 0
    | Some a -> a in
  let mem = create_memory arch bytes addr in
  let dis = create_dis arch in
  match Dis.insn_of_mem dis mem with
  | Ok (mem, Some insn, _) ->
    let insn_name = Insn.(name @@ of_basic insn) in
    mem, insn, insn_name
  | _ -> failwith "disasm failed"

let lookup_var c var = match c#lookup var with
  | None -> None
  | Some r ->
    match Bil.Result.value r with
    | Bil.Imm word -> Some word
    | Bil.Bot | Bil.Mem _ -> None

let find_gpr arch name =
  try
    match arch with
    | `ppc -> String.Map.find_exn P32.gpr name
    | _ -> String.Map.find_exn P64.gpr name
  with _ ->
    sprintf "gpr %s not" name |> failwith

let get_bil ?addr arch bytes =
  let mem,insn,_ = get_insn ?addr arch bytes in
  to_bil arch mem insn

let check_bil bil =
  match Type.check bil with
  | Error te ->
    let err =
      sprintf "The lifted code is not well-typed: %s"
        (Type.Error.to_string te) in
    failwith err
  | Ok () -> ()

(** [check_gpr ?addr init_bil bytes var expected arch ctxt] -
    tests if a result bound to the [var] is equal to
    [exptected]. Evaluates bil, that is a concatenation
    of [init_bil] and code, obtained from lifting [bytes].
    [addr] is an instruction address, 0 by default. *)
let check_gpr ?addr init bytes var expected arch ctxt =
  let mem,insn,insn_name = get_insn ?addr arch bytes in
  let bil = Or_error.ok_exn @@ to_bil arch mem insn in
  check_bil (init @ bil);
  let c = Stmt.eval (init @ bil) (new Bili.context) in
  match lookup_var c var with
  | None -> assert_bool "var not found OR it's result not Imm" false
  | Some w ->
    if not (Word.equal w expected) ||
        (Word.bitwidth w <> Word.bitwidth expected) then
      printf "\n%s: check failed for %s: expected %s <> %s\n"
        insn_name
        (Var.name var)
        (Word.to_string expected)
        (Word.to_string w);
    assert_equal ~cmp:Word.equal w expected

(** [eval ?addr init_bil bytes arch] - evaluates bil, that is a concatenation
    of [init_bil] and code, obtained from lifting [bytes].
    [addr] is an instruction address, 0 by default. *)
let eval ?addr init bytes arch =
  let mem,insn,_ = get_insn ?addr arch bytes in
  let bil = Or_error.ok_exn @@ to_bil arch mem insn in
  check_bil (init @ bil);
  Stmt.eval (init @ bil) (new Bili.context)

let load_word ctxt mem addr endian size =
  let bits = Size.in_bits size in
  let tmp = Var.create ~fresh:true "tmp" (Type.imm bits) in
  let bil = Bil.[
      tmp := load ~mem:(var mem) ~addr:(int addr) endian size;
    ] in
  check_bil bil;
  let ctxt = Stmt.eval bil ctxt in
  lookup_var ctxt tmp

let check_mem init bytes mem ~addr ~size expected ?(endian=BigEndian) arch ctxt =
  let memory,insn,insn_name = get_insn arch bytes in
  let bil = Or_error.ok_exn @@ to_bil arch memory insn in
  check_bil (init @ bil);
  let c = Stmt.eval (init @ bil) (new Bili.context) in
   match load_word c mem addr endian size with
  | None -> assert_bool "word not found OR it's result not Imm" false
  | Some w ->
    if not (Word.equal w expected) then
      printf "\n%s: check failed for %s: expected %s <> %s\n"
        insn_name
        (Addr.to_string addr)
        (Word.to_string expected)
        (Word.to_string w);
    assert_equal ~cmp:Word.equal w expected

let concat_words ws = match ws with
  | [] -> failwith "words list is empty!"
  | w :: ws ->
    List.fold ~init:w ~f:(fun ws w -> Word.concat ws w) ws

(** [make_bytes ws] - returns a string representation of concated words [ws] *)
let make_bytes ws =
  let bytes = concat_words ws in
  let bytes = Seq.to_list @@ Word.enum_chars bytes BigEndian in
  String.of_char_list bytes

let is_equal_words w = function
  | None -> false
  | Some w' -> Word.equal w w'

let string_of_bytes bytes =
  String.fold ~init:"" ~f:(fun acc b ->
      sprintf "%s%02X " acc (Char.to_int b)) bytes

let arch_width a = Arch.addr_size a |> Size.in_bits

type form = [
  | `D
  | `M
  | `MD
  | `MDS
  | `VA
  | `X
  | `XL
  | `XFX
  | `XO
  | `XS
] [@@deriving sexp]

let make_insn ?name ?(arch=`ppc) form fields =
  let b0 = Word.b0 in
  let word ~width n = Word.of_int ~width n in
  let bytes =
    match form, fields with
    | `D, [opcode; rt; ra; d] ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:5 rt;
        word ~width:5 ra;
        word ~width:16 d;
      ]
    | `M, [opcode; rs; ra; sh; mb; me; rc] ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:5 rs;
        word ~width:5 ra;
        word ~width:5 sh;
        word ~width:5 mb;
        word ~width:5 me;
        word ~width:1 rc;
      ]
    | `MD, [opcode; rs; ra; sh1; me; opt; sh2; rc] ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:5 rs;
        word ~width:5 ra;
        word ~width:5 sh1;
        word ~width:6 me;
        word ~width:3 opt;
        word ~width:1 sh2;
        word ~width:1 rc;
      ]
    | `MDS, [opcode; rs; ra; rb; mb; opt; rc] ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:5 rs;
        word ~width:5 ra;
        word ~width:5 rb;
        word ~width:6 mb;
        word ~width:4 opt;
        word ~width:1 rc;
      ]
    | `VA, [opcode; rt; ra; rb; rc; opt_opcode;] ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:5 rt;
        word ~width:5 ra;
        word ~width:5 rb;
        word ~width:5 rc;
        word ~width:6 opt_opcode;
      ]
    | `X, [opcode; rt; ra; rb; opt_opcode; rc] ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:5 rt;
        word ~width:5 ra;
        word ~width:5 rb;
        word ~width:10 opt_opcode;
        word ~width:1 rc;
      ]
    | `X, [opcode; rt; ra; rb; opt_opcode;] ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:5 rt;
        word ~width:5 ra;
        word ~width:5 rb;
        word ~width:10 opt_opcode;
        b0;
      ]
    | `XL, [opcode; bf; x; bfa; y; z; opt_opcode; w;] ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:3 bf;
        word ~width:2 x;
        word ~width:3 bfa;
        word ~width:2 y;
        word ~width:5 z;
        word ~width:10 opt_opcode;
        word ~width:1 w;
      ]
    | `XL, [opcode; bt; ba; bb; opt_opcode; x] ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:5 bt;
        word ~width:5 ba;
        word ~width:5 bb;
        word ~width:10 opt_opcode;
        word ~width:1 x;
      ]
    | `XFX, [opcode; rs; x; data; y; opt_opcode; z] ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:5 rs;
        word ~width:1 x;
        word ~width:8 data;
        word ~width:1 x;
        word ~width:10 opt_opcode;
        word ~width:1 z;
      ]
    | `XFX, [opcode; rs; data; opt_opcode; x] ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:5 rs;
        word ~width:10 data;
        word ~width:10 opt_opcode;
        word ~width:1 x;
      ]
    | `XO, [opcode; rt; ra; rb; oe; opt_opcode; rc] ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:5 rt;
        word ~width:5 ra;
        word ~width:5 rb;
        word ~width:1 oe;
        word ~width:9 opt_opcode;
        word ~width:1 rc;
      ]
    | `XS, [opcode; rs; ra; sh1; opt; sh2; rc] ->
      make_bytes [
        word ~width:6 opcode;
        word ~width:5 rs;
        word ~width:5 ra;
        word ~width:5 sh1;
        word ~width:9 opt;
        word ~width:1 sh2;
        word ~width:1 rc;
      ]
    | _ -> failwith "unexpected argument set for given insn form" in
  let () = match name with
    | None -> ()
    | Some name ->
      let _,insn,insn_name = get_insn arch bytes in
      if not (String.equal name insn_name) then
        let err =
          sprintf "error: failed to construct %s insn, got a %s"
            name insn_name in
        failwith err in
  bytes
