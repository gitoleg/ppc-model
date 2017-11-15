open Core_kernel.Std
open Bap.Std
open OUnit2

module Dis = Disasm_expert.Basic
open Ppc_model

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

let lookup_var c var = match c#lookup var with
  | None -> None
  | Some r ->
    match Bil.Result.value r with
    | Bil.Imm word -> Some word
    | Bil.Bot | Bil.Mem _ -> None

(** [find_gpr name] - find a GPR by it's name *)
let find_gpr name =
  Var.Set.find_exn Hardware.gpr
    ~f:(fun v -> String.equal (Var.name v) name)

(** [check_gpr init_bil bytes var expected arch ctxt] -
    tests if a result bound to the [var] is equal to
    [exptected]. Evaluates bil, that is a concatenation
    of [init_bil] and code, obtained from lifting [bytes]. *)
let check_gpr init bytes var expected arch ctxt =
  let mem,insn = get_insn arch bytes in
  let bil = Or_error.ok_exn @@ to_bil arch mem insn in
  let c = Stmt.eval (init @ bil) (new Bili.context) in
  match lookup_var c var with
  | None -> assert_bool "var not found OR it's result not Imm" false
  | Some w -> assert_equal ~cmp:Word.equal w expected

(** [eval init_bil bytes arch] - evaluates bil, that is a concatenation
    of [init_bil] and code, obtained from lifting [bytes]. *)
let eval init bytes arch =
  let mem,insn = get_insn arch bytes in
  let bil = Or_error.ok_exn @@ to_bil arch mem insn in
  Stmt.eval (init @ bil) (new Bili.context)

let load_word ctxt mem addr endian size =
  let bits = Size.in_bits size in
  let tmp = Var.create ~fresh:true "tmp" (Type.imm bits) in
  let bil = Bil.[
      tmp := load ~mem:(var mem) ~addr:(int addr) endian size;
    ] in
  let ctxt = Stmt.eval bil ctxt in
  lookup_var ctxt tmp

let check_mem init bytes mem ~addr ~size expected ?(endian=BigEndian) arch ctxt =
  let memory,insn = get_insn arch bytes in
  let bil = Or_error.ok_exn @@ to_bil arch memory insn in
  let c = Stmt.eval (init @ bil) (new Bili.context) in
   match load_word c mem addr endian size with
  | None -> assert_bool "word not found OR it's result not Imm" false
  | Some w -> assert_equal ~cmp:Word.equal w expected
