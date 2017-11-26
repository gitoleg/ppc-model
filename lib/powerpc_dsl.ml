open Core_kernel.Std
open Bap.Std
open Regular.Std

open Powerpc_model
open Hardware

type sign = Signed | Unsigned  [@@deriving bin_io, compare, sexp]

module Sign = struct
  type t = sign [@@deriving bin_io, compare, sexp]
  include Regular.Make(struct
      type nonrec t = t [@@deriving bin_io, compare, sexp]
      let module_name = Some ("Powerpc_dsl.Sign")
      let hash = Hashtbl.hash
      let version = "0.1"
      let pp fmt t =
        let s = Sexp.to_string (sexp_of_t t) in
        Format.fprintf fmt "%s" s
    end)
end

type exp = {
  sign : sign;
  width : int;
  body : Bil.exp;
} [@@deriving bin_io, compare, sexp]

type 'a p = sign -> 'a

let ppc_fail format =
  let fail str = failwith (sprintf "PowerPC lifter fail: %s" str) in
  Printf.ksprintf fail format

(** will also try to find R# when got X#, (e.g. R3 when got X3)
    for reasons depended only from llvm side *)
let find_gpr reg =
  let is_var_named name var = String.equal name (Var.name var) in
  let find name = Var.Set.find ~f:(is_var_named name) gpr in
  let reg_name = Reg.name reg in
  match find reg_name with
  | Some r -> Some r
  | None ->
    if String.is_prefix reg_name ~prefix:"X" then
      let name = String.substr_replace_first
          reg_name ~pattern:"X" ~with_:"R" in
      find name
    else None

let find_cr_bit reg =
  let name = Reg.name reg in
  Int.Map.data cr |>
  List.find ~f:(fun v -> String.equal (Var.name v) name)

let reg_searches = [find_gpr; find_cr_bit;]

let find reg =
  List.filter_map reg_searches ~f:(fun f -> f reg) |> function
  | [] ->
    ppc_fail "Register not found: %s" (Reg.name reg)
  | hd :: [] -> hd
  | _ -> ppc_fail "Register name %s is ambigous!!!" (Reg.name reg)

let int_of_imm = function
  | Op.Reg _ | Op.Fmm _ -> ppc_fail "imm operand expected"
  | Op.Imm x -> match Imm.to_int x with
    | Some x -> x
    | None -> ppc_fail "failed to convert imm operand to int"

let imm sign op  =
  let imm = int_of_imm op in
  let width = 64 in
  { sign; width; body = Bil.(int (Word.of_int ~width imm)); }

let signed f = f Signed
let unsigned f = f Unsigned

let var sign  =
  let width = 64 in {
    sign;
    width;
    body = Bil.var (Var.create ~fresh:true "tmp" (Type.Imm width))
  }

let reg sign op = match op with
  | Op.Imm _ | Op.Fmm _ ->
    ppc_fail "reg operand expected"
  | Op.Reg x ->
    try
      let v = find x in
      let width = match Var.typ v with
        | Type.Imm w -> w
        | _ -> ppc_fail "register %s has unexpected type" (Var.name v)
      in
      { sign; width; body = Bil.var (find x) }
    with _ -> { sign; width = 64; body = Bil.int (Word.zero 64) }

let int sign value =
  { sign; width = 64; body = Bil.int (Word.of_int ~width:64 value); }

module RTL = struct

  type cast  = Bil.cast  [@@deriving bin_io, compare, sexp]
  type binop = Bil.binop [@@deriving bin_io, compare, sexp]
  type unop  = Bil.unop  [@@deriving bin_io, compare, sexp]
  type t = bil  [@@deriving bin_io, compare, sexp]

  let coerce x width sign =
    let body =
      if sign = x.sign && width = x.width then x.body
      else
        match sign with
        | Unsigned -> Bil.(cast unsigned width x.body)
        | Signed -> Bil.(cast signed width x.body) in
    { body; sign; width; }

  let derive_sign s s' =
    if Sign.equal s s' then s
    else Signed

  let binop_with_coerce op lhs rhs =
    let sign = derive_sign lhs.sign rhs.sign in
    let width = max lhs.width rhs.width in
    let lhs = coerce lhs width sign in
    let rhs = coerce rhs width sign in
    { sign; width; body = Bil.(binop op lhs.body rhs.body)}

  let move lhs rhs =
    match lhs.body with
    | Bil.Var v ->
      let rhs = coerce rhs lhs.width lhs.sign in
      Bil.[v := rhs.body]
    | _ -> ppc_fail "variable expected on left side of :="

  let plus lhs rhs =
    binop_with_coerce Bil.plus lhs rhs

  let concat lhs rhs =
    let sign = derive_sign lhs.sign rhs.sign in
    let width = lhs.width + rhs.width in
    let body = Bil.(lhs.body ^ rhs.body) in
    { sign; width; body; }

  let lshift lhs rhs = binop_with_coerce Bil.lshift lhs rhs

  module Infix = struct
    let (:=) = move
    let (+)  = plus
    let (^)  = concat
    let (lsl) = lshift
  end

  let int = Bil.int
  let extract = Bil.extract
  let var = Bil.var
  let bil_of_rtl = List.concat

  include Infix
end

type rtl = RTL.t [@@deriving bin_io, compare, sexp]

type cpu = {
  load   : exp -> size -> exp;
  store  : exp -> exp -> size -> rtl;
  addr  : addr;
}

let byte = `r8
let halfword = `r16
let word = `r32
let doubleword = `r64
let bit_t = Type.imm 1
let byte_t = Type.imm 8
let halfword_t = Type.imm 16
let word_t = Type.imm 32
let doubleword_t = Type.imm 64
let zero = { sign = Unsigned; width = 1; body = Bil.int Word.b0 }
let one  = { sign = Unsigned; width = 1; body = Bil.int Word.b1 }

let load addr_size ~addr endian size = match addr_size with
  | `r32 -> Bil.(load ~mem:(var PPC32.mem) ~addr endian size)
  | `r64 -> Bil.(load ~mem:(var PPC64.mem) ~addr endian size)

let store addr_size ~addr data endian size =
  match addr_size with
  | `r32 ->
    Bil.(PPC32.mem := store ~mem:(var PPC32.mem) ~addr data endian size)
  | `r64 ->
    Bil.(PPC64.mem := store ~mem:(var PPC64.mem) ~addr data endian size)

let make_cpu addr_size endian mem =
  let extract_addr a = match addr_size with
    | `r32 -> Bil.extract 31 0 a
    | `r64 -> a in
  let load exp size =
    let addr = extract_addr exp.body in
    let width = Size.in_bits size in {
      sign = exp.sign;
      width;
      body = load addr_size ~addr endian size;
    } in
  let store addr data size =
    let addr = extract_addr addr.body in
    [ store addr_size ~addr data.body endian size ] in
  let addr = Memory.min_addr mem in
  { load; store; addr; }
