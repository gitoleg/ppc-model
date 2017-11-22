open Core_kernel.Std
open Bap.Std
open Regular.Std

open Powerpc_model
open Hardware

type s = Signed | Unsigned  [@@deriving bin_io, compare, sexp]

module Sign = struct
  type t = s [@@deriving bin_io, compare, sexp]
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

type exp = s * Bil.exp  [@@deriving bin_io, compare, sexp]

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

let imm sign op : exp =
  let imm = int_of_imm op in
  sign, Bil.(int (Word.of_int ~width:64 imm))

let signed f = f Signed
let unsigned f = f Unsigned

let var sign : exp =
  sign,
  Bil.var @@
  Var.create ~fresh:true "tmp" (Type.Imm 64)

let reg sign op = match op with
  | Op.Imm _ | Op.Fmm _ ->
    ppc_fail "reg operand expected"
  | Op.Reg x ->
    try
      sign, Bil.var (find x)
    with _ -> sign, Bil.int (Word.zero 64)

module RTL = struct

  type cast  = Bil.cast  [@@deriving bin_io, compare, sexp]
  type binop = Bil.binop [@@deriving bin_io, compare, sexp]
  type unop  = Bil.unop  [@@deriving bin_io, compare, sexp]
  type e   = Bil.exp   [@@deriving bin_io, compare, sexp]
  type t = Bil.stmt  [@@deriving bin_io, compare, sexp]

  module Infix = struct

    let upcast ?(width=64) e = function
      | Unsigned -> Bil.(cast unsigned width e)
      | Signed -> Bil.(cast signed width e)

    let (:=) = fun (ls,lhs) (rs,rhs) ->
      match lhs with
      | Bil.Var v ->
        let width = match Var.typ v with
          | Type.Imm w -> w
          | _ -> ppc_fail "variable of unexpected type" in
        Bil.(v := upcast ~width rhs ls)
        (* if Sign.equal ls rs then *)
        (*   Bil.(v := upcast ~width (upcast rhs rs) ls) *)
        (* else *)
        (*   ppc_fail "assignment %s to %s is deprecated" *)
        (*     (Sign.to_string rs) (Sign.to_string ls) *)
      | _ -> ppc_fail "variable expected on left side of :="

    let (+) = fun lhs rhs ->
      let upcast (s,e) = upcast e s in
      Unsigned, Bil.(upcast lhs + upcast rhs)

  end

  let cast = Bil.cast
  let var = Bil.var
  let unsigned = Bil.unsigned
  let signed = Bil.signed
  let high = Bil.high
  let low = Bil.low

  let int = Bil.int
  let extract = Bil.extract
  let concat = Bil.concat
  let if_ = Bil.if_
  let jmp = Bil.jmp

  let bil = ident

  include Infix

end

type rtl = RTL.t [@@deriving bin_io, compare, sexp]

type cpu = {
  load   : exp -> size -> exp;
  store  : exp -> exp -> size -> rtl;
  mem    : mem;
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

let zero = RTL.int Word.b0
let one = RTL.int Word.b1

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
    | `r32 -> RTL.extract 31 0 a
    | `r64 -> a in
  let load (x,addr) size =
    let addr = extract_addr addr in
    x, RTL.(load addr_size ~addr endian size) in
  let store (_,addr) (_,data) size =
    let addr = extract_addr addr in
    store addr_size ~addr data endian size in
  { load; store; mem }

let low32 exp = RTL.extract 31 0 exp
