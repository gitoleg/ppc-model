open Core_kernel.Std
open Bap.Std

open Powerpc_utils

type bap_exp = exp

type s = Signed | Unsigned [@@deriving bin_io, compare, sexp]

type exp = {
  body : Bil.exp;
  sign : s;
  width : int;
} [@@deriving bin_io, compare, sexp]

type t = bil [@@deriving bin_io, compare, sexp]
type rtl = t [@@deriving bin_io, compare, sexp]

module Exp = struct

  type t = exp [@@deriving bin_io, compare, sexp]
  type cast  = Bil.cast  [@@deriving bin_io, compare, sexp]
  type binop = Bil.binop [@@deriving bin_io, compare, sexp]
  type unop  = Bil.unop  [@@deriving bin_io, compare, sexp]

  let coerce x width sign =
    let body =
      if sign = x.sign && width = x.width then x.body
      else
        match sign with
        | Unsigned -> Bil.(cast unsigned width x.body)
        | Signed -> Bil.(cast signed width x.body) in
    { body; sign; width; }

  let derive_sign s s' = match s, s' with
    | Signed, _ | _, Signed -> Signed
    | _ -> Unsigned

  let binop op lhs rhs =
    let sign = lhs.sign in
    let width = lhs.width in
    let body = Bil.binop op lhs.body rhs.body in
    {sign; width; body;}

  let binop_with_coerce op lhs rhs =
    let sign = derive_sign lhs.sign rhs.sign in
    let width = max lhs.width rhs.width in
    let lhs = coerce lhs width sign in
    let rhs = coerce rhs width sign in
    { sign; width; body = Bil.(binop op lhs.body rhs.body); }

  let plus lhs rhs =
    binop_with_coerce Bil.plus lhs rhs

  let concat lhs rhs =
    let sign = derive_sign lhs.sign rhs.sign in
    let width = lhs.width + rhs.width in
    let body = Bil.(lhs.body ^ rhs.body) in
    { sign; width; body; }

  let lt lhs rhs = binop_with_coerce Bil.lt lhs rhs
  let gt lhs rhs = binop_with_coerce Bil.lt rhs lhs
  let lshift lhs rhs = binop Bil.lshift lhs rhs
  let rshift lhs rhs = binop Bil.rshift lhs rhs

  module Infix = struct
    let (+)  = plus
    let (^)  = concat
    let (<)  = lt
    let (>)  = gt
    let (lsl) = lshift
    let (lsr) = rshift
  end

  let of_var ?(signed=false) var =
    let width = var_bitwidth var in
    let sign = if signed then Signed else Unsigned in
    { sign; width; body = Bil.var var; }

  let of_vars ?(signed=false) vars = match vars with
    | [] -> ppc_fail "can't constuct an expression from empty var list"
    | hd :: tl ->
      let width = List.fold vars ~init:0 ~f:(fun a x -> a + var_bitwidth x) in
      let body = List.fold vars ~init:(Bil.var hd)
          ~f:(fun a v -> Bil.(a ^ var v)) in
      let sign = if signed then Signed else Unsigned in
      { sign; width; body; }

  let of_word ?(signed=false) w =
    let width = Word.bitwidth w in
    let sign = if signed then Signed else Unsigned in
    let body = Bil.int w in
    {sign; width; body}

  let load mem addr endian size =
    let width = Size.in_bits size in
    let sign = Unsigned in
    let body = Bil.load (Bil.var mem) addr.body endian size in
    {sign; width; body;}

  let extract hi lo exp =
    let body = Bil.extract hi lo exp.body in
    let width = hi - lo + 1 in
    let sign = exp.sign in
    {sign; width; body;}

  let signed e = {e with sign = Signed}
  let unsigned e = {e with sign = Unsigned}

  let body e = e.body

end

open Exp

let bil_of_t = List.concat

let store mem addr data endian size =
  Bil.[mem := store (var mem) addr.body data.body endian size]

let move lhs rhs =
  match lhs.body with
  | Bil.Var v ->
    let rhs = coerce rhs lhs.width lhs.sign in
    Bil.[v := rhs.body]
  | _ -> ppc_fail "variable expected on left side of :="


module Infix = struct
  let (:=) = move
end
