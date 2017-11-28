open Core_kernel.Std
open Bap.Std

open Powerpc_utils

type bap_exp = exp

type s = Signed | Unsigned [@@deriving bin_io, compare, sexp]

type binop = Bil.binop [@@deriving bin_io, compare, sexp]
type unop  = Bil.unop  [@@deriving bin_io, compare, sexp]

type body =
  | Var of var
  | Word of word
  | Load of (var * body * endian * size)
  | Concat of body * body list
  | Binop of binop * body * body
  | Extract of (int * int * body)
  | Cast of (s * int * body)
  | Unop of (unop * body)
[@@deriving bin_io, compare, sexp]

type exp = {
  body : body;
  sign : s;
  width : int;
} [@@deriving bin_io, compare, sexp]

type t = bil [@@deriving bin_io, compare, sexp]
type rtl = t [@@deriving bin_io, compare, sexp]

let rec of_body = function
  | Var v -> Bil.var v
  | Word w -> Bil.int w
  | Load (mem, addr, endian, size) ->
    Bil.(load (var mem) (of_body addr) endian size)
  | Concat (x, xs) ->
    let xs = List.map ~f:of_body xs in
    List.fold ~init:(of_body x) ~f:Bil.(^) xs
  | Binop (op, x, y) -> Bil.binop op (of_body x) (of_body y)
  | Extract (hi, lo, x) -> Bil.extract hi lo (of_body x)
  | Cast (Signed, width, x) -> Bil.(cast signed width (of_body x))
  | Cast (Unsigned, width, x) -> Bil.(cast unsigned width (of_body x))
  | Unop (op, x) -> Bil.unop op (of_body x)

let is_var = function
  | Var _ -> true
  | _ -> false

module Exp = struct

  (** TODO: think again about signess + 1-bit values *)
  let cast x width sign =
    if x.width = 1 then
      {width; sign=x.sign; body = Cast (x.sign, width, x.body)}
    else
      {width; sign; body = Cast (sign, width, x.body)}

  let derive_sign s s' = match s, s' with
    | Signed, _ | _, Signed -> Signed
    | _ -> Unsigned

  let unop op x = { x with body = Unop (op, x.body)}

  let binop op lhs rhs =
    let sign = lhs.sign in
    let width = lhs.width in
    let body = Binop(op, lhs.body, rhs.body) in
    {sign; width; body;}

  let binop_with_cast op lhs rhs =
    let sign = derive_sign lhs.sign rhs.sign in
    let width = max lhs.width rhs.width in
    let lhs = cast lhs width sign in
    let rhs = cast rhs width sign in
    { sign; width; body = Binop (op, lhs.body, rhs.body); }

  let plus lhs rhs =
    binop_with_cast Bil.plus lhs rhs

  let minus lhs rhs =
    binop_with_cast Bil.minus lhs rhs

  let concat lhs rhs =
    let sign = derive_sign lhs.sign rhs.sign in
    let width = lhs.width + rhs.width in
    let body = Concat (lhs.body, [rhs.body]) in
    { sign; width; body; }

  let lt x y = binop_with_cast Bil.lt x y
  let gt x y = binop_with_cast Bil.lt y x
  let eq x y = binop_with_cast Bil.eq y x
  let slt x y = binop_with_cast Bil.slt x y
  let sgt x y = binop_with_cast Bil.slt y x
  let lshift x y = binop Bil.lshift x y
  let rshift x y = binop Bil.rshift x y
  let bit_and x y = binop_with_cast Bil.bit_and x y
  let bit_xor x y = binop_with_cast Bil.bit_xor x y
  let bit_or x y = binop_with_cast Bil.bit_or x y
  let not x = unop Bil.not x

  let var_bitwidth v =
    match Var.typ v with
    | Type.Imm w -> w
    | _ ->
      ppc_fail "variable %s doesn't has notion of bitwidth" (Var.name v)

  let of_var var =
    let width = var_bitwidth var in
    { sign = Unsigned; width; body = Var var; }

  let of_vars vars = match vars with
    | [] -> ppc_fail "can't constuct an expression from empty var list"
    | v :: vars ->
      let width = List.fold vars ~init:0 ~f:(fun a x -> a + var_bitwidth x) in
      let vars = List.map ~f:(fun x -> Var x) vars in
      { sign = Unsigned; width; body = Concat (Var v, vars); }

  let of_word w =
    let width = Word.bitwidth w in
    {sign = Unsigned; width; body = Word w }

  let load mem addr endian size =
    let width = Size.in_bits size in
    let sign = Unsigned in
    let body = Load (mem,addr.body,endian,size) in
    {sign; width; body;}

  let extract hi lo e =
    let width = hi - lo + 1 in
    if width = e.width then e
    else
      let sign = e.sign in
      match e.body with
      | Concat (x,xs) ->
        if List.for_all (x::xs) ~f:is_var then
          let vars = List.map ~f:(function
              | Var v -> v
              | _ -> ppc_fail "variable expected") (x::xs) in
          let bounds,_ =
            List.fold ~init:([],0)
              ~f:(fun (acc,n) v ->
                  let len = var_bitwidth v in
                  (n + len - 1, n, v) :: acc, n + len) vars in
          List.find
            ~f:(fun (hi',lo',_) -> hi = hi' && lo = lo') bounds |> function
          | Some (_,_,v) -> {sign; width; body = Var v}
          | None -> {sign; width; body = Extract (hi,lo,e.body)}
        else
          {sign; width; body = Extract (hi,lo,e.body)}
      | _ -> {sign; width; body = Extract (hi,lo,e.body)}

  let signed e = {e with sign = Signed}
  let unsigned e = {e with sign = Unsigned}
  let width e = e.width
  let body e = of_body e.body

end

let bil_of_t = List.concat

let store mem addr data endian size =
  Bil.[mem := store (var mem) (of_body addr.body) (of_body data.body) endian size]

let if_ probe then_ else_ =
  let probe = Exp.cast probe 1 Unsigned in
  let probe = of_body probe.body in
  let then_ = bil_of_t then_ in
  let else_ = bil_of_t else_ in
  Bil.[ if_ probe then_ else_ ]

let move lhs rhs =
  match lhs.body with
  | Var v ->
    let rhs = Exp.cast rhs lhs.width lhs.sign in
    Bil.[v := of_body rhs.body]
  | _ -> ppc_fail "unexpected left side of :="

module Infix = struct
  open Exp
  let (:=) = move
  let (+)  = plus
  let (-)  = minus
  let (^)  = concat
  let (<)  = lt
  let (>)  = gt
  let (<$) = slt
  let (>$) = sgt
  let (=)  = eq
  let (lsl)  = lshift
  let (lsr)  = rshift
  let (land) = bit_and
  let (lor)  = bit_or
  let (lxor) = bit_xor
  let (lnot) = not
end
