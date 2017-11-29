open Core_kernel.Std
open Bap.Std

open Powerpc_utils

type bil_exp = exp

type s = Signed | Unsigned [@@deriving bin_io, compare, sexp]

type binop = Bil.binop [@@deriving bin_io, compare, sexp]
type unop  = Bil.unop  [@@deriving bin_io, compare, sexp]

type body =
  | Vars of var * var list
  | Word of word
  | Load of (var * body * endian * size)
  | Concat of body * body
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

let rec bil_exp = function
  | Vars (v, []) -> Bil.var v
  | Vars (v, vars) ->
    List.fold vars ~init:(Bil.var v) ~f:(fun e v -> Bil.(e ^ var v))
  | Word w -> Bil.int w
  | Load (mem, addr, endian, size) ->
    Bil.(load (var mem) (bil_exp addr) endian size)
  | Concat (x, y) -> Bil.(bil_exp x ^ bil_exp y)
  | Binop (op, x, y) -> Bil.binop op (bil_exp x) (bil_exp y)
  | Extract (hi, lo, x) -> Bil.extract hi lo (bil_exp x)
  | Cast (Signed, width, x) -> Bil.(cast signed width (bil_exp x))
  | Cast (Unsigned, width, x) -> Bil.(cast unsigned width (bil_exp x))
  | Unop (op, x) -> Bil.unop op (bil_exp x)

let var_bitwidth v =
  match Var.typ v with
  | Type.Imm w -> w
  | _ ->
    ppc_fail "variable %s doesn't has notion of bitwidth" (Var.name v)

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
    let body = Concat (lhs.body, rhs.body) in
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

  let of_var var =
    let width = var_bitwidth var in
    { sign = Unsigned; width; body = Vars (var, []); }

  let of_vars vars = match vars with
    | [] -> ppc_fail "can't constuct an expression from empty var list"
    | v :: vars ->
      let width = List.fold (v::vars) ~init:0 ~f:(fun a x -> a + var_bitwidth x) in
      { sign = Unsigned; width; body = Vars (v, vars); }

  let of_word w =
    let width = Word.bitwidth w in
    {sign = Unsigned; width; body = Word w }

  let load mem addr endian size =
    let width = Size.in_bits size in
    let sign = Unsigned in
    let body = Load (mem,addr.body,endian,size) in
    {sign; width; body;}

  let extract_of_vars e hi lo vars =
    let width = hi - lo + 1 in
    let bounds,_ =
      List.fold ~init:([],0)
        ~f:(fun (acc,n) v ->
            let len = var_bitwidth v in
            let hi = e.width - n - 1 in
            let lo = e.width - n - len in
            (hi, lo, v) :: acc, n + len) vars in
    let bounds = List.rev bounds in
    let has_hi = List.exists ~f:(fun (hi',_,_) -> hi = hi') bounds in
    let has_lo = List.exists ~f:(fun (_,lo',_) -> lo = lo') bounds in
    if has_hi && has_lo then
      let vars = List.filter_map ~f:(fun (hi', lo', v) ->
          if hi' <= hi && lo' >= lo then Some v
          else None) bounds in
      let v = List.hd_exn vars in
      let vars = List.tl_exn vars in
      {sign=e.sign; width; body = Vars (v,vars)}
    else
      {sign=e.sign; width; body = Extract (hi,lo,e.body)}

let extract hi lo e =
    let width = hi - lo + 1 in
    if width = e.width then e
    else
      match e.body with
      | Vars (v,vars) when vars <> [] ->
        extract_of_vars e hi lo (v :: vars)
      | _ ->
        { sign=e.sign; width; body = Extract (hi,lo,e.body) }

  let signed e = {e with sign = Signed}
  let unsigned e = {e with sign = Unsigned}
  let width e = e.width
  let bil_exp e = bil_exp e.body

end

type t =
  | Move of exp * exp
  | Jmp of exp
  | Store of var * exp * exp * endian * size
  | If of exp * t list * t list
[@@deriving bin_io, compare, sexp]

type rtl = t [@@deriving bin_io, compare, sexp]

let store mem addr data endian size : t =
  Store (mem, addr, data, endian, size)

let jmp addr : t = Jmp addr
let move x y : t = Move (x,y)
let if_ cond then_ else_ = If (cond, then_, else_)

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

module Translate = struct

  let store mem addr data endian size =
    Bil.[mem := store (var mem) (bil_exp addr.body) (bil_exp data.body) endian size]

  let if_ probe then_ else_ =
    let probe = Exp.cast probe 1 Unsigned in
    let probe = bil_exp probe.body in
    Bil.[ if_ probe then_ else_ ]

  let move lhs rhs =
    match lhs.body with
    | Vars (v, []) ->
      let rhs = Exp.cast rhs lhs.width lhs.sign in
      Bil.[v := bil_exp rhs.body]
    | Vars (v, vars) ->
      let rec assign es n = function
        | [] -> es
        | v :: vars ->
          let w = var_bitwidth v in
          let hi = n + w - 1 in
          let lo = n in
          let es = Bil.(v := extract hi lo (bil_exp rhs.body)) :: es in
          assign es (n + w) vars in
      assign [] 0 (List.rev (v::vars))
    | _ -> ppc_fail "unexpected left side of :="

  let jmp exp = Bil.[ jmp (bil_exp exp.body)]

  let rec stmt_to_bil = function
    | Move (x,y) -> move x y
    | Store (mem, addr, data, endian, size) ->
      store mem addr data endian size
    | Jmp a -> jmp a
    | If (cond, then_, else_) ->
      let then_ = to_bil then_ in
      let else_ = to_bil else_ in
      if_ cond then_ else_
  and
    to_bil stmts =
    List.concat (List.map ~f:stmt_to_bil stmts)
end

let bil_of_t = Translate.to_bil
