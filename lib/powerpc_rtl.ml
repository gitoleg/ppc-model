(* open Core_kernel.Std *)
(* open Bap.Std *)

(* open Powerpc_utils *)

(* type bap_exp = exp *)

(* type s = Signed | Unsigned [@@deriving bin_io, compare, sexp] *)

(* type exp = { *)
(*   body : Bil.exp; *)
(*   sign : s; *)
(*   width : int; *)
(* } [@@deriving bin_io, compare, sexp] *)

(* type t = bil [@@deriving bin_io, compare, sexp] *)
(* type rtl = t [@@deriving bin_io, compare, sexp] *)

(* let flatten_concat_vars x y = *)
(*   let rec loop acc x = match x with *)
(*     | Bil.Concat (e, e') -> loop (e :: acc) e' *)
(*     | Bil.Var _ -> *)
(*       let acc = List.rev (x :: acc) in *)
(*       List.map acc *)
(*         (function *)
(*           | Bil.Var v -> v *)
(*           | _ -> ppc_fail "variable expression expected") *)
(*     | _ -> *)
(*       ppc_fail "failed to flatten a list of concated expressions" in *)
(*   loop [] x @ loop [] y *)


(* module Exp = struct *)

(*   type t = exp [@@deriving bin_io, compare, sexp] *)
(*   type cast  = Bil.cast  [@@deriving bin_io, compare, sexp] *)
(*   type binop = Bil.binop [@@deriving bin_io, compare, sexp] *)
(*   type unop  = Bil.unop  [@@deriving bin_io, compare, sexp] *)

(*   let coerce x width sign = *)
(*     let body = *)
(*       if sign = x.sign && width = x.width then x.body *)
(*       else *)
(*         match sign with *)
(*         | Unsigned -> Bil.(cast unsigned width x.body) *)
(*         | Signed -> Bil.(cast signed width x.body) in *)
(*     { body; sign; width; } *)

(*   let derive_sign s s' = match s, s' with *)
(*     | Signed, _ | _, Signed -> Signed *)
(*     | _ -> Unsigned *)

(*   let binop op lhs rhs = *)
(*     let sign = lhs.sign in *)
(*     let width = lhs.width in *)
(*     let body = Bil.binop op lhs.body rhs.body in *)
(*     {sign; width; body;} *)

(*   let binop_with_coerce op lhs rhs = *)
(*     let sign = derive_sign lhs.sign rhs.sign in *)
(*     let width = max lhs.width rhs.width in *)
(*     let lhs = coerce lhs width sign in *)
(*     let rhs = coerce rhs width sign in *)
(*     { sign; width; body = Bil.(binop op lhs.body rhs.body); } *)

(*   let plus lhs rhs = *)
(*     binop_with_coerce Bil.plus lhs rhs *)

(*   let concat lhs rhs = *)
(*     let sign = derive_sign lhs.sign rhs.sign in *)
(*     let width = lhs.width + rhs.width in *)
(*     let body = Bil.(lhs.body ^ rhs.body) in *)
(*     { sign; width; body; } *)

(*   let lt lhs rhs = binop_with_coerce Bil.lt lhs rhs *)
(*   let gt lhs rhs = binop_with_coerce Bil.lt rhs lhs *)
(*   let lshift lhs rhs = binop Bil.lshift lhs rhs *)
(*   let rshift lhs rhs = binop Bil.rshift lhs rhs *)

(*   module Infix = struct *)
(*     let (+)  = plus *)
(*     let (^)  = concat *)
(*     let (<)  = lt *)
(*     let (>)  = gt *)
(*     let (lsl) = lshift *)
(*     let (lsr) = rshift *)
(*   end *)

(*   let var_bitwidth v = *)
(*     match Var.typ v with *)
(*     | Type.Imm w -> w *)
(*     | _ -> *)
(*       ppc_fail "variable %s doesn't has notion of bitwidth" (Var.name v) *)

(*   let of_var var = *)
(*     let width = var_bitwidth var in *)
(*     { sign = Unsigned; width; body = Bil.var var; } *)

(*   let of_vars vars = match vars with *)
(*     | [] -> ppc_fail "can't constuct an expression from empty var list" *)
(*     | hd :: tl -> *)
(*       let width = List.fold vars ~init:0 ~f:(fun a x -> a + var_bitwidth x) in *)
(*       let body = List.fold vars ~init:(Bil.var hd) *)
(*           ~f:(fun a v -> Bil.(a ^ var v)) in *)
(*       { sign = Unsigned; width; body; } *)

(*   let of_word w = *)
(*     let width = Word.bitwidth w in *)
(*     let body = Bil.int w in *)
(*     {sign = Unsigned; width; body} *)

(*   let load mem addr endian size = *)
(*     let width = Size.in_bits size in *)
(*     let sign = Unsigned in *)
(*     let body = Bil.load (Bil.var mem) addr.body endian size in *)
(*     {sign; width; body;} *)

(*   let extract hi lo exp = *)
(*     let body = Bil.extract hi lo exp.body in *)
(*     let width = hi - lo + 1 in *)
(*     let sign = exp.sign in *)
(*     {sign; width; body;} *)

(*   (\* let smart_extract hi lo e = *\) *)
(*   (\*   let width = hi - lo + 1 in *\) *)
(*   (\*   let sign = e.sign in *\) *)
(*   (\*   match e.body with *\) *)
(*   (\*   | Bil.Concat (x, y) -> *\) *)
(*   (\*     let vars = flatten_concat_vars x y in *\) *)
(*   (\*     let vars = List.fold vars ~init:([],0) *\) *)
(*   (\*         ~f:(fun a v -> a + var_bitwidth v) in *\) *)

(*   (\*   | _ -> extract hi lo e *\) *)



(*   let signed e = {e with sign = Signed} *)
(*   let unsigned e = {e with sign = Unsigned} *)

(*   let body e = e.body *)

(* end *)

(* open Exp *)

(* let bil_of_t = List.concat *)

(* let store mem addr data endian size = *)
(*   Bil.[mem := store (var mem) addr.body data.body endian size] *)

(* let move lhs rhs = *)
(*   match lhs.body with *)
(*   | Bil.Var v -> *)
(*     let rhs = coerce rhs lhs.width lhs.sign in *)
(*     Bil.[v := rhs.body] *)
(*   | _ -> ppc_fail "unexpected left side of :=" *)

(* module Infix = struct *)
(*   let (:=) = move *)
(* end *)

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

let is_var = function
  | Var _ -> true
  | _ -> false

module Exp = struct

  type t = exp [@@deriving bin_io, compare, sexp]

  let coerce x width sign =
    {x with width = width; sign = sign}

  let derive_sign s s' = match s, s' with
    | Signed, _ | _, Signed -> Signed
    | _ -> Unsigned

  let binop op lhs rhs =
    let sign = lhs.sign in
    let width = lhs.width in
    let body = Binop(op, lhs.body, rhs.body) in
    {sign; width; body;}

  let binop_with_coerce op lhs rhs =
    let sign = derive_sign lhs.sign rhs.sign in
    let width = max lhs.width rhs.width in
    let lhs = coerce lhs width sign in
    let rhs = coerce rhs width sign in
    { sign; width; body = Binop (op, lhs.body, rhs.body); }

  let plus lhs rhs =
    binop_with_coerce Bil.plus lhs rhs

  let minus lhs rhs =
    binop_with_coerce Bil.minus lhs rhs

  let concat lhs rhs =
    let sign = derive_sign lhs.sign rhs.sign in
    let width = lhs.width + rhs.width in
    let body = Concat (lhs.body, [rhs.body]) in
    { sign; width; body; }

  let lt lhs rhs = binop_with_coerce Bil.lt lhs rhs
  let gt lhs rhs = binop_with_coerce Bil.lt rhs lhs
  let eq lhs rhs = binop_with_coerce Bil.eq rhs lhs
  let lshift lhs rhs = binop Bil.lshift lhs rhs
  let rshift lhs rhs = binop Bil.rshift lhs rhs

  module Infix = struct
    let (+)  = plus
    let (-)  = minus
    let (^)  = concat
    let (<)  = lt
    let (>)  = gt
    let (=)  = eq
    let (lsl) = lshift
    let (lsr) = rshift
  end

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

  let body e = of_body e.body

end

open Exp

let bil_of_t = List.concat

let coerce x width sign =
  if sign = x.sign && width = x.width then of_body x.body
  else
    match sign with
    | Unsigned -> Bil.(cast unsigned width (of_body x.body))
    | Signed -> Bil.(cast signed width (of_body x.body))

let store mem addr data endian size =
  Bil.[mem := store (var mem) (of_body addr.body) (of_body data.body) endian size]

let move lhs rhs =
  match lhs.body with
  | Var v ->
    let rhs = coerce rhs lhs.width lhs.sign in
    Bil.[v := rhs]
  | _ -> ppc_fail "unexpected left side of :="

module Infix = struct
  let (:=) = move
end
