open Core_kernel.Std
open Bap.Std

module DSL = struct

  type cast = Bil.cast

  type exp =
    | BinOp   of binop * exp * exp  (** binary operation  *)
    | UnOp    of unop * exp         (** unary operation *)
    | Var     of var                (** variable *)
    | Int     of word               (** immediate value *)
    | Cast    of cast * int * exp   (** casting  *)
    | Unknown of string * typ       (** unknown or undefined value *)
    | Extract of int * int * exp    (** extract portion of word  *)
    | Concat  of exp * exp          (** concatenate two words  *)

  type stmt =
    | Move    of var * exp  (** assign value of expression to variable *)
    | Jmp     of exp        (** jump to absolute address *)
    | Special of string     (** Statement with semantics not expressible in BIL *)
    | If      of exp * stmt list * stmt list (** if/then/else statement  *)
    | CpuExn  of int                         (** CPU exception *)
    | Load of addr * endian * size
    | Store of addr * word * endian * size

  include Bil.Infix

  let cast = Bil.cast
  let var = Bil.var
  let unsigned = Bil.unsigned
  let signed = Bil.signed
  let high = Bil.high
  let low = Bil.low
  let int = Bil.int
  let extract = Bil.extract
  let concat = Bil.concat
end

open Ppc_model

let find_var vars name =
  Var.Set.find vars
    ~f:(fun v -> String.equal name (Var.name v))

let find_register regs reg =
  match find_var regs (Reg.name reg) with
  | None ->
    (Or_error.errorf "unknown register %s" (Reg.name reg))
  | Some reg -> Ok reg

let find_register_exn regs reg =
  Or_error.ok_exn (find_register regs reg)

let find_gpr reg = find_register_exn Registers.gpr reg

let find_gpr_opt reg = Result.ok (find_register Registers.gpr reg)

let load32 addr size endian =
  Bil.(load ~mem:(var PPC32.mem) ~addr size endian)

let load64 addr size endian =
  Bil.(load ~mem:(var PPC64.mem) ~addr size endian)
