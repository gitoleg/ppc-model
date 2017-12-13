open Core_kernel.Std
open Bap.Std

open Powerpc_rtl
open Powerpc_dsl
open Powerpc_utils

module Model = Powerpc_model
open Model.Hardware

type cpu = {
  load      : exp -> bitwidth -> exp;
  store     : exp -> exp -> bitwidth -> rtl;
  jmp       : exp -> rtl;
  addr      : exp;
  addr_size : bitwidth;
  gpr       : int -> exp;
  fpr       : int -> exp;
  vr        : int -> exp;
  xer       : exp;
  ctr       : exp;
  lr        : exp;
  tar       : exp;
  cr        : exp;
  cr0       : exp;
  cr1       : exp;
  cr2       : exp;
  cr3       : exp;
  cr4       : exp;
  cr5       : exp;
  cr6       : exp;
  cr7       : exp;
  so        : exp;
  ca        : exp;
  ov        : exp;
  ca32      : exp;
  ov32      : exp;
}

let size_of_width x =
  let x = int_of_bitwidth x in
  match Size.of_int x with
  | Ok s -> s
  | Error _ -> ppc_fail "invalid size: %d" x

let make_cpu addr_size endian memory =
  let extract_addr a = match addr_size with
    | `r32 -> low word a
    | `r64 -> a in
  let mem,addr_size = match addr_size with
    | `r32 -> Model.mem32, word
    | `r64 -> Model.mem64, doubleword in
  let load exp width =
    let size = size_of_width width in
    let addr = extract_addr exp in
    Exp.load mem addr endian size in
  let store addr data width =
    let size = size_of_width width in
    let addr = extract_addr addr in
    store mem addr data endian size in
  let addr = Exp.of_word @@ Memory.min_addr memory in
  let jmp e = jmp (low addr_size e) in
  let find name regs n =
    try
      Int.Map.find_exn regs n
    with _ ->
      ppc_fail "%s with number %d not found" name n in
  let gpr n = find "GPR" gpri n in
  let fpr n = find "FPR" fpri n in
  let vr  n = find "VR" vri n in
  let cr0 = Int.Map.find_exn cri_fields 0 in
  let cr1 = Int.Map.find_exn cri_fields 1 in
  let cr2 = Int.Map.find_exn cri_fields 2 in
  let cr3 = Int.Map.find_exn cri_fields 3 in
  let cr4 = Int.Map.find_exn cri_fields 4 in
  let cr5 = Int.Map.find_exn cri_fields 5 in
  let cr6 = Int.Map.find_exn cri_fields 6 in
  let cr7 = Int.Map.find_exn cri_fields 7 in
  { load; store; jmp; addr; addr_size;
    gpr; fpr; vr;
    cr; cr0; cr1; cr2; cr3; cr4; cr5; cr6; cr7;
    xer; ctr; lr; tar;
    so; ca; ov; ca32; ov32; }
