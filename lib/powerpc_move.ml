
open Powerpc

(** Fixed-Point Move To Special Purpose Register
    Page 116 of IBM Power ISATM Version 3.0 B
    example:
    7c 20 4b a6  mtspr  9, r1 *)
let mtspr cpu ops =
  let sr = unsigned imm ops.(0) in
  let rs = unsigned reg ops.(1) in
  let tmp = unsigned var (bitwidth 10) in
  let num = unsigned var word in
  let xer_num = unsigned const word 1 in
  let lr_num  = unsigned const word 8 in
  let ctr_num = unsigned const word 9 in
  RTL.[
    tmp := sr;
    num := extract tmp 5 9 ^ extract tmp 0 4;
    switch (num) [
      case xer_num  [ cpu.xer := rs ];
      case lr_num   [ cpu.lr  := rs ];
      case ctr_num  [ cpu.ctr := rs ];
    ];
  ]

(** Fixed-Point Move From Special Purpose Register
    Page 116 of IBM Power ISATM Version 3.0 B
    example:
    7c 20 42 a6  mfspr  r1,8 *)
let mfspr cpu ops =
  let rt = unsigned reg ops.(0) in
  let sr = unsigned imm ops.(1) in
  let tmp = unsigned var (bitwidth 10) in
  let num = unsigned var word in
  let xer_num = unsigned const word 1 in
  let lr_num  = unsigned const word 8 in
  let ctr_num = unsigned const word 9 in
  RTL.[
    tmp := sr;
    num := extract tmp 5 9 ^ extract tmp 0 4;
    switch (num) [
      case xer_num [ rt := cpu.xer ];
      case lr_num  [ rt := cpu.lr  ];
      case ctr_num [ rt := cpu.ctr ];
    ];
  ]

(** Fixed-Point Move To CR Fields
    Page 121 of IBM Power ISATM Version 3.0 B
    example:
    7c 20 81 20  mtcrf  8, r1 *)
let mtcrf cpu ops =
  let fx = unsigned imm ops.(0) in
  let rs = unsigned reg ops.(1) in
  let mask = unsigned var word in
  let bit_i = unsigned var bit in
  let halfbyte_i = unsigned var (bitwidth 4) in
  let ones = unsigned const (bitwidth 4) 0xF in
  let ind = unsigned var byte in
  let fxm = unsigned var byte in
  RTL.[
    mask := zero;
    ind := zero;
    fxm := fx;
    foreach halfbyte_i mask [
      bit_i := msb (fxm lsl ind);
      if_ (bit_i = one) [
        halfbyte_i := ones;
      ] [
        halfbyte_i := zero;
      ];
      ind := ind + one;
    ];
    cpu.cr := (low word rs land mask) lor (cpu.cr land (lnot mask));
  ]

(** Fixed-Point Move From CR
    Page 122 of IBM Power ISATM Version 3.0 B
    example:
    7c 20 00 26  mfcr  r1 *)
let mfcr cpu ops =
  let rt = unsigned reg ops.(0) in
  RTL.[ rt := cpu.cr ]

let () =
  "MTSPR" >> mtspr;
  "MFSPR" >> mfspr;
  "MTCRF" >> mtcrf;
  "MFCR"  >> mfcr;
