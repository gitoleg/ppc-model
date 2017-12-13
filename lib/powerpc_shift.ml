open Powerpc

(** Fix-point Shift Left Word
    Page 107 of IBM Power ISATM Version 3.0 B
    example:
    7d 2a 58 30     slw r10, r9, r11 *)
let slw cpu ops =
  let ra = unsigned reg ops.(0) in
  let rs = unsigned reg ops.(1) in
  let rb = unsigned reg ops.(2) in
  RTL.[
    if_ (nth bit rb 58 = zero) [
      ra := low word rs lsl last rb 5;
    ] [
      ra := zero;
    ]
  ]

(** Fix-point Shift Right Word
    Page 107 of IBM Power ISATM Version 3.0 B
    example:
    7d 2a 5c 30     srw r10, r9, r11 *)
let srw cpu ops =
  let ra = unsigned reg ops.(0) in
  let rs = unsigned reg ops.(1) in
  let rb = unsigned reg ops.(2) in
  RTL.[
    if_ (nth bit rb 58 = zero) [
      ra := low word rs lsr last rb 5;
    ] [
      ra := zero;
    ]
  ]

(** Fix-point Shift Right Algebraic Word Immediate
    Page 108 of IBM Power ISATM Version 3.0 B
    example:
    7d 2a 5e 70     srawi r10, r9, 11 *)
let srawi cpu ops =
  let ra = unsigned reg ops.(0) in
  let rs = unsigned reg ops.(1) in
  let sh = unsigned imm ops.(2) in
  let mask = unsigned var doubleword in
  let width = unsigned const doubleword 32 in
  let carry_ones = unsigned var bit in
  RTL.[
    mask := zero;
    mask := lnot mask;
    carry_ones := ((lnot (mask lsl sh)) land rs) <> zero;
    cpu.ca := carry_ones land (low word rs <$ zero);
    cpu.ca32 := cpu.ca;
    if_ (low word rs >=$ zero) [
      ra := low word rs lsr sh;
    ] [
      mask := mask lsr (width + sh);
      ra := (low word rs lsr sh) lor (lnot mask);
    ]
  ]

(** Fix-point Shift Right Algebraic Word
    Page 108 of IBM Power ISATM Version 3.0 B
    example:
    7d 2a 5e 30     sraw r10, r9, r11 *)
let sraw cpu ops =
  let ra = unsigned reg ops.(0) in
  let rs = unsigned reg ops.(1) in
  let rb = unsigned reg ops.(2) in
  let mask = unsigned var doubleword in
  let width = unsigned const doubleword 32 in
  let carry_ones = unsigned var bit in
  let s = unsigned var bit in
  let shift = unsigned var byte in
  RTL.[
    mask := zero;
    shift := last rb 6;
    when_ (nth bit rb 58 = zero) [
      mask := (lnot mask) lsl shift;
    ];
    s := low word rs <$ zero;
    carry_ones := ((lnot mask) land rs) <> zero;
    cpu.ca   := carry_ones land s;
    cpu.ca32 := cpu.ca;
    if_ (s = zero) [
      ra := low word rs lsr shift;
    ] [
      mask := mask lsr (width + shift);
      ra := (low word rs lsr shift) lor (lnot mask);
    ]
  ]

(** Fix-point Shift Left Doubleword
    Page 109 of IBM Power ISATM Version 3.0 B
    example:
    7d 2a 58 36     sld r10, r9, r11 *)
let sld cpu ops =
  let ra = unsigned reg ops.(0) in
  let rs = unsigned reg ops.(1) in
  let rb = unsigned reg ops.(2) in
  RTL.[
    ra := rs lsl (last rb 6)
  ]

(** Fix-point Shift Right Doubleword
    Page 109 of IBM Power ISATM Version 3.0 B
    example:
    7d 2a 5c 35     srd r10, r9, r11 *)
let srd cpu ops =
  let ra = unsigned reg ops.(0) in
  let rs = unsigned reg ops.(1) in
  let rb = unsigned reg ops.(2) in
  RTL.[
    ra := rs lsr (last rb 6)
  ]


(** Fix-point Shift Right Algebraic Doubleword Immediate
    Page 110 of IBM Power ISATM Version 3.0 B
    example:
    7d 2a 26 74     sradi r10, r9, 4 *)
let sradi cpu ops =
  let ra = unsigned reg ops.(0) in
  let rs = unsigned reg ops.(1) in
  let sh = unsigned imm ops.(2) in
  let mask = unsigned var doubleword in
  let carry_ones = unsigned var bit in
  RTL.[
    mask := zero;
    mask := lnot mask;
    carry_ones := ((lnot (mask lsl sh)) land rs) <> zero;
    cpu.ca   := carry_ones land (rs <$ zero);
    cpu.ca32 := cpu.ca;
    if_ (rs >=$ zero) [
      ra := rs lsr sh;
    ] [
      mask := mask lsr sh;
      ra := (rs lsr sh) lor (lnot mask);
    ]
  ]

(** Fix-point Shift Right Algebraic Doubleword
    Page 110 of IBM Power ISATM Version 3.0 B
    example:
    7d 2a 5e 34     srad r10, r9, r11 *)
let srad cpu ops =
  let ra = unsigned reg ops.(0) in
  let rs = unsigned reg ops.(1) in
  let rb = unsigned reg ops.(2) in
  let mask = unsigned var doubleword in
  let carry_ones = unsigned var bit in
  let s = unsigned var bit in
  let shift = unsigned var byte in
  RTL.[
    mask := zero;
    shift := last rb 7;
    when_ (nth bit rb 57 = zero) [
      mask := (lnot mask) lsl shift;
    ];
    s := rs <$ zero;
    carry_ones := ((lnot mask) land rs) <> zero;
    cpu.ca   := carry_ones land s;
    cpu.ca32 := cpu.ca;
    if_ (s = zero) [
      ra := rs lsr shift;
    ] [
      mask := mask lsr shift;
      ra := (rs lsr shift) lor (lnot mask);
    ]
  ]

(** TODO: add *_dot instructions *)
let () =
  "SLW"   >: slw;
  "SRW"   >: srw;
  "SRAWI" >: srawi;
  "SRAW"  >: sraw;
  "SLD"   >: sld;
  "SRD"   >: srd;
  "SRADI" >: sradi;
  "SRAD"  >: srad;
