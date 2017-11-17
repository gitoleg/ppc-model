open Core_kernel.Std
open Bap.Std
open OUnit2

open Ppc_model.Hardware
open Ppc_tests_helpers

let typecheck bytes arch ctxt =
  let bil = get_bil arch bytes in
  assert_bool "typecheck failed" (Result.is_ok bil)

let suite = "branch" >::: [
   "b32"     >:: typecheck "\x4b\xff\xfe\xf0" `ppc;
   "ba32"    >:: typecheck "\x4b\xff\xfe\xf2" `ppc;
   "bl32"    >:: typecheck "\x4b\xff\xfe\xf1" `ppc;
   "bla32"   >:: typecheck "\x4b\xff\xfe\xf3" `ppc;
   "bc32"    >:: typecheck "\x42\x9f\x00\x04" `ppc;
   "bca32"   >:: typecheck "\x42\x9f\x00\x06" `ppc;
   "bcl32"   >:: typecheck "\x42\x9f\x00\x05" `ppc;
   "bcla32"  >:: typecheck "\x42\x9f\x00\x07" `ppc;

   "b64"     >:: typecheck "\x4b\xff\xfe\xf0" `ppc64;
   "ba64"    >:: typecheck "\x4b\xff\xfe\xf2" `ppc64;
   "bl64"    >:: typecheck "\x4b\xff\xfe\xf1" `ppc64;
   "bla64"   >:: typecheck "\x4b\xff\xfe\xf3" `ppc64;
   "bc64"    >:: typecheck "\x42\x9f\x00\x04" `ppc64;
   "bca64"   >:: typecheck "\x42\x9f\x00\x06" `ppc64;
   "bcl64"   >:: typecheck "\x42\x9f\x00\x05" `ppc64;
   "bcla64"  >:: typecheck "\x42\x9f\x00\x07" `ppc64;
  ]
