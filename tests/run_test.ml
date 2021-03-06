open Core_kernel.Std
open OUnit2
open Bap_plugins.Std

let () =
  match Plugins.load () |> Result.all with
  | Ok plugins -> ()
  | Error (path, er) ->
    Printf.eprintf "failed to load plugin from %s: %s"
      path (Error.to_string_hum er)

let suite = "PPC" >::: [
    Powerpc_add_tests.suite;
    Powerpc_arith_tests.suite;
    Powerpc_branch_tests.suite;
    Powerpc_compare_tests.suite;
    Powerpc_cr_tests.suite;
    Powerpc_dsl_tests.suite;
    Powerpc_logical_tests.suite;
    Powerpc_load_tests.suite;
    Powerpc_move_tests.suite;
    Powerpc_rotate_tests.suite;
    Powerpc_shift_tests.suite;
    Powerpc_store_tests.suite;
  ]

let () = run_test_tt_main suite
