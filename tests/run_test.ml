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
    Ppc_typecheck_test.suite;
    Ppc_lift_test.suite;
  ]

let () = run_test_tt_main suite