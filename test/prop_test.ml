open Core.Std
open Quickcheck

let%test_module "bool" =
    (module struct
      let sexp_of = Bool.sexp_of_t
      let gen = Bool.gen
      let can_generate f = test_can_generate gen ~sexp_of ~f ~trials:1000
      let%test_unit _ = can_generate (fun x -> x = true)
      let%test_unit _ = can_generate (fun x -> x = false)
    end)

(* let () = Ppx_inline_test_lib.Runtime.exit () *)
