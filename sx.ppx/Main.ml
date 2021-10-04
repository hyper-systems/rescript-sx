let () =
  let t0 = Unix.gettimeofday () in
  Stdlib.at_exit (fun () ->
      let lib_bs = Sys.getcwd () in
      let input_name = !Location.input_name in
      Sx.write_module_cache ~lib_bs ~input_name;
      Printf.eprintf "sx: Processed module %s in %0.4fs\n" input_name
        (Unix.gettimeofday () -. t0));
  Ppxlib.Driver.run_as_ppx_rewriter ()
