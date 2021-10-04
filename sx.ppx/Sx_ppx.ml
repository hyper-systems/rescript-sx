module Driver = Ppxlib.Driver
module Extension = Ppxlib.Extension
module Expansion_context = Ppxlib.Expansion_context
module Ast_pattern = Ppxlib.Ast_pattern
module Ast_builder = Ppxlib.Ast_builder
module Code_path = Ppxlib.Code_path

let _output_path = ref "sx.css"
let output_path () = Fpath.(v ("../../" ^ !_output_path))

let tailwind_path = ref "/tmp/tailwind.json"

let tailwind = lazy (Sx.of_file !tailwind_path)

let error ~loc msg = Location.raise_errorf ~loc "%s" msg

(* TODO: Get the module name and delete cache if the ppx is NOT used in the module. *)
let process ~ctxt sx_class_name =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  (* TODO return normalized class name!!! *)
  let sx_class_name, css =
    try Sx.process (Lazy.force tailwind) sx_class_name
    with Failure msg -> error ~loc msg
  in
  Sx.update_global_css css;
  Ast_builder.Default.estring ~loc sx_class_name


let my_extension =
  Extension.V3.declare "sx" Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    process


let rule = Ppxlib.Context_free.Rule.extension my_extension

let () =
  Ppxlib.Driver.add_arg "--output"
    (Arg.String (fun key -> _output_path := key))
    ~doc:"<file> Where the generated CSS should be saved.";
  Driver.register_transformation ~rules:[ rule ] "sx"
