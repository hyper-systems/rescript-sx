module String_map = Map.Make (String)
module String_set = Set.Make (String)

module Css = struct
  type t = {
    global : string list String_map.t;
    sm : string list String_map.t;
    md : string list String_map.t;
    lg : string list String_map.t;
    xl : string list String_map.t;
    xl2 : string list String_map.t;
  }

  let empty =
    {
      global = String_map.empty;
      sm = String_map.empty;
      md = String_map.empty;
      lg = String_map.empty;
      xl = String_map.empty;
      xl2 = String_map.empty;
    }


  let is_empty self =
    String_map.is_empty self.global
    && String_map.is_empty self.sm
    && String_map.is_empty self.md
    && String_map.is_empty self.lg
    && String_map.is_empty self.xl
    && String_map.is_empty self.xl2


  let add_to_scope scope_id ~selector ~properties self =
    let add = String_map.add selector properties in
    match scope_id with
    | None -> { self with global = add self.global }
    | Some `sm -> { self with sm = add self.sm }
    | Some `md -> { self with md = add self.md }
    | Some `lg -> { self with lg = add self.lg }
    | Some `xl -> { self with xl = add self.xl }
    | Some `xl2 -> { self with xl2 = add self.xl2 }


  let pp_scope formatter =
    let pp_binding formatter (key, rules) =
      Format.fprintf formatter "@[<v2>.%s {@,%a@]@,}" key
        (Format.pp_print_list ~pp_sep:Format.pp_print_cut Format.pp_print_string)
        rules
    in
    Fmt.pf formatter "@[<v>%a@]"
      (Fmt.iter_bindings ~sep:Fmt.cut String_map.iter pp_binding)


  let pp formatter self =
    let pp_media size =
      Format.fprintf formatter "@[<v2>@@media (min-width: %dpx) {@,%a@]@,}@.@."
        size pp_scope
    in

    if not (String_map.is_empty self.global) then
      Format.fprintf formatter "%a@.@." pp_scope self.global;

    if not (String_map.is_empty self.sm) then pp_media 640 self.sm;

    if not (String_map.is_empty self.md) then pp_media 768 self.md;

    if not (String_map.is_empty self.lg) then pp_media 1024 self.lg;

    if not (String_map.is_empty self.xl) then pp_media 1280 self.xl;

    if not (String_map.is_empty self.xl2) then pp_media 1536 self.xl2


  let union self other =
    let combine _selector self_properties _other_properties =
      Some self_properties
    in
    {
      global = String_map.union combine self.global other.global;
      sm = String_map.union combine self.sm other.sm;
      md = String_map.union combine self.md other.md;
      lg = String_map.union combine self.lg other.lg;
      xl = String_map.union combine self.xl other.xl;
      xl2 = String_map.union combine self.xl2 other.xl2;
    }
end

let responsive = [ "sm"; "md"; "lg"; "xl"; "2xl" ]

let pseudo_class_variants =
  String_set.of_list
    [
      "hover";
      "focus";
      "active";
      "focus-within";
      "focus-visible";
      "motion-safe";
      "disabled";
      "visited";
      "checked";
      "first";
      "last";
      "odd";
      "even";
    ]


let list_concat_map f l =
  let rec aux f acc = function
    | [] -> List.rev acc
    | x :: l ->
      let xs = f x in
      aux f (List.rev_append xs acc) l
  in
  aux f [] l


type t = string list String_map.t

type 'a iter = ('a -> unit) -> unit

type 'a fmt = Format.formatter -> 'a -> unit

module Lexer = struct
  type token =
    [ `Utility of string
    | `Variant of string
    | `Utility_group_start of string
    | `Variant_group_start of string
    | `Group_end ]

  let pp : token fmt =
   fun ppf token ->
    let str = Format.pp_print_string ppf in
    match token with
    | `Utility x -> str x
    | `Variant x -> str (x ^ ":")
    | `Utility_group_start x -> str (x ^ "(")
    | `Variant_group_start x -> str (x ^ ":(")
    | `Group_end -> str ")"


  let variant = [%sedlex.regexp? Plus (lowercase | '-' | '0' .. '9'), ':']

  let utility = [%sedlex.regexp? Plus (lowercase | '-' | '0' .. '9')]

  type t = { mutable buf : Sedlexing.lexbuf; mutable group_count : int }

  let token self : token list =
    let buf = self.buf in
    let rec loop acc =
      match%sedlex buf with
      | Plus (' ' | '\t' | '\n') -> loop acc
      | variant ->
        let lexeme = Sedlexing.Latin1.lexeme buf in
        let variant = String.sub lexeme 0 (String.length lexeme - 1) in
        loop (`Variant variant :: acc)
      | utility -> loop (`Utility (Sedlexing.Latin1.lexeme buf) :: acc)
      | '(' -> failwith "Unexpected '('"
      | variant, '(' ->
        self.group_count <- self.group_count + 1;
        let lexeme = Sedlexing.Latin1.lexeme buf in
        let variant = String.sub lexeme 0 (String.length lexeme - 2) in
        loop (`Variant_group_start variant :: acc)
      | utility, '(' ->
        self.group_count <- self.group_count + 1;
        let lexeme = Sedlexing.Latin1.lexeme buf in
        let utility = String.sub lexeme 0 (String.length lexeme - 1) in
        loop (`Utility_group_start utility :: acc)
      | ')' ->
        self.group_count <- self.group_count - 1;
        if self.group_count < 0 then failwith "sx: Unexpected ')'"
        else loop (`Group_end :: acc)
      | eof ->
        if self.group_count <> 0 then failwith "sx: Missing ')'";
        acc
      | any ->
        Fmt.failwith "sx: Unexpected character: '%s'"
          (Sedlexing.Latin1.lexeme buf)
      | _ -> failwith "sx: Unexpected input"
    in
    List.rev (loop [])


  let read input =
    let buf = Sedlexing.Latin1.from_string input in
    let self = { buf; group_count = 0 } in
    token self
end

type attribute =
  [ `Utility of string
  | `Variant of string * attribute
  | `Utility_group of string * attribute list
  | `Variant_group of string * attribute list ]

let add_utility_prefix ~prefix name =
  match prefix with
  | [] -> name
  | prefix -> String.concat "-" prefix ^ "-" ^ name


type canonical = {
  scope : [ `sm | `md | `lg | `xl | `xl2 ] option;
  variants : string list;
  utility : string;
}

let process_variants input =
  let rec loop input scope_acc variants_acc =
    match input with
    | [] -> (scope_acc, variants_acc)
    | "sm" :: input -> loop input (`sm :: scope_acc) variants_acc
    | "md" :: input -> loop input (`md :: scope_acc) variants_acc
    | "lg" :: input -> loop input (`lg :: scope_acc) variants_acc
    | "xl" :: input -> loop input (`xl :: scope_acc) variants_acc
    | "2xl" :: input -> loop input (`xl2 :: scope_acc) variants_acc
    | other :: input
      when String_set.exists (String.equal other) pseudo_class_variants ->
      loop input scope_acc (other :: variants_acc)
    | unknown :: _ -> raise (Failure ("sx: Unknown variant name: " ^ unknown))
  in
  match loop input [] [] with
  | [], variants -> (None, variants)
  | [ responsive ], variants -> (Some responsive, variants)
  | _ -> raise (Failure "sx: Too many responsive variants")


let ungroup (attr : attribute) =
  let rec loop ~utility_prefix ~variant_prefix attr =
    match attr with
    | `Utility name ->
      let scope, variants = process_variants variant_prefix in
      let utility = add_utility_prefix ~prefix:(List.rev utility_prefix) name in
      [ { scope; variants; utility } ]
    | `Variant (name, attr) ->
      let variant_prefix = name :: variant_prefix in
      loop ~utility_prefix ~variant_prefix attr
    | `Utility_group (name, attrs) ->
      list_concat_map
        (loop ~variant_prefix ~utility_prefix:(name :: utility_prefix))
        attrs
    | `Variant_group (name, attrs) ->
      list_concat_map
        (loop ~variant_prefix:(name :: variant_prefix) ~utility_prefix)
        attrs
  in
  loop ~utility_prefix:[] ~variant_prefix:[] attr


let rec pp_many formatter = Fmt.hbox (Fmt.list ~sep:Fmt.sp pp) formatter

and pp formatter attr =
  let str = Fmt.string formatter in
  match attr with
  | `Utility name -> str name
  | `Variant (name, attr) ->
    str (name ^ ":");
    pp formatter attr
  | `Utility_group (name, attrs) ->
    str (name ^ "(");
    pp_many formatter attrs;
    str ")"
  | `Variant_group (name, attrs) ->
    str (name ^ ":(");
    pp_many formatter attrs;
    str ")"


module Utility_parser = struct
  let ( let* ) = Parser.( let* )

  let ( <|> ) = Parser.( <|> )

  let rec attribute () =
    utility <|> variant () <|> utility_proup () <|> variant_proup ()


  and utility : (Lexer.token, attribute) Parser.t =
   fun input ->
    match input with
    | `Utility x :: input -> Some (`Utility x, input)
    | _ -> None


  and variant_name : (Lexer.token, string) Parser.t =
   fun input ->
    match input with
    | `Variant x :: input -> Some (x, input)
    | _ -> None


  and variant () : (Lexer.token, attribute) Parser.t =
    let* name = variant_name in
    let* rest = attribute () in
    Parser.return (`Variant (name, rest))


  and group_end input =
    match input with
    | `Group_end :: input -> Some ((), input)
    | _ -> None


  and utility_group_name input =
    match input with
    | `Utility_group_start x :: input -> Some (x, input)
    | _ -> None


  and variant_group_name input =
    match input with
    | `Variant_group_start x :: input -> Some (x, input)
    | _ -> None


  and utility_proup () : (Lexer.token, attribute) Parser.t =
    let* name = utility_group_name in
    let* items = Parser.many (attribute ()) in
    let* () = group_end in
    Parser.return (`Utility_group (name, items))


  and variant_proup () : (Lexer.token, attribute) Parser.t =
    let* name = variant_group_name in
    let* items = Parser.many (attribute ()) in
    let* () = group_end in
    Parser.return (`Variant_group (name, items))
end

let of_file path =
  let json = Yojson.Safe.from_file path in
  let items = Yojson.Safe.Util.to_assoc json in
  let add_item acc (key, item) =
    let rules =
      Yojson.Safe.Util.to_list item |> List.map Yojson.Safe.Util.to_string
    in
    String_map.add key rules acc
  in
  List.fold_left add_item String_map.empty items


let string_of_scope scope =
  match scope with
  | `sm -> "sm"
  | `md -> "md"
  | `lg -> "lg"
  | `xl -> "xl"
  | `xl2 -> "2xl"


let make_class_name ~scope ~variants ~utility =
  match (scope, variants) with
  | None, [] -> utility
  | Some scope, [] -> string_of_scope scope ^ ":" ^ utility
  | _ ->
    let selector_prefix =
      Option.fold scope ~none:variants ~some:(fun scope ->
          string_of_scope scope :: variants)
    in
    String.concat ":" selector_prefix ^ ":" ^ utility


let make_selector_name ~scope ~variants ~utility =
  match (scope, variants) with
  | None, [] -> utility
  | Some scope, [] -> string_of_scope scope ^ "\\:" ^ utility
  | _ ->
    let selector_prefix =
      Option.fold scope ~none:variants ~some:(fun scope ->
          string_of_scope scope :: variants)
    in
    let name = String.concat "\\:" selector_prefix ^ "\\:" ^ utility in
    name ^ ":" ^ String.concat ":" variants


let css_of_attributes ~mapping attributes =
  let add_attribute css { scope; variants; utility } =
    let properties =
      try String_map.find utility mapping
      with Not_found ->
        raise (Failure ("Unknown tailwind utility: " ^ utility))
    in
    let selector = make_selector_name ~scope ~variants ~utility in
    Css.add_to_scope scope ~selector ~properties css
  in
  List.fold_left add_attribute Css.empty attributes


let parse class_name =
  let input = Lexer.read class_name in
  Parser.run (Parser.many (Utility_parser.attribute ())) input


module Global = struct
  let module_css = ref Css.empty
end

let process (mapping : string list String_map.t) class_name =
  match parse class_name with
  | Error (`With_leftover (_x, _leftover)) -> failwith "Leftover input"
  | Error `Empty | Ok [] -> ("", Css.empty)
  | Ok attributes ->
    let attributes = list_concat_map ungroup attributes in
    let canonical_class_list =
      List.map
        (fun { scope; variants; utility } ->
          make_class_name ~scope ~variants ~utility)
        attributes
    in
    (* Add extra spaces to avoid accidental concatenation as in:
       <hr className={%sx("mx-2") ++ "foo"} /> *)
    ( String.concat " " (" " :: canonical_class_list) ^ " ",
      css_of_attributes ~mapping attributes )


let update_global_css css =
  Global.module_css := Css.union !Global.module_css css


(* This function is registered as a [at_exit] hook and is executed when all of
   the extension analysis is complete. The result of the analysis is stored in
   a single cache file for a given input source file.

   Currently the cache file is rewritten every time.

   Upon completion we might discover that the file does not have any [%sx]
   extensions. In which case we (1) don't need to write the cache file and (2)
   need to delete any old cache files. *)
let write_module_cache ~lib_bs ~input_name =
  let sx_cache_dir =
    let root = Fpath.(v lib_bs |> parent |> parent) in
    let dir = Fpath.(root / "node_modules" / ".cache" / "sx") in
    Bos.OS.Dir.create dir |> Result.get_ok |> ignore;
    dir
  in
  let module_name = Fpath.(v input_name |> rem_ext ~multi:true |> basename) in
  let cache_file = Fpath.(sx_cache_dir / module_name |> add_ext "mldata") in
  if Css.is_empty !Global.module_css then (
    prerr_endline
      ("sx: Deleting a potential stale cache file: "
     ^ Fpath.to_string cache_file);
    Bos.OS.File.delete cache_file |> Result.get_ok)
  else (
    Fmt.epr "sx: Creating a cache file: %a@." Fpath.pp cache_file;
    let chan = open_out_bin (Fpath.to_string cache_file) in
    output_value chan !Global.module_css)


let read_module_cache path =
  let chan = open_in_bin (Fpath.to_string path) in
  let css : Css.t = input_value chan in
  css
