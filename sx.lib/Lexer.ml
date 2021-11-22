type 'a fmt = Format.formatter -> 'a -> unit

type token =
  [ `Utility of string
  | `Content of string
  | `Variant of string
  | `Utility_group_start of string
  | `Variant_group_start of string
  | `Group_end ]

let pp : token fmt =
 fun ppf token ->
  let str = Format.pp_print_string ppf in
  match token with
  | `Utility x -> str x
  | `Content x -> str ("content-\\[" ^ x ^ "\\]")
  | `Variant x -> str (x ^ ":")
  | `Utility_group_start x -> str (x ^ "(")
  | `Variant_group_start x -> str (x ^ ":(")
  | `Group_end -> str ")"


let variant = [%sedlex.regexp? Plus (lowercase | '-' | '0' .. '9'), ':']

let utility = [%sedlex.regexp? Plus (lowercase | '-' | '0' .. '9')]
let content = [%sedlex.regexp? Compl ']']

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
    | "content-[", Plus (Compl ']'), ']' ->
      let lexeme = Sedlexing.Latin1.lexeme buf in
      let content = String.sub lexeme 9 (String.length lexeme - 10) in
      loop (`Content content :: acc)
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