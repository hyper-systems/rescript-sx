type ('token, 'a) t = 'token list -> ('a * 'token list) option

let ( let* ) (p : ('token, 'a) t) (f : 'a -> ('token, 'b) t) : ('token, 'b) t =
 fun input ->
  match p input with
  | Some (x', input) -> f x' input
  | None -> None


let empty _ = None

let return x input = Some (x, input)

let map p f input =
  match p input with
  | Some (x, input) -> Some (f x, input)
  | None -> None


let ( <|> ) p1 p2 input =
  match p1 input with
  | Some _ as x -> x
  | None -> p2 input


let satisfy test input =
  match input with
  | x :: input' when test x -> Some (x, input')
  | _ -> None


let option default x = x <|> return default

let rec many p =
  option []
    (let* x = p in
     let* xs = many p in
     return (x :: xs))


let run p input =
  match p input with
  | Some (x, []) -> Ok x
  | Some (x, letfover) -> Error (`With_leftover (x, letfover))
  | None -> Error `Empty
