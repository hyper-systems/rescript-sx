let fmt = Printf.sprintf

type output = { global : string list; properties : string list }

type property = { name : string; rules : string list }

let opacity =
  let gen ~id ~value =
    {
      name = "opacity-" ^ string_of_int id;
      rules = [ "opacity: " ^ string_of_float value ];
    }
  in
  List.map
    (fun (id, value) -> gen ~id ~value)
    [
      (0, 0.0);
      (10, 0.1);
      (20, 0.2);
      (25, 0.25);
      (30, 0.3);
      (40, 0.4);
      (50, 0.5);
      (60, 0.6);
      (70, 0.7);
      (75, 0.75);
      (80, 0.8);
      (90, 0.9);
      (95, 0.95);
      (100, 1.0);
    ]


let ring =
  let gen ~id =
    [
      {
        name = "ring-" ^ string_of_int id;
        rules =
          [
            fmt
              "box-shadow: var(--tw-ring-inset) 0 0 0 calc(%dpx + \
               var(--tw-ring-offset-width)) var(--tw-ring-color);"
              id;
          ];
      };
    ]
  in
  List.concat_map (fun id -> gen ~id) [ 0; 1; 2; 3; 4; 8 ]


let ring_insert =
  [ { name = "ring-inset"; rules = [ "--tw-ring-inset: inset;" ] } ]


let ring_offset =
  let gen ~id =
    {
      name = "ring-offset-" ^ string_of_int id;
      rules =
        [
          fmt "--tw-ring-offset-width: %dpx;" id;
          "box-shadow: 0 0 0 var(--tw-ring-offset-width) \
           var(--tw-ring-offset-color), var(--tw-ring-shadow);";
        ];
    }
  in

  List.map (fun id -> gen ~id) [ 0; 1; 2; 3; 4; 8 ]


type color = Rgb of (int * int * int) | Current | Transparent

let ring_color ~colors =
  let gen ~name ~color =
    match color with
    | Transparent ->
      { name = "ring-" ^ name; rules = [ "--tw-ring-color: transparent;" ] }
    | Current ->
      { name = "ring-" ^ name; rules = [ "--tw-ring-color: currentColor;" ] }
    | Rgb (r, g, b) ->
      {
        name = "ring-" ^ name;
        rules =
          [
            fmt "--tw-ring-color: rgba(%d, %d, %d, var(--tw-ring-opacity));" r g
              b;
          ];
      }
  in
  List.map (fun (name, color) -> gen ~name ~color) colors
