let naloga_dan = 17


type steam_push =
    | PushLeft
    | PushRight


let parse_line line =
    let directions_chars = line |> String.to_seq |> List.of_seq |> List.filter (( <> ) '\n')
    in
    let dir_char_to_dir char =
        match char with
        | '<' -> PushLeft
        | '>' -> PushRight
        | _ -> failwith "Found bad char in directions string."
    in
    List.map dir_char_to_dir directions_chars


type point = int * int

type vector = Vector of point

let ( ++ ) point1 point2 =
    (fst point1 + fst point2, snd point1 + snd point2)

let shift_point point (Vector vector) =
    point ++ vector

type rock = RockComponents of (vector list)

let rocks = [
    RockComponents [Vector (0, 0); Vector (1, 0); Vector (2, 0); Vector (3, 0)];
    RockComponents [Vector (1, 0); Vector (0, 1); Vector (1, 1); Vector (2, 1); Vector (1, 2)];
    RockComponents [Vector (0, 0); Vector (1, 0); Vector (2, 0); Vector (2, 1); Vector (2, 2)];
    RockComponents [Vector (0, 0); Vector (0, 1); Vector (0, 2); Vector (0, 3)];
    RockComponents [Vector (0, 0); Vector (1, 0); Vector (0, 1); Vector (1, 1)]
]

let rock_name rock =
    match rock with
    | RockComponents [Vector (0, 0); Vector (1, 0); Vector (2, 0); Vector (3, 0)] -> "-"
    | RockComponents [Vector (1, 0); Vector (0, 1); Vector (1, 1); Vector (2, 1); Vector (1, 2)] -> "+"
    | RockComponents [Vector (0, 0); Vector (1, 0); Vector (2, 0); Vector (2, 1); Vector (2, 2)] -> "mirror L"
    | RockComponents [Vector (0, 0); Vector (0, 1); Vector (0, 2); Vector (0, 3)] -> "|"
    | RockComponents [Vector (0, 0); Vector (1, 0); Vector (0, 1); Vector (1, 1)] -> "square"
    | _ -> "unknown"

let get_modulo list i =
    List.nth list (i mod (List.length list))

let get_rock_height (RockComponents rock) =
    let dys = List.map (fun (Vector (x, y)) -> y + 1) rock
    in
    List.fold_left max 0 dys

let chamber_width = 7

type tile =
    | Empty
    | Rock
    | Wall

let get_from_grid grid (x, y) =
    if (y < 0) || (y >= Array.length grid) then
        Wall
    else if (x < 0) || (x >= Array.length grid.(y)) then
        Wall
    else
        grid.(y).(x)

let rock_intersects grid (RockComponents components) rock_point =
    (* No components of the rock are on full tiles (and is not intersecting the wall of the chamber). *)
        List.exists
            (fun (Vector diff) -> get_from_grid grid (rock_point ++ diff) <> Empty)
            components


let place_rock_on_grid grid (RockComponents rock) (x, y) =
    if rock_intersects grid (RockComponents rock) (x, y) then
        failwith "Can't place an intersecting rock on the grid."
    else
        List.iter
            (fun (Vector (dx, dy)) -> grid.(y + dy).(x + dx) <- Rock)
            rock


let drop_rock_update_grid directions grid tallest_rock rock time =
    let height = get_rock_height rock
    in
    let starting_point = (2, tallest_rock + 4)
    in
    let rec get_resting_pos directions grid rock point time =
        let dx = match get_modulo directions time with
            | PushLeft -> -1
            | PushRight -> 1
        in
        let after_steam =
            if rock_intersects grid rock (point ++ (dx, 0)) then
                point
            else
                point ++ (dx, 0)
        in
        let after_fall = after_steam ++ (0, -1)
        in
        let time = time + 1
        in
        if rock_intersects grid rock after_fall then
            after_steam, time (* if a rock intersects something after falling it has come to rest. *)
        else
            get_resting_pos directions grid rock after_fall time
    in
    let final_location, new_time = get_resting_pos directions grid rock starting_point time
    in
    let () = place_rock_on_grid grid rock final_location
    in
    (* the y-value of the upper left corner of the block is the new tallest block,
     unless the current tallest block is taller *)
    new_time, max (snd final_location + height - 1) tallest_rock


let rec drop_rocks grid directions rocks_left rocks_list tallest_rock rock_i time =
    if rocks_left <= 0 then tallest_rock + 1 else
    let rock = get_modulo rocks_list rock_i
    in
    let new_time, new_tallest = drop_rock_update_grid directions grid tallest_rock rock time
    in
    drop_rocks grid directions (rocks_left - 1) rocks_list new_tallest (rock_i + 1) new_time

let print_grid grid =
    let grid = grid |> Array.to_list |> List.rev |> Array.of_list
    in
    let print_tile tile =
        print_string (
            match tile with
            | Wall -> "|"
            | Rock -> "R"
            | Empty -> "."
        )
    in
    let print_line line =
        if Array.for_all (( = ) Empty) line then () else
            let () = print_string "|"
            in
            let () = Array.iter print_tile line
            in
            print_string "|\n"
    in
    Array.iter print_line grid;
    print_string "+-------+\n"


let naloga1 vsebina_datoteke =
    let directions = parse_line vsebina_datoteke
    in
    let heights = List.map get_rock_height rocks
    in
    let rec get_max_height amount values =
        if amount <= 0 then 0 else
        (get_modulo values (amount - 1)) + get_max_height (amount - 1) values
    in
    let max_height = get_max_height 2022 heights
    in
    let grid = Array.make_matrix max_height chamber_width Empty
    in
    let tallest_rock = -1 (* the floor is the tallest rock. *)
    in
    drop_rocks grid directions 2022 rocks tallest_rock 0 0 |> string_of_int


let window_height = 1000000

let naloga2 vsebina_datoteke =
    "IDK"


let preberi_datoteko ime_datoteke =
    let chan = open_in ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina

let _ =
    let izpisi_datoteko ime_datoteke vsebina =
        let chan = open_out ime_datoteke in
        output_string chan vsebina;
        close_out chan
    in
    let vsebina_datoteke = preberi_datoteko ( Printf.sprintf "dan_%d.in" naloga_dan ) in
    let odgovor1 = (naloga1 vsebina_datoteke) ^ "\n"
    and odgovor2 = (naloga2 vsebina_datoteke) ^ "\n"
    in
    izpisi_datoteko ( Printf.sprintf "dan_%d_1.out" naloga_dan ) odgovor1;
    izpisi_datoteko ( Printf.sprintf "dan_%d_2.out" naloga_dan ) odgovor2

