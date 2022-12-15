let naloga_dan = 14


type point = int * int

type line = {a: point; b: point}


let make_pair text=
    let split_text = Str.split (Str.regexp ",") text
    in
    let () = assert (List.length split_text = 2)
    in
    (List.nth split_text 0 |> int_of_string, List.nth split_text 1 |> int_of_string)

let make_lines points =
    let add_line lines new_point =
        match lines with
        | [] -> failwith "add_line needs non empty list of lines to work."
        | l :: ls -> {a = l.b; b = new_point} :: lines
    in
    let () = assert (List.length points >= 2)
    in
    let first_line = {a = List.nth points 0; b = List.nth points 1}
    in
    let other_points = (List.filteri (fun i _ -> i > 1) points)
    in
    List.fold_left add_line [first_line] other_points |> List.rev

let parse_line line =
    let split_line = Str.split (Str.regexp " -> ") line
    in
    let pairs = List.map make_pair split_line
    in
    make_lines pairs

let parse_lines lines =
    let split_lines = Str.split (Str.regexp "\n") lines
    in
    let split_lines = List.filter (( <> ) "") split_lines
    in
    List.map parse_line split_lines

type bound = {lower: int; upper: int}
let update_bound {lower; upper} point = {lower = min lower point; upper = max upper point}

type bound_2d = {x: bound; y: bound}
let update_bounds bounds (x, y) =
    {x = update_bound bounds.x x; y = update_bound bounds.y y}

let get_max_bounds lines =
    let fold_bounds old_bounds new_line =
        let new_bounds = update_bounds old_bounds new_line.a
        in
        let newer_bounds = update_bounds new_bounds new_line.b
        in
        newer_bounds
    in
    let empty_bound = {lower = max_int; upper = min_int}
    in
    List.fold_left fold_bounds {x = empty_bound; y = empty_bound} lines

let move_closer (x1, y1) (x2, y2) =
    if x1 < x2 then
        (x1 + 1, y1)
    else if x1 > x2 then
        (x1 - 1, y1)
    else if y1 < y2 then
        (x1, y1 + 1)
    else if y1 > y2 then
        (x1, y1 - 1)
    else
        failwith "Cannot move identical points closer"


type tile =
    | Empty
    | Stone
    | Source
    | Sand

let sand_source = (500, 0)

let make_stones lines =
    let bounds = get_max_bounds lines
    in
    let bounds = update_bounds bounds sand_source (* source of the sand must be on grid *)
    in
    let min_x = bounds.x.lower
    and min_y = bounds.y.lower
    and width =  bounds.x.upper - bounds.x.lower + 1
    and height = bounds.y.upper - bounds.y.lower + 1
    in
    let () = Printf.printf "First part: width: %d, height: %d\n" width height
    in
    let stones = Array.make_matrix height width Empty
    in
    let rec write_line {a; b} =
        stones.(snd a - min_y).(fst a - min_x) <- Stone;
        match a = b with
        | true -> ()
        | false -> write_line {a = move_closer a b; b = b}
    in
    let _ = List.map write_line lines
    in
    stones.(0-min_y).(500-min_x) <- Source;
    stones, min_x, min_y

let print_stones stones =
    let print_stone stone =
        print_string (
            match stone with
            | Empty -> " "
            | Stone -> "#"
            | Source -> "+"
            | Sand -> "o"
        )
    in
    let print_line line =
        Array.iter print_stone line;
        print_string "\n"
    in
    Array.iter print_line stones;
    print_string "-------------\n"


type fall_result =
    | Rest
    | Void
    | OnSource

let rec falling_sand (x, y) (min_x, min_y) stones =
    if y+1 - min_y > Array.length stones - 1 then
        Void
    else if stones.(y+1 - min_y).(x - min_x) = Empty then
        falling_sand (x, y+1) (min_x, min_y) stones
    else if x-1 - min_x < 0 then
        Void
    else if stones.(y+1 - min_y).(x-1 - min_x) = Empty then
        falling_sand (x-1, y+1) (min_x, min_y) stones
    else if x+1 - min_x > Array.length stones.(y+1 - min_y) - 1 then
        Void
    else if stones.(y+1 - min_y).(x+1 - min_x) = Empty then
        falling_sand (x+1, y+1) (min_x, min_y) stones
    else
        if stones.(y - min_y).(x - min_x) = Empty then
            let () = stones.(y - min_y).(x - min_x) <- Sand
            in
            Rest
        else
            OnSource


let naloga1 vsebina_datoteke =
    let lines = parse_lines vsebina_datoteke |> List.flatten
    in
    let stones, min_x, min_y = make_stones lines
    in
    let rec spawn_sand sand_point counter =
        match falling_sand sand_point (min_x, min_y) stones with
        | Rest -> spawn_sand sand_source (counter + 1)
        | Void -> counter
        | OnSource -> failwith "first simulation shouldn't cover up source."
    in
    spawn_sand sand_source 0 |> string_of_int


let make_wide_stones lines =
    (* make lines without the floor line as you need the max y to calculate where the floor line is *)
    let bounds = get_max_bounds lines
    in
    let bounds = update_bounds bounds sand_source (* source of the sand must be on grid *)
    in
    let max_y = bounds.y.upper
    and height = bounds.y.upper - bounds.y.lower + 1
    in
    let floor_line = {
        a = ((fst sand_source) - (height + 2) - 5, max_y + 2);
        b = ((fst sand_source) + (height + 2) + 5, max_y + 2)
    }
    in
    (* re-make all four mesurements with the floor line addded. *)
    let new_lines = floor_line :: lines
    in
    let bounds = get_max_bounds new_lines
    in
    let bounds = update_bounds bounds sand_source (* source of the sand must be on grid *)
    in
    let min_x = bounds.x.lower
    and min_y = bounds.y.lower
    and width  = bounds.x.upper - bounds.x.lower + 1
    and height = bounds.y.upper - bounds.y.lower + 1
    in
    let () = Printf.printf "Second part: width: %d, height: %d\n" width height
    in
    let stones = Array.make_matrix height width Empty
    in
    let rec write_line {a; b} =
        stones.(snd a - min_y).(fst a - min_x) <- Stone;
        match a = b with
        | true -> ()
        | false -> write_line {a = move_closer a b; b = b}
    in
    let _ = List.map write_line new_lines
    in
    stones.((snd sand_source)-min_y).((fst sand_source)-min_x) <- Source;
    stones, min_x, min_y

let naloga2 vsebina_datoteke =
    let lines = parse_lines vsebina_datoteke |> List.flatten
    in
    let stones, min_x, min_y = make_wide_stones lines
    in
    let rec spawn_sand sand_point counter =
        match falling_sand sand_point (min_x, min_y) stones with
        | Rest -> spawn_sand sand_source (counter + 1)
        | Void -> failwith "second simulation shouldn't reach the void."
        | OnSource -> counter + 1 (* must also count the piece of sand that covered up the source.*)
    in
    spawn_sand sand_source 0 |> string_of_int


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

