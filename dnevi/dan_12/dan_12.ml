let naloga_dan = 12


type height = Height of char

let assert_height h = assert (('a' <= h) && (h <= 'z'))


type distance =
    | Unknown
    | Dist of int


let find_in_grid grid thing =
    let rec find_in_array arr x start =
        if arr.(start) = x then Some start else
            if ((Array.length arr)-1) > start then find_in_array arr x (start+1) else None
    in
    let rec find_in_grid' x start =
        match find_in_array grid.(start) x 0 with
        | Some x_index -> Some (start, x_index)
        | None ->
            if (Array.length grid) - 1 > start then
                find_in_grid' x (start+1)
            else
                None
    in
    find_in_grid' thing 0


let print_grid grid =
    let print_line line =
        Array.iter (fun (Height h) -> print_string (String.make 1 h)) line;
        print_string "\n"
    in
    Array.iter print_line grid;
    print_string "\n"

let print_distances distances =
    let print_line line =
        Array.iter (
            fun dist ->
                match dist with
                | Unknown -> print_string "/ "
                | Dist d -> if d <= 9 then print_string (Printf.sprintf "%d " d) else print_int d
        ) line;
        print_string "\n"
    in
    Array.iter print_line distances;
    print_string "\n"


let parse_grid grid =
    let lines = Str.split (Str.regexp "\n") grid
    in
    let lines = List.filter (( <> ) "") lines
    in
    let lines = List.map (fun x -> x |> String.to_seq |> Array.of_seq) lines |> Array.of_list
    in
    let find_char char =
        match find_in_grid lines char with
        | Some a -> a
        | None -> failwith "Could not find S in height grid."
    in
    let s_y, s_x = find_char 'S'
    in
    let e_y, e_x = find_char 'E'
    in
    let make_height char =
        match char with
        | 'S' -> Height 'a'
        | 'E' -> Height 'z'
        | _ ->
            assert_height char;
            Height char
    in
    Array.map (Array.map make_height) lines, (s_x, s_y), (e_x, e_y)


let naloga1 vsebina_datoteke =
    let grid, (s_x, s_y), (e_x, e_y) = parse_grid vsebina_datoteke
    in
    let distances = Array.map (Array.map (fun x -> Unknown)) grid
    in
    distances.(s_y).(s_x) <- Dist 0;
    let can_reach (Height from_height) (Height to_height) = (Char.code to_height) - (Char.code from_height) <= 1
    in
    let move_makes_sence (x, y) (x', y') =
        (can_reach grid.(y).(x) grid.(y').(x')) &&
        (
            let here = match distances.(y).(x) with
                | Unknown -> failwith "Moved to an unknown spot"
                | Dist d -> d
            in
            match distances.(y').(x') with
            | Unknown -> true
            (* move to a tile only if you can get there shorter than the path that already exists *)
            | Dist dest -> dest > here + 1
        )
    in
    let is_in_grid (x, y) =
        (0 <= x) && (x <= (Array.length grid.(0)) - 1) && (0 <= y) && (y <= (Array.length grid) - 1)
    in
    let increment_distance dist =
        match dist with
        | Dist x -> Dist (x+1)
        | Unknown -> failwith "Cannot increment unknown distance."
    in
    let rec border_distances (x, y) =
        let set_dist (x', y') =
            if (is_in_grid (x', y')) then
                if (move_makes_sence (x, y) (x', y')) then 
                    let () = distances.(y').(x') <- increment_distance distances.(y).(x) in
                    border_distances (x', y')
                else ()
            else ()
        in
        set_dist (x-1, y);
        set_dist (x+1, y);
        set_dist (x, y-1);
        set_dist (x, y+1)
    in
    border_distances (s_x, s_y);
    match distances.(e_y).(e_x) with
    | Dist d -> d |> string_of_int
    | Unknown -> "unknown @ " ^ (string_of_int e_x) ^ " " ^ (string_of_int e_y)





let naloga2 vsebina_datoteke =
    let grid, (s_x, s_y), (e_x, e_y) = parse_grid vsebina_datoteke
    in
    let distances = Array.map (Array.map (fun x -> Unknown)) grid
    in
    distances.(e_y).(e_x) <- Dist 0;
    let can_reach (Height from_height) (Height to_height) = (Char.code to_height) - (Char.code from_height) <= 1
    in
    let move_makes_sence (x, y) (x', y') =
        (can_reach grid.(y').(x') grid.(y).(x)) &&
        (
            let here = match distances.(y).(x) with
                | Unknown -> failwith "Moved to an unknown spot"
                | Dist d -> d
            in
            match distances.(y').(x') with
            | Unknown -> true
            (* move to a tile only if you can get there shorter than the path that already exists *)
            | Dist dest -> dest > here + 1
        )
    in
    let is_in_grid (x, y) =
        (0 <= x) && (x <= (Array.length grid.(0)) - 1) && (0 <= y) && (y <= (Array.length grid) - 1)
    in
    let increment_distance dist =
        match dist with
        | Dist x -> Dist (x+1)
        | Unknown -> failwith "Cannot increment unknown distance."
    in
    let rec border_distances (x, y) =
        let set_dist (x', y') =
            if (is_in_grid (x', y')) then
                if (move_makes_sence (x, y) (x', y')) then 
                    let () = distances.(y').(x') <- increment_distance distances.(y).(x) in
                    border_distances (x', y')
                else ()
            else ()
        in
        set_dist (x-1, y);
        set_dist (x+1, y);
        set_dist (x, y-1);
        set_dist (x, y+1)
    in
    border_distances (e_x, e_y);
    let rec get_starting_points acc x y =
        let new_acc =
            if grid.(y).(x) = Height 'a' then
                (x, y) :: acc
            else
                acc
        in
        if x + 1 > (Array.length distances.(y)) - 1 then
            if y + 1 <= (Array.length distances) - 1 then
                get_starting_points new_acc 0 (y + 1)
            else
                new_acc
        else
            get_starting_points new_acc (x + 1) y
    in
    let starting_points = get_starting_points [] 0 0
    in
    let starting_distances = List.map 
        (
            fun (x, y) -> match distances.(y).(x) with
                | Dist d -> d
                | Unknown -> max_int
        )
        starting_points
    in
    List.fold_left min max_int starting_distances |> string_of_int




let _ =
    let preberi_datoteko ime_datoteke =
        let chan = open_in ime_datoteke in
        let vsebina = really_input_string chan (in_channel_length chan) in
        close_in chan;
        vsebina
    and izpisi_datoteko ime_datoteke vsebina =
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

