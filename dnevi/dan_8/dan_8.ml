let naloga_dan = 8


let naloga1 vsebina_datoteke =
    let lines_unfiltered = Str.split (Str.regexp "\n") vsebina_datoteke
    in
    let lines = List.filter (( <> ) "") lines_unfiltered |> Array.of_list
    in
    let string_to_array str = String.to_seq str |> Array.of_seq
    in
    let int_of_char c =
        match c with
        | '0' -> 0
        | '1' -> 1
        | '2' -> 2
        | '3' -> 3
        | '4' -> 4
        | '5' -> 5
        | '6' -> 6
        | '7' -> 7
        | '8' -> 8
        | '9' -> 9
        | _ -> failwith "Can't convert bad char into int."
    in
    let heights = Array.map string_to_array lines |> Array.map (Array.map int_of_char)
    in
    let height = Array.length heights
    in
    let width = Array.length heights.(0)
    in
    let rec range_lower_than x y dx dy value matrix =
        if (dx <= 1) && (dy <= 1) then
            matrix.(y).(x) < value
        else if (dx > 1) && (dy = 1) then
            (matrix.(y).(x) < value) && (range_lower_than (x+1) y (dx-1) dy value matrix)
        else if (dx = 1) && (dy > 1) then
            (matrix.(y).(x) < value) && (range_lower_than x (y+1) dx (dy-1) value matrix)
        else
            (range_lower_than x y 1 dy value matrix) && (range_lower_than (x+1) y (dx-1) dy value matrix)
    in
    let is_visible x y matrix =
        let value = matrix.(y).(x)
        in
        (if x > 0 then (range_lower_than 0 y x 1 value matrix) else true) ||
        (if x < (width-1) then (range_lower_than (x+1) y (width-x-1) 1 value matrix) else true) ||
        (if y > 0 then (range_lower_than x 0 1 y value matrix) else true) ||
        (if y < (height-1) then (range_lower_than x (y+1) 1 (height-y-1) value matrix) else true)
    in
    let rec test_visibility x y dx dy matrix out_matrix =
        if (dx <= 1) && (dy <= 1) then
            if is_visible x y matrix then out_matrix.(y).(x) <- 1 else ()
        else if (dx > 1) && (dy = 1) then
            let _ = if is_visible x y matrix then out_matrix.(y).(x) <- 1 else () in
            (test_visibility (x+1) y (dx-1) dy matrix out_matrix)
        else if (dx = 1) && (dy > 1) then
            let _ = if is_visible x y matrix then out_matrix.(y).(x) <- 1 else () in
            (test_visibility x (y+1) dx (dy-1) matrix out_matrix)
        else
            let _ = test_visibility x y 1 dy matrix out_matrix in
            test_visibility (x+1) y (dx-1) dy matrix out_matrix
    in
    let visiblity = Array.make_matrix width height 0
    in
    test_visibility 0 0 width height heights visiblity;
    let array_sum arr = Array.fold_left ( + ) 0 arr
    in
    array_sum (Array.map array_sum visiblity) |> string_of_int




let naloga2 vsebina_datoteke =
    let lines_unfiltered = Str.split (Str.regexp "\n") vsebina_datoteke
    in
    let lines = List.filter (( <> ) "") lines_unfiltered |> Array.of_list
    in
    let string_to_array str = String.to_seq str |> Array.of_seq
    in
    let int_of_char c =
        match c with
        | '0' -> 0
        | '1' -> 1
        | '2' -> 2
        | '3' -> 3
        | '4' -> 4
        | '5' -> 5
        | '6' -> 6
        | '7' -> 7
        | '8' -> 8
        | '9' -> 9
        | _ -> failwith "Can't convert bad char into int."
    in
    let heights = Array.map string_to_array lines |> Array.map (Array.map int_of_char)
    in
    let height = Array.length heights
    in
    let width = Array.length heights.(0)
    in
    let rec sight_with_step x y stepx stepy value matrix =
        if (x+stepx >= 0) && (x+stepx <= width-1) && (y+stepy >= 0) && (y+stepy <= height-1) then
            if matrix.(y+stepy).(x+stepx) >= value then 1
            else 1 + sight_with_step (x+stepx) (y+stepy) stepx stepy value matrix
        else 0
    in
    let scenic_score x y matrix =
        let value = matrix.(y).(x)
        in
        (sight_with_step x y (-1) 0 value matrix) *
        (sight_with_step x y  1 0 value matrix) *
        (sight_with_step x y 0 (-1) value matrix) *
        (sight_with_step x y 0  1 value matrix)
    in
    let rec test_visibility x y dx dy matrix out_matrix =
        if (dx <= 1) && (dy <= 1) then
            out_matrix.(y).(x) <- scenic_score x y matrix
        else if (dx > 1) && (dy = 1) then
            let _ = out_matrix.(y).(x) <- scenic_score x y matrix in
            (test_visibility (x+1) y (dx-1) dy matrix out_matrix)
        else if (dx = 1) && (dy > 1) then
            let _ = out_matrix.(y).(x) <- scenic_score x y matrix in
            (test_visibility x (y+1) dx (dy-1) matrix out_matrix)
        else
            let _ = test_visibility x y 1 dy matrix out_matrix in
            test_visibility (x+1) y (dx-1) dy matrix out_matrix
    in
    let visiblity = Array.make_matrix width height 0
    in
    test_visibility 0 0 width height heights visiblity;
    let array_max arr = Array.fold_left max min_int arr
    in
    array_max (Array.map array_max visiblity) |> string_of_int


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
