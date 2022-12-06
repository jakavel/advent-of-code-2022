let naloga_dan = 4

let naloga1 vsebina_datoteke =
    let split_by char line =
        let sides = String.split_on_char char line
        in
        match sides with
        | [left; right] -> (left, right)
        | _ -> failwith ("Can't split line " ^ line)
    in
    let parse_line line =
        let split_by_comma = split_by ',' line
        in
        let lstart, lend = split_by '-' (fst split_by_comma)
        in
        let rstart, rend = split_by '-' (snd split_by_comma)
        in
        ((int_of_string lstart, int_of_string lend), (int_of_string rstart, int_of_string rend))
    in
    let remove_blanks lines =
        let rec aux acc lines' =
            match lines' with
            | [] -> acc
            | l :: ls -> if l <> "" then aux (l :: acc) ls else aux acc ls
        in
        List.rev (aux [] lines)
    in
    let results =
        List.map parse_line (remove_blanks (String.split_on_char '\n' vsebina_datoteke))
    in
    let is_contained intervals =
        match intervals with
        | ((lstart, lend), (rstart, rend)) ->
                if ((lstart <= rstart) && (rend <= lend)) || ((rstart <= lstart) && (lend <= rend))
                then 1 else 0
    in
    string_of_int (List.fold_left ( + ) 0 (List.map is_contained results))


let naloga2 vsebina_datoteke =
    let split_by char line =
        let sides = String.split_on_char char line
        in
        match sides with
        | [left; right] -> (left, right)
        | _ -> failwith ("Can't split line " ^ line)
    in
    let parse_line line =
        let split_by_comma = split_by ',' line
        in
        let lstart, lend = split_by '-' (fst split_by_comma)
        in
        let rstart, rend = split_by '-' (snd split_by_comma)
        in
        ((int_of_string lstart, int_of_string lend), (int_of_string rstart, int_of_string rend))
    in
    let remove_blanks lines =
        let rec aux acc lines' =
            match lines' with
            | [] -> acc
            | l :: ls -> if l <> "" then aux (l :: acc) ls else aux acc ls
        in
        List.rev (aux [] lines)
    in
    let results =
        List.map parse_line (remove_blanks (String.split_on_char '\n' vsebina_datoteke))
    in
    let is_overlapped intervals =
        match intervals with
        | ((lstart, lend), (rstart, rend)) ->
                let larger_start = max lstart rstart
                in
                let smaller_end = min lend rend
                in
                if smaller_end >= larger_start then 1 else 0
    in
    string_of_int (List.fold_left ( + ) 0 (List.map is_overlapped results))

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

