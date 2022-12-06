let naloga_dan = 3

let naloga1 vsebina_datoteke =
    let rec string_intersect left_right =
        let left = fst left_right
        in
        let right = snd left_right
        in
        let start = (String.get left 0)
        in
        if String.contains right start then start else
            string_intersect ((String.sub left 1 ((String.length left) - 1)), right)
    in
    let split_in_half word =
        let half_length = (String.length word) / 2
        in
        (
            String.sub word 0 half_length,
            String.sub word half_length half_length
        )
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
        List.map (fun line -> string_intersect (split_in_half line))
            (remove_blanks (Str.split (Str.regexp "\n") vsebina_datoteke))
    in
    let priority letter =
        let char_code = Char.code letter
        in
        if 65 <= char_code && char_code <= 90 then char_code - 65 + 27 else
            char_code - 97 + 1
    in
    string_of_int (List.fold_left ( + ) 0 (List.map priority results))

let naloga2 vsebina_datoteke =
    let rec triple_intersect left_middle_right =
        match left_middle_right with
        (left, middle, right) ->
            let start = (String.get left 0)
            in
            if (String.contains right start) && (String.contains middle start) then start else
                triple_intersect ((String.sub left 1 ((String.length left) - 1)), middle, right)
    in
    let remove_blanks lines =
        let rec aux acc lines' =
            match lines' with
            | [] -> acc
            | l :: ls -> if l <> "" then aux (l :: acc) ls else aux acc ls
        in
        List.rev (aux [] lines)
    in
    let priority letter =
        let char_code = Char.code letter
        in
        if 65 <= char_code && char_code <= 90 then char_code - 65 + 27 else
            char_code - 97 + 1
    in
    let make_triplets lines =
        let rec aux acc lines' =
            match lines' with
            | [] -> acc
            | a :: b :: c :: others -> aux ((a, b, c) :: acc) others
            | _ -> failwith "Number of line not divisible by 3"
        in
        aux [] lines
    in
    let triplets = make_triplets (remove_blanks (Str.split (Str.regexp "\n") vsebina_datoteke))
    in
    let results =
        List.map triple_intersect triplets 
    in
    string_of_int (List.fold_left ( + ) 0 (List.map priority results))

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

