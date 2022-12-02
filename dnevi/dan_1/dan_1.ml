let naloga_dan = 1

let naloga1 vsebina_datoteke =
    let str_of_nums_into_list numbers =
        List.map int_of_string (Str.split (Str.regexp "\n") numbers)
    in
    let sum_of_list list =
        let rec aux acc summands =
            match summands with
            | [] -> acc
            | x :: xs -> aux (acc+x) xs
        in
        aux 0 list
    in
    let all_sums =
        List.map sum_of_list (List.map str_of_nums_into_list (Str.split (Str.regexp "\n\n") vsebina_datoteke))
    in
    string_of_int (List.fold_left max min_int all_sums)

let naloga2 vsebina_datoteke =
    let str_of_nums_into_list numbers =
        List.map int_of_string (Str.split (Str.regexp "\n") numbers)
    in
    let sum_of_list list =
        let rec aux acc summands =
            match summands with
            | [] -> acc
            | x :: xs -> aux (acc+x) xs
        in
        aux 0 list
    in
    let all_sums =
        List.map sum_of_list (List.map str_of_nums_into_list (Str.split (Str.regexp "\n\n") vsebina_datoteke))
    in
    let first_three values' =
        let rec aux acc values =
            if List.length acc = 3 then acc else
            match values with
            | [] -> acc
            | x :: xs -> aux (x :: acc) xs
        in
        List.rev (aux [] values')
    in
    let int_compare a b =
        if a > b then 0 else 1
    in
    let get_top_three values =
        first_three (List.sort int_compare values)
    in
    string_of_int (sum_of_list (get_top_three all_sums))

let _ = print_string ((naloga2 "1\n5\n\n2\n\n3\n\n4") ^ "\n")

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
