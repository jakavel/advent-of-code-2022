let naloga_dan = 2

let naloga1 vsebina_datoteke =
    let points_from_sign line =
        if List.length (Str.split (Str.regexp " ") line) < 1 then
            let _ = print_string line in -10000
        else
        let my_sign = List.nth (Str.split (Str.regexp " ") line) 1
        in
        if my_sign = "X" then 1 else
        if my_sign = "Y" then 2 else
        if my_sign = "Z" then 3 else
        failwith ("Unknown sign " ^ my_sign)
    in
    let points_from_win = function
        | "A X" -> 3
        | "A Y" -> 6
        | "A Z" -> 0
        | "B X" -> 0
        | "B Y" -> 3
        | "B Z" -> 6
        | "C X" -> 6
        | "C Y" -> 0
        | "C Z" -> 3
        | line -> failwith ("Invalid line " ^ line)
    in
    let all_points line = (points_from_win line) + (points_from_sign line)
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
        List.map all_points (remove_blanks (Str.split (Str.regexp "\n") vsebina_datoteke))
    in
    string_of_int (List.fold_left ( + ) 0 results)

let naloga2 vsebina_datoteke =
    let points_from_sign line =
        match line with
        | "A X" -> 3
        | "A Y" -> 1
        | "A Z" -> 2
        | "B X" -> 1
        | "B Y" -> 2
        | "B Z" -> 3
        | "C X" -> 2
        | "C Y" -> 3
        | "C Z" -> 1
        | line -> failwith ("Invalid line " ^ line)
    in
    let points_from_win line =
        let instruction = List.nth (Str.split (Str.regexp " ") line) 1
        in
        match instruction with
        | "X" -> 0
        | "Y" -> 3
        | "Z" -> 6
        | _ -> failwith ("Unknown instruction " ^ instruction)
    in
    let all_points line = (points_from_win line) + (points_from_sign line)
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
        List.map all_points (remove_blanks (Str.split (Str.regexp "\n") vsebina_datoteke))
    in
    string_of_int (List.fold_left ( + ) 0 results)

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
