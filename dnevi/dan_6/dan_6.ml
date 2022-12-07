let naloga_dan = 6

let naloga1 vsebina_datoteke =
    let rec all_unique chars =
        match chars with
        | "" -> true
        | _ ->
            let len = String.length chars
            in
            let cs = String.sub chars 1 (len-1) 
            in
            if String.contains cs (String.get chars 0) then false else all_unique cs
    in
    let pos_ok pos data = String.sub data pos 4 |> all_unique
    in
    let rec get_first_ok test_pos data =
        if pos_ok test_pos data then test_pos else get_first_ok (test_pos+1) data
    in
    (get_first_ok 1 vsebina_datoteke) + 4 |> string_of_int
        


let naloga2 vsebina_datoteke =
    let rec all_unique chars =
        match chars with
        | "" -> true
        | _ ->
            let len = String.length chars
            in
            let cs = String.sub chars 1 (len-1) 
            in
            if String.contains cs (String.get chars 0) then false else all_unique cs
    in
    let pos_ok pos data = String.sub data pos 14 |> all_unique
    in
    let rec get_first_ok test_pos data =
        if pos_ok test_pos data then test_pos else get_first_ok (test_pos+1) data
    in
    (get_first_ok 1 vsebina_datoteke) + 14 |> string_of_int

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

