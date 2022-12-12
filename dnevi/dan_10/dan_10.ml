let naloga_dan = 10

type instruction =
    | Noop
    | Addx of int
    | Addx2 of int

let naloga1 vsebina_datoteke =
    let lines_unfiltered = Str.split (Str.regexp "\n") vsebina_datoteke
    in
    let lines = List.filter (( <> ) "") lines_unfiltered
    in
    let split_in_two r string =
        let split = Str.split r string
        in
        if List.length split >= 2 then
            (List.nth split 0, List.nth split 1)
        else
            failwith ("String " ^ string ^ " did not contain the given regexp.")
    in
    let parse_line line =
        if line = "noop" then Noop else
        if (String.sub line 0 4) = "addx" then
            let _, num = split_in_two (Str.regexp " ") line
            in
            Addx (int_of_string num)
        else
            failwith ("Can't parse line " ^ line)
    in
    let instructions = List.map parse_line lines
    in
    let execute_instruction ins registry =
        match ins with
        | Noop -> None, registry
        | Addx value -> Some (Addx2 value), registry
        | Addx2 value -> None, registry+value
    in
    let add_test_point test_points point cycle=
        if (((cycle - 20) mod 40) = 0) && (cycle >= 20) && (cycle <= 220) then
            (cycle * point) :: test_points
        else
            test_points
    in
    let rec execute_instructions instructions registry cycle test_points =
        let new_test_points = add_test_point test_points registry cycle
        in
        match instructions with
        | [] -> new_test_points
        | i :: is ->
            let new_i, new_registry = execute_instruction i registry
            in
            match new_i with
            | None -> execute_instructions is new_registry (cycle+1) new_test_points
            | Some new_instruction -> execute_instructions (new_instruction::is) new_registry (cycle+1) new_test_points
    in
    let results = execute_instructions instructions 1 1 []
    in
    List.fold_left ( + ) 0 results |> string_of_int



let naloga2 vsebina_datoteke =
    let lines_unfiltered = Str.split (Str.regexp "\n") vsebina_datoteke
    in
    let lines = List.filter (( <> ) "") lines_unfiltered
    in
    let split_in_two r string =
        let split = Str.split r string
        in
        if List.length split >= 2 then
            (List.nth split 0, List.nth split 1)
        else
            failwith ("String " ^ string ^ " did not contain the given regexp.")
    in
    let parse_line line =
        if line = "noop" then Noop else
        if (String.sub line 0 4) = "addx" then
            let _, num = split_in_two (Str.regexp " ") line
            in
            Addx (int_of_string num)
        else
            failwith ("Can't parse line " ^ line)
    in
    let instructions = List.map parse_line lines
    in
    let execute_instruction ins registry =
        match ins with
        | Noop -> None, registry
        | Addx value -> Some (Addx2 value), registry
        | Addx2 value -> None, registry+value
    in
    let add_test_point test_points registry cycle=
        let crt_pos = (cycle-1) mod 40
        in
        if abs (registry - crt_pos) <= 1 then "#" :: test_points else "." :: test_points
    in
    let rec execute_instructions instructions registry cycle test_points =
        let new_test_points = add_test_point test_points registry cycle
        in
        match instructions with
        | [] -> new_test_points
        | i :: is ->
            let new_i, new_registry = execute_instruction i registry
            in
            match new_i with
            | None -> execute_instructions is new_registry (cycle+1) new_test_points
            | Some new_instruction -> execute_instructions (new_instruction::is) new_registry (cycle+1) new_test_points
    in
    let results = execute_instructions instructions 1 1 [] |> List.rev
    in
    let get_first_n n list = List.filteri (fun i _ -> i < n) list
    in
    let get_without_first_n n list = List.filteri (fun i _ -> i >= n) list
    in
    let rec list_into_groups length list =
        match List.length list with
        | n when n > length -> get_first_n length list :: list_into_groups length (get_without_first_n length list)
        | n -> [list]
    in
    let out_lines = list_into_groups 40 results
    in
    String.concat "\n" (List.map (String.concat "") out_lines)


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
