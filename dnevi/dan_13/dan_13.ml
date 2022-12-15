let naloga_dan = 13


type deep_list =
    | ListList of deep_list list
    | ListInt of int


let rec parse_line line =
    let rec line_loop str i depth =
        if str = "" then None else
        let new_depth =
            match str.[i] with
            | '[' -> depth + 1
            | ']' -> depth - 1
            | _ -> depth
        in
        if str.[i] = ',' && new_depth = 1 then
            Some i
        else
            if (String.length str) - 1 >= i + 1 then line_loop str (i+1) new_depth else None
    in
    match line_loop line 0 0 with
    | Some comma -> (
        let first = parse_line (String.sub line 1 (comma-1))
        in
        let rest = parse_line ("[" ^ (String.sub line (comma+1) ((String.length line)-comma-1)))
        in
        match rest with
        | ListInt i -> failwith "rest shouldn't be int"
        | ListList l -> ListList (first :: l)
    )
    | None ->
        if line = "[]" then
            ListList ([])
        else if String.length line = 0 then
            failwith "can't parse empty string."
        else if (line.[0] = '[') && (line.[(String.length line) - 1] = ']') then
            let insides = parse_line (String.sub line 1 ((String.length line) - 2))
            in
            ListList [insides]
        else
            ListInt (int_of_string line)

let parse_lines lines =
    let pairs = Str.split (Str.regexp "\n\n") lines
    in
    let pairs = List.filter (( <> ) "") pairs
    in
    let make_pair text =
        let pairs_list = Str.split (Str.regexp "\n") text
        in
        (List.nth pairs_list 0, List.nth pairs_list 1)
    in
    let pairs = List.map make_pair pairs
    in
    List.map (fun (a, b) -> parse_line a, parse_line b) pairs


let rec order_ok (left, right) =
    match left with
    | ListInt l_int -> (
        match right with
        | ListInt r_int -> (
            if l_int < r_int then
                Some true
            else if l_int > r_int then
                Some false
            else
                None
        )
        | ListList (r :: rs) -> (
            match order_ok (ListList [left], right) with
            | Some bool -> Some bool
            | None -> Some true (* left ran out of items first *)
        )
        | ListList [] -> Some false (* right ran out of items first *)
    )
    | ListList (l :: ls) -> (
        match right with
        | ListInt _ -> order_ok (left, ListList [right])
        | ListList (r :: rs) -> (
            match order_ok (l, r) with
            | Some bool -> Some bool
            | None -> order_ok (ListList ls, ListList rs)
        )
        | ListList [] -> Some false
    )
    | ListList [] -> (
        match right with
        | ListInt _ -> order_ok (left, ListList [right])
        | ListList (r :: rs) -> Some true
        | ListList [] -> None
    )

let naloga1 vsebina_datoteke =
    let pairs = parse_lines vsebina_datoteke
    in
    let ordered = List.map order_ok pairs
    in
    let remove_option opt =
        match opt with
        | Some v -> v
        | None -> false
    in
    let ordered = List.map remove_option ordered
    in
    let values = List.mapi (fun i is_ordered -> if is_ordered then i + 1 else 0) ordered
    in
    List.fold_left ( + ) 0 values |> string_of_int


let naloga2 vsebina_datoteke =
    let dividers = [ListList [ListList [ListInt 2]]; ListList [ListList [ListInt 6]]]
    in
    let pairs = parse_lines vsebina_datoteke
    in
    let packets = (List.map fst pairs) @ (List.map snd pairs)
    in
    let packets = dividers @ packets
    in
    let compare left right =
        match order_ok (left, right) with
        | Some true -> -1
        | Some false -> 1
        | None -> 0
    in
    let sorted = List.sort compare packets
    in
    let get_index i packet = if List.exists (( = ) packet) dividers then i + 1 else 1
    in
    let good_indexes = List.mapi get_index sorted
    in
    List.fold_left ( * ) 1 good_indexes |> string_of_int


let preberi_datoteko ime_datoteke =
    let chan = open_in ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina

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

