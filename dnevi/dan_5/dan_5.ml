let naloga_dan = 5

type crate = Crate of char

type order = {from: int; dest: int; amount: int}

let naloga1 vsebina_datoteke =
    let list_to_pair list =
        match list with
        | x :: xs -> (
            match xs with
            | y :: ys -> (x, y)
            | [] -> failwith "List split by  list_to_pair needs to be of length >= 2."
        )
        | [] -> failwith "List split by  list_to_pair needs to be of length >= 2."
    in
    let start, orders_str = list_to_pair (Str.split (Str.regexp "\n\n") vsebina_datoteke)
    in
    let index_line_checker = fun line -> Str.string_match (Str.regexp "^[0-9 ]+$") line 0
    in
    let index_line = List.find index_line_checker (Str.split (Str.regexp "\n") start)
    in
    let indexes = (Str.split (Str.regexp " +") index_line)
    in
    let num_of_columns = List.nth indexes ((List.length indexes)-1) |> int_of_string
    in 
    let get_column col text =
        let text_list = (Str.split (Str.regexp "\n") text)
        in
        let rec get_column' col' text' =
            match text' with
            | [] -> []
            | first :: left_over -> (
                if index_line_checker first then [] else
                let char_in_column = String.get first (1 + 4*(col'-1))
                in
                if char_in_column = ' ' then get_column' col' left_over else
                Crate char_in_column
                ::
                get_column' col' left_over
            )
        in
        get_column' col text_list
    in
    let rec get_columns' col text =
        if col > 0 then (get_column col text) :: (get_columns' (col-1) text) else []
    in
    let get_columns col text = List.rev (get_columns' col text)
    in
    let columns = get_columns num_of_columns start
    in
    let parse_order order =
        let _ = Str.string_match (Str.regexp {|^move \([0-9]+\) from \([0-9]+\) to \([0-9]+\)$|}) order 0
        in
        {
            amount=int_of_string (Str.matched_group 1 order);
            from=int_of_string (Str.matched_group 2 order);
            dest=int_of_string (Str.matched_group 3 order)
        }
    in
    let parse_orders orders_list =
        let rec parse_orders' orders_list' =
            match orders_list' with
            | [] -> []
            | order :: orders -> (parse_order order) :: (parse_orders' orders)
        in
        parse_orders' (Str.split (Str.regexp "\n") orders_list)
    in
    let parsed_orders = parse_orders orders_str
    in
    let rec list_set i list new_i =
        match list with
        | [] -> []
        | x :: xs ->
        if i = 0 then new_i :: xs else x :: (list_set (i-1) xs new_i)
    in
    let rec execute_order order state =
        (* let print_status status = print_string (
            let print_column i column =
                let new_column = List.map (fun (Crate c) -> String.make 1 c) column
                in
                string_of_int (i+1) ^ " " ^ (String.concat ";" new_column)
            in
            let stred_columns = List.mapi print_column status
            in
            "\n" ^ (String.concat "|\n" stred_columns) ^ "\n\n"
        )
        in
        let _ = print_status state in *)
        if order.amount = 0 then state else
            let donor_column = (List.nth state (order.from-1))
            in
            let donor_box, new_donor_column = (
                match donor_column with
                | [] -> failwith (Printf.sprintf "Column %d has too few boxes." order.from)
                | box :: boxes -> box, boxes
            )
            in
            let new_state = list_set (order.dest-1) state (donor_box :: (List.nth state (order.dest-1)))
            in
            let newer_state = list_set (order.from-1) new_state new_donor_column
            in
            execute_order {order with amount=order.amount-1} newer_state
    in
    let rec execute_orders orders state =
        match orders with
        | [] -> state
        | o :: os -> execute_orders os (execute_order o state)
    in
    let final_boxes = execute_orders parsed_orders columns
    in
    let rec top_crates state =
        match state with
        | [] -> ""
        | stack :: stacks -> (let Crate char = List.hd stack in String.make 1 char) ^ top_crates stacks
    in
    top_crates final_boxes

let naloga2 vsebina_datoteke =
    let list_to_pair list =
        match list with
        | x :: xs -> (
            match xs with
            | y :: ys -> (x, y)
            | [] -> failwith "List split by  list_to_pair needs to be of length >= 2."
        )
        | [] -> failwith "List split by  list_to_pair needs to be of length >= 2."
    in
    let start, orders_str = list_to_pair (Str.split (Str.regexp "\n\n") vsebina_datoteke)
    in
    let index_line_checker = fun line -> Str.string_match (Str.regexp "^[0-9 ]+$") line 0
    in
    let index_line = List.find index_line_checker (Str.split (Str.regexp "\n") start)
    in
    let indexes = (Str.split (Str.regexp " +") index_line)
    in
    let num_of_columns = List.nth indexes ((List.length indexes)-1) |> int_of_string
    in 
    let get_column col text =
        let text_list = (Str.split (Str.regexp "\n") text)
        in
        let rec get_column' col' text' =
            match text' with
            | [] -> []
            | first :: left_over -> (
                if index_line_checker first then [] else
                let char_in_column = String.get first (1 + 4*(col'-1))
                in
                if char_in_column = ' ' then get_column' col' left_over else
                Crate char_in_column
                ::
                get_column' col' left_over
            )
        in
        get_column' col text_list
    in
    let rec get_columns' col text =
        if col > 0 then (get_column col text) :: (get_columns' (col-1) text) else []
    in
    let get_columns col text = List.rev (get_columns' col text)
    in
    let columns = get_columns num_of_columns start
    in
    let parse_order order =
        let _ = Str.string_match (Str.regexp {|^move \([0-9]+\) from \([0-9]+\) to \([0-9]+\)$|}) order 0
        in
        {
            amount=int_of_string (Str.matched_group 1 order);
            from=int_of_string (Str.matched_group 2 order);
            dest=int_of_string (Str.matched_group 3 order)
        }
    in
    let parse_orders orders_list =
        let rec parse_orders' orders_list' =
            match orders_list' with
            | [] -> []
            | order :: orders -> (parse_order order) :: (parse_orders' orders)
        in
        parse_orders' (Str.split (Str.regexp "\n") orders_list)
    in
    let parsed_orders = parse_orders orders_str
    in
    let rec list_set i list new_i =
        match list with
        | [] -> []
        | x :: xs ->
        if i = 0 then new_i :: xs else x :: (list_set (i-1) xs new_i)
    in
    let rec pick_up_order order state holder =
        (* let print_status status = print_string (
            let print_column i column =
                let new_column = List.map (fun (Crate c) -> String.make 1 c) column
                in
                string_of_int (i+1) ^ " " ^ (String.concat ";" new_column)
            in
            let stred_columns = List.mapi print_column status
            in
            "\n" ^ (String.concat "|\n" stred_columns) ^ "\n\n"
        )
        in
        let _ = print_status state in *)
        if order.amount = 0 then state, holder else
            let donor_column = (List.nth state (order.from-1))
            in
            let donor_box, new_donor_column = (
                match donor_column with
                | [] -> failwith (Printf.sprintf "Column %d has too few boxes." order.from)
                | box :: boxes -> box, boxes
            )
            in
            (*
            let new_state = list_set (order.dest-1) state (donor_box :: (List.nth state (order.dest-1)))
            in *)
            let new_state = list_set (order.from-1) state new_donor_column
            in
            pick_up_order {order with amount=order.amount-1} new_state (holder @ [donor_box])
    in
    let rec execute_order order state =
        (* let print_status status = print_string (
            let print_column i column =
                let new_column = List.map (fun (Crate c) -> String.make 1 c) column
                in
                string_of_int (i+1) ^ " " ^ (String.concat ";" new_column)
            in
            let stred_columns = List.mapi print_column status
            in
            "\n" ^ (String.concat "|\n" stred_columns) ^ "\n\n"
        )
        in
        let _ = print_status state in *)
        let new_state, holder = pick_up_order order state []
        in
        let new_recieving_column = holder @ (List.nth new_state (order.dest-1))
        in
        list_set (order.dest-1) new_state new_recieving_column
    in
    let rec execute_orders orders state =
        match orders with
        | [] -> state
        | o :: os -> execute_orders os (execute_order o state)
    in
    let final_boxes = execute_orders parsed_orders columns
    in
    let rec top_crates state =
        match state with
        | [] -> ""
        | stack :: stacks -> (let Crate char = List.hd stack in String.make 1 char) ^ top_crates stacks
    in
    top_crates final_boxes

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

