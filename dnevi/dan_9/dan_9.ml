let naloga_dan = 9

type direction =
    | Up
    | Down
    | Left
    | Right
    | UpRight
    | UpLeft
    | DownRight
    | DownLeft

type position = {x: int; y: int}

type order = {dir: direction; amount: int}

type extremes = {x_dir: int * int; y_dir: int * int}

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
        let s_dir, s_amount = split_in_two (Str.regexp " ") line
        in
        let amount = int_of_string s_amount
        in
        let dir = match s_dir with
        | "U" -> Up
        | "D" -> Down
        | "L" -> Left
        | "R" -> Right
        | str -> failwith ("Bad direction " ^ str)
        in
        {dir=dir; amount=amount}
    in
    let orders = List.map parse_line lines
    in
    let update_extreme (l, r) n =
        min l n, max r n
    in
    let update_extremes extremes position =
        {x_dir=update_extreme extremes.x_dir position.x; y_dir=update_extreme extremes.y_dir position.y}
    in
    let project_x dir =
        match dir with
        | Up -> 0
        | Down -> 0
        | Left -> -1
        | Right -> 1
        | UpRight -> 1
        | UpLeft -> -1
        | DownRight -> 1
        | DownLeft -> -1
    in
    let project_y dir =
        match dir with
        | Up -> 1
        | Down -> -1
        | Left -> 0
        | Right -> 0
        | UpRight -> 1
        | UpLeft -> 1
        | DownRight -> -1
        | DownLeft -> -1
    in
    let rec update_position position order =
        match order.amount with
        | 0 -> position
        | repeat -> update_position
            {x=(position.x+project_x order.dir); y=(position.y+project_y order.dir)}
            {order with amount=repeat-1}
    in
    let rec find_extremes orders position extremes =
        match orders with
        | [] -> extremes
        | o :: os ->
            let new_position = update_position position o
            in
            let new_extremes = update_extremes extremes new_position
            in
            find_extremes os new_position new_extremes
    in
    let ext = find_extremes orders {x=0; y=0} {x_dir=(0,0);y_dir=(0,0)}
    in
    let min_x, max_x = ext.x_dir
    in
    let min_y, max_y = ext.y_dir
    in
    let width = 1 + max_x - min_x
    in
    let height = 1 + max_y - min_y
    in
    let positions = Array.make_matrix width height 0
    in
    let rec update_head position direction =
        {x=(position.x+project_x direction); y=(position.y+project_y direction)}
    in
    let head_next_to_tail head tail =
        (abs (head.x - tail.x) <= 1) && (abs (head.y - tail.y) <= 1)
    in
    let move_head_and_tail head tail order =
        match order.amount with
        | 0 -> head, tail
        | _ ->
            let new_head = update_head head order.dir
            in
            let new_tail = if head_next_to_tail new_head tail then tail else head
            in
            new_head, new_tail
    in
    let rec find_tail_positions head tail orders out_array =
        let _ = out_array.(tail.x-min_x).(tail.y-min_y) <- 1
        in
        match orders with
        | [] -> ()
        | o :: os ->
            match o.amount with
            | 0 -> find_tail_positions head tail os out_array
            | amount ->
                let new_head, new_tail = move_head_and_tail head tail o
                in
                find_tail_positions new_head new_tail ({o with amount=amount-1} :: os) out_array
    in
    let _ = find_tail_positions {x=0; y=0} {x=0; y=0} orders positions
    in
    Array.fold_left ( + ) 0 (Array.map (Array.fold_left ( + ) 0) positions) |> string_of_int



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
        let s_dir, s_amount = split_in_two (Str.regexp " ") line
        in
        let amount = int_of_string s_amount
        in
        let dir = match s_dir with
        | "U" -> Up
        | "D" -> Down
        | "L" -> Left
        | "R" -> Right
        | str -> failwith ("Bad direction " ^ str)
        in
        {dir=dir; amount=amount}
    in
    let orders = List.map parse_line lines
    in
    let update_extreme (l, r) n =
        min l n, max r n
    in
    let update_extremes extremes position =
        {x_dir=update_extreme extremes.x_dir position.x; y_dir=update_extreme extremes.y_dir position.y}
    in
    let project_x dir =
        match dir with
        | Up -> 0
        | Down -> 0
        | Left -> -1
        | Right -> 1
        | UpRight -> 1
        | UpLeft -> -1
        | DownRight -> 1
        | DownLeft -> -1
    in
    let project_y dir =
        match dir with
        | Up -> 1
        | Down -> -1
        | Left -> 0
        | Right -> 0
        | UpRight -> 1
        | UpLeft -> 1
        | DownRight -> -1
        | DownLeft -> -1
    in
    let rec update_position position order =
        match order.amount with
        | 0 -> position
        | repeat -> update_position
            {x=(position.x+project_x order.dir); y=(position.y+project_y order.dir)}
            {order with amount=repeat-1}
    in
    let rec find_extremes orders position extremes =
        match orders with
        | [] -> extremes
        | o :: os ->
            let new_position = update_position position o
            in
            let new_extremes = update_extremes extremes position
            in
            find_extremes os new_position new_extremes
    in
    let ext = find_extremes orders {x=0; y=0} {x_dir=(0,0);y_dir=(0,0)}
    in
    let min_x, max_x = ext.x_dir
    in
    let min_y, max_y = ext.y_dir
    in
    let width = 1 + max_x - min_x
    in
    let height = 1 + max_y - min_y
    in
    let positions = Array.make_matrix width height 0
    in
    let knot_next_to_knot head tail =
        (abs (head.x - tail.x) <= 1) && (abs (head.y - tail.y) <= 1)
    in
    let find_move head tail =
        let dx = head.x - tail.x
        in
        let dy = head.y - tail.y
        in
        if dx > 0 && dy > 0 then UpRight else
        if dx < 0 && dy > 0 then UpLeft else
        if dx > 0 && dy < 0 then DownRight else
        if dx < 0 && dy < 0 then DownLeft else
        if dx > 0 && dy = 0 then Right else
        if dx < 0 && dy = 0 then Left else
        if dx = 0 && dy > 0 then Up else
        if dx = 0 && dy < 0 then Down else failwith "Head and tail match."
    in
    let rec move_knots knots dir =
        match knots with
        | [] -> []
        | k :: ks ->
            let new_k = {x=k.x+project_x dir; y=k.y+project_y dir}
            in
            match ks with
            | [] -> [new_k]
            | k' :: ks' ->
                if knot_next_to_knot k' new_k then new_k :: ks else
                let new_dir = find_move new_k k'
                in
                new_k :: move_knots ks new_dir
    in
    let rec get_tail knots =
        match knots with
        | k :: ks when List.length ks > 0 -> get_tail ks
        | k :: ks when List.length ks = 0 -> k
        | [] -> failwith "Can't get tail from empty set"
        | _ -> failwith "Can't have negative list lenght."
    in
    let rec find_tail_positions knots orders out_array =
        let tail = get_tail knots
        in
        let _ = out_array.(tail.x-min_x).(tail.y-min_y) <- 1
        in
        (*
        let _ = List.iter (fun knot -> print_int knot.x; print_string " "; print_int knot.y; print_string "\n") knots
        in
        let _ = print_string "\n\n"
        in
        *)
        match orders with
        | [] -> ()
        | o :: os ->
            match o.amount with
            | 0 -> find_tail_positions knots os out_array
            | amount ->
                let new_knots = move_knots knots o.dir
                in
                find_tail_positions new_knots ({o with amount=amount-1} :: os) out_array
    in
    let rec make_knots len = if len <= 0 then [] else {x=0; y=0} :: make_knots (len-1)
    in
    let knots = make_knots 10
    in
    let _ = find_tail_positions knots orders positions
    in
    Array.fold_left ( + ) 0 (Array.map (Array.fold_left ( + ) 0) positions) |> string_of_int
    (*
    let into_strings = (Array.map (Array.map string_of_int) positions)
    in
    Array.fold_left ( fun a b -> a ^ "\n" ^ b ) "" (Array.map (Array.fold_left ( ^ ) "") into_strings)
    *)


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
