let naloga_dan = 11

type item = Item of int
type monkey_id = Id of int

type monkey = {
    id: monkey_id;
    items: item list;
    operation: item -> int;
    test: item -> bool;
    if_true: monkey_id;
    if_false: monkey_id;
}

let parse_data data =
    let monkies = Str.split (Str.regexp "\n\n") data
    in
    let parse_monkey monkey =
        let _ = Str.string_match (Str.regexp {|Monkey \([0-9]+\):|}) monkey 0
        in
        let monkey_id = Id (Str.matched_group 1 monkey |> int_of_string)
        in
        let _ = Str.search_forward (Str.regexp {|Starting items: \([0-9, ]+\)|}) monkey 0
        in
        let items =
            let items_string = Str.matched_group 1 monkey
            in
            let items_list = Str.split (Str.regexp ", ") items_string
            in
            List.map (fun d -> Item (int_of_string d)) items_list
        in
        let _ = Str.search_forward (Str.regexp {|Operation: new = old \([\+\*]+\) \([0-9old]+\)|}) monkey 0
        in
        let operation =
            let sign = Str.matched_group 1 monkey
            in
            let operand = Str.matched_group 2 monkey
            in
            let func =
                if sign = "+" then ( + ) else
                if sign = "*" then ( * ) else
                    failwith ("Bad opration sign " ^ sign)
            in
            fun (Item old) -> func old (if operand = "old" then old else int_of_string operand)
        in
        let _ = Str.search_forward (Str.regexp {|Test: divisible by \([0-9]+\)|}) monkey 0
        in
        let test =
            let div_checker = Str.matched_group 1 monkey |> int_of_string
            in
            fun (Item num) -> (num mod div_checker) = 0
        in
        let _ = Str.search_forward (Str.regexp {|If true: throw to monkey \([0-9]+\)|}) monkey 0
        in
        let if_true = Id (Str.matched_group 1 monkey |> int_of_string)
        in
        let _ = Str.search_forward (Str.regexp {|If false: throw to monkey \([0-9]+\)|}) monkey 0
        in
        let if_false = Id (Str.matched_group 1 monkey |> int_of_string)
        in
        {
            id=monkey_id;
            items=items;
            operation=operation;
            test=test;
            if_true=if_true;
            if_false=if_false;
        }
    in
    List.map parse_monkey monkies


let naloga1 vsebina_datoteke =
    let monkies = parse_data vsebina_datoteke
    in
    let rec give_to_monkey monkies id item =
        match monkies with
        | [] -> []
        | m :: ms -> if m.id = id then
                {m with items=m.items @ [item]} :: ms
            else
                m :: give_to_monkey ms id item
    in
    let rec monkey_do_stuff monkies id inspections =
        let monkey = List.find (fun mk -> mk.id = id) monkies
        in
        match monkey.items with
        | [] -> monkies
        | item :: items -> 
            inspections.(match monkey.id with Id i -> i) <- inspections.(match monkey.id with Id i -> i) + 1;
            let new_item = Item ((monkey.operation item) / 3)
            in
            let target = if monkey.test new_item then monkey.if_true else monkey.if_false
            in
            let new_others = give_to_monkey monkies target new_item
            in
            let new_monkies = List.map (fun mk -> if mk.id = id then {monkey with items=items} else mk) new_others
            in
            monkey_do_stuff new_monkies id inspections
    in
    let max_monkey_id = (List.fold_left
        (fun l r ->
            let Id l_id, Id r_id = l.id, r.id
            in
            if l_id > r_id then l else r
        )
        (List.nth monkies 0)
        monkies).id
    in
    let execute_round monkies inspections =
        let rec aux monkies in_turn =
            let new_monkies = monkey_do_stuff monkies in_turn inspections
            in
            if in_turn = max_monkey_id then 
                new_monkies 
            else
                aux new_monkies (match in_turn with Id in_turn_id -> Id (in_turn_id+1))
        in
        aux monkies (Id 0)
    in
    let rec execute_rounds monkies remaining_rounds inspections =
        let new_monkies = execute_round monkies inspections
        in
        match remaining_rounds with
        | r when r <= 1 -> new_monkies
        | _ -> execute_rounds new_monkies (remaining_rounds-1) inspections
    in
    let inspections = Array.init (match max_monkey_id with Id i -> i+1) (fun i -> 0)
    in
    let _ = execute_rounds monkies 20 inspections
    in
    let _ = Array.sort (fun a b -> b - a) inspections
    in
    inspections.(0) * inspections.(1) |> string_of_int


let naloga2 vsebina_datoteke =
    let monkies = parse_data vsebina_datoteke
    in
    let get_mod_from_test test =
        let rec aux test' counter =
            if test' (Item counter) then counter else aux test' (counter+1)
        in
        aux test 1
    in
    let monkey_mods = List.map (fun mk -> get_mod_from_test mk.test) monkies
    in
    let total_mod = List.fold_left ( * ) 1 monkey_mods
    in
    let rec give_to_monkey monkies id item =
        match monkies with
        | [] -> []
        | m :: ms -> if m.id = id then
                {m with items=m.items @ [item]} :: ms
            else
                m :: give_to_monkey ms id item
    in
    let rec monkey_do_stuff monkies id inspections =
        let monkey = List.find (fun mk -> mk.id = id) monkies
        in
        match monkey.items with
        | [] -> monkies
        | item :: items -> 
            inspections.(match monkey.id with Id i -> i) <- inspections.(match monkey.id with Id i -> i) + 1;
            let new_item = Item ((monkey.operation item) mod total_mod)
            in
            let target = if monkey.test new_item then monkey.if_true else monkey.if_false
            in
            let new_others = give_to_monkey monkies target new_item
            in
            let new_monkies = List.map (fun mk -> if mk.id = id then {monkey with items=items} else mk) new_others
            in
            monkey_do_stuff new_monkies id inspections
    in
    let max_monkey_id = (List.fold_left
        (fun l r ->
            let Id l_id, Id r_id = l.id, r.id
            in
            if l_id > r_id then l else r
        )
        (List.nth monkies 0)
        monkies).id
    in
    let execute_round monkies inspections =
        let rec aux monkies in_turn =
            let new_monkies = monkey_do_stuff monkies in_turn inspections
            in
            if in_turn = max_monkey_id then 
                new_monkies 
            else
                aux new_monkies (match in_turn with Id in_turn_id -> Id (in_turn_id+1))
        in
        aux monkies (Id 0)
    in
    let rec execute_rounds monkies remaining_rounds inspections =
        let new_monkies = execute_round monkies inspections
        in
        match remaining_rounds with
        | r when r <= 1 -> new_monkies
        | _ -> execute_rounds new_monkies (remaining_rounds-1) inspections
    in
    let inspections = Array.init (match max_monkey_id with Id i -> i+1) (fun i -> 0)
    in
    let _ = execute_rounds monkies 10000 inspections
    in
    let _ = Array.sort (fun a b -> b - a) inspections
    in
    inspections.(0) * inspections.(1) |> string_of_int


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

