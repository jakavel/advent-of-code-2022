let naloga_dan = 7

type dir =
    | Unknown of string
    | File of {name: string; size: int}
    | Dir of {name: string; children: dir list}

type loc =
    | Location of {name: string; child: loc}
    | End

type sized_dir =
    | SUnknown of string
    | SFile of {name: string; size: int}
    | SDir of {name: string; children: sized_dir list; size: int}


let naloga1 vsebina_datoteke =
    let to_first_newline text =
        let first_newline = Str.bounded_split (Str.regexp "\n") text 2
        in
        List.nth first_newline 0, if List.length first_newline >= 2 then List.nth first_newline 1 else ""
    in
    let to_first_space text =
        let first_space = Str.bounded_split (Str.regexp " ") text 2
        in
        List.nth first_space 0, if List.length first_space >= 2 then List.nth first_space 1 else ""
    in
    let wierd_commands = Str.split (Str.regexp {|\$ |}) vsebina_datoteke
    in
    let process_command command =
        let call, out = to_first_newline command
        in
        (to_first_space call, out)
    in
    let commands = List.map process_command wierd_commands
    in
    let remove_empty_lines lines = List.filter (fun line -> line <> "") lines
    in
    let get_dir_contents_from_ls ls_output =
        (* make dir list from the output of ls *)
        let process_line line =
            let prefix, name = to_first_space line
            in
            match prefix with
            | "dir" -> Unknown name
            | num -> File {name=name; size=int_of_string num}
        in
        List.map process_line (remove_empty_lines (Str.split (Str.regexp "\n") ls_output))
    in
    let rec update_cwd cwd new_dir =
        if new_dir = ".." then
            match cwd with
            | Location {name; child} -> (
                match child with
                | Location _ -> Location {name=name; child=update_cwd child new_dir}
                | End -> End
            )
            | End -> End
        else
        match cwd with
        | Location {name; child} -> Location {name=name; child=update_cwd child new_dir}
        | End -> Location {name=new_dir; child=End}
    in
    let dir_name_is search_name some_dir =
        match some_dir with
        | Unknown name -> name = search_name
        | File _ -> false
        | Dir {name; _} -> name = search_name
    in
    let dir_name_isnt search_name some_dir = not (dir_name_is search_name some_dir)
    in
    let rec write_dir cwd state dir_contents =
        match cwd with
        | Location {name; child} -> (
            match child with
            | End -> (
                match state with
                | Unknown name_to_write_to -> Dir {name=name; children=dir_contents}
                | _ -> failwith "write_dir: got to a node but state is not Unknown."
            )
            | Location child_loc -> (
                match state with
                | Dir state_dir -> (
                    let modifier_child = List.find (dir_name_is child_loc.name) state_dir.children
                    in
                    let unmodifier_children = List.filter (dir_name_isnt child_loc.name) state_dir.children
                    in
                    Dir {state_dir with children=(write_dir child modifier_child dir_contents) :: unmodifier_children}
                )
                | _ -> failwith "write_dir: got to a node when cwd still wants to go deeper."
            )
        )
        | End -> (
            match state with
            | Unknown dname -> Dir {name=dname; children=dir_contents}
            | _ -> failwith "write_dir was used when the cwd isn't Unknown."
        )
    in
    let execute_command command cwd state =
        let (prog, args), out = command
        in
        match prog with
        | "ls" -> (
            cwd, write_dir cwd state (get_dir_contents_from_ls out)
        )
        | "cd" -> update_cwd cwd args, state
        | _ -> failwith ("Found bad program " ^ prog)
    in
    let rec execute_commands commands' cwd state=
        match commands' with
        | [] -> cwd, state
        | c :: cs ->
            let new_cwd, new_state = execute_command c cwd state
            in
            execute_commands cs new_cwd new_state
    in
    let _, final_state = execute_commands commands (End) (Unknown "/")
    in
    let rec size_of directory =
        match directory with
        | Unknown thing -> failwith ("Can't mesure size of unknown directory " ^ thing)
        | File file -> file.size
        | Dir child_dir -> List.fold_left ( + ) 0 (List.map size_of child_dir.children)
    in
    let rec make_sized_state state =
        match state with
        | Unknown thing -> failwith ("Can't mesure size of unknown directory " ^ thing)
        | File {name; size} -> SFile {name=name; size=size}
        | Dir {name; children} -> SDir {name=name; size=size_of state; children=(List.map make_sized_state children)}
    in
    let sized_final_state = make_sized_state final_state
    in
    let rec count_small_files sized_state =
        match sized_state with
        | SUnknown thing -> failwith ("Can't mesure size of unknown directory " ^ thing)
        | SFile {name; size} -> 0
        | SDir {name; size; children} ->
            let contribution = if size <= 100000 then size else 0
            in
            contribution + List.fold_left ( + ) 0 (List.map count_small_files children)
    in
    string_of_int (count_small_files sized_final_state)



let naloga2 vsebina_datoteke =
    let to_first_newline text =
        let first_newline = Str.bounded_split (Str.regexp "\n") text 2
        in
        List.nth first_newline 0, if List.length first_newline >= 2 then List.nth first_newline 1 else ""
    in
    let to_first_space text =
        let first_space = Str.bounded_split (Str.regexp " ") text 2
        in
        List.nth first_space 0, if List.length first_space >= 2 then List.nth first_space 1 else ""
    in
    let wierd_commands = Str.split (Str.regexp {|\$ |}) vsebina_datoteke
    in
    let process_command command =
        let call, out = to_first_newline command
        in
        (to_first_space call, out)
    in
    let commands = List.map process_command wierd_commands
    in
    let remove_empty_lines lines = List.filter (fun line -> line <> "") lines
    in
    let get_dir_contents_from_ls ls_output =
        (* make dir list from the output of ls *)
        let process_line line =
            let prefix, name = to_first_space line
            in
            match prefix with
            | "dir" -> Unknown name
            | num -> File {name=name; size=int_of_string num}
        in
        List.map process_line (remove_empty_lines (Str.split (Str.regexp "\n") ls_output))
    in
    let rec update_cwd cwd new_dir =
        if new_dir = ".." then
            match cwd with
            | Location {name; child} -> (
                match child with
                | Location _ -> Location {name=name; child=update_cwd child new_dir}
                | End -> End
            )
            | End -> End
        else
        match cwd with
        | Location {name; child} -> Location {name=name; child=update_cwd child new_dir}
        | End -> Location {name=new_dir; child=End}
    in
    let dir_name_is search_name some_dir =
        match some_dir with
        | Unknown name -> name = search_name
        | File _ -> false
        | Dir {name; _} -> name = search_name
    in
    let dir_name_isnt search_name some_dir = not (dir_name_is search_name some_dir)
    in
    let rec write_dir cwd state dir_contents =
        match cwd with
        | Location {name; child} -> (
            match child with
            | End -> (
                match state with
                | Unknown name_to_write_to -> Dir {name=name; children=dir_contents}
                | _ -> failwith "write_dir: got to a node but state is not Unknown."
            )
            | Location child_loc -> (
                match state with
                | Dir state_dir -> (
                    let modifier_child = List.find (dir_name_is child_loc.name) state_dir.children
                    in
                    let unmodifier_children = List.filter (dir_name_isnt child_loc.name) state_dir.children
                    in
                    Dir {state_dir with children=(write_dir child modifier_child dir_contents) :: unmodifier_children}
                )
                | _ -> failwith "write_dir: got to a node when cwd still wants to go deeper."
            )
        )
        | End -> (
            match state with
            | Unknown dname -> Dir {name=dname; children=dir_contents}
            | _ -> failwith "write_dir was used when the cwd isn't Unknown."
        )
    in
    let execute_command command cwd state =
        let (prog, args), out = command
        in
        match prog with
        | "ls" -> (
            cwd, write_dir cwd state (get_dir_contents_from_ls out)
        )
        | "cd" -> update_cwd cwd args, state
        | _ -> failwith ("Found bad program " ^ prog)
    in
    let rec execute_commands commands' cwd state=
        match commands' with
        | [] -> cwd, state
        | c :: cs ->
            let new_cwd, new_state = execute_command c cwd state
            in
            execute_commands cs new_cwd new_state
    in
    let _, final_state = execute_commands commands (End) (Unknown "/")
    in
    let rec size_of directory =
        match directory with
        | Unknown thing -> failwith ("Can't mesure size of unknown directory " ^ thing)
        | File file -> file.size
        | Dir child_dir -> List.fold_left ( + ) 0 (List.map size_of child_dir.children)
    in
    let rec make_sized_state state =
        match state with
        | Unknown thing -> failwith ("Can't mesure size of unknown directory " ^ thing)
        | File {name; size} -> SFile {name=name; size=size}
        | Dir {name; children} -> SDir {name=name; size=size_of state; children=(List.map make_sized_state children)}
    in
    let sized_final_state = make_sized_state final_state
    in
    let disk_size = 70000000
    in
    let update_size = 30000000
    in
    let used_size = size_of final_state
    in
    let need_to_free_up = update_size - (disk_size - used_size)
    in
    let rec find_dir_to_delete sized_state =
        match sized_state with
        | SUnknown thing -> failwith ("Can't mesure size of unknown directory " ^ thing)
        | SFile {name; size} -> max_int
        | SDir {name; size; children} -> 
            let effective_size = if size >= need_to_free_up then size else max_int
            in
            min effective_size (List.fold_left min max_int (List.map find_dir_to_delete children))
                
    in
    find_dir_to_delete sized_final_state |> string_of_int


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
