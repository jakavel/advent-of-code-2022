let naloga_dan = 15

type point = int * int

type beacon = Beacon of point

type sensor = Sensor of point

let point_of_beacon (Beacon p) = p
let point_of_sensor (Sensor p) = p


let parse_line line =
    let line_re =
        (Str.regexp {|Sensor at x=\([0-9-]+\), y=\([0-9-]+\): closest beacon is at x=\([0-9-]+\), y=\([0-9-]+\)|})
    in
    let _ = Str.string_match line_re line 0
    in
    Sensor (Str.matched_group 1 line |> int_of_string, Str.matched_group 2 line |> int_of_string),
    Beacon (Str.matched_group 3 line |> int_of_string, Str.matched_group 4 line |> int_of_string)

let parse_lines lines =
    let split_lines = Str.split (Str.regexp "\n") lines
    in
    let split_lines = List.filter (( <> ) "") split_lines
    in
    List.map parse_line split_lines


let ( ++ ) left right =
    (fst left + fst right, snd left + snd right)

type beacon_sphere = {origin: sensor; radius: int}

type interval =
    | EmptyInterval
    | Interval of int * int

type bound = {lower: int; upper: int}
let update_bound {lower; upper} point = {lower = min lower point; upper = max upper point}

let get_max_bounds intervals =
    let empty_bound = {lower = max_int; upper = min_int}
    in
    let update_bounds_with_interval bound new_interval =
        match new_interval with
        | Interval (a, b) ->
            let new_bound = update_bound bound a
            in
            update_bound new_bound b
        | EmptyInterval -> bound
    in
    List.fold_left update_bounds_with_interval empty_bound intervals


let distance point other =
    (abs (fst point - fst other)) + (abs (snd point - snd other))


let get_interval_from_sphere test_y sphere =
    let s_x, s_y = point_of_sensor sphere.origin
    in
    let offset = abs (s_y - test_y)
    in
    let effective_radius = sphere.radius - offset
    in
    if effective_radius <= 0 then
        EmptyInterval
    else
        Interval (s_x - effective_radius, s_x + effective_radius)


let test_y = 2000000 (* 2000000 for the real test *)

let is_in_interval point interval =
    match interval with
    | EmptyInterval -> false
    | Interval (a, b) -> (a <= point) && (point <= b)


let rec make_spheres beacons sensors =
    match beacons, sensors with
    | b :: bs, s :: ss ->
        let radius = distance (point_of_beacon b) (point_of_sensor s)
        in
        {origin=s; radius=radius} :: make_spheres bs ss
    | [], [] -> []
    | _ -> failwith "make_spheres got lists of different lengths."


let rec is_a_beacon point beacons =
    match beacons with
    | [] -> false
    | Beacon b_point :: bs ->
        if point = b_point then true else is_a_beacon point bs


let naloga1 vsebina_datoteke =
    let sensors, beacons = parse_lines vsebina_datoteke |> List.split
    in
    let spheres = make_spheres beacons sensors
    in
    let intervals = List.map (get_interval_from_sphere test_y) spheres
    in
    let bounds = get_max_bounds intervals
    in
    let occupied = ref 0
    in
    for i=bounds.lower to bounds.upper do
        if not (is_a_beacon (i, test_y) beacons) then
        if List.exists (is_in_interval i) intervals then
            occupied := !occupied + 1
    done;
    !occupied |> string_of_int


let rec snip_intervals lower upper intervals =
    match intervals with
    | [] -> []
    | i :: is ->
        let snipped_i = match i with
        | EmptyInterval -> EmptyInterval
        | Interval (a, b) -> Interval (max lower a, min upper b)
        in
        snipped_i :: snip_intervals lower upper is

let min_coordinates, max_coordinates = 0, 4000000


let rec combine_intervals merged remaining =
    let (covered_left, covered_right) =
        match List.nth merged (List.length merged - 1) with
        | Interval (a, b) -> (a, b)
        | _ -> failwith "Expected an interval to be the last item on the merged array."
    in
    let others = List.filteri (fun i _ -> i < List.length merged - 1) merged
    in
    match remaining with
    | Interval (a, b) :: rs ->
        if a > covered_right + 1 then
            combine_intervals (merged @ [Interval (a, b)]) rs
        else
            combine_intervals (others @ [Interval (covered_left, max b (covered_right))]) rs
    | [] -> merged
    | _ -> failwith "combine intervals encountered an empty interval"


let get_missing_point (intervals, y) =
    let intervals = List.filter (( <> ) EmptyInterval) intervals
    in
    let intervals = snip_intervals min_coordinates max_coordinates intervals
    in
    let compare_by_left i j =
        match i, j with
        | Interval (i_a, _), Interval (j_a, _) -> i_a - j_a
        | _ -> failwith "Can't sort empty intervals."
    in
    let sorted_intervals = List.sort compare_by_left intervals
    in
    let merged = combine_intervals [List.nth sorted_intervals 0] sorted_intervals
    in
    match merged with
    | Interval (a, b) :: Interval (c, d) :: _ -> (
        Printf.printf "y: %d (b, c): %d, %d\n" y b c;
        assert (c - b = 2);
        Some ((b + 1), y)
    )
    | [Interval (a, b)] ->
        if a = min_coordinates + 1 then
            Some (min_coordinates, y)
        else if b = max_coordinates - 1 then
            Some (max_coordinates, y)
        else if (a = min_coordinates) && (b = max_coordinates) then
            None
        else
            failwith "non deterministic interval."
    | _ -> failwith "intervals were merged into an empty array."

let naloga2 vsebina_datoteke =
    let sensors, beacons = parse_lines vsebina_datoteke |> List.split
    in
    let spheres = make_spheres beacons sensors
    in
    let make_intervals_for y = List.rev_map (get_interval_from_sphere y) spheres
    in
    let possble_y = List.init (max_coordinates - min_coordinates + 1) (fun i -> min_coordinates + i)
    in
    let missing_points = List.rev_map (fun y -> ((y |> make_intervals_for), y) |> get_missing_point) possble_y
    in
    let find_not_none value =
        match value with
        | Some _ -> true
        | None -> false
    in
    match List.find find_not_none missing_points with
    | Some (x, y) -> (
        let wierd_value = Int64.add (Int64.mul (Int64.of_int x) 4000000L) (Int64.of_int y)
        in
        Printf.sprintf "%Ld\n" wierd_value
    )
    | None -> failwith "what"


let preberi_datoteko ime_datoteke =
    let chan = open_in ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina

let _ =
    let izpisi_datoteko ime_datoteke vsebina =
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

