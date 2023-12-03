
(* 
let acc = ref [] in
    try
        while true do
            acc := read_line () :: !acc;
        done
    with
        End_of_file -> print_string (String.concat "\n" !acc)


let gameId = split(":")[0] | split(" ")[1]

let sets = split(":")[1] split(";") flatmap(parse_set())

parse_set() = split(",") map(parse_entry)

    arr = split(set_str, " ")
    count = arr[0]
    colour = arr[1]
*)

let (<<) f g x = f(g(x));;

type set_entry = {count : int; colour : string}
type game = {id: int; entries : set_entry list}

let print_set_entry entry = Printf.sprintf "Count: %d, Colour: %s" entry.count entry.colour
let print_game game = Printf.sprintf "Game: %d => Set: %s" game.id (String.concat "; " (List.map print_set_entry game.entries))

(* Parsing *)

let parse_entry entry_str =  
    let ls = String.trim entry_str
        |> String.split_on_char ' ' in
    let count = List.hd ls |> Stdlib.int_of_string in
    let colour = List.tl ls |> List.hd in
    {count = count; colour = colour}

let parse_set set_str = 
    let entries_str = String.split_on_char ',' set_str in
    List.map (parse_entry << String.trim) entries_str

let parse_sets all_sets_str = 
    let set_strs = String.split_on_char ';' all_sets_str in
    List.map parse_set set_strs |> List.flatten

let parse_line line = 
    let game_str = String.split_on_char ':' line |> List.hd in
    let game_id = String.split_on_char ' ' game_str |> List.tl |> List.hd |> Stdlib.int_of_string in
    let all_sets_str = String.split_on_char ':' line |> List.tl |> List.hd in
    {id = game_id; entries = parse_sets all_sets_str}

(* Max by colour *)

let product game =
    let get_count = fun e -> e.count in
    let is_colour colour = fun e -> e.colour = colour in
    let max_number_list lst = 
        List.fold_left max min_int lst in
    let max_colour colour = 
        let filtered = List.filter (is_colour colour) game.entries in
        let counts = List.map get_count filtered in
        max_number_list counts in
    let red = max_colour "red" in
    let green = max_colour "green" in
    let blue = max_colour "blue" in
    let product = red * green * blue in
    print_endline (Printf.sprintf "Red:%d, Green:%d, Blue:%d, Product: %d" red green blue product);
    product

(* IO *)

let read_lines ic =
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []


let () = 
    let lines = read_lines Stdlib.stdin in
    let parsed_lines = List.map parse_line lines in
    let products = List.map product parsed_lines in
    let sum_of_products = List.fold_left (+) 0 products in
    print_endline (Printf.sprintf "Sum of products: %d" sum_of_products);
    print_endline "End !"





(*
    List.iter (print_endline << print_game) parsed

Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
*)
