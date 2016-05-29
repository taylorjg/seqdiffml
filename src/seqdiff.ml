open Core.Std

type value_and_count = { value : int; count : int }

type diff =
    | Duplicate of value_and_count
    | Missing of value_and_count

let () =
    let xs = [0; 1; 2; 3; 4; 4; 4; 4; 5; 6; 7; 8; 11; 12] in
    List.map ~f:string_of_int xs
    |> String.concat ~sep:" "
    |> print_endline
