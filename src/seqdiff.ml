open Core.Std

type diff =
    | Duplicate of { value : int; count : int }
    | Missing of { value : int; count : int }

let diffs xs =
    let rec loop xs ev rc =
        match xs with
        | v :: vs when v = ev - 1 -> loop vs ev (rc + 1)
        | v :: vs ->
            let d1 = if rc > 0 then Duplicate { value = (ev - 1); count = rc } |> Some else None in
            let d2 = if v > ev then Missing { value = ev; count = v - ev } |> Some else None in
            let ds = loop vs (v + 1) 0 in
            (match (d1, d2) with
            | (Some d1, Some d2) -> d1 :: d2 :: ds
            | (Some d1, None) -> d1 :: ds
            | (None, Some d2) -> d2 :: ds
            | _ -> ds)
        | [] -> if rc > 0 then [Duplicate { value = (ev - 1); count = rc }] else []
    in
    loop xs 0 0

let string_of_diff = function
    | Duplicate { value = v; count = c } -> sprintf "Duplicate (%d, %d)" v c
    | Missing { value = v; count = c } -> sprintf "Missing (%d, %d)" v c

let print_list f xs =
    List.map ~f xs
    |> String.concat ~sep:", "
    |> print_endline

let () =
    let xs = [0; 1; 2; 3; 4; 4; 4; 4; 5; 6; 7; 8; 11; 12] in
    print_list string_of_int xs;
    print_list string_of_diff (diffs xs)
