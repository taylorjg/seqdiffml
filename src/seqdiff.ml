open Core.Std

type value_and_count = { value : int; count : int }

type diff =
    | Duplicate of value_and_count
    | Missing of value_and_count

let diffs xs =
    let rec loop xs ev rc =
        match xs with
        | [] -> if rc > 0 then [Duplicate { value = (ev - 1); count = rc }] else []
        | v :: vs when v = ev - 1 -> loop vs ev (rc + 1)
        | v :: vs ->
            let d1 = if rc > 0 then Duplicate { value = (ev - 1); count = rc } |> Some else None in
            let d2 = if v > ev then Missing { value = ev; count = v - ev } |> Some else None in
            let ds = loop vs (v + 1) 0 in
            match (d1, d2) with
            | (Some d1, Some d2) -> d1 :: d2 :: ds
            | (Some d1, None) -> d1 :: ds
            | (None, Some d2) -> d2 :: ds
            | (None, None) -> ds
    in
    loop xs 0 0

let print_diff d =
    match d with
    | Duplicate { value = v; count = c } -> printf "Duplicate (%d, %d)\n" v c
    | Missing { value = v; count = c } -> printf "Missing (%d, %d)\n" v c

let () =
    let xs = [0; 1; 2; 3; 4; 4; 4; 4; 5; 6; 7; 8; 11; 12] in
    List.map ~f:string_of_int xs
    |> String.concat ~sep:" "
    |> print_endline;
    diffs xs
    |> List.iter ~f:print_diff
