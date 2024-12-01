open Base

(* let pi = ;; *)

let parsed =
  pi
  |> String.split_lines
  |> List.map ~f:(fun s -> String.split s ~on:' ')
  |> List.map ~f:(List.filter ~f:(fun s -> not (String.equal s "")))
  |> List.map ~f:(List.map ~f:Int.of_string)
  |> List.transpose_exn
;;

(* Part 1 *)
let () =
  parsed
  |> List.map ~f:(List.sort ~compare:Int.compare)
  |> List.transpose_exn
  |> List.map ~f:(fun l ->
    match l with
    | [ f; s ] -> abs (f - s)
    | _ -> failwith "whatever")
  |> List.fold_left ~f:( + ) ~init:0
  |> Stdlib.Printf.printf "%d\n"
;;

(* Part 2 *)
let left, right =
  parsed
  |> function
  | [ a; b ] -> a, b
  | _ -> failwith "unreachable"
;;

let () =
  List.map ~f:(fun iteml -> List.count ~f:(fun itemr -> Int.equal iteml itemr) right) left
  |> List.map2_exn ~f:( * ) left
  |> List.fold_left ~f:( + ) ~init:0
  |> Stdlib.Printf.printf "%d\n"
;;
