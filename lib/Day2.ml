open Ext

module Solution : Day.SOLUTION_TYPE = struct
  let day_number = 2

  let example = {|7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9|}

  let is_safe (report : int list) : bool =
    report
    |> List.pairwise
    |> List.map (Pair.join ( - ))
    |> Pair.fork
         (fun deltas -> deltas |> List.pairwise |> List.map (Pair.join Int.same_polarity))
         (fun deltas -> deltas |> List.map (Fun.compose (Int.in_range 1 3) Int.abs))
    |> Pair.both all
    |> Pair.join ( && )
  ;;

  let parse (input : string) : int list list =
    input
    |> String.split_on_char '\n'
    |> List.filter String.is_empty
    |> List.map String.split_words
    |> List.map (List.map int_of_string)
  ;;

  let get_part1 (input : string) : int option =
    input
    |> parse
    |> List.map is_safe
    |> List.map Bool.to_int
    |> List.fold_left ( + ) 0
    |> Option.some
  ;;

  let get_part2 (_ : string) : int option = None
end

let () = DayManager.register (module Day.MakeDay (Solution))
