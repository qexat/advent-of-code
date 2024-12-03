open Ext

module Solution : Day.SOLUTION_TYPE = struct
  let day_number = 1

  let example = {|3   4
4   3
2   5
1   3
3   9
3   3|}

  let parse (input : string) : int list list =
    input
    |> String.split_on_char '\n'
    |> List.filter String.is_empty
    |> List.map String.split_words
    |> List.map (List.map int_of_string)
  ;;

  let () =
    parse example
    |> List.iter (fun l ->
      List.iter (Printf.printf "%d ") l;
      Printf.printf "\n")
  ;;

  let get_part1 (_ : string) : int option = None
  let get_part2 (_ : string) : int option = None
end

let () = DayManager.register (module Day.MakeDay (Solution))
