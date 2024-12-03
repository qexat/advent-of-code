let error (message : string) : 'a =
  Printf.fprintf stderr "\x1b[1;31mError:\x1b[22;39m %s\n" message;
  exit 1
;;

module Parser = struct
  let usage_message = "aoc -day <number> [-attempt]"
  let day_ref = ref 0
  let is_attempt = ref false

  let spec =
    [ "-day", Arg.Set_int day_ref, "Advent of Code day number"
    ; "-attempt", Arg.Set is_attempt, "Use the puzzle input instead of the example"
    ]
  ;;

  type parsed_args =
    { day_number : int
    ; is_attempt : bool
    }

  let parse () : parsed_args =
    Arg.parse spec (fun _ -> ()) usage_message;
    let day = !day_ref in
    if not (1 <= day && day <= 25)
    then error (Printf.sprintf "day %d is invalid" day)
    else { day_number = day; is_attempt = !is_attempt }
  ;;
end

let () =
  let parsed_args = Parser.parse () in
  match Aoc.DayManager.get parsed_args.day_number with
  | None -> error (Printf.sprintf "day %d is not registered" parsed_args.day_number)
  | Some (module Day) -> Day.print_results ~is_attempt:parsed_args.is_attempt ()
;;
