module type SOLUTION_TYPE = sig
  val day_number : int
  val example : string
  val get_part1 : string -> int option
  val get_part2 : string -> int option
end

module type DAY_TYPE = sig
  include SOLUTION_TYPE

  val print_results : is_attempt:bool -> unit -> unit
end

module MakeDay (Solution : SOLUTION_TYPE) : DAY_TYPE = struct
  include Solution

  let get_filename (day_number : int) : string = Printf.sprintf "./inputs/%d" day_number

  let get_input ~(is_attempt : bool) () =
    match is_attempt with
    | false -> example
    | true ->
      let filename = get_filename Solution.day_number in
      let file = open_in filename in
      In_channel.input_all file
  ;;

  let print_part (number : int) (maybe_result : int option) : unit =
    Printf.printf "\x1b[1;34mPart %d\x1b[22;39m\n" number;
    Printf.printf "  ";
    match maybe_result with
    | None -> Printf.printf "\x1b[2mNo solution yet.\x1b[22m\n"
    | Some result -> Printf.printf "\x1b[36m%d\x1b[39m\n" result
  ;;

  let print_results ~(is_attempt : bool) () =
    let input = get_input ~is_attempt () in
    Printf.printf "\x1b[1;35m───── Day %d ─────\x1b[22;39m\n" day_number;
    print_part 1 (Solution.get_part1 input);
    print_part 2 (Solution.get_part2 input)
  ;;
end
