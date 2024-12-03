let days : (module Day.DAY_TYPE) list ref = ref []
let is_valid_day_number (number : int) : bool = 1 <= number && number <= 25
let register (day : (module Day.DAY_TYPE)) : unit = days := day :: !days

let get (number : int) : (module Day.DAY_TYPE) option =
  if not (is_valid_day_number number)
  then None
  else List.find_opt (fun (module M : Day.DAY_TYPE) -> number = M.day_number) !days
;;
