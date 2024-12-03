module Int = struct
  include Int

  let same_polarity (i0 : t) (i1 : t) : bool = i0 >= 0 = (i1 >= 0)
  let in_range (left : t) (right : t) (i : t) : bool = left <= i && i <= right
end

module List = struct
  include List

  let rec pairwise : 'a t -> ('a * 'a) t = function
    | first :: second :: rest -> (first, second) :: pairwise (second :: rest)
    | _ -> []
  ;;
end

module Pair = struct
  type ('a, 'b) t = 'a * 'b

  let join (f : 'a -> 'b -> 'c) : ('a, 'b) t -> 'c = fun (left, right) -> f left right
  let disjoin (f : ('a, 'b) t -> 'c) : 'a -> 'b -> 'c = fun left right -> f (left, right)

  let both (f : 'a -> 'b) : ('a, 'a) t -> ('b, 'b) t =
    fun (left, right) -> f left, f right
  ;;

  let fork (f : 'a -> 'b) (g : 'a -> 'c) : 'a -> ('b, 'c) t =
    fun value -> f value, g value
  ;;
end

module String = struct
  include String

  let is_empty (s : t) : bool = s <> empty
  let split_words (s : t) : t list = s |> split_on_char ' ' |> List.filter is_empty
end

let all (values : bool list) : bool = List.fold_left ( && ) true values
