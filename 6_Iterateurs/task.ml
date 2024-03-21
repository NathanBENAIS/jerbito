(* ('a -> 'b) -> 'a list -> 'b list *)
let map f l = List.map f l

(* (int -> 'a -> 'b) -> 'a list -> 'b list *)
let mapi f l = List.mapi f l

(* ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc *)
let fold_left f acc l = List.fold_left f acc l

(* ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc *)
let fold_right f l acc = List.fold_right f l acc

(* ('a -> bool) -> 'a list -> 'a list *)
let filter f l = List.filter f l

(* ('a -> 'b option) -> 'a list -> 'b list *)
let filter_map f l = List.filter_map f l

(* ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a list -> 'acc * 'b list *)
let map_accum f acc l =
  let rec aux acc l =
    match l with
    | [] -> (acc, [])
    | x :: xs ->
      let (acc', y) = f acc x in
      let (acc'', ys) = aux acc' xs in
      (acc'', y :: ys)
  in
  aux acc l

(* ('a -> 'b list) -> 'a list -> 'b list *)
let flat_map f l = List.concat (List.map f l)

(* 'a option -> ('a -> 'b) -> 'b option *)
let map_option f = function
  | None -> None
  | Some x -> Some (f x)

(* 'a option -> ('a -> 'b option) -> 'b option *)
let bind_option f = function
  | None -> None
  | Some x -> f x

(* unit -> 'a list *)
let create_list () = []
