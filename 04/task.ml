(* Q1 *)

let first a = 
  match a with 
  (x,_) -> x
let second a = 
  match a with 
  (_,y) -> y

(*  Q2 *)
  let sum (a1, b1) (a2, b2) = (a1 + a2, b1 ^ b2)
(* Q3 *)
  let suffix_prefix str c =
  let len = String.length str in
  let rec aux i =
    if i >= len then (None, None)
    else if str.[i] = c then
      let prev = if i > 0 then Some str.[i-1] else None in
      let next = if i < len - 1 then Some str.[i+1]
      else None in
      (prev, next)
    else aux (i+1)
  in
  aux 0
(* Q4 *)

let rec zip l1 l2 = 
  match l1, l2 with
    | [], [] -> []
    | [], _ | _, [] -> raise (Invalid_argument "Listes de taille différente")
    | x::xs, y::ys -> (x, y) :: zip xs ys

let rec unzip l = 
  match l with
    | [] -> ([], [])
    | (x, y)::xs -> let (l1, l2) = unzip xs in (x::l1, y::l2)


(* Q5 *)

exception Empty;;


let rec min_max lst = 
match lst with
  | [] -> raise Empty
  | [x] -> (x, x)
  | x :: tl -> let (a, b) = min_max tl in
               (min x a, max x b);;


let min_max_fold lst =
  if lst = [] then raise Empty
  else let base = (List.hd lst, List.hd lst) in
       List.fold_left (fun (a, b) x -> (min a x, max b x)) base lst;;
(* Q6 *)





  
