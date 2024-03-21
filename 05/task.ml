(* File (LIFO) *)
type 'a queue = {
  front : 'a list;
  rear_rev : 'a list
}

let empty = { front = []; rear_rev = [] }

let is_empty q = q.front = []

let hd q = List.hd q.front

let check = function (* private *)
| { front = []; rear_rev } -> { front = List.rev rear_rev; rear_rev = [] }
| q -> q

let snoc x q = check { q with rear_rev = x :: q.rear_rev }

let tl q = check { q with front = List.tl q.front }

let uncons = function
| { front = x :: front; _ } as q -> Some (x, { q with front })
| _ -> None
(* File (LIFO) *)
let of_list lst =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (snoc hd acc) tl
  in
  let q = aux empty lst in
  { front = List.rev q.rear_rev; rear_rev = [] }



let rec to_list q =
  if is_empty q then []
  else hd q :: to_list (tl q)

let rec of_list q u =
  match u with 
  [] -> q 
  | hd :: u -> of_list (snoc hd q) u

  let of_list u = of_list empty u
  
  (* [1;2;3;4;5] |> of_list |> to_list;; *)


  (* Fonction d'association *)

let rec def f x y z =
  if x = z then y
  else f z



let sq x =
  if 0 <= x && x <= 10 then x * x
  else failwith "undefined"

type 'a tstamp = { data : 'a; date : float; }
(* let bogus_sq x = def (fun z -> z * z) x 42 42 *)

(* let obs_contat a b =
  match a,b with
  |[],[] -> []
  |[],v -> v
  |u,[] -> u
  |u,v -> u @ List.map 
  (fun {data;date} -> {data;date = date +. = (delay (list_last u).date (List.hd v).date)})v;; *)
  







  




type 'a tstamp = { data : 'a; date : float; }




(* of_list [1;2;3;4;5];; *)












(* type 'a tsmap = {data: 'a ; date : float'}

let tsmap_joint {ts: } *)