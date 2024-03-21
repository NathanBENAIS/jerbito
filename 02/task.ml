
 let print_bool b = 
 if b then print_string "true" else print_string "false"
 
(* Q1 *)
let last lst =
  match List.rev lst with
  | [] -> failwith "List is empty"
  | hd :: _ -> hd
  
(* Q2 *)
let swap lst =
match lst with
| first :: second :: rest -> second :: first :: rest
| _ -> lst

(* Q3 *)

let repeat x n =
if n < 0 then []
else List.init n (fun _ -> x)

(* Q4 *)
let range_i i j =
if i > j then []
else List.init (j - i + 1) (fun k -> k + i)

(* Q5 *)
let rec decr_list lst =
  match lst with
  | [] -> []
  | hd :: tl -> (hd - 1) :: decr_list tl


(* Q6 *)
(* rev : 'a list -> 'a list *)
let rev lst =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (hd :: acc) tl
  in
  aux [] lst


 (* mem : 'a -> 'a list -> bool *)

 
let rec mem x lst =
  match lst with
  | [] -> false
  | hd :: tl -> if hd = x then true else mem x tl

(* append : 'a list -> 'a list -> 'a list *)
 let rec append lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | hd :: tl -> hd :: (append tl lst2)
  

(*repeat : 'a -> int -> 'a list *)
let rec repeattt x n =
  if n <= 0 then []
  else x :: repeattt x (n - 1)
(* Q7 *)
let rec flat lst =
  match lst with
  | [] -> []
  | hd :: tl -> append hd (flat tl)
(* Q8 *)
  let rec interpose x lst =
  match lst with
  | [] -> []
  | hd :: tl -> x :: hd :: interpose x tl
(* Q9 *)
let rec stutter lst=
  match lst with
  | [] -> []
  | hd :: tl -> hd :: hd :: stutter tl
(* Q10 *)
let rec add_list lst1 lst2 =
  match lst1, lst2 with
  | [], _ -> lst2
  | _, [] -> lst1
  | hd1 :: tl1, hd2 :: tl2 -> (hd1 + hd2) :: add_list tl1 tl2
  
(* Q11 *)
 let rec remove_dup lst =
  match lst with
  | [] -> []
  | [x] -> [x]
  | hd1 :: (hd2 :: _ as tl) -> if hd1 = hd2 then remove_dup tl else hd1 :: remove_dup tl

(* Q12 *)
let rec is_sorted lst =
  match lst with
  | [] | [_] -> true
  | hd1 :: hd2 :: tl -> hd1 <= hd2 && is_sorted (hd2 :: tl)
(* Q13 *)
let incr_list lst =
  List.map (fun x -> x + 1) lst
(* Q14 *)
let only_less n lst = 
    List.filter (fun x -> x < n) lst


  