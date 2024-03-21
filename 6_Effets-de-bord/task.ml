(*  *)

 let print_bool b = 
 if b then print_string "true" else print_string "false"

let hello_world () : unit = print_string "Hello, world!"



let print_int_option (x : int option) : unit =
  match x with
  | None -> print_string "None"
  | Some x -> print_int x


 (* Q4 *)
type json =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Array of json list
  | Object of (string * json) list;;

let rec print_list printer lst =
  match lst with
  | [] -> ()
  | [x] -> printer x
  | hd :: tl -> printer hd; print_string ", "; print_list printer tl;;

let rec print_json j =
  match j with
  | Null -> print_string "Null"
  | Bool b -> print_string (string_of_bool b)
  | Int i -> print_int i
  | Float f -> print_float f
  | String s -> print_string s
  | Array a ->
      print_string "[";
      print_list print_json a;
      print_string "]"
  | Object o ->
      print_string "{";
      print_list (fun (k, v) -> print_string k; print_string ": "; print_json v) o;
      print_string "}";;

(*

print_json Null;;

print_json (Bool true);;

print_json (Int 1);;


print_json (Float 1.23);;

print_json (String "test");;


print_json (Array [Int 1; Int 2; Int 3]);;

(* Object *)

print_json (Object [("key1", Int 1); ("key2", String "value")]);;

let example_json =
Object
  [
    ("name", String "Alice");
    ("age", Int 25);
    ("is_student", Bool false);
    ("height", Float 1.65);
    ("languages", Array [String "OCaml"; String "Python"; String "JavaScript"]);
  ];;
print_json example_json;; 


*)


(*  Tableaux et références *)
let rec fibonacci n =
  if n <= 0 then  0
  else if n = 1 then
    1
  else
    (fibonacci (n - 1) + fibonacci (n - 2));;

    
(* Q2 *)
let fibonacci n =
let memo = Array.make (n+1) None in
let rec fib n =
  match memo.(n) with
  | Some res -> res
  | None ->
    let res =
      if n <= 0 then 0
      else if n = 1 then 1
      else fib (n - 1) + fib (n - 2)
    in
    memo.(n) <- Some res;
    res
in
fib n;;

(* fibonacci 4  *)



let rec length = 
  match l with
 | [] -> 0
 | _::tl -> 1 + length tl 
 