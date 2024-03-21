let char_succ c = 
  if c < '\255' then
    c|> int_of_char |> succ |> char_of_int
    else
      '\000'
      
let string_cons c s = 
String.make 1 c ^ s

let rec char_range lo hi =
if lo > hi then
  ""
else
  string_cons lo (char_range (char_succ lo) hi)


(*  *)
let char_pred c =
  if c > '\000' then
    c |> int_of_char |> pred |> char_of_int
  else
    '\255'

let rec char_range_rev_cat s lo hi =
  if hi < lo then
    s
  else
    char_range_rev_cat (string_cons hi s) lo (char_succ hi)




let _ =
let a = int_of_string "81" in 
let b = float_of_int a in 
let c = sqrt b in 
let d = int_of_float c in
d



let p =
  let cr = "cream" in 
  let i_u_we = 
  let sr = string_cons 's' cr in
  "I " ^ sr ^ ", you " ^sr^ ", we all "^ sr ^  " for ice " in           i_u_we ^ " for ice " ^ cr ;;
  

  