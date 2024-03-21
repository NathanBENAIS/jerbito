let print_bool b = 
if b then print_string "true" else print_string "false"


(* Q1 *)
type partie = Victoire | Nul | Defaite

let valeur_partie = function
  | Victoire -> 1.0
  | Nul -> 0.5
  | Defaite -> 0.0

(* Q2 *)

  type semaine = Lundi | Mardi | Mercredi | Jeudi | Vendredi | Samedi | Dimanche

  let est_un_weekend = function
  | Samedi | Dimanche -> true
  | _ -> false

(* Q3 Q4 Q5*)
type etat = Metro | Boulot | Dodo | Vacances;;

let action_suivante etat = 
match etat with
| Metro -> Boulot
| Boulot -> Dodo
| Dodo -> Vacances
|Vacances -> Metro

let afficher_enum  = function  
  | Metro -> "Metro"
  | Boulot -> "Boulot"
  | Dodo -> "Dodo"
  | Vacances -> "Vacances"

  (*Q6  *)

  (* type couleur = Pique | Coeur | Carreau | Trefle

  type valeur = As | Sept | Huit | Neuf | Dix | Valet | Dame | Roi

  type carte_a_jouer = Carte of valeur * couleur


let est_rouge = function 
| Carte(_,Coeur ) | Carte(_,Carreau ) -> true
| _ -> false

let est_une_tete = function
  | Carte(Roi,_) | Carte(Dame,_) | Carte(Valet,_)-> true
  | _ -> false *)
  
(* 
let score_normal = function
  | Carte(As, _) -> 11 
  | Carte(Dix, _) -> 10
  | Carte(Valet, _) -> 2
  | Carte(Dame, _) -> 3
  | Carte(Roi, _) -> 4
  |  _ -> 0
  
let score_atout  = function
  | Carte(As, _) -> 11
  | Carte(Neuf, _) -> 14
  | Carte(Dix, _) -> 10
  | Carte(Valet, _) -> 20
  | Carte(Dame, _) -> 3
  | Carte(Roi, _) -> 4
  | _ -> 0
 *)
  (* Constructeurs avec données *)

  type nombre = Entier of int | Flottant of float
  
let somme a b = match a, b with
  | Entier x, Entier y -> Entier (x + y)
  | Flottant x, Flottant y -> Flottant (x +. y)
  | Entier x, Flottant y | Flottant y, Entier x -> Flottant (float_of_int x +. y)

let division a b = match a, b with
  | Entier x, Entier y -> if x mod y = 0 then Entier (x / y) else Flottant (float_of_int x /. float_of_int y)
  | Flottant x, Flottant y -> Flottant (x /. y)
  | Entier x, Flottant y | Flottant y, Entier x -> Flottant (float_of_int x /. y)
  
  
  type temperature = Celsius of float


(* type couleur = Pique | Coeur | Carreau | Trefle

type valeur = As | Sept | Huit | Neuf | Dix | Valet | Dame | Roi

type carte_a_jouer = Carte of valeur * couleur

type couleur_carte = Rouge of carte_a_jouer | Noir of carte_a_jouer

type tete_carte = Tete of carte_a_jouer | NonTete of carte_a_jouer

let est_rouge = function 
| Rouge(_) -> true
| _ -> false

let est_une_tete = function
  | Tete(_) -> true
  | _ -> false


let score_normal = function
  | Carte(As, _) -> 11 
  | Carte(Dix, _) -> 10
  | Carte(Valet, _) -> 2
  | Carte(Dame, _) -> 3
  | Carte(Roi, _) -> 4
  |  _ -> 0

let score_atout  = function
  | Carte(As, _) -> 11
  | Carte(Neuf, _) -> 14
  | Carte(Dix, _) -> 10
  | Carte(Valet, _) -> 20
  | Carte(Dame, _) -> 3
  | Carte(Roi, _) -> 4
  | _ -> 0 

type atout = Atout of couleur | NonAtout

let score carte atout = match carte, atout with
  | Carte(_, couleur), Atout(atout_couleur) when couleur = atout_couleur -> score_atout carte
  | _ -> score_normal carte

*)

(* Types enregistrements (ou produit) *)


(* 
type couleur = {type_couleur : string}
type valeur = {type_valeur : string}


type point = { x : float; y : float }
type couleur = {type_couleur : string}
type valeur = {type_valeur : string}

(* type carte_a_jouer = {valeur : valeur; couleur : couleur} *)
type carte_a_jouer = 
| Carte of {valeur : valeur; couleur : couleur}
| Joker

(* let est_rouge carte = 
  match carte.Carte.couleur.type_couleur with 
  | "Coeur" | "Carreau" -> true
  | _ -> false

let est_une_tete carte =
  match carte.valeur.type_valeur with
  | "Roi" | "Dame" | "Valet" -> true
  | _ -> false *)

(* let est_une_tete carte = 
match carte with 
| Carte {valeur = {type_valeur = "Roi"},{type_valeur = "Dame"},{type_valeur = "Valet"}, _}
 *)
(* let est_une_tete carte = 
match carte with
| Carte {valeur = {type_valeur = "Roi"}, *)



  let score_normal carte = 
  match carte with
  | Carte c -> (match c.valeur.type_valeur with
    | "As" -> 11
    | "Dix" -> 10
    | "Valet" -> 2
    | "Dame" -> 3
    | "Roi" -> 4
    | _ -> 0)
  | Joker -> 0

  
  let score_atout carte = 
  match carte with
  | Carte c -> (match c.valeur.type_valeur with
    | "As" -> 11
    | "Neuf" -> 14
    | "Dix" -> 10
    | "Valet" -> 20
    | "Dame" -> 3
    | "Roi" -> 4
    | _ -> 0)
  | Joker -> 0
 *)




