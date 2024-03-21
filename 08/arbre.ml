(* Q1 *)
(* type 'a arbre_binaire =
| Feuille
| Noeud of 'a * 'a arbre_binaire * 'a arbre_binaire


let rec print_tree  = function
  | Feuille -> print_string "Feuille"
  | Noeud(v, g, d) -> 
    print_string ("Noeud(" ^ string_of_int v ^ ", ");
    print_tree g;
    print_string ", ";
    print_tree d;
    print_string ")"
     *)
(* Q2 *)
type 'a arbre_binaire =
  | Vide
  | Noeud of 'a * 'a arbre_binaire * 'a arbre_binaire

let rec imprimer_arbre arbre =
  match arbre with
  | Vide -> ()
  | Noeud (valeur, gauche, droite) ->
    imprimer_arbre gauche;
    print_int valeur;
    print_string " ";
    imprimer_arbre droite

type arbre_entiers =
  | Vide
  | Noeud_entier of int * arbre_entiers * arbre_entiers

let rec ajouter_tout_a_gauche arbre nouvel_entier =
  match arbre with
  | Vide -> Noeud_entier (nouvel_entier, Vide, Vide)
  | Noeud_entier (valeur, gauche, droite) ->
    let nouveau_gauche = ajouter_tout_a_gauche gauche nouvel_entier in
    Noeud_entier (valeur, nouveau_gauche, droite)
type 'a arbre_binaire =
  | Vide
  | Noeud of 'a * 'a arbre_binaire * 'a arbre_binaire

let rec imprimer_arbre arbre =
  match arbre with
  | Vide -> ()
  | Noeud (valeur, gauche, droite) ->
    imprimer_arbre gauche;
    print_int valeur;
    print_string " ";
    imprimer_arbre droite

type arbre_entiers =
| Vide
| Noeud_entier of int * arbre_entiers * arbre_entiers

let rec imprimer_arbre_entiers arbre =
match arbre with
| Vide -> ()
| Noeud_entier (valeur, gauche, droite) ->
  imprimer_arbre_entiers gauche;
  print_int valeur;
  print_string " ";
  imprimer_arbre_entiers droite

let rec ajouter_tout_a_gauche arbre nouvel_entier =
  match arbre with
  | Vide -> Noeud_entier (nouvel_entier, Vide, Vide)
  | Noeud_entier (valeur, gauche, droite) ->
    let nouveau_gauche = ajouter_tout_a_gauche gauche nouvel_entier in
    Noeud_entier (valeur, nouveau_gauche, droite)