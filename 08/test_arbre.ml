(* let () = print_string "Hello world" *)


(* Arbre.print_tree string_of_int *)
(*let _ =
 let arbre = Noeud(1, Noeud(2, Feuille, Feuille), Noeud(3, Feuille, Feuille)) in
Arbre.print_tree arbre
 *)

 (* Arbre.print_tree string_of_int (Noeud(1, Noeud(2, Feuille, Feuille), Noeud);; *)

let arbre_exemple =  Arbre.Noeud (1, Noeud (2, Feuille, Feuille),Noeud (3, Feuille, Feuille))

let () =
  Arbre.print_tree arbre_exemple;  
  print_newline ();


  let arbre_exemple =
    Arbre.Noeud (1,
           Noeud (2, Vide, Vide),
           Noeud (3, Vide, Vide))

  let () =
    Arbre.imprimer_arbre arbre_exemple;;

  print_newline();;

  let exemple_arbre_entiers =
    Arbre.Noeud_entier (1,
      Noeud_entier (2, Vide, Vide),
      Noeud_entier (3, Vide, Vide))

  let () =
    Arbre.imprimer_arbre_entiers exemple_arbre_entiers;
    print_newline();;


  (* Ajout d'un nouvel entier tout à gauche )
  let nouvel_arbre = Arbre.ajouter_tout_a_gauche exemple_arbre_entiers 4

  ( Impression de l'arbre résultant *)
  let () =
    Arbre.imprimer_arbre_entiers nouvel_arbre;

  print_newline();;