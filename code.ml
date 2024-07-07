type graph = int list array
type par = P | I   (*pair, impair*)
type parite = par option  
let etat = Vierge | Ouvert | Ferme

let exemple = [|[1]; [0;2;3]; [1;4]; [1;4]; [2;3;5]; [4;6]; [5]|]


(* Les fonctions suivantes vont me permettre de calculer la parité du nombre d'arêtes, de sommets et de sommets pendants dans le graphe.
Ces valeurs serviront ensuite pour déterminer une stratégie gagnante au jeu.
Les sommets pendants sont intéressants car ils forment un arbre pendant, dont les feuilles sont libérable en un seul coup,
et donc rapportent un point directement ! *)



let parite_arete g =    (*renvoie la parité du nombre d'arêtes*)
  let a = ref 0 in 
  for i = 0 to Array.length g - 1 do
    a := !a + List.length g.(i)
  done;
  if !a mod 4 = 0 then P else I   (*4 car les aretes sont comptees en double*) 

  
  
let parite_sommet g =    (*renvoie la parité du nombre de sommets*)
  if (Array.length g) mod 2 = 0 then P else I 



type pendant = (*savoir si l'arete est pendante pour prog dynamique*)
  |Ferme     (*pas encore parcouru*)
  |Ouvert    (*En cours d'étude*)
  |Pendant
  |Non_pendant

  

let parite_pendant g = 
  let n = Array.length g in
  let visite = Array.make n Ferme in  
  let a = ref 0 in

  let rec aux l i parent compt =    (* Le compteur compte le nb de voisins non pendants *)
    visite.(i) <- Ouvert;
    if List.length g.(i) = 1 then begin visite.(i) <- Pendant; a := !a + 1 end
    else
      match l with   
      |[] -> if compt > 1 then visite.(i) <- Non_pendant else visite.(i) <- Pendant
      |h :: t when h = parent -> aux t i parent compt
      |h :: t -> match visite.(h) with  
                    |Pendant -> aux t i parent compt 
                    |Non_pendant -> if compt > 0 then visite.(i) <- Non_pendant 
                                    else aux t i parent (compt+1)
                    |Ouvert ->    (* il y a un cycle *)
                          visite.(h) <- Non_pendant; 
                          visite.(i) <- Non_pendant
                    |Ferme -> aux g.(h) h i 0; (* on réexécute sur le sommet i *)
                              aux l i parent compt 
  in

  for i = 0 to n - 1 do
    if visite.(i) = Ferme then
      aux g.(i) i i 0
  done;
  visite
