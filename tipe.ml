type graph = int list array
type par = P | I   (*pair, impair*)
type parite = par option  
let etat = Vierge | Ouvert | Ferme

let exemple = [|[1]; [0;2;3]; [1;4]; [1;4]; [2;3;5]; [4;6]; [5]|]


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


let parite_pendant_faux g = 
  let n = Array.length g in
  let visite = Array.make n Ferme in  (*si non visité alors None, si visité et fermé alors Some true, sinon Some false*)
  let a = ref 0 in
  let rec aux l i parent = match l with    (*determine si le sommet associé à la liste en entrée fait partie d'un arbre pendant*)
    |[] -> visite.(i) <- Non_pendant      
    |h :: t when h = parent -> aux t i parent
    |h :: t -> if visite.(h) = Pendant then aux t i parent  
              else if visite.(h) = Non_pendant then visite.(i) <- ????
              else if visite.(h) = Ouvert then begin visite.(h) <- Non_pendant; visite.(i) <- Non_pendant end  (*il y a un cycle*)
              else aux g.(h) h i; aux l i parent
  in
  for i = 0 to n - 1 do
    if visite.(i) = Ferme then 
      if List.length g.(i) = 1 then begin visite.(i) <- Pendant; a := !a + 1 end
      else aux g.(i) i i
  done;
  if !a = 0 then None
  else if !a mod 2 = 0 then Some P 
  else Some I 



let parite_pendant g = 
  let n = Array.length g in
  let visite = Array.make n Vierge in   (* type état *)
  let pendant = Array.make n None in    (* type bool option *)
  let rec aux x pere =       (*determine si le sommet x en entrée fait partie d'un arbre pendant*)
    visite.(x) <- Ouvert;
    let m = Array.length g.(x) in  
    if m = 1 then pendant.(x) <- true Some
    else List.iter 
