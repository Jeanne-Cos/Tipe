type graph = int list array
type par = P | I   (*pair, impair*)
type parite = par option  
let etat = Vierge | Ouvert | Ferme

let exemple = [|[1]; [0;2;3]; [1;4]; [1;4]; [2;3;5]; [4;6]; [5]|]


let parite_arete g =
  let a = ref 0 in 
  for i = 0 to Array.length g - 1 do
    a := !a + List.length g.(i)
  done;
  if !a mod 4 = 0 then P else I   (*4 car les aretes sont comptees en double*) 
  
let parite_sommet g = 
  if (Array.length g) mod 2 = 0 then P else I 



type pendante = bool option    (*savoir si l'arete est pendante pour prog dynamique*)

let parite_pendant_faux g = 
  let n = Array.length g in
  let visite = Array.make n None in
  let a = ref 0 in
  let rec aux l i p = match l with    (*determine si le sommet associé à la liste en entrée fait partie d'un arbre pendant*)
    |[] -> visite.(i) <- Some false       (*p pour le précédent*)
    |[x] -> visite.(i) <- Some true; a := !a + 1; if visite.(x) = None then aux g.(x) x i 
    |h :: t when h = p -> aux t i p
    |h :: t -> if visite.(h) = Some true then aux t i p
              else if visite.(h) = Some false then visite.(i) <- Some false 
              else aux g.(h) h i; aux l i p
  in
  for i = 0 to n - 1 do
    if visite.(i) = None then aux g.(i) i i 
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
