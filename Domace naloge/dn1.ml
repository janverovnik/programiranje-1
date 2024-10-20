(*OGREVANJE*)

(*Števke*)

let rec stevke b n =
  let y = n / b in 
  match y with
  |0 -> n mod b :: []
  |_ -> stevke b y @ n mod b :: []

(*Začetek seznama*)

let rec take n sez =
  match sez with
  |[] -> []
  |prvi :: rep -> if n = 0 then [] else prvi :: take (n - 1) rep 

(*Odstranjevanje ujemajočih*)

let rec drop_while f sez =
  match sez with
  |[] -> []
  |prvi :: rep -> if f prvi = false then sez else drop_while f rep

(*Funkcija filter_mapi*)

let filter_mapi f sez =
  let rec pomozna i f sez =
    (match sez with
    |[] -> []
    |prvi :: rep -> match f i prvi with
      |None -> pomozna (i + 1) f rep
      |Some x -> x :: pomozna (i + 1) f rep) in
  pomozna 0 f sez


(*CURRY-HOWARDOV IZOMORFIZEM*)

type ('a, 'b) sum = In1 of 'a | In2 of 'b

let phi1 (a, b) = (b, a)
let psi1 (c, d) = (d, c)

let phi3 ((a, b), c) = (a, (b, c))
let psi3 (d, (e, f)) = ((d, e), f)

let phi7 f = (fun c -> fst (f c), fun c -> snd (f c))
let psi7 (f, g) = fun c -> (f c, g c)








(*POLINOMI*)

type polinom = int list

(*Odstranjevanje odvečnih ničel*)

let pocisti pol =
  let rec puci sez =
    (match sez with
    |[] -> []
    |prvi :: rep -> if prvi = 0 then puci(rep) else sez) in
  List.rev (puci (List.rev pol))
  
(*Seštevanje*)

let rec dodaj_nic_d n sez =
  match n with
  |0 -> sez
  |_ -> dodaj_nic_d (n - 1) sez @ [0]

 let ( +++ ) pol1 pol2 =
  let n = List.length pol1 - List.length pol2 in
  if n > 0 
    then pocisti(List.map2 (fun x y -> x + y) (dodaj_nic_d n pol2) pol1) 
    else pocisti(List.map2 (fun x y -> x + y) (dodaj_nic_d (Int.abs n) pol1) pol2) 

(*Množenje*)

let rec dodaj_nic_l n sez =
  match n with
  |0 -> sez
  |_ -> [0] @ dodaj_nic_l (n - 1) sez 

let ( *** ) pol1 pol2 =
  let posamezni m n sez =
    dodaj_nic_l m (List.map (fun x -> n * x) sez) in
  let rec mrow i pol1 pol2 =
    match pol1 with
    |[] -> []
    |prvi :: rep -> posamezni i prvi pol2 +++ mrow (i + 1) rep pol2 in
  mrow 0 pol1 pol2

 (*Izračun vrednosti v točki*)

 let vrednost pol x =
  let posamezni m n x =
    n * int_of_float((float_of_int x) ** (float_of_int m)) in
  let rec mrow i pol x =
    match pol with
    |[] -> 0
    |prvi :: rep -> posamezni i prvi x + mrow (i + 1) rep x in
  mrow 0 pol x
    