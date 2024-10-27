(*OGREVANJE*)

(*Števke*)

let rec stevke b n =
  let y = n / b in 
  match y with
  |0 -> n mod b :: []
  |_ -> stevke b y @ n mod b :: []

(*Začetek seznama*)

let take n sez =
  let rec aux n sez acc =
  match sez with
  |[] -> []
  |prvi :: rep -> if n = 0 then acc else aux (n - 1) rep (acc @ [prvi]) in
  aux n sez []

(*Odstranjevanje ujemajočih*)

let rec drop_while f sez =
  match sez with
  |[] -> []
  |prvi :: rep -> if f prvi = false then sez else drop_while f rep

(*Funkcija filter_mapi*)

let filter_mapi f sez =
  let rec pomozna i f sez =
    match sez with
    |[] -> []
    |prvi :: rep -> match f i prvi with
      |None -> pomozna (i + 1) f rep
      |Some x -> x :: pomozna (i + 1) f rep in
  pomozna 0 f sez


(*IZOMORFIZMI MNOŽIC*)

type ('a, 'b) sum = In1 of 'a | In2 of 'b

let phi1 = fun (a, b) -> (b, a)
let psi1 = fun (c, d) -> (d, c) (*let psi1=phi1*)

let phi2 =
  function
  |In1 sum -> In2 sum
  |In2 sum -> In1 sum
let psi2 = phi2

let phi3 = fun ((a, b), c) -> (a, (b, c))
let psi3 = fun (d, (e, f)) -> ((d, e), f)

let phi4 =
  function
  |In1 sum -> In1 (In1 sum)
  |In2 (In1 sum) -> In1 (In2 sum)
  |In2 (In2 sum) -> In2 sum
let psi4 =
  function
  |In1 (In1 sum) -> In1 sum
  |In1 (In2 sum) -> In2 (In1 sum)
  |In2 sum -> In2 (In1 sum)

let phi5 =
  function
  |(a, In1 sum) -> In1 (a, sum)
  |(a, In2 sum) -> In2 (a, sum)
let psi5 =
  function
  |In1 (a, sum) -> (a, In1 sum)
  |In2 (a, sum) -> (a, In2 sum)

let phi6 f = ((fun sum -> f (In1 sum)), (fun sum -> f (In2 sum)))
let psi6 (f, g) =  
  function
  |In1 sum -> f sum
  |In2 sum -> g sum

let phi7 f = ((fun c -> fst (f c)), (fun c -> snd (f c)))
let psi7 (f, g) = fun c -> (f c, g c)


(*POLINOMI*)

type polinom = int list

(*Odstranjevanje odvečnih ničel*)

let pocisti : polinom -> polinom = fun pol ->
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

 let ( +++ ) : polinom -> polinom -> polinom = fun pol1 pol2 ->
  let n = List.length pol1 - List.length pol2 in
  if n > 0 
    then pocisti(List.map2 (fun x y -> x + y) (dodaj_nic_d n pol2) pol1) 
    else pocisti(List.map2 (fun x y -> x + y) (dodaj_nic_d (Int.abs n) pol1) pol2) 

(*Množenje*)

let rec dodaj_nic_l n sez =
  match n with
  |0 -> sez
  |_ -> [0] @ dodaj_nic_l (n - 1) sez 

let ( *** ) : polinom -> polinom -> polinom = fun pol1 pol2 ->
  let posamezni m n sez =
    dodaj_nic_l m (List.map (fun x -> n * x) sez) in
  let rec mrow i pol1 pol2 =
    match pol1 with
    |[] -> []
    |prvi :: rep -> posamezni i prvi pol2 +++ mrow (i + 1) rep pol2 in
  mrow 0 pol1 pol2

(*Izračun vrednosti v točki*)

let vrednost : polinom -> int -> int = fun pol x ->
  let posamezni m n x =
    n * int_of_float((float_of_int x) ** (float_of_int m)) in
  let rec mrow i pol x =
    match pol with
    |[] -> 0
    |prvi :: rep -> posamezni i prvi x + mrow (i + 1) rep x in
  mrow 0 pol x

(*Odvajanje*)

let odvod : polinom -> polinom = fun pol ->
  let rec aux i sez acc =
    match sez with
    |[] -> acc
    |prvi :: rep -> aux (i + 1) rep (acc @ [i * prvi] ) in
  let lop = List.tl pol in
  aux 1 lop [] 

(*Lep izpis*)

(* let change_into_super n kljuc = 
  let rec aux i n acc kljuc =
    match i with
    |(-1) -> acc
    |_ -> aux (i - 1) n (String.make 1 kljuc.[int_of_char (string_of_int n).[i]] ^ acc) kljuc in  
  aux (String.length (string_of_int n) - 1) n "" kljuc *) (* <- String.length "⁰¹²³⁴⁵⁶⁷⁸⁹" = 27 :( *)

let izpis : polinom -> string = fun pol -> 
  let rec aux i pol acc =
    match pol with
    |[] -> acc
    |prvi :: rep ->
      match prvi with
      |0 -> aux (i + 1) rep acc
      |_ -> if prvi = 1 then 
          (if rep = [] then 
            (if i = 0 then "1" else
             if i = 1 then ("- " ^ "x" ^ acc) else
              "- " ^ "x^" ^ (string_of_int i) ^ acc) else
            if i = 0 then aux (i + 1) rep (" + 1" ^ acc) else 
          if i = 1 then aux (i + 1) rep (" + " ^ "x" ^ acc) else
          aux (i + 1) rep (" + " ^ "x^" ^ (string_of_int i) ^ acc)) 
        else if prvi < 0 then  
          (if rep = [] then 
            (if i = 0 then "- " ^ string_of_int prvi else
             if i = 1 then ("- " ^ (string_of_int prvi) ^ " x" ^ acc) else
              "- " ^ (string_of_int prvi) ^ " x^" ^ (string_of_int i) ^ acc) else
            if i = 0 then aux (i + 1) rep (" - " ^ (string_of_int prvi) ^ acc) else 
          if i = 1 then aux (i + 1) rep (" - " ^ (string_of_int prvi) ^ " x" ^ acc) else
          aux (i + 1) rep (" - " ^ (string_of_int prvi) ^ " x^" ^ (string_of_int i) ^ acc))
        else
          (if rep = [] then 
            (if i = 0 then string_of_int prvi else
             if i = 1 then ((string_of_int prvi) ^ " x" ^ acc) else
              (string_of_int prvi) ^ " x^" ^ (string_of_int i) ^ acc) else
            if i = 0 then aux (i + 1) rep (" + " ^ (string_of_int prvi) ^ acc) else 
          if i = 1 then aux (i + 1) rep (" + " ^ (string_of_int prvi) ^ " x" ^ acc) else
          aux (i + 1) rep (" + " ^ (string_of_int prvi) ^ " x^" ^ (string_of_int i) ^ acc)) in
  aux 0 pol ""


(*SAMODEJNO ODVAJANJE*)

type odvedljiva = (float -> float) * (float -> float)

(*Vrednost odvoda*)
  
let vrednost : odvedljiva -> float -> float = fun (f, f') a -> f a
let vrednost_odvoda : odvedljiva -> float -> float = fun (f, f') a -> f' a 

(*Osnovne funkcije*)

let konstanta : float -> odvedljiva = fun x -> ((fun x -> x), (fun x -> 0.))
let identiteta : odvedljiva = ((fun x -> x),(fun x -> 1.))

(*Produkt in kvocient*)

let ( **. ) : odvedljiva -> odvedljiva -> odvedljiva = 
  fun (f, f') (g, g') -> ((fun x -> f x *. g x), (fun x -> f' x *. g x +. f x *. g' x))

let ( //. ) : odvedljiva -> odvedljiva -> odvedljiva =
  fun (f, f') (g, g') -> ((fun x -> f x /. g x), (fun x -> (f' x *. g x -. f x *. g' x) /. ((g x) ** 2.)))

(*Kompozitum*)

let ( @@. ) : odvedljiva -> odvedljiva -> odvedljiva =
  fun (f, f') (g, g') -> ((fun x -> f (g x)), (fun x -> f' (g x) *. g' x ))

(*SUBSTITUCIJSKA ŠIFRA*)

let quick_brown_fox = "THEQUICKBRWNFXJMPSOVLAZYDG"
let rot13 = "NOPQRSTUVWXYZABCDEFGHIJKLM"

let indeks c = Char.code c - Char.code 'A'
let crka i = Char.chr (i + Char.code 'A') 

(*Šifriranje*)

let sifriraj kljuc niz =
  String.map (fun char -> if (-1) < indeks char && indeks char < 26 then kljuc.[indeks char] else char) niz
          
(*Inverzni ključ*)
let inverz kljuc =
  String.mapi (fun i char -> crka (String.index kljuc(crka i)))

(*Ugibanje ključa*)

let besede = "the of to and a in is it you that he was for on are with as i his they be at one have this from or had by word but what some we can out other were all there when up use your how said an each she which do their time if will way about many then them write would like so these her long make thing see him two has look more day could go come did number sound no most people my over know water than call first who may down side been now find any new work part take get place made live where after back little only round man year came show every good me give our under name very through just form sentence great think say help low line differ turn cause much mean before move right boy old too same tell does set three want air well also play small end put home read hand port large spell add even land here must big high such follow act why ask men change went light kind off need house picture try us again animal point mother world near build self earth father head stand own page should country found answer school grow study still learn plant cover food sun four between state keep eye never last let thought city tree cross farm hard start might story saw far sea draw left late run don't while press close night real life few north open seem together next white children begin got walk example ease paper group always music those both mark often letter until mile river car feet care second book carry took science eat room friend began idea fish mountain stop once base hear horse cut sure watch color face wood main enough plain girl usual young ready above ever red list though feel talk bird soon body dog family direct pose leave song measure door product black short numeral class wind question happen complete ship area half rock order fire south problem piece told knew pass since top whole king space heard best hour better true . during hundred five remember step early hold west ground interest reach fast verb sing listen six table travel less morning ten simple several vowel toward war lay against pattern slow center love person money serve appear road map rain rule govern pull cold notice voice unit power town fine certain fly fall lead cry dark machine note wait plan figure star box noun field rest correct able pound done beauty drive stood contain front teach week final gave green oh quick develop ocean warm free minute strong special mind behind clear tail produce fact street inch multiply nothing course stay wheel full force blue object decide surface deep moon island foot system busy test record boat common gold possible plane stead dry wonder laugh thousand ago ran check game shape equate hot miss brought heat snow tire bring yes distant fill east paint language among grand ball yet wave drop heart am present heavy dance engine position arm wide sail material size vary settle speak weight general ice matter circle pair include divide syllable felt perhaps pick sudden count square reason length represent art subject region energy hunt probable bed brother egg ride cell believe fraction forest sit race window store summer train sleep prove lone leg exercise wall catch mount wish sky board joy winter sat written wild instrument kept glass grass cow job edge sign visit past soft fun bright gas weather month million bear finish happy hope flower clothe strange gone jump baby eight village meet root buy raise solve metal whether push seven paragraph third shall held hair describe cook floor either result burn hill safe cat century consider type law bit coast copy phrase silent tall sand soil roll temperature finger industry value fight lie beat excite natural view sense ear else quite broke case middle kill son lake moment scale loud spring observe child straight consonant nation dictionary milk speed method organ pay age section dress cloud surprise quiet stone tiny climb cool design poor lot experiment bottom key iron single stick flat twenty skin smile crease hole trade melody trip office receive row mouth exact symbol die least trouble shout except wrote seed tone join suggest clean break lady yard rise bad blow oil blood touch grew cent mix team wire cost lost brown wear garden equal sent choose fell fit flow fair bank collect save control decimal gentle woman captain practice separate difficult doctor please protect noon whose locate ring character insect caught period indicate radio spoke atom human history effect electric expect crop modern element hit student corner party supply bone rail imagine provide agree thus capital won't chair danger fruit rich thick soldier process operate guess necessary sharp wing create neighbor wash bat rather crowd corn compare poem string bell depend meat rub tube famous dollar stream fear sight thin triangle planet hurry chief colony clock mine tie enter major fresh search send yellow gun allow print dead spot desert suit current lift rose continue block chart hat sell success company subtract event particular deal swim term opposite wife shoe shoulder spread arrange camp invent cotton born determine quart nine truck noise level chance gather shop stretch throw shine property column molecule select wrong gray repeat require broad prepare salt nose plural anger claim continent oxygen sugar death pretty skill women season solution magnet silver thank branch match suffix especially fig afraid huge sister steel discuss forward similar guide experience score apple bought led pitch coat mass card band rope slip win dream evening condition feed tool total basic smell valley nor double seat arrive master track parent shore division sheet substance favor connect post spend chord fat glad original share station dad bread charge proper bar offer segment slave duck instant market degree populate chick dear enemy reply drink occur support speech nature range steam motion path liquid log meant quotient teeth shell neck"

let slovar = List.map (String.uppercase_ascii) (String.split_on_char ' ' besede)

(*Razširjanje ključa s črko*)

let dodaj_zamenjavo kljuc (a, b) = 
  let je_v char niz =
    match niz with
    |"" -> false
    |_ -> try let _ = String.index niz char in true with Not_found -> false in
  let zamenjaj_ati niz (a, b) =
    String.mapi (fun i x -> if i = indeks a then b else x) niz in
  if kljuc.[indeks a] = b then Some kljuc else
  if je_v b kljuc then None else 
  if kljuc.[indeks a] <> '_' then None else
  Some (zamenjaj_ati kljuc (a, b))
  
(*Razširjanje ključa z besedo*)

let dodaj_zamenjave kljuc (bes1, bes2) =
  if String.length bes1 <> String.length bes2 then None else
  let rec pomozna i kljuc (bes1, bes2) =
    match i with 
    |(-1) -> Some kljuc
    |_ -> match dodaj_zamenjavo kljuc (bes1.[i], bes2.[i]) with 
          |Some x -> pomozna (i - 1) x (bes1, bes2)
          |None -> None in
  pomozna (String.length bes1 - 1) kljuc (bes1, bes2)

(*Vse možne razširitve*)

let mozne_razsiritve kljuc bes slo =
  let rec aux kljuc bes slo acc =
    match slo with
    |[] -> acc
    |prvi :: rep -> match dodaj_zamenjave kljuc (bes, prvi) with
                    |Some x -> aux kljuc bes rep (x :: acc)  
                    |None -> aux kljuc bes rep acc in
  List.rev (aux kljuc bes slo [])

(*Odšifriranje*)

(* let odsifriraj niz =
  let kljuc = String.make 26 '_' in
  let list2 = List.map (fun bes -> mozne_razsiritve kljuc bes slovar) (List.map (String.trim) (String.split_on_char ' ' niz)) in
  let rec preveri kljuc list_kljucev acc =
    match list_kljucev with
  match list2 with
  |[] -> None
  |prvi_list :: ostali ->
    let rec najdi_kljuc sez acc =
      match sez with
      |[] -> acc
      |prvi_kljuc :: rep -> match dodaj_zamenjave *)

        
    
    
  