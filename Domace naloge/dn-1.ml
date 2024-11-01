(*----------------------------------------------------------------------------*
 # 1. domača naloga
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Ogrevanje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### Števke
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `stevke : int -> int -> int list`, ki sprejme pozitivni celi
 števili $b$ in $n$ ter vrne seznam števk števila $n$ v bazi $b$. Pri tem tudi
 za baze, ki so večje od $10$, uporabimo števke od $0$ do $b - 1$.
[*----------------------------------------------------------------------------*)

let rec stevke b n =
  let y = n / b in 
  match y with
  |0 -> n mod b :: []
  |_ -> stevke b y @ n mod b :: []

let primer_1_1 = stevke 10 12345
(* val primer_1_1 : int list = [1; 2; 3; 4; 5] *)

let primer_1_2 = stevke 2 42
(* val primer_1_2 : int list = [1; 0; 1; 0; 1; 0] *)

let primer_1_3 = stevke 16 (3 * 16 * 16 * 16 + 14 * 16 * 16 + 15 * 16 + 9)
(* val primer_1_3 : int list = [3; 14; 15; 9] *)

(*----------------------------------------------------------------------------*
 ### Začetek seznama
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `take : int -> 'a list -> 'a list`, ki sprejme naravno
 število in vrne ustrezno število elementov z začetka danega seznama. Če je
 podani seznam krajši od zahtevane dolžine, naj funkcija vrne kar celoten
 seznam.
[*----------------------------------------------------------------------------*)

let take n sez =
  let rec aux n sez acc =
  match sez with
  |[] -> []
  |prvi :: rep -> if n = 0 then acc else aux (n - 1) rep (acc @ [prvi]) in
  aux n sez []

let primer_1_4 = take 3 [1; 2; 3; 4; 5]
(* val primer_1_4 : int list = [1; 2; 3] *)

let primer_1_5 = take 10 [1; 2; 3; 4; 5]
(* val primer_1_5 : int list = [1; 2; 3; 4; 5] *)

(*----------------------------------------------------------------------------*
 ### Odstranjevanje ujemajočih
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `drop_while : ('a -> bool) -> 'a list -> 'a list`, ki z
 začetka seznama odstrani vse elemente, ki zadoščajo danemu predikatu. Ko najde
 element, ki predikatu ne zadošča, vrne preostanek seznama.
[*----------------------------------------------------------------------------*)

let rec drop_while f sez =
  match sez with
  |[] -> []
  |prvi :: rep -> if f prvi = false then sez else drop_while f rep

let primer_1_6 = drop_while (fun x -> x < 5) [3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5]
(* val primer_1_6 : int list = [5; 9; 2; 6; 5; 3; 5] *)

let primer_1_7 = drop_while (fun x -> x < 5) [9; 8; 7; 6; 5; 4; 3; 2; 1; 0]
(* val primer_1_7 : int list = [9; 8; 7; 6; 5; 4; 3; 2; 1; 0] *)

(*----------------------------------------------------------------------------*
 ### Funkcija `filter_mapi`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `filter_mapi : (int -> 'a -> 'b option) -> 'a list -> 'b
 list`, ki deluje tako kot `List.filter_map`, le da funkcija poleg elemenov dobi
 še njihove indekse.
[*----------------------------------------------------------------------------*)

let filter_mapi f sez =
  let rec pomozna i f sez =
    match sez with
    |[] -> []
    |prvi :: rep -> match f i prvi with
      |None -> pomozna (i + 1) f rep
      |Some x -> x :: pomozna (i + 1) f rep in
  pomozna 0 f sez

let primer_1_8 =
  filter_mapi
    (fun i x -> if i mod 2 = 0 then Some (x * x) else None)
    [1; 2; 3; 4; 5; 6; 7; 8; 9]
(* val primer_1_8 : int list = [1; 9; 25; 49; 81] *)

(*----------------------------------------------------------------------------*
 ## Izomorfizmi množic
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Na predavanjih smo videli, da funkciji `curry : ('a * 'b -> 'c) -> ('a -> ('b
 -> 'c))` in `uncurry : ('a -> ('b -> 'c)) -> ('a * 'b -> 'c)` predstavljata
 izomorfizem množic $C^{A \times B} \cong (C^B)^A$, če kartezični produkt
 predstavimo s produktnim, eksponent pa s funkcijskim tipom.

 Podobno velja tudi za ostale znane izomorfizme, če disjunktno unijo
   $$A + B = \{ \mathrm{in}_1(a) \mid a \in A \} \cup \{ \mathrm{in}_2(b) \mid b
 \in B \}$$
 predstavimo s tipom `('a, 'b) sum`, definiranim z:
[*----------------------------------------------------------------------------*)

type ('a, 'b) sum = In1 of 'a | In2 of 'b

(*----------------------------------------------------------------------------*
 Napišite pare funkcij `phi1` & `psi1`, …, `phi7` & `psi7`, ki predstavljajo
 spodnje izomorfizme množic. Tega, da so si funkcije inverzne, ni treba
 dokazovati.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### $A \times B \cong B \times A$
[*----------------------------------------------------------------------------*)

let phi1 = fun (a, b) -> (b, a)
let psi1 = fun (c, d) -> (d, c) (*let psi1=phi1*)

(*----------------------------------------------------------------------------*
 ### $A + B \cong B + A$
[*----------------------------------------------------------------------------*)

let phi2 =
  function
  |In1 sum -> In2 sum
  |In2 sum -> In1 sum
let psi2 = phi2

(*----------------------------------------------------------------------------*
 ### $A \times (B \times C) \cong (A \times B) \times C$
[*----------------------------------------------------------------------------*)

let phi3 = fun ((a, b), c) -> (a, (b, c))
let psi3 = fun (d, (e, f)) -> ((d, e), f)

(*----------------------------------------------------------------------------*
 ### $A + (B + C) \cong (A + B) + C$
[*----------------------------------------------------------------------------*)

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

(*----------------------------------------------------------------------------*
 ### $A \times (B + C) \cong (A \times B) + (A \times C)$
[*----------------------------------------------------------------------------*)

let phi5 =
  function
  |(a, In1 sum) -> In1 (a, sum)
  |(a, In2 sum) -> In2 (a, sum)
let psi5 =
  function
  |In1 (a, sum) -> (a, In1 sum)
  |In2 (a, sum) -> (a, In2 sum)

(*----------------------------------------------------------------------------*
 ### $A^{B + C} \cong A^B \times A^C$
[*----------------------------------------------------------------------------*)

let phi6 f = ((fun sum -> f (In1 sum)), (fun sum -> f (In2 sum)))
let psi6 (f, g) =  
  function
  |In1 sum -> f sum
  |In2 sum -> g sum

(*----------------------------------------------------------------------------*
 ### $(A \times B)^C \cong A^C \times B^C$
[*----------------------------------------------------------------------------*)

let phi7 f = ((fun c -> fst (f c)), (fun c -> snd (f c)))
let psi7 (f, g) = fun c -> (f c, g c)

(*----------------------------------------------------------------------------*
 ## Polinomi
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Polinome $a_0 + a_1 x + \cdots + a_n x^n$ predstavimo s seznami celoštevilskih
 koeficientov od prostega do vodilnega člena. Na primer, polinom $1 - 2 x + 3
 x^2$ predstavimo s seznamom `[1; -2; 3]`.
[*----------------------------------------------------------------------------*)

type polinom = int list

(*----------------------------------------------------------------------------*
 ### Odstranjevanje odvečnih ničel
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `pocisti : polinom -> polinom`, ki s konca seznama
 koeficientov odstrani odvečne ničle.
[*----------------------------------------------------------------------------*)

let pocisti : polinom -> polinom = fun pol ->
  let rec puci sez =
    (match sez with
    |[] -> []
    |prvi :: rep -> if prvi = 0 then puci(rep) else sez) in
  List.rev (puci (List.rev pol))

let primer_3_1 = pocisti [1; -2; 3; 0; 0]
(* val primer_3_1 : int list = [1; -2; 3] *)

(*----------------------------------------------------------------------------*
 ### Seštevanje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `( +++ ) : polinom -> polinom -> polinom`, ki sešteje dva
 polinoma.
[*----------------------------------------------------------------------------*)

let rec dodaj_nic_d n sez =
  match n with
  |0 -> sez
  |_ -> dodaj_nic_d (n - 1) sez @ [0]

 let ( +++ ) : polinom -> polinom -> polinom = fun pol1 pol2 ->
  let n = List.length pol1 - List.length pol2 in
  if n > 0 
    then pocisti(List.map2 (fun x y -> x + y) (dodaj_nic_d n pol2) pol1) 
    else pocisti(List.map2 (fun x y -> x + y) (dodaj_nic_d (Int.abs n) pol1) pol2) 

let primer_3_2 = [1; -2; 3] +++ [1; 2]
(* val primer_3_2 : int list = [2; 0; 3] *)

let primer_3_3 = [1; -2; 3] +++ [1; 2; -3]
(* val primer_3_3 : int list = [2] *)

(*----------------------------------------------------------------------------*
 ### Množenje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `( *** ) : polinom -> polinom -> polinom`, ki zmnoži dva
 polinoma.
[*----------------------------------------------------------------------------*)

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

let primer_3_4 = [1; 1] *** [1; 1] *** [1; 1]
(* val primer_3_4 : int list = [1; 3; 3; 1] *)

let primer_3_5 = [1; 1] *** [1; -1]
(* val primer_3_5 : int list = [1; 0; -1] *)

(*----------------------------------------------------------------------------*
 ### Izračun vrednosti v točki
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `vrednost : polinom -> int -> int`, ki izračuna vrednost
 polinoma v danem argumentu.
[*----------------------------------------------------------------------------*)

let vrednost : polinom -> int -> int = fun pol x ->
  let posamezni m n x =
    n * int_of_float((float_of_int x) ** (float_of_int m)) in
  let rec mrow i pol x =
    match pol with
    |[] -> 0
    |prvi :: rep -> posamezni i prvi x + mrow (i + 1) rep x in
  mrow 0 pol x

let primer_3_6 = vrednost [1; -2; 3] 2
(* val primer_3_6 : int = 9 *)

(*----------------------------------------------------------------------------*
 ### Odvajanje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `odvod : polinom -> polinom`, ki izračuna odvod polinoma.
[*----------------------------------------------------------------------------*)

let odvod : polinom -> polinom = fun pol ->
  let rec aux i sez acc =
    match sez with
    |[] -> acc
    |prvi :: rep -> aux (i + 1) rep (acc @ [i * prvi] ) in
  let lop = List.tl pol in
  aux 1 lop [] 

let primer_3_7 = odvod [1; -2; 3]
(* val primer_3_7 : int list = [-2; 6] *)

(*----------------------------------------------------------------------------*
 ### Lep izpis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `izpis : polinom -> string`, ki polinom lepo izpiše. Na
 primer, `izpis [1; -2; 3]` vrne `"3 x^2 - 2 x + 1"` oziroma še bolje kot `"3 x²
 - 2 x + 1"`. Pozorni bodite, da izpis začnete z vodilnim členom.
[*----------------------------------------------------------------------------*)

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

let primer_3_8 = izpis [1; 2; 1]
(* val primer_3_8 : string = "x² + 2 x + 1" *)

let primer_3_9 = izpis [1; 0; -1; 0; 1; 0; -1; 0; 1; 0; -1; 0; 1]
(* val primer_3_9 : string = "x¹² - x¹⁰ + x⁸ - x⁶ + x⁴ - x² + 1" *)

let primer_3_10 = izpis [0; -3; 3; -1]
(* val primer_3_10 : string = "-x³ + 3 x² - 3 x" *)

(*----------------------------------------------------------------------------*
 ## Samodejno odvajanje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Ob razmahu strojnega učenja, ki optimalno rešitev išče s pomočjo gradientnega
 spusta, si želimo čim bolj enostavno računati odvode. Odvod funkcije $f$ v
 točki $x_0$ lahko seveda ocenimo tako, da v

 $$\frac{f (x_0 + h) - f(x_0)}{h}$$

 vstavimo dovolj majhno število $h$.
[*----------------------------------------------------------------------------*)

let priblizek_odvoda f x0 h =
  (f (x0 +. h) -. f x0) /. h
(* val priblizek_odvoda : (float -> float) -> float -> float -> float = <fun> *)

let primer_4_1 =
  let f x = sin x +. cos x +. exp x in
  List.map (priblizek_odvoda f 1.) [0.1; 0.01; 0.001; 0.0001; 0.00001]
(* val primer_4_1 : float list =
  [2.48914386298364931; 2.42384618742050861; 2.41778190719976749;
   2.41717997997881184; 2.41711983210990411] *)

(*----------------------------------------------------------------------------*
 Pri samodejnem odvajanju izkoristimo to, da poznamo odvode elementarnih
 funkcij, odvode sestavljenih funkcij pa lahko izračunamo iz posameznih odvodov.
 Tako bomo vsako funkcijo predstavili s parom: prvotno funkcijo in njenim
 odvodom.
[*----------------------------------------------------------------------------*)

type odvedljiva = (float -> float) * (float -> float)

let sinus : odvedljiva = (sin, cos)
let kosinus : odvedljiva = (cos, (fun x -> -. sin x))
let eksp : odvedljiva = (exp, exp)
let ( ++. ) : odvedljiva -> odvedljiva -> odvedljiva =
  (* pozorni bodite, da anonimni funkciji v paru date med oklepaje *)
  fun (f, f') (g, g') -> ((fun x -> f x +. g x), (fun x -> f' x +. g' x))
(* val sinus : odvedljiva = (<fun>, <fun>) *)
(* val kosinus : odvedljiva = (<fun>, <fun>) *)
(* val eksp : odvedljiva = (<fun>, <fun>) *)
(* val ( ++. ) : odvedljiva -> odvedljiva -> odvedljiva = <fun> *)

let primer_4_2 =
  let (_, f') = sinus ++. kosinus ++. eksp in
  f' 1.
(* val primer_4_2 : float = 2.41711314951928813 *)

(*----------------------------------------------------------------------------*
 ### Vrednost odvoda
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkciji `vrednost : odvedljiva -> float -> float` in `odvod :
 odvedljiva -> float -> float`, ki izračunata vrednost funkcije in njenega
 odvoda v danem argumentu.
[*----------------------------------------------------------------------------*)

let vrednost : odvedljiva -> float -> float = fun (f, f') a -> f a
let vrednost_odvoda : odvedljiva -> float -> float = fun (f, f') a -> f' a

(*----------------------------------------------------------------------------*
 ### Osnovne funkcije
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkciji `konstanta : float -> odvedljiva` in `identiteta :
 odvedljiva`, ki predstavljata konstantno in identično funkcijo.
[*----------------------------------------------------------------------------*)

let konstanta : float -> odvedljiva = fun x -> ((fun x -> x), (fun x -> 0.))
let identiteta : odvedljiva = ((fun x -> x), (fun x -> 1.))

(*----------------------------------------------------------------------------*
 ### Produkt in kvocient
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkciji `( **. ) : odvedljiva -> odvedljiva -> odvedljiva` in `( //.
 ) : odvedljiva -> odvedljiva -> odvedljiva`, ki predstavljata produkt in
 kvocient dveh odvedljivih funkcij.
[*----------------------------------------------------------------------------*)

let ( **. ) : odvedljiva -> odvedljiva -> odvedljiva = 
  fun (f, f') (g, g') -> ((fun x -> f x *. g x), (fun x -> f' x *. g x +. f x *. g' x))

let ( //. ) : odvedljiva -> odvedljiva -> odvedljiva =
  fun (f, f') (g, g') -> ((fun x -> f x /. g x), (fun x -> (f' x *. g x -. f x *. g' x) /. ((g x) ** 2.)))

let kvadrat = identiteta **. identiteta
(* val kvadrat : odvedljiva = (<fun>, <fun>) *)

(*----------------------------------------------------------------------------*
 ### Kompozitum
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `( @@. ) : odvedljiva -> odvedljiva -> odvedljiva`, ki
 predstavlja kompozitum dveh odvedljivih funkcij.
[*----------------------------------------------------------------------------*)

let ( @@. ) : odvedljiva -> odvedljiva -> odvedljiva =
  fun (f, f') (g, g') -> ((fun x -> f (g x)), (fun x -> f' (g x) *. g' x ))

(* POZOR: Primer je zaenkrat zakomentiran, saj ob prazni rešitvi nima tipa *)
(* let vedno_ena = (kvadrat @@. sinus) ++. (kvadrat @@. kosinus) *)
(* val vedno_ena : odvedljiva = (<fun>, <fun>) *)

(* POZOR: Primer je zaenkrat zakomentiran, saj brez vedno_ena ne deluje *)
(* let primer_4_3 = vrednost vedno_ena 12345. *)
(* val primer_4_3 : float = 0.999999999999999889 *)

(* POZOR: Primer je zaenkrat zakomentiran, saj brez vedno_ena ne deluje *)
(* let primer_4_4 = odvod vedno_ena 12345. *)
(* val primer_4_4 : float = 0. *)

(*----------------------------------------------------------------------------*
 ## Substitucijska šifra
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Substitucijska šifra je preprosta šifra, pri kateri črke abecede med seboj
 permutiramo. Na primer, če bi (angleško) abecedo permutirali kot

 ```
 A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
 T H E Q U I C K B R W N F X J M P S O V L A Z Y D G
 ```

 bi besedo `HELLO` šifrirali kot `KUNNJ`. Ključe, s katerimi šifriramo besedila
 bomo predstavili kar z nizi črk, v katere se slikajo črke abecede.
[*----------------------------------------------------------------------------*)

let quick_brown_fox = "THEQUICKBRWNFXJMPSOVLAZYDG"
let rot13 = "NOPQRSTUVWXYZABCDEFGHIJKLM"
(* val quick_brown_fox : string = "THEQUICKBRWNFXJMPSOVLAZYDG" *)
(* val rot13 : string = "NOPQRSTUVWXYZABCDEFGHIJKLM" *)

(*----------------------------------------------------------------------------*
 Včasih bomo v primerih uporabljali tudi krajše ključe, a vedno lahko
 predpostavite, da bodo ključi permutacije začetnega dela abecede. Prav tako si
 pri delu lahko pomagate s funkcijama `indeks` in `crka`:
[*----------------------------------------------------------------------------*)

let indeks c = Char.code c - Char.code 'A'
let crka i = Char.chr (i + Char.code 'A') 
(* val indeks : char -> int = <fun> *)
(* val crka : int -> char = <fun> *)

(*----------------------------------------------------------------------------*
 ### Šifriranje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `sifriraj : string -> string -> string`, ki besedilo šifrira
 z danim ključem. Vse znake, ki niso velike tiskane črke, pustimo pri miru.
[*----------------------------------------------------------------------------*)

let sifriraj kljuc niz =
  String.map (fun char -> if (-1) < indeks char && indeks char < 26 then kljuc.[indeks char] else char) niz

let primer_5_1 = sifriraj quick_brown_fox "HELLO, WORLD!"
(* val primer_5_1 : string = "KUNNJ, ZJSNQ!" *)

let primer_5_2 = "VENI, VIDI, VICI" |> sifriraj rot13
(* val primer_5_2 : string = "IRAV, IVQV, IVPV" *)

let primer_5_3 = "VENI, VIDI, VICI" |> sifriraj rot13 |> sifriraj rot13
(* val primer_5_3 : string = "VENI, VIDI, VICI" *)

(*----------------------------------------------------------------------------*
 ### Inverzni ključ
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `inverz : string -> string`, ki iz ključa izračuna njegov
 inverz.
[*----------------------------------------------------------------------------*)

let inverz kljuc =
  String.mapi (fun i char -> crka (String.index kljuc(crka i)))

let primer_5_4 = inverz quick_brown_fox
(* val primer_5_4 : string = "VIGYCMZBFOHUPLSQDJRAETKNXW" *)

let primer_5_5 = inverz rot13
(* val primer_5_5 : string = "NOPQRSTUVWXYZABCDEFGHIJKLM" *)

let primer_5_6 = inverz "BCDEA"
(* val primer_5_6 : string = "EABCD" *)

(*----------------------------------------------------------------------------*
 ### Ugibanje ključa
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Včasih seveda ne poznamo ključa, a vemo, da je besedilo v angleščini. Tako
 lahko ključ poskusimo uganiti tako, da šifrirane besede paroma primerjamo z
 besedami iz slovarja, ki smo si ga sposodili [s
 spleta](https://gist.github.com/deekayen/4148741).
[*----------------------------------------------------------------------------*)

let besede = "the of to and a in is it you that he was for on are with as i his they be at one have this from or had by word but what some we can out other were all there when up use your how said an each she which do their time if will way about many then them write would like so these her long make thing see him two has look more day could go come did number sound no most people my over know water than call first who may down side been now find any new work part take get place made live where after back little only round man year came show every good me give our under name very through just form sentence great think say help low line differ turn cause much mean before move right boy old too same tell does set three want air well also play small end put home read hand port large spell add even land here must big high such follow act why ask men change went light kind off need house picture try us again animal point mother world near build self earth father head stand own page should country found answer school grow study still learn plant cover food sun four between state keep eye never last let thought city tree cross farm hard start might story saw far sea draw left late run don't while press close night real life few north open seem together next white children begin got walk example ease paper group always music those both mark often letter until mile river car feet care second book carry took science eat room friend began idea fish mountain stop once base hear horse cut sure watch color face wood main enough plain girl usual young ready above ever red list though feel talk bird soon body dog family direct pose leave song measure door product black short numeral class wind question happen complete ship area half rock order fire south problem piece told knew pass since top whole king space heard best hour better true . during hundred five remember step early hold west ground interest reach fast verb sing listen six table travel less morning ten simple several vowel toward war lay against pattern slow center love person money serve appear road map rain rule govern pull cold notice voice unit power town fine certain fly fall lead cry dark machine note wait plan figure star box noun field rest correct able pound done beauty drive stood contain front teach week final gave green oh quick develop ocean warm free minute strong special mind behind clear tail produce fact street inch multiply nothing course stay wheel full force blue object decide surface deep moon island foot system busy test record boat common gold possible plane stead dry wonder laugh thousand ago ran check game shape equate hot miss brought heat snow tire bring yes distant fill east paint language among grand ball yet wave drop heart am present heavy dance engine position arm wide sail material size vary settle speak weight general ice matter circle pair include divide syllable felt perhaps pick sudden count square reason length represent art subject region energy hunt probable bed brother egg ride cell believe fraction forest sit race window store summer train sleep prove lone leg exercise wall catch mount wish sky board joy winter sat written wild instrument kept glass grass cow job edge sign visit past soft fun bright gas weather month million bear finish happy hope flower clothe strange gone jump baby eight village meet root buy raise solve metal whether push seven paragraph third shall held hair describe cook floor either result burn hill safe cat century consider type law bit coast copy phrase silent tall sand soil roll temperature finger industry value fight lie beat excite natural view sense ear else quite broke case middle kill son lake moment scale loud spring observe child straight consonant nation dictionary milk speed method organ pay age section dress cloud surprise quiet stone tiny climb cool design poor lot experiment bottom key iron single stick flat twenty skin smile crease hole trade melody trip office receive row mouth exact symbol die least trouble shout except wrote seed tone join suggest clean break lady yard rise bad blow oil blood touch grew cent mix team wire cost lost brown wear garden equal sent choose fell fit flow fair bank collect save control decimal gentle woman captain practice separate difficult doctor please protect noon whose locate ring character insect caught period indicate radio spoke atom human history effect electric expect crop modern element hit student corner party supply bone rail imagine provide agree thus capital won't chair danger fruit rich thick soldier process operate guess necessary sharp wing create neighbor wash bat rather crowd corn compare poem string bell depend meat rub tube famous dollar stream fear sight thin triangle planet hurry chief colony clock mine tie enter major fresh search send yellow gun allow print dead spot desert suit current lift rose continue block chart hat sell success company subtract event particular deal swim term opposite wife shoe shoulder spread arrange camp invent cotton born determine quart nine truck noise level chance gather shop stretch throw shine property column molecule select wrong gray repeat require broad prepare salt nose plural anger claim continent oxygen sugar death pretty skill women season solution magnet silver thank branch match suffix especially fig afraid huge sister steel discuss forward similar guide experience score apple bought led pitch coat mass card band rope slip win dream evening condition feed tool total basic smell valley nor double seat arrive master track parent shore division sheet substance favor connect post spend chord fat glad original share station dad bread charge proper bar offer segment slave duck instant market degree populate chick dear enemy reply drink occur support speech nature range steam motion path liquid log meant quotient teeth shell neck"
(* val besede : string =
  "the of to and a in is it you that he was for on are with as i his they be at one have this from or had by word but what some we can out other were all there when up use your how said an each she which do their time if will way about many then them write would like so these her long make thing see h"... (* string length 5837; truncated *) *)

(*----------------------------------------------------------------------------*
 Sestavite vrednost `slovar : string list`, ki vsebuje vse besede iz slovarja,
 pretvorjene v velike tiskane črke.
[*----------------------------------------------------------------------------*)

let slovar = List.map (String.uppercase_ascii) (String.split_on_char ' ' besede)

let primer_5_7 = take 42 slovar
(* val primer_5_7 : string list =
  ["THE"; "OF"; "TO"; "AND"; "A"; "IN"; "IS"; "IT"; "YOU"; "THAT"; "HE";
   "WAS"; "FOR"; "ON"; "ARE"; "WITH"; "AS"; "I"; "HIS"; "THEY"; "BE"; "AT";
   "ONE"; "HAVE"; "THIS"; "FROM"; "OR"; "HAD"; "BY"; "WORD"; "BUT"; "WHAT";
   "SOME"; "WE"; "CAN"; "OUT"; "OTHER"; "WERE"; "ALL"; "THERE"; "WHEN"; "UP"] *)

(* POZOR: Primer je zaenkrat zakomentiran, saj ob prazni rešitvi sproži izjemo *)
(* let primer_5_8 = List.nth slovar 321 *)
(* val primer_5_8 : string = "MEASURE" *)

(*----------------------------------------------------------------------------*
 ### Razširjanje ključa s črko
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Med ugibanjem seveda ne bomo poznali celotnega ključa. V tem primeru bomo za
 neznane črke uporabili znak `_`. Na primer, če bi vedeli, da je črka `A` v
 besedilu šifrirana kot `X`, črka `C` pa kot `Y`, bi ključ zapisali kot
 `"X_Y_______________________"`.

 Napišite funkcijo `dodaj_zamenjavo : string -> char * char -> string option`,
 ki sprejme ključ ter ga poskusi razširiti z zamenjavo dane črke. Funkcija naj
 vrne `None`, če razširitev vodi v ključ, ki ni bijektiven (torej če ima črka že
 dodeljeno drugo zamenjavo ali če smo isto zamenjavo dodelili dvema različnima
 črkama).
[*----------------------------------------------------------------------------*)

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

let primer_5_9 = dodaj_zamenjavo "AB__E" ('C', 'X')
(* val primer_5_9 : string option = Some "ABX_E" *)

let primer_5_10 = dodaj_zamenjavo "ABX_E" ('C', 'X')
(* val primer_5_10 : string option = Some "ABX_E" *)

let primer_5_11 = dodaj_zamenjavo "ABY_E" ('C', 'E')
(* val primer_5_11 : string option = None *)

(*----------------------------------------------------------------------------*
 ### Razširjanje ključa z besedo
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 S pomočjo funkcije `dodaj_zamenjavo` sestavite še funkcijo `dodaj_zamenjave :
 string -> string * string -> string option`, ki ključ razširi z zamenjavami, ki
 prvo besedo preslikajo v drugo.
[*----------------------------------------------------------------------------*)

let dodaj_zamenjave kljuc (bes1, bes2) =
  if String.length bes1 <> String.length bes2 then None else
  let rec pomozna i kljuc (bes1, bes2) =
    match i with 
    |(-1) -> Some kljuc
    |_ -> match dodaj_zamenjavo kljuc (bes1.[i], bes2.[i]) with 
          |Some x -> pomozna (i - 1) x (bes1, bes2)
          |None -> None in
  pomozna (String.length bes1 - 1) kljuc (bes1, bes2)

let primer_5_12 = dodaj_zamenjave "__________________________" ("HELLO", "KUNNJ")
(* val primer_5_12 : string option = Some "____U__K___N__J___________" *)

let primer_5_13 = dodaj_zamenjave "ABCDU_____________________" ("HELLO", "KUNNJ")
(* val primer_5_13 : string option = Some "ABCDU__K___N__J___________" *)

let primer_5_14 = dodaj_zamenjave "ABCDE_____________________" ("HELLO", "KUNNJ")
(* val primer_5_14 : string option = None *)

(*----------------------------------------------------------------------------*
 ### Vse možne razširitve
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite funkcijo `mozne_razsiritve : string -> string -> string list ->
 string list`, ki vzame ključ, šifrirano besedo ter slovar vseh možnih besed,
 vrne pa seznam vseh možnih razširitev ključa, ki šifrirano besedo slikajo v eno
 od besed v slovarju.
[*----------------------------------------------------------------------------*)

let mozne_razsiritve kljuc bes slo =
  let rec aux kljuc bes slo acc =
    match slo with
    |[] -> acc
    |prvi :: rep -> match dodaj_zamenjave kljuc (bes, prvi) with
                    |Some x -> aux kljuc bes rep (x :: acc)  
                    |None -> aux kljuc bes rep acc in
  List.rev (aux kljuc bes slo [])

let primer_5_15 =
  slovar
  |> mozne_razsiritve (String.make 26 '_') "KUNNJ"
  |> List.map (fun kljuc -> (kljuc, sifriraj kljuc "KUNNJ"))
(* val primer_5_15 : (string * string) list =
  [("_________YC__R______A_____", "CARRY");
   ("_________DS__O______T_____", "STOOD");
   ("_________NG__E______R_____", "GREEN");
   ("_________LW__E______H_____", "WHEEL");
   ("_________PS__E______L_____", "SLEEP");
   ("_________YH__P______A_____", "HAPPY");
   ("_________RF__O______L_____", "FLOOR");
   ("_________DS__E______P_____", "SPEED");
   ("_________DB__O______L_____", "BLOOD");
   ("_________YH__R______U_____", "HURRY");
   ("_________LS__E______T_____", "STEEL");
   ("_________TS__E______H_____", "SHEET")] *)

(*----------------------------------------------------------------------------*
 ### Odšifriranje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `odsifriraj : string -> string option`, ki sprejme šifrirano
 besedilo in s pomočjo slovarja besed ugane odšifrirano besedilo. Funkcija naj
 vrne `None`, če ni mogoče najti nobenega ustreznega ključa.
[*----------------------------------------------------------------------------*)

 let odsifriraj niz =
  let rec preveri kljuc bes sez =
    match sez with
    |[] -> None
    |prvi :: rep -> match dodaj_zamenjave kljuc (prvi, bes) with 
                    |None -> preveri kljuc bes rep
                    |Some x -> Some x in
  let kljuc1 = String.make 26 '_' in
  let list1 = (List.map (String.trim) (String.split_on_char ' ' niz)) in
  match list1 with
  |[] -> None
  |[""] -> Some ""
  |bes :: rep -> 
    let kl_list = mozne_razsiritve kljuc1 bes slovar in
    let bes_list_list = List.map (fun (list_kljucev, bes) -> (List.map (fun kljuc -> sifriraj kljuc bes) list_kljucev, bes)) (List.map (fun (bes, bes1) -> ((mozne_razsiritve (String.make 26 '_') bes slovar), bes1)) (List.map (fun x -> (x, x)) rep)) in
    let rec pomozna1 kl_list =
      match kl_list with
      |[] -> None
      |kl_prvi :: kl_rep ->
        let rec pomozna2 kljuc sez =
          match sez with
          |[] -> None
          |(prvi_sez, prva_bes) :: ostalo -> 
            match preveri kljuc prva_bes prvi_sez with
            |None -> None
            |Some x -> pomozna2 x ostalo in
        match pomozna2 kl_prvi bes_list_list with
        |None -> pomozna1 kl_rep
        |Some kljuc -> Some (sifriraj kljuc niz) in
    pomozna1 kl_list

let primer_5_16 = sifriraj quick_brown_fox "THIS IS A VERY HARD PROBLEM"
(* val primer_5_16 : string = "VKBO BO T AUSD KTSQ MSJHNUF" *)

let primer_5_17 = odsifriraj "VKBO BO T AUSD KTSQ MSJHNUF"
(* val primer_5_17 : string option = Some "THIS IS A VERY HARD PROBLEM" *)
