(*----------------------------------------------------------------------------*
 # Podatkovni tipi
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Valute

 Pri modeliranju denarja ponavadi uporabljamo racionalna števila. Problemi se
 pojavijo, ko uvedemo različne valute. Oglejmo si dva pristopa k izboljšavi
 varnosti pri uporabi valut.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Definirajte tipa `euro` in `dollar`, kjer ima vsak od tipov zgolj en
 konstruktor, ki sprejme racionalno število. Nato napišite funkciji
 `euro_to_dollar` in `dollar_to_euro`, ki primerno pretvarjata valuti (točne
 vrednosti pridobite na internetu ali pa si jih izmislite).

 Namig: Občudujte informativnost tipov funkcij.
[*----------------------------------------------------------------------------*)

type euro = float

type dollar = float 

let dollar_to_euro : euro -> dollar = fun a -> 
  1.08 *. a

let euro_to_dollar : dollar -> euro = fun a ->
  a /. 1.08

(* let primer_valute_1 = dollar_to_euro (Dollar 0.5) *)
(* val primer_valute_1 : euro = Euro 0.4305 *)

(*----------------------------------------------------------------------------*
 Definirajte tip `currency` kot en vsotni tip z konstruktorji za jen, funt in
 švedsko krono. Nato napišite funkcijo `to_pound`, ki primerno pretvori valuto
 tipa `currency` v funte.

 Namig: V tip dodajte še švicarske franke in se navdušite nad dejstvom, da vas
 Ocaml sam opozori, da je potrebno popraviti funkcijo `to_pound`.
[*----------------------------------------------------------------------------*)

type currency =
  |Jen of int * int
  |Funt of int * int
  |SvKrona of int * int

let to_pound : currency -> currency =
  function
  |Jen (a, b) -> Funt (a, b * 200)
  |Funt (a, b) -> Funt (a, b)
  |SvKrona (a ,b) -> Funt (a * 10, b * 137)


(* let primer_valute_2 = to_pound (Yen 100.) *)
(* val primer_valute_2 : currency = Pound 0.700000000000000067 *)

(*----------------------------------------------------------------------------*
 ## Mešani seznami

 Želimo uporabljati sezname, ki hranijo tako cela števila kot tudi logične
 vrednosti. To bi lahko rešili tako da uvedemo nov tip, ki predstavlja celo
 število ali logično vrednost, v nadaljevanju pa bomo raje konstruirali nov tip
 seznamov.

 Spomnimo se, da lahko tip `list` predstavimo s konstruktorjem za prazen seznam
 `Nil`(oz. `[]` v Ocamlu) in pa konstruktorjem za člen `Cons(x, xs)` (oz. `x ::
 xs` v Ocamlu).
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Definirajte tip `intbool_list` z konstruktorji za:

 - prazen seznam,
 - člen s celoštevilsko vrednostjo,
 - člen z logično vrednostjo.

 Nato napišite testni primer, ki bi predstavljal `[5; true; false; 7]`.
[*----------------------------------------------------------------------------*)

type intbool_list =
  |Int of int * intbool_list
  |Bool of bool * intbool_list
  |Nil

let test : intbool_list = Int (5, (Bool (true, (Bool (false, (Int (7, Nil)))))))

(*----------------------------------------------------------------------------*
 Funkcija `intbool_map f_int f_bool ib_list` preslika vrednosti `ib_list` v nov
 `intbool_list` seznam, kjer na elementih uporabi primerno od funkcij `f_int`
 oz. `f_bool`.
[*----------------------------------------------------------------------------*)

let rec intbool_map f_int f_bool ib_list =
  match ib_list with
  |Nil -> Nil
  |Int (n, rep) -> Int (f_int n, intbool_map f_int f_bool rep) 
  |Bool (bool, rep) -> Bool (f_bool bool, intbool_map f_int f_bool rep)

(*----------------------------------------------------------------------------*
 Funkcija `intbool_reverse` obrne vrstni red elementov `intbool_list` seznama.
 Funkcija je repno rekurzivna.
[*----------------------------------------------------------------------------*)

let intbool_reverse ib_list =
  let rec aux ib_list acc =
    match ib_list with
    |Nil -> acc
    |Int (n, rep) -> aux rep (Int (n, acc))
    |Bool (bool, rep) -> aux rep (Bool (bool, acc)) in
  aux ib_list Nil


(*----------------------------------------------------------------------------*
 Funkcija `intbool_separate ib_list` loči vrednosti `ib_list` v par `list`
 seznamov, kjer prvi vsebuje vse celoštevilske vrednosti, drugi pa vse logične
 vrednosti. Funkcija je repno rekurzivna in ohranja vrstni red elementov.
[*----------------------------------------------------------------------------*)

let intbool_separate ib_list =
  let rec aux ib_list acc1 acc2 =
    match ib_list with
    |Nil -> (List.rev acc1, List.rev acc2)
    |Int (n, rep) -> aux rep (n :: acc1) acc2
    |Bool (bool, rep) -> aux rep acc1 (bool :: acc2) in
  aux ib_list [] []

(*----------------------------------------------------------------------------*
 ## Čarodeji

 Določeni ste bili za vzdrževalca baze podatkov za svetovno priznano čarodejsko
 akademijo "Effemef". Vaša naloga je konstruirati sistem, ki bo omogočil
 pregledno hranjenje podatkov.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Čarodeje razvrščamo glede na vrsto magije, ki se ji posvečajo. Definirajte tip
 `magic`, ki loči med magijo ognja, magijo ledu in magijo arkane oz. fire, frost
 in arcane. Ko se čarodej zaposli na akademiji, se usmeri v zgodovino,
 poučevanje ali raziskovanje oz. historian, teacher in researcher. Definirajte
 tip `specialisation`, ki loči med temi zaposlitvami.
[*----------------------------------------------------------------------------*)

type magic =
 |Fire
 |Frost
 |Arcane

type specialisation =
 |Historian
 |Teacher
 |Researcher

(*----------------------------------------------------------------------------*
 Vsak od čarodejev začne kot začetnik, nato na neki točki postane študent, na
 koncu pa SE lahko tudi zaposli. Definirajte tip `status`, ki določa ali je
 čarodej:

 - začetnik `Newbie`,
 - študent `Student` (in kateri vrsti magije pripada in koliko časa študira),
 - zaposlen `Employed` (in vrsto magije in specializacijo).

 Nato definirajte zapisni tip `wizard` z poljem za ime in poljem za trenuten
 status ter dodajte primer `professor`, ki je zaposlen učitelj magije ognja, in
 `jaina`, ki je četrto leto študentka magije ledu.
[*----------------------------------------------------------------------------*)

type status =
 |Newbie
 |Student of magic * int
 |Employed of magic * specialisation

type wizard = {
 name : string;
 status : status
}

let professor  = {name = "Astonidas"; status = Employed (Fire, Teacher)}

let jaina = { name = "Jaina"; status = Student (Frost, 4) } 

(*----------------------------------------------------------------------------*
 Želimo prešteti koliko uporabnikov posamezne od vrst magije imamo na akademiji.
 Definirajte zapisni tip `magic_counter`, ki v posameznem polju hrani število
 uporabnikov magije. Nato definirajte funkcijo `update counter magic`, ki vrne
 nov števec s posodobljenim poljem glede na vrednost `magic`.
[*----------------------------------------------------------------------------*)

type magic_counter = {
 fire : int;
 frost : int;
 arcane : int
}

let update counter tip =
  let f, i, a = counter.fire, counter.frost, counter.arcane in
  match tip with
  |Fire -> {fire = f + 1; frost = i; arcane = a}
  |Frost -> {fire = f; frost = i + 1; arcane = a}
  |Arcane -> {fire = f; frost = i; arcane = a + 1}

(* let primer_carovniki_1 = update {fire = 1; frost = 1; arcane = 1} Arcane *)
(* val primer_carovniki_1 : magic_counter = {fire = 1; frost = 1; arcane = 2} *)

(*----------------------------------------------------------------------------*
 Funkcija `count_magic` sprejme seznam čarodejev in vrne števec uporabnikov
 različnih vrst magij.
[*----------------------------------------------------------------------------*)

let count_magic wlist = 
  let rec aux wlist acc =
    match wlist with 
    |[] -> acc
    |prvi :: rep -> 
      match prvi.status with
      |Newbie -> aux rep acc 
      |Student (magic, _) -> aux rep (update acc magic)
      |Employed (magic, _) -> aux rep (update acc magic) in
  aux wlist {fire = 0; frost = 0; arcane = 0}

(* let primer_carovniki_2 = count_magic [professor; professor; professor] *)
(* val primer_carovniki_2 : magic_counter = {fire = 3; frost = 0; arcane = 0} *)

(*----------------------------------------------------------------------------*
 Želimo poiskati primernega kandidata za delovni razpis. Študent lahko postane
 zgodovinar po vsaj treh letih študija, raziskovalec po vsaj štirih letih
 študija in učitelj po vsaj petih letih študija. Funkcija `find_candidate magic
 specialisation wizard_list` poišče prvega primernega kandidata na seznamu
 čarodejev in vrne njegovo ime, čim ustreza zahtevam za `specialisation` in
 študira vrsto `magic`. V primeru, da ni primernega kandidata, funkcija vrne
 `None`.
[*----------------------------------------------------------------------------*)

let rec find_candidate magic spec wlist = 
  match wlist with
  |[] -> None
  |prvi :: rep -> 
    match prvi.status with
    |Newbie -> find_candidate magic spec rep
    |Employed (_, _) -> find_candidate magic spec rep
    |Student (cagim, n) -> 
      if cagim <> magic then None else
      match spec with
      |Historian -> if n >= 3 then Some prvi.name else find_candidate magic spec rep
      |Researcher -> if n >= 4 then Some prvi.name else find_candidate magic spec rep
      |Teacher -> if n >= 5 then Some prvi.name else find_candidate magic spec rep
      

(* let primer_carovniki_3 =
  find_candidate Frost Researcher [professor; jaina] *)
(* val primer_carovniki_3 : string option = Some "Jaina" *)
