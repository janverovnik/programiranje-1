(*----------------------------------------------------------------------------*
 # 4. domača naloga
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Pri tej nalogi boste napisali svoj simulator Turingovih strojev. Zaradi
 preprostosti bomo za abecedo vzeli kar znake tipa `char`, za prazni znak bomo
 izbrali presledek `' '`, stanja pa bomo predstavili z nizi. Za možne premike
 zafiksiramo tip `direction`:
[*----------------------------------------------------------------------------*)

type direction = Left | Right
type state = string

(*----------------------------------------------------------------------------*
 ## Implementacija trakov
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite modul `Tape`, ki implementira spodnjo signaturo, kjer je:

 - `t` tip v obe smeri neomejenih trakov in glavo na danem mestu;
 - `make`, ki naredi nov trak z znaki iz niza ter glavo na prvem znaku;
 - `read`, ki vrne znak pod glavo;
 - `write`, ki pod glavo zapiše dani znak;
 - `move`, ki glavo premakne v dano smer;
 - `print`, ki izpiše vsebino traku (brez presledkov na začetku in koncu) ter
 pod njim z `^` označi mesto glave.

 Zadnji dve funkciji naj vrneta nov trak, obstoječega pa naj pustita
 nespremenjenega.

 Ker je tip `t` abstrakten, si lahko privoščite poljubno implementacijo, zato
 poskrbite tako za učinkovitost kot za preglednost kode.
[*----------------------------------------------------------------------------*)

module type TAPE = sig
  type t

  val make : string -> t
  val move : direction -> t -> t
  val read : t -> char
  val write : char -> t -> t
  val print : t -> unit
end

module Tape : TAPE = struct
  (* Ideja je ta: imamo dva seznama, levi je na levo omejen z ' ', desni pa na desno omejen z ' ', 
  vmes pa je karkoli, pri čemer je glava desnega seznama glava turingovega stroja, glava levega seznama 
  pa znak za glavo turingovega stroja, torej je levi seznam obrnjen:
  levi_sez: [] :: x_n :: x_(n - 1) :: ... :: x_1
                                              ^                                       
                        glava levega seznama, torej znak za glavo stroja
  desni_sez : y_1 :: y_2 :: ... :: []
               ^
  glava desnega seznama, torej glava stroja 
  Tak pristop ne dopušča dvojnih presledkov med nizi, saj jih takoj pobriše v enojni, kar lahko morda vidimo kot prednost.*)

  type t = (char list) * (char list)

  let make niz =
    let desni_sez = List.of_seq (String.to_seq (niz ^ " ")) in
    ([' '], desni_sez)

  let move dir tape = 
    match dir with
    |Left -> (
              match tape with
              |(_, []) -> failwith"desni_sez ne more biti prazen"
              |([], desni_sez) -> ([], ' ' :: desni_sez)
              |(x :: rep, [' ']) -> if x = ' ' then (rep, [' ']) else (rep, x :: [' '])
              |(x :: rep, desni_sez) -> (rep, x :: desni_sez)
              )
    |Right -> (
              match tape with
              |(_, []) -> failwith"desni_sez ne more biti prazen"
              |([], y1 :: y2 :: rep) -> if y2 = ' ' then ([], y2 :: rep) else ([y1], y2 :: rep)
              |(levi_sez, [' ']) -> (' ' :: levi_sez, [' '])
              |(levi_sez, y :: rep) -> (y :: levi_sez, rep)
              )

  let read tape = match tape with 
    |(_, []) -> failwith"desni_sez ne more biti prazen"
    |(levi_sez, y :: rep) -> y
  
  let write ch tape = match tape with 
  |(_, []) -> failwith"desni_sez ne more biti prazen"
  |(levi_sez, y :: rep) -> 
    if levi_sez = [] then (' ' :: [], ch :: rep) else 
    if rep = [] then (levi_sez, ch :: ' ' :: []) else (levi_sez, ch :: rep)

  let print tape = 
    let rec aux levi_sez desni_sez acc pointer =
      match levi_sez, desni_sez with
      |[], [] -> (acc, pointer)
      |[], y :: ys -> aux [] ys (acc ^ Char.escaped y) pointer
      |' ' :: [], [] -> aux [' '] [] acc (pointer ^ "^")
      |x :: xs, [] -> aux xs [] (Char.escaped x ^ acc) (pointer ^ " ")
      |' ' :: [], y :: ys -> aux [] ys (acc ^ Char.escaped y) (pointer ^ "^")
      |x :: xs, y :: ys -> aux xs ys (Char.escaped x ^ acc ^ Char.escaped y) (pointer ^ " ") in
    match tape with
    |(_, []) -> failwith"desni_sez ne more biti prazen"
    |(levi_sez, desni_sez) ->
    match aux levi_sez desni_sez "" "" with |(acc, pointer) ->
      print_endline acc;
      print_endline pointer
      

end

let primer_trak = Tape.(
  make "ABCDE"
  |> move Left
  |> move Left
  |> move Right
  |> move Right
  |> move Right
  |> move Right
  |> write '!'
  |> print
)
(*
AB!DE
  ^
*)
(* val primer_trak : unit = () *)

(*----------------------------------------------------------------------------*
 ## Implementacija Turingovih strojev
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite modul `Machine`, ki implementira spodnjo signaturo, kjer je:

 - `t` tip Turingovih strojev;
 - `make`, ki naredi nov stroj z danim začetnim stanjem in seznamom preostalih
 stanj ter prazno prehodno funkcijo;
 - `initial`, ki vrne začetno stanje stroja;
 - `add_transition`, ki prehodno funkcijo razširi s prehodom $(q, a) \mapsto
 (q', a', d)$;
 - `step`, ki za dano stanje in trak izvede en korak stroja, če je to mogoče.

 Zadnji dve funkciji naj vrneta spremenjene vrednosti, obstoječe argumente pa
 naj pustita nespremenjene. Prav tako pri zadnjih dveh funkcijah lahko
 predpostavite, da ju bomo klicali le na poprej podanih stanjih.

 Tudi tu je tip `t` abstrakten, zato poskrbite za učinkovitost in preglednost
 kode.
[*----------------------------------------------------------------------------*)

module StrCh = (*hvala Filip*)
  struct
    type t = string * char
    let compare (x0,y0) (x1,y1) =
      match Stdlib.compare x0 x1 with
        | 0 -> Stdlib.compare y0 y1
        | c -> c
  end

module Slovar = Map.Make(StrCh)

module type MACHINE = sig
  type t
  val make : state -> state list -> t
  val initial : t -> state
  val add_transition : state -> char -> state -> char -> direction -> t -> t
  val step : t -> state -> Tape.t -> (state * Tape.t) option
end

module Machine : MACHINE = struct

  type 'a tree = Prazno | Sestavljeno of 'a tree * 'a * 'a tree
  let make_tree a = Sestavljeno (Prazno, a, Prazno)

  let nov_manjsi state1 ch1 state2 ch2 = (*I assume da kle ne bodo kkšna grozodejanja kukr dve različni prehodni, ki vzameta isto stanje pa char :3*)
    if state1 <> state2 then state1 < state2 else ch1 < ch2 

  type t = {
    stanja : state list;
    zac_st : state;
    preh_fun : (state * char * state * char * direction) list;
  }

  let make state st_list = {stanja = state :: st_list; zac_st = state; preh_fun = []}
  let initial stroj = stroj.zac_st

  (*let add_transition state ch state' ch' dir stroj = 
    let rec dodaj state ch state' ch' dir drevo = match drevo with
      |Prazno -> make_tree (state, ch, state', ch', dir)
      |Sestavljeno (l, (state', ch', a, b, c), d) -> (
        if nov_manjsi state ch state' ch' then 
        Sestavljeno (dodaj state ch state' ch' dir l, (state', ch', a, b, c), d) else
        Sestavljeno (l, (state', ch', a, b, c), dodaj state ch state' ch' dir d) 
       ) in 
    let drevo = dodaj state ch state' ch' dir stroj.preh_fun in
    {stroj with preh_fun = drevo} *)

  let add_transition state ch state' ch' dir stroj = {stroj with preh_fun = (state, ch, state', ch', dir) :: stroj.preh_fun} 

 (* let step : t -> state -> Tape.t -> (state * Tape.t) option = fun stroj state tape ->
    let rec najdi state a drevo = match drevo with
      |Prazno -> None
      |Sestavljeno (l, x, d) -> (
        match x with |(state', ch, n, m, dir) ->
          if state = state' && a = ch then Some x else
          if nov_manjsi state a state' ch then najdi state a l 
          else najdi state a d 
      ) in
    match najdi state (Tape.read tape) stroj.preh_fun with
      |None -> None
      |Some (state', ch, nov_state, nov_ch, dir) -> 
        Some (nov_state, Tape.(tape |> write nov_ch |> move dir)) *)
    
  let step : t -> state -> Tape.t -> (state * Tape.t) option = fun stroj state tape ->
    let rec najdi state a list = match list with
      |[] -> None
      |x :: rep -> match x with |(state', ch, n, m, dir) -> if state' = state && ch = a then Some x else najdi state a rep in
    match najdi state (Tape.read tape) stroj.preh_fun with
      |None -> None
      |Some (state', ch, nov_state, nov_ch, dir) -> 
        Some (nov_state, Tape.(tape |> write nov_ch |> move dir)) 
end

(*----------------------------------------------------------------------------*
 Primer stroja "Binary Increment" na <http://turingmachine.io> lahko
 implementiramo kot:
[*----------------------------------------------------------------------------*)

let binary_increment =
  Machine.(
    make "right" [ "carry"; "done" ] 
    |> add_transition "right" '1' "right" '1' Right
    |> add_transition "right" '0' "right" '0' Right
    |> add_transition "right" ' ' "carry" ' ' Left
    |> add_transition "carry" '1' "carry" '0' Left
    |> add_transition "carry" '0' "done" '1' Left
    |> add_transition "carry" ' ' "done" '1' Left
  )

(* val binary_increment : Machine.t = <abstr> *)

(*----------------------------------------------------------------------------*
 Zapišite funkciji `slow_run` in `speed_run` tipa `Machine.t -> str -> unit`, ki
 simulirata Turingov stroj na traku, na katerem je na začetku zapisan dani niz.
 Prva naj izpiše trakove in stanja pri vseh vmesnih korakih, druga pa naj izpiše
 le končni trak. Slednjo bomo uporabljali tudi pri meritvi učinkovitosti
 izvajanja.
[*----------------------------------------------------------------------------*)

let slow_run stroj niz =
  let zac_tape = Tape.make niz in
  let zac_st = Machine.initial stroj in
  let rec delaj stroj state tape =
    if state = "done" then 
      (
      Tape.print tape;
      print_endline "done";
      )
    else
      (
      Tape.print tape;
      print_endline state;
      match Machine.step stroj state tape with
      |None -> failwith"Command no work"
      |Some (nov_state, nov_trak) -> delaj stroj nov_state nov_trak
      ) in
  delaj stroj zac_st zac_tape

let primer_slow_run =
  slow_run binary_increment "1011"
(*
1011
^
right
1011
 ^
right
1011
  ^
right
1011
   ^
right
1011
    ^
right
1011
   ^
carry
1010
  ^
carry
1000
 ^
carry
1100
^
done
*)
(* val primer_slow_run : unit = () *)

let speed_run stroj niz =
  let tape = ref (Tape.make niz) in
  let current = ref (Machine.initial stroj) in
  while !current <> "done" do
    match Machine.step stroj !current !tape with
      |None -> failwith"a"
      |Some (nov_state, nov_trak) -> 
        current := nov_state;
        tape := nov_trak;
    ()
  done;
  Tape.print !tape;
  print_endline "done"

  

(*let primer_speed_run =
  speed_run binary_increment "1011"*)
(*
1100
^
*)
(* val primer_speed_run : unit = () *)

(*----------------------------------------------------------------------------*
 ## Krajši zapis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Ko definiramo Turingov stroj, prehode običajno združujemo najprej po stanjih,
 nato pa še po znakih. Prav tako pri dosti prehodih samo premikamo glavo, trak
 in stanje pa pustimo pri miru. Zapišite funkcije:

 - `for_state`
 - `for_character`
 - `for_characters`
 - `move`
 - `switch_and_move`
 - `write_and_move`
 - `write_switch_and_move`

 s katerimi bi lahko zgornji primer na krajše zapisali kot spodaj.
 Implementacijo in tipe ugotovite sami.
[*----------------------------------------------------------------------------*)

(* let binary_increment' =
  Machine.make "right" ["carry"; "done"]
  |> for_state "right" [
    for_characters "01" @@ move Right;
    for_character ' ' @@ switch_and_move "carry" Left
  ]
  |> for_state "carry" [
    for_character '1' @@ switch_and_move "carry" Left;
    for_characters "0 " @@ write_switch_and_move '1' "done" Left
  ]   *)
(* val binary_increment' : Machine.t = <abstr> *)

(*----------------------------------------------------------------------------*
 ## Primeri Turingovih strojev
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Pri tej nalogi boste sestavljali stroje, ki bodo iz začetnega niza na traku na
 različne načine izračunali nov niz. Pri tem lahko predpostavite, da je začetni
 niz sestavljen iz ničel in enic, preostanek traku pa je prazen. Na koncu
 izvajanja naj bo glava na začetku novega niza, z izjemo tega niza pa naj bo
 trak prazen. Ni pa treba, da se izračunani niz začne na istem mestu na traku,
 kot se je začel prvotni niz.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### Obračanje niza
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite Turingov stroj, ki začetni niz obrne na glavo.
[*----------------------------------------------------------------------------*)

let reverse = ()

(*let primer_reverse = speed_run reverse "0000111001"*)
(* 
1001110000          
^
*)
(* val primer_reverse : unit = () *)

(*----------------------------------------------------------------------------*
 ### Podvajanje niza
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite Turingov stroj, ki podvoji začetni niz.
[*----------------------------------------------------------------------------*)

let duplicate = ()

(*let primer_duplicate = speed_run duplicate "010011"*)
(* 
001100001111       
^
*)
(* val primer_duplicate : unit = () *)

(*----------------------------------------------------------------------------*
 ### Eniški zapis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite Turingov stroj, ki na začetku na traku sprejme število $n$, zapisano
 v dvojiškem zapisu, na koncu pa naj bo na traku zapisanih natanko $n$ enic.
[*----------------------------------------------------------------------------*)

let to_unary = ()

(*let primer_to_unary = speed_run to_unary "1010"*)
(* 
1111111111
^
*)
(* val primer_to_unary : unit = () *)

(*----------------------------------------------------------------------------*
 ### Dvojiški zapis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite ravno obratni Turingov stroj, torej tak, ki na začetku na traku
 sprejme število $n$ enic, na koncu pa naj bo na traku zapisano število $n$ v
 dvojiškem zapisu.
[*----------------------------------------------------------------------------*)

let to_binary = ()

(*let primer_to_binary = speed_run to_binary (String.make 42 '1')*)
(* 
101010                                           
^
*)
(* val primer_to_binary : unit = () *)
