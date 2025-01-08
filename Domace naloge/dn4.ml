type direction = Left | Right
type state = string

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
  glava desnega seznama, torej glava stroja *)
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