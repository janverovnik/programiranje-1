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
  levi_sez: [] :: char n :: char (n - 1) :: ... :: char 1
                                                     ^                                       
                              glava levega seznama, torej znak za glavo stroja
  desni_sez : char 1 :: ... :: []
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
              |([], _) | (_, []) -> failwith"seznami ne morejo biti prazni"
              |([' '], _) -> failwith"brezveze"
              |(x :: rep, desni_sez) -> (rep, x :: desni_sez)
              )
    |Right -> (
              match tape with
              |([], _) | (_, []) -> failwith"seznami ne morejo biti prazni"
              |(_, [' ']) -> failwith"brezveze"
              |(levi_sez, y :: rep) -> (y :: levi_sez, rep)
              )

  let read tape = match tape with 
    |([], _) | (_, []) -> failwith"seznami ne morejo biti prazni"
    |(levi_sez, y :: rep) -> y
  
  let write ch tape = match tape with 
  |([], _) | (_, []) -> failwith"seznami ne morejo biti prazni"
  |(levi_sez, y :: rep) -> (levi_sez, ch :: rep)

  let print = failwith "todo"
end