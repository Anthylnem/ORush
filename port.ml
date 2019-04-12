open String
open Char
open Printf

exception Invalid_argument of string

type boat = {identifiant : char;
            longueur : int;
            orientation : char;
            mutable x : int;
            mutable y : int}

type state = boat list

let world_size = 6

let boat_of_string s =
  (*On verifie toutes les conditions de l'environnement du jeu*)
  if String.length s > 5 then raise (Invalid_argument "Chaîne trop grande");
  if (s.[0]) < 'A' || s.[0] >'Z' then raise (Invalid_argument "Identifiant incorrect");

  let tmp3 = Char.escaped s.[3] in
  let tmp4 = Char.escaped s.[4] in
  if (int_of_string (tmp3) >= 6) || (int_of_string (tmp4) >= 6) then raise (Invalid_argument "Pas dans la matrice");

  if (s.[1] < '2') || (s.[1]>'3')
  then raise (Invalid_argument "Taille du bateau invalide");

  if not((s.[2] == 'V') || (s.[2] == 'H'))
  then raise (Invalid_argument "Orientation incorrecte");

  let tmp = Char.escaped s.[4] in
  let tmp1 = Char.escaped s.[1] in

  if (s.[2] == 'V') && (int_of_string (tmp) + int_of_string(tmp1) > world_size)
  then raise  (Invalid_argument "Depasse le tableau en vertical");

  let tmp = Char.escaped s.[3] in

  if (s.[2] == 'H') && (int_of_string (tmp) + int_of_string(tmp1) > world_size)
  then raise  (Invalid_argument "Depasse le tableau en horizontal")

  else {identifiant = s.[0];
        longueur = (int_of_char s.[1])-48;
        orientation = s.[2];
        x = (int_of_char s.[3])-48;
        y = (int_of_char s.[4])-48}

let string_of_boat b =
  Char.escaped(b.identifiant)^string_of_int(b.longueur)^escaped(b.orientation)^string_of_int(b.x)^string_of_int(b.y)

let grid_of_state (s:state) =
  (*Representation matricielle du state*)
  let p = Array.make_matrix world_size world_size '~' in
  List.iter(fun bateau ->
      for j=0 to bateau.longueur-1 do
      match bateau.orientation with
      |'H' -> p.((bateau.x)+j).((bateau.y)) <- bateau.identifiant
      | _  -> p.(bateau.x).((bateau.y)+j) <- bateau.identifiant
    done;) s;
  p

let add_boat b (s:state) :state =
  let world = grid_of_state s in
  Printf.printf "Bateau %c -> (%d,%d)\n" b.identifiant b.x b.y;
  for i = 0 to b.longueur-1 do
    match b.orientation with
    |'V' -> if world.(b.x).(b.y+i)<>'~' then raise (Invalid_argument "chevauchement !")
    | _  -> if world.(b.x+i).(b.y)<>'~' then raise (Invalid_argument "chevauchement !")
  done;
  List.append s [b]

let print_matrix2 ch = ch |> Array.iter (fun xs -> xs |> Array.iter (fun x -> Printf.printf " %c " x); print_string "\n");;

let print_matrix (m:char array array) =
  (*Affichage de la matrice representant le state : utiliser grid_of_state pour le parametrage*)
  Printf.printf "\n";
  for i=0 to Array.length m -1 do
    for j=0 to Array.length m.(0) -1 do
      Printf.printf " %c " m.(j).(i);
    done;
    Printf.printf "\n"
  done;
  Printf.printf "\n"

let input_state ic :state=
  let create_state = ref [] in
  try
    while true do
      let line = input_line ic in
      let bateau = boat_of_string line in
      create_state := (add_boat bateau !create_state)
    done;
    !create_state;
  with
  |Invalid_argument "" -> Printf.printf
    "Erreur d'ajout :: Positions initiales invalides";!create_state;

  |End_of_file -> !create_state

let output_state (state:state) oc =
  let grid = grid_of_state state in
    for i=0 to (Array.length grid) do
      for j=0 to (Array.length grid.(0)) do
        Printf.fprintf oc "%c" grid.(i).(j)
      done;
      print_newline ();
    done

let boatCopy (b:boat) =
  {identifiant = b.identifiant; longueur = b.longueur;
   orientation = b.orientation; x = b.x; y = b.y}

let stateCopy (state:state) =
  let rec build s newS =
    match s with
    |[] -> newS
    |hb::lb -> build lb ((boatCopy hb)::newS)
  in build state []
    (*
let ()=
  let btest =  boat_of_string "A2H12" in
  let b2 = boatCopy btest in
  print_string ((string_of_boat b2)^"\n");
  let state = [] in
  let state = add_boat b2 state in
  let sc = stateCopy state in
  print_matrix (grid_of_state sc)
*)

(*let () =
  let boat = boat_of_string "B2H12" in
  let boat2 = boat_of_string "A3V41" in
  let state = [] in
  let state = add_boat boat state in
  let p = grid_of_state state in
  print_matrix p;
  let s = add_boat boat2 state in
  let p = grid_of_state s in
  print_matrix p;;
  let test = open_in "tests/pos12.txt" in
  let state = input_state test in
  print_matrix (grid_of_state state);
  *)
