open Port;;

type move = char * char;;
exception Cannot_move;;

let string_of_move (m:move) =
  match m with
  |id,mv -> (Char.escaped id)^(Char.escaped mv);;

let move_of_string str =
  ((String.get str 0),(String.get str 1));;

let apply_move (m:move) (state:state) =
  (*On copie le state pour que l'etat initiale ne soit pas modifié*)
  let world = Port.grid_of_state state in
  let s = stateCopy state in
  let (id,mv) = m in
  let bateau = List.find(fun boat -> boat.identifiant = id) s in

  match bateau.orientation with

  (*Si bateau vertical*)
  |'V' -> (match mv with
      (*Si le bateau vertical avance (vers le bas)*)
      |'>' -> if bateau.y+ bateau.longueur <6 && world.(bateau.x).(bateau.y + bateau.longueur) = '~' then
          (bateau.y <- bateau.y+1;s) else raise Cannot_move
      (*Si le bateau vertical recule (vers le haut)*)
      | _  -> if bateau.y-1 >=0 && world.(bateau.x).(bateau.y-1) = '~' then
          (bateau.y <- bateau.y-1;s) else raise Cannot_move)

  (*Si bateau horizontal*)
  | _ -> (match mv with
      (*Si le bateau horizontal avance (vers la droite)*)
      |'>' -> if bateau.x+ bateau.longueur <6 && world.(bateau.x+ bateau.longueur).(bateau.y) = '~' then
          (bateau.x <- bateau.x+1;s) else raise Cannot_move
      (*Si le bateau horizontal recule (vers la gauche)*)
      | _  -> if bateau.x-1 >=0 && world.(bateau.x-1).(bateau.y) = '~' then
          (bateau.x <- bateau.x-1;s) else raise Cannot_move)

let win s =
  let bateau = List.find(fun boat -> boat.identifiant = 'A') s in
  match bateau.longueur with
  |2 -> if bateau.x = 4 && bateau.y = 2 then true else false
  |_ -> if bateau.x = 3 && bateau.y = 2 then true else false

let check_solution (s:state) moves =
  (*On verifie si tous les mouvements ne generent aucune erreur (chevauchement)
    et on verifie si tout s'est bien deroulé, que l'etat final soit bien un etat win*)
  try
    let i = ref 0 in
    let s_tmp = ref s in
      while !i<=(String.length moves -2) do
        s_tmp := apply_move (moves.[!i],moves.[!i+1]) !s_tmp;
        i := !i+2
      done;
      print_matrix (grid_of_state !s_tmp);
      win !s_tmp;
  with
  |Cannot_move -> print_string "Erreur de deplacement\n"; false
  |Not_found -> print_string "Bateau A Inexistant\n"; false


(*******************************************************************************************************)
(*       Batterie de test                                                                              *)
(*******************************************************************************************************)

      (*
let () =
  let boat = boat_of_string "A3H32" in
  let state = [] in
  let state = add_boat boat state in
  let res = win state in
  if res = true then Printf.printf "Win\n"
  else Printf.printf "Lose\n"
*)
      (*
let () =
  let boat = boat_of_string "B2H32" in
  let state = [] in
  let state = add_boat boat state in
  let p = grid_of_state state in
  print_matrix p;
  let m_t = ('B','>') in
  let state2 = state in
  let res_b = apply_move m_t state2 in
  print_matrix (grid_of_state state);
  print_matrix (grid_of_state res_b);
*)

(*
let () =
  let boat = boat_of_string "B2V12" in
  let boat2 = boat_of_string "A3H41" in
  let state = [] in
  let state = add_boat boat state in
  let s = add_boat boat2 state in
  let p = grid_of_state s in
  print_matrix p;
  let m_t = "B>A>A>B>B>" in
  let res_b = check_solution s m_t in
  if res_b = true then Printf.printf "Tout va bien" else Printf.printf "A ne sort pas\n"
*)
(*
let () =
  let move_test = "H<I<H<I<H<I<B>C<E<F>G>K>F>G>F>A>A>A>" in
  let test = open_in "tests/pos7.txt" in
  let state = Port.input_state test in
  print_matrix (Port.grid_of_state state);
  let new_s = check_solution state move_test in
  if new_s = true then print_string "Bonne Solution !\n" else print_string "PAS BIEN !\n"
*)
