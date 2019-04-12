open Port;;
open Moves;;

exception Solver_Problem;;
exception Fin;;


(*******************************************************************************************************)
(*       Fonctions pour la recherche de solution                                                       *)
(*******************************************************************************************************)

let can_move (mv:move) (s:state) =
  try
    ignore (apply_move mv s) ;
    true;
  with
  |Cannot_move -> false

let move_possible (b:boat) (s:state)=
  let avancer = (b.identifiant,'>') in
  let reculer = (b.identifiant,'<') in

  if (can_move avancer s) = true && (can_move reculer s) = true then List.append [avancer][reculer]
  else if (can_move avancer s) = true then [avancer]
  else if (can_move reculer s) = true then [reculer]
  else [];;

let all_possible_moves (s:state) =
  let list_moves = ref [] in
  List.iter (fun bateau -> list_moves := List.append (move_possible bateau s) !list_moves) s;
  !list_moves;;

let all_reachable_states (s:state) (ml:move list) l =
  let rec all_state (s:state) (ml:move list) list_state =
    match ml with
    | [] -> list_state
    | mv::tl -> all_state s tl ((apply_move mv s)::list_state)
  in all_state s ml l

let print_moves (m:move list) =
  List.iter(fun (id,mv) -> Printf.printf " %c%c  " id mv) m;;


(*******************************************************************************************************)
(*       Ancienne implementation  :: recherche des etats deja visites *)
(*******************************************************************************************************)

let boatEqual (b1:boat) (b2:boat) =
  if ((b1.identifiant = b2.identifiant) && (b1.x = b2.x) && (b1.y = b2.y)) then true
  else false;;

let stateEqual (s1:state) (s2:state) =
  let nbEquals = ref 0 in
  List.iter(fun bat1 -> List.iter (fun bat2 -> if (boatEqual bat1 bat2) = true then nbEquals:= 1+ !nbEquals)s2)s1;
  if !nbEquals = (List.length s1) then true else false

let containState (s:state) (sl:state list) =
  let equal = ref false in
  List.iter (fun state -> if (stateEqual s state) = true then equal:=true) sl;
  !equal

(*Construit la liste des tuples (State,totalMoves)*)
let tuples (lm:move list) (ls:state list) totalMoves =
  let rec build (lm:move list) (ls:state list) totmoves lt =
    match lm,ls with
    |[],[] -> lt
    (*On concatène pour chaque totalMoves de noeud (state, totalMoves) le deplacement associé par le changement d'etat*)
    |hm::ms,hs::ss -> build ms ss totmoves (List.append lt [(hs,(totmoves^(string_of_move hm)))])
    | _ -> []
  in build lm ls totalMoves [];;

(*******************************************************************************************************)
(*       Derniere implementation : plus optimale, necessite de construire la string representant l'etat*)
(*******************************************************************************************************)

(*Premiere implementation avec une reference, cependant il existe une structure plus optimisée avec les Buffer*)
let string_of_state2 (s:state) :string=
  let strState = ref "" in
  List.iter (fun boat -> strState:= (!strState ^(string_of_boat boat)) ) s;
  !strState

let string_of_state (s:state) :string=
(*Permettra de verifier dans visite si un etat a deja été visité en transformant la matrice de caractere en une string*)
  let buff = Buffer.create (6*6) in
  Array.iter(fun row -> Array.iter (fun case -> Buffer.add_char buff case)row) (grid_of_state s);
  Buffer.contents buff


(*******************************************************************************************************)
(*      Solver  : utilisation -> Queue + HashTable                                                                                       *)
(*******************************************************************************************************)

let solve_state (s:state) =

  (*file = Noeud qui restent encore à explorer*)
  let file = Queue.create() in
  Queue.add (s,"") file;
  (*Visite = Marquer les noeuds trouvés pour ne plus les prendre en compte apres*)
  let visite = Hashtbl.create 300 in
  Hashtbl.add visite (string_of_state s) "";

  let rec solve ()=

    (*Verification si la file est vide*)
    if (Queue.is_empty file) then ""

    else
      (*On recupere l'etat en tete de file*)
      let curr_state = Queue.take file in
      let (state,mv) = curr_state in

      (*On verifie si l'etat est win*)
      if (win state = true) then mv
      else
        let liste_moves  = all_possible_moves state in
        let liste_states = all_reachable_states state (List.rev liste_moves) [] in

        (*Ajoute les states non visités et associe le mouvement correspondant au changement d'etat (par rapport au curr_state)
                -> On sait que les elements de liste_moves et liste_states sont concordants (par index) donc on peut utilisé List.iter2*)
        List.iter2 (fun moves state ->
            if not (Hashtbl.mem visite (string_of_state state)) then (
              Hashtbl.add visite (string_of_state state) (string_of_move moves);
              Queue.add (state, mv ^ (string_of_move moves)) file))
          liste_moves liste_states ;

        (*On appelle recursivement solve jusqu'a trouver un etat win ou si la solution est impossible retourne la solution ""
                -> Si solution impossible, la file retombera à 0, on aura visité tous les etats possibles*)
        solve ()

  in solve ()

let solve_input (ic:in_channel) :string =
  let state = input_state ic in
  let sol = solve_state state in
  sol
