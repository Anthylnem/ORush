(*#load "port.cmo";;*)
open Port;;
(*#load "moves.cmo";;*)
open Moves;;

exception Solver_Problem;;
exception Fin;;

let can_move (mv:move) (s:state) =
  try
    ignore (apply_move mv s) ;
    true;
  with
  |Cannot_move -> false

let move_possible (b:boat) (s:state)=
  let avancer = (b.identifiant,'>') in
  let reculer = (b.identifiant,'<') in

  if (can_move avancer s) = true && (can_move reculer s) = true then [avancer;reculer]
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
(*
let boatEqual (b1:boat) (b2:boat) =
  if ((b1.identifiant = b2.identifiant) && (b1.x = b2.x) && (b1.y = b2.y)) then true
  else false;;

let stateEqual2 (s1:state) (s2:state) =
  let nbEquals = ref 0 in
  List.iter(fun bat1 -> List.iter (fun bat2 -> if (boatEqual bat1 bat2) = true then nbEquals:= 1+ !nbEquals)s2)s1;
  if !nbEquals = (List.length s1) then true else false
*)
let stateEqual (s1:state) (s2:state) =
  if (string_of_state s1)=(string_of_state s2) then true else false

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
  in build lm ls totalMoves []

let solve_state (s:state) =
  let file = Queue.create() in
  Queue.add (s,"") file;
  let rec solve file marque =

    if (Queue.is_empty file) then ""

    else
      let curr_state = Queue.take file in
      let (state,mv) = curr_state in

      if (win state = true) then mv

      else
        let liste_moves  = all_possible_moves state in
        let liste_states = all_reachable_states state liste_moves [] in
        let liste_states_moves = tuples liste_moves liste_states mv in

        let visite = ref [] in
        List.iter (fun (state,mv) -> if (containState state marque) = false then
                      (Queue.add (state,mv) file); visite:= List.append [state] !visite ) liste_states_moves;
        solve file (List.append !visite marque)

  in solve file []

let solve_input (ic:in_channel) :string =
  let state = input_state ic in
  let sol = solve_state state in
  sol
