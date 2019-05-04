open Dom
open Port
open Moves

type etat = { mutable port : state;
              mutable cur_boat : boat}

let btest =  boat_of_string "A2H12"
let btest2 =  boat_of_string "B2V42"
let state = []
let state = add_boat btest state
let state = add_boat btest2 state
let etat_jeu = {port = state; cur_boat = btest}

(*Recuperation du context graphique du canvas*)
let get_context =
  let canvas = Document.get_element_by_id document "port" in
  Canvas.get_context_2d (Canvas.of_element canvas)

(*Dessiner un bateau suivant son orientation*)
let draw_boat context boat =
  Canvas.RenderingContext2D.set_fill_style context "green";

  if boat.orientation = 'V' then
    Canvas.RenderingContext2D.fill_rect context (boat.x*100) (boat.y*100) 100 (boat.longueur*100)
  else
    Canvas.RenderingContext2D.fill_rect context (boat.x*100) (boat.y*100) (boat.longueur*100) 100

(*Affichage de la grille du Port*)
let draw_grid context =
  for i=0 to 6 do
    Canvas.RenderingContext2D.move_to context (i*100) 0;
    Canvas.RenderingContext2D.line_to context (i*100) 600 ;
    Canvas.RenderingContext2D.stroke context;

    Canvas.RenderingContext2D.move_to context 0 (i*100);
    Canvas.RenderingContext2D.line_to context 600 (i*100);
    Canvas.RenderingContext2D.stroke context
  done

(*Rafraichir le Canvas*)
let refresh =
  let context = get_context in
  Canvas.RenderingContext2D.set_fill_style context "white";
  Canvas.RenderingContext2D.fill_rect context 0 0 600 600;
  draw_grid context;
  List.iter (draw_boat context) etat_jeu.port

let draw_rect boat=
  let context = get_context in
  refresh;
  Canvas.RenderingContext2D.set_stroke_style context "red";
  if boat.orientation = 'V' then
    Canvas.RenderingContext2D.stroke_rect context (boat.x*100) (boat.y*100) 100 (boat.longueur*100)
  else
    Canvas.RenderingContext2D.stroke_rect context (boat.x*100) (boat.y*100) (boat.longueur*100) 100

let click event =
  Printf.printf "SALUT";
  let x = int_of_float((Event.page_x event)/. 100.) in
  let y = int_of_float((Event.page_y event)/. 100.) in
  let mat = grid_of_state etat_jeu.port in
  let var = mat.(x).(y) in

  if var <> '~' then
    etat_jeu.cur_boat <- (List.find (fun boat -> (boat.identifiant = var)) etat_jeu.port);
    draw_rect etat_jeu.cur_boat

let click_recule () =
  etat_jeu.port <- apply_move (etat_jeu.cur_boat.identifiant,'<') etat_jeu.port;
  refresh

let click_avance () =
  etat_jeu.port <- apply_move (etat_jeu.cur_boat.identifiant,'>') etat_jeu.port;
  refresh


let draw () =

  (*Obligation de création de plusieurs methodes car impossible de passer
    des valeurs en argument dans le listener*)

  let haut = Document.get_element_by_id document "haut" in
  Element.set_onclick haut click_recule;
  let bas = Document.get_element_by_id document "bas" in
  Element.set_onclick bas click_avance;

  let gauche = Document.get_element_by_id document "gauche" in
  Element.set_onclick gauche click_recule;
  let droite = Document.get_element_by_id document "droite" in
  Element.set_onclick droite click_avance;

  (*Recuperation du canvas et ajout d'un listener*)
  let canvas = Document.get_element_by_id document "port" in
  Element.set_attribute canvas "width" "600";
  Element.set_attribute canvas "height" "600";
  Element.add_event_listener canvas "click" click false;

  (*Recuperation du context graphique du canvas*)
  let context = Canvas.get_context_2d (Canvas.of_element canvas) in

  draw_grid context;

  (*let channel = (open_in "/users/nfs/Etu4/3676584/Documents/2018-2019/S2/3I008/ORush/pos1.txt")  in
  let initState = input_state channel in
  List.iter (draw_boat context) initState;
    close_in channel*)

  List.iter (draw_boat context) state


let _ = draw ()
