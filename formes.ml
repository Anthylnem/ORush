open Dom
open Port
open Moves
open Solver


type etat = { mutable port : state;
              mutable cur_boat : boat;
              mutable port_init : state}

let init_boat = boat_of_string "A2H12"
let etat_jeu = {port = []; cur_boat = init_boat; port_init = []}
let cpt = ref 0
    (*bleu,vert,violet,jaune,cyan*)
let colors = ["#26A6FF";"#4DC136";"#C136BE";"#FFD109";"#3CEAEA"]

(*Recuperation du context graphique du canvas*)
let get_context =
  let canvas = Document.get_element_by_id document "port" in
  Canvas.get_context_2d (Canvas.of_element canvas)

(*Dessiner un bateau suivant son orientation*)
let draw_boat context boat color=
  Canvas.RenderingContext2D.set_fill_style context color;                         (*ROUGE*)
  if boat.identifiant = 'A' then Canvas.RenderingContext2D.set_fill_style context "#FF2626";
  match boat.orientation with
  |'V' -> Canvas.RenderingContext2D.fill_rect context (boat.x*100) (boat.y*100) 100 (boat.longueur*100)
  | _  -> Canvas.RenderingContext2D.fill_rect context (boat.x*100) (boat.y*100) (boat.longueur*100) 100

(*Affichage de la grille du Port*)
let draw_grid context =
  Canvas.RenderingContext2D.set_stroke_style context "black";
  for i=0 to 6 do
    Canvas.RenderingContext2D.move_to context (i*100) 0;
    Canvas.RenderingContext2D.line_to context (i*100) 600 ;
    Canvas.RenderingContext2D.stroke context;

    Canvas.RenderingContext2D.move_to context 0 (i*100);
    Canvas.RenderingContext2D.line_to context 600 (i*100);
    Canvas.RenderingContext2D.stroke context
  done

  let draw_rect boat=
    let context = get_context in
    Canvas.RenderingContext2D.set_stroke_style context "red";
    if boat.orientation = 'V' then
      Canvas.RenderingContext2D.stroke_rect context (boat.x*100) (boat.y*100) 100 (boat.longueur*100)
    else
      Canvas.RenderingContext2D.stroke_rect context (boat.x*100) (boat.y*100) (boat.longueur*100) 100

(*Rafraichir le Canvas*)
let refresh () =
  let context = get_context in
  Canvas.RenderingContext2D.set_fill_style context "white";
  Canvas.RenderingContext2D.fill_rect context 0 0 600 600;
  draw_grid context;
  let rec draw_color b c =
    match b,c with
    |[],_ -> ()
    |_ , [] -> ()
    |hb::tb, hc::tc -> draw_boat context hb hc; draw_color tb tc
  in
  draw_color etat_jeu.port colors;
  draw_rect etat_jeu.cur_boat

let click event =
  let x = (Event.offset_x event)/ 100 in
  let y = (Event.offset_y event)/ 100 in
  let mat = grid_of_state etat_jeu.port in
  let var = mat.(x).(y) in

  if var <> '~' then
    etat_jeu.cur_boat <- (List.find (fun boat -> (boat.identifiant = var)) etat_jeu.port);

  refresh();
  draw_rect etat_jeu.cur_boat

let is_win () =
  let mv = Document.get_element_by_id document "mv" in
  let txt = Char.escaped(etat_jeu.cur_boat.identifiant) in
  Element.set_text_content mv txt;
  if win etat_jeu.port then
    let haut = Document.get_element_by_id document "haut" in
    Element.set_attribute haut "disabled" "";
    let bas = Document.get_element_by_id document "bas" in
    Element.set_attribute bas "disabled" "";
    let gauche = Document.get_element_by_id document "gauche" in
    Element.set_attribute gauche "disabled" "";
    let droite = Document.get_element_by_id document "droite" in
    Element.set_attribute droite "disabled" "";
    let h1 = Document.get_element_by_id document "h1" in
    Element.set_text_content h1 ("ORush : PARTIE GAGNEE en "^(string_of_int !cpt)^" coups !")

let click_recule () =
  etat_jeu.port <- apply_move (etat_jeu.cur_boat.identifiant,'<') etat_jeu.port;
  etat_jeu.cur_boat <- (List.find (fun boat -> (boat.identifiant = etat_jeu.cur_boat.identifiant)) etat_jeu.port);
  refresh ();
  cpt := !cpt +1;
  is_win ()

let click_avance () =
  etat_jeu.port <- apply_move (etat_jeu.cur_boat.identifiant,'>') etat_jeu.port;
  etat_jeu.cur_boat <- (List.find (fun boat -> (boat.identifiant = etat_jeu.cur_boat.identifiant)) etat_jeu.port);
  refresh ();
  cpt := !cpt +1;
  is_win ()

let rec add_boat boats state :state=
  match boats with
  | [] -> state
  | hd::tl -> add_boat tl (boat_of_string hd::state)

(*
let disable_button str =
  (*Boutons verticaux*)
  let haut = Document2.get_element_by_id document2 "haut" in
  Element2.set_attribute haut "disabled" str;
  let bas = Document2.get_element_by_id document2 "bas" in
  Element2.set_attribute bas "disabled" str;

  (*Boutons horizontaux*)
  let gauche = Document2.get_element_by_id document2 "gauche" in
  Element2.set_attribute gauche "disabled" str;
  let droite = Document2.get_element_by_id document2 "droite" in
  Element2.set_attribute droite "disabled" str
*)

let add_state () =
  let text = Document.get_element_by_id document "text" in
  let boats = Element.value text in
  let list_boats = String.split_on_char '\n' boats in
  etat_jeu.port <- List.rev (add_boat list_boats []);
  etat_jeu.cur_boat <- List.find (fun boat -> (boat.identifiant = 'A')) etat_jeu.port;
  etat_jeu.port_init <- List.rev (add_boat list_boats []);
  refresh ();
  (*disable_button false;*)

  (*Empecher une autre soumission*)
  let sub = Document.get_element_by_id document "submit" in
  Element.set_attribute sub "disabled" ""

let solver () =
  let solution = solve_state etat_jeu.port_init in
  let soluce = Document.get_element_by_id document "solution" in
  Element.set_text_content soluce solution;
  let i = ref 0 in
  while !i<=(String.length solution -2) do
    (*Find n'arrive pas à trouver le bon bateau *)
    let c = solution.[!i] in
    let boat = List.find(fun boat -> (boat.identifiant = c)) etat_jeu.port in
    etat_jeu.cur_boat <- boat;
    match solution.[!i+1] with
    |'>' -> click_avance ()
    | _  -> click_recule ();
    i := !i+2
  done

let main () =

  (*Obligation de création de plusieurs methodes car impossible de passer
    des valeurs en argument dans le listener*)

  let solve = Document.get_element_by_id document "solve" in
  Element.set_onclick solve solver;

  let submit = Document.get_element_by_id document "submit" in
  Element.set_onclick submit add_state;

  (*Initialisation des boutons de deplacements. Tant que le state n'a pas été soumis -> bouton à disable*)

  (*Boutons verticaux*)
  let haut = Document.get_element_by_id document "haut" in
  Element.set_onclick haut click_recule;
  let bas = Document.get_element_by_id document "bas" in
  Element.set_onclick bas click_avance;

  (*Boutons horizontaux*)
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

  draw_grid context

let _ = main ()
