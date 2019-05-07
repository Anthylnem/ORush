open Dom
open Port
open Moves
open Solver


type etat = { mutable port : state;
              mutable cur_boat : boat;
              mutable solution : string}


(*///////////////////////////////////////////////////////////////////////////////////////////////////////////////
Variables Globales
////////////////////////////////////////////////////////////////////////////////////////////////////////////////*)


let init_boat = boat_of_string "A2H12"
let etat_jeu = {port = []; cur_boat = init_boat; solution=""}
let cpt = ref 0
let colors = Hashtbl.create 13


(*///////////////////////////////////////////////////////////////////////////////////////////////////////////////
Fonctions d'affichage
////////////////////////////////////////////////////////////////////////////////////////////////////////////////*)


let init_colors () =
  Hashtbl.add colors 'A' "#FF2626"; (* ROUGE *)
  Hashtbl.add colors 'B' "#26A6FF"; (* BLEU *)
  Hashtbl.add colors 'C' "#4DC136"; (* VERT *)
  Hashtbl.add colors 'D' "#C136BE"; (* VIOLET *)
  Hashtbl.add colors 'E' "#FFD109"; (* JAUNE *)
  Hashtbl.add colors 'F' "#3CEAEA"; (* CYAN *)
  Hashtbl.add colors 'G' "#E033C6"; (* ROSE *)
  Hashtbl.add colors 'H' "#FFBC12"; (* ORANGE *)
  Hashtbl.add colors 'I' "#FA8072"; (* SAUMON *)
  Hashtbl.add colors 'J' "#4B0082"; (* INDIGO *)
  Hashtbl.add colors 'K' "#006400"; (* DARKGREEN *)
  Hashtbl.add colors 'L' "#F5DEB3"; (* WHEAT *)
  Hashtbl.add colors 'M' "#8B0000"  (* DARKRED *)

(*Recuperation du context graphique du canvas*)
let get_context =
  let canvas = Document.get_element_by_id document "port" in
  Canvas.get_context_2d (Canvas.of_element canvas)


(*Dessiner un bateau suivant son orientation*)
let draw_boat context l color=
  Canvas.RenderingContext2D.set_fill_style context color;
  try
    let boat = List.find (fun boat -> (boat.identifiant = l)) etat_jeu.port in
    let isCur = if boat.identifiant = etat_jeu.cur_boat.identifiant then true else false in

      match boat.orientation with
      |'V' -> Canvas.RenderingContext2D.fill_rect context (boat.x*100) (boat.y*100) 100 (boat.longueur*100);
        Canvas.RenderingContext2D.set_fill_style context "#888899";
        if isCur then
          Canvas.RenderingContext2D.set_fill_style context "white";
        Canvas.RenderingContext2D.fill_rect context (boat.x*100+40) (boat.y*100+((boat.longueur*100)/2)-14) 20 20;
        Canvas.RenderingContext2D.set_fill_style context "white";
        if isCur then
          Canvas.RenderingContext2D.set_fill_style context "black";
        Canvas.RenderingContext2D.fillText context (Char.escaped boat.identifiant) (boat.x*100+46) (boat.y*100+((boat.longueur*100)/2))

      | _  -> Canvas.RenderingContext2D.fill_rect context (boat.x*100) (boat.y*100) (boat.longueur*100) 100;
        Canvas.RenderingContext2D.set_fill_style context "#888899";
        if isCur then
          Canvas.RenderingContext2D.set_fill_style context "white";
        Canvas.RenderingContext2D.fill_rect context (boat.x*100+((boat.longueur*100)/2)-10) (boat.y*100+36) 20 20;
        Canvas.RenderingContext2D.set_fill_style context "white";
        if isCur then
          Canvas.RenderingContext2D.set_fill_style context "black";
        Canvas.RenderingContext2D.fillText context (Char.escaped boat.identifiant) (boat.x*100+((boat.longueur*100)/2)-4) (boat.y*100+50)

  with Not_found -> ()


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

(*Encadre le bateau selectionné + affichage special *)
  let draw_rect boat=
    let context = get_context in
    Canvas.RenderingContext2D.set_stroke_style context "white";
    if boat.orientation = 'V' then
      Canvas.RenderingContext2D.stroke_rect context (boat.x*100) (boat.y*100) 100 (boat.longueur*100)
    else
      Canvas.RenderingContext2D.stroke_rect context (boat.x*100) (boat.y*100) (boat.longueur*100) 100


(*///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  Fonctions de mise à jour des données
////////////////////////////////////////////////////////////////////////////////////////////////////////////////*)


(*Desactivation des boutons du solver*)
let disable_func_solve ()=
  let solve = Document.get_element_by_id document "solve" in
  Element.set_attribute solve "disabled" "";
  let next = Document.get_element_by_id document "next" in
  Element.set_attribute next "disabled" "";
  let finish = Document.get_element_by_id document "finish" in
  Element.set_attribute finish "disabled" ""

(*Rafraichir le Canvas*)
let refresh () =
  let context = get_context in
  Canvas.RenderingContext2D.set_fill_style context "#778899";
  Canvas.RenderingContext2D.fill_rect context 0 0 600 600;
  draw_grid context;

  Hashtbl.iter (fun l c -> draw_boat context l c) colors;
  draw_rect etat_jeu.cur_boat

(*Selectionner le nouveau bateau dans le port et faire un affichage specifique*)
let click event =
  let x = (Event.offset_x event)/ 100 in
  let y = (Event.offset_y event)/ 100 in
  let mat = grid_of_state etat_jeu.port in
  let var = mat.(x).(y) in
  if var <> '~' then
    etat_jeu.cur_boat <- (List.find (fun boat -> (boat.identifiant = var)) etat_jeu.port);

  refresh();
  draw_rect etat_jeu.cur_boat

(*Verifie si l'etat du port est gagnant -> desactive tous les boutons de deplacements et affiche sur la fenetre le nombre de coups effectués*)
let is_win () =
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
    Element.set_text_content h1 ("ORush : PARTIE GAGNEE en "^(string_of_int !cpt)^" coups !");
    disable_func_solve()

(*Mise à jour des données apres avoir avancé ou reculé un Bateau*)
let maj_click ()=
  etat_jeu.cur_boat <- (List.find (fun boat -> (boat.identifiant = etat_jeu.cur_boat.identifiant)) etat_jeu.port);
  refresh ();
  cpt := !cpt +1;
  is_win ()

let click_recule () =
  etat_jeu.port <- apply_move (etat_jeu.cur_boat.identifiant,'<') etat_jeu.port;
  maj_click()

let click_avance () =
  etat_jeu.port <- apply_move (etat_jeu.cur_boat.identifiant,'>') etat_jeu.port;
  maj_click()


(*///////////////////////////////////////////////////////////////////////////////////////////////////////////////
Initialisation du port
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////*)


(*Ajout des bateaux via une liste de string*)
let rec add_boat boats state :state=
  match boats with
  | [] -> state
  | hd::tl -> add_boat tl (boat_of_string hd::state)

(*Création du port via les bateaux transmis dans la zone de texte : Une seule création sera permise*)
let add_state () =
  let text = Document.get_element_by_id document "text" in
  let boats = Element.value text in
  let list_boats = String.split_on_char '\n' boats in
  etat_jeu.port <- List.rev (add_boat list_boats []);
  etat_jeu.cur_boat <- List.find (fun boat -> (boat.identifiant = 'A')) etat_jeu.port;
  refresh ();

  (*Empecher une autre soumission*)
  let sub = Document.get_element_by_id document "submit" in
  Element.set_attribute sub "disabled" ""


(*///////////////////////////////////////////////////////////////////////////////////////////////////////////////
Solver
////////////////////////////////////////////////////////////////////////////////////////////////////////////////*)


(*Recherche de la solution à partir du state courant (possible meme en cours de partie)*)
let solver () =
  let solution = solve_state etat_jeu.port in
  let soluce = Document.get_element_by_id document "solution" in
  etat_jeu.solution <- solution;

  (*Affichage de la solution sur la fenetre*)
  let affichage = ref "" in
  for i=0 to String.length solution-1 do
    affichage := !affichage^(Char.escaped solution.[i]);
    if (i mod 15) = 0 && i<> 0 then
      affichage := !affichage^"\n";
  done;
  Element.set_text_content soluce !affichage

(*application du mouvement suivant de la solution générée*)
let next_move () =
  let solution = etat_jeu.solution in
  let size = String.length solution in

  if size >= 2 then
    let move = String.sub solution 0 2 in
    etat_jeu.solution <- String.sub solution 2 (size-2);
    etat_jeu.cur_boat <- List.find(fun boat -> (boat.identifiant = move.[0])) etat_jeu.port;
    (match move.[1] with
    |'>' -> click_avance ()
    | _  -> click_recule ());
    if String.length etat_jeu.solution = 0 then
      disable_func_solve()

(*Application de l'intégralité de la solution (en un coup)*)
let finisher () =
  if String.length etat_jeu.solution <> 0 then
    let solution = etat_jeu.solution in
    etat_jeu.solution <- solution;
    let i = ref 0 in
      while !i<=(String.length solution -2) do
      etat_jeu.cur_boat <- List.find(fun boat -> (boat.identifiant = solution.[!i])) etat_jeu.port;
      (match solution.[!i+1] with
      |'>' -> click_avance ()
      | _  -> click_recule ());
      i := !i+2
    done;
      disable_func_solve()


(*///////////////////////////////////////////////////////////////////////////////////////////////////////////////
Main
////////////////////////////////////////////////////////////////////////////////////////////////////////////////*)


(*Initialisation des listeners sur les differents elements de la fenetre*)
let main () =
  init_colors ();
  (*Obligation de création de plusieurs methodes car impossible de passer
    des valeurs en argument dans le listener*)

  (*Boutons du solver*)
  let solve = Document.get_element_by_id document "solve" in
  Element.set_onclick solve solver;
  let next = Document.get_element_by_id document "next" in
  Element.set_onclick next next_move;
  let finish = Document.get_element_by_id document "finish" in
  Element.set_onclick finish finisher;
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

  (*Couleur de fond*)
  let context = get_context in
  Canvas.RenderingContext2D.set_fill_style context "#778899";
  Canvas.RenderingContext2D.fill_rect context 0 0 600 600;
  (*Affichage de la grille*)
  draw_grid context

let _ = main ()
