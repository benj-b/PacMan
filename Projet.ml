(* Binome : Benjamin Bernaud & Raphael Granger *)

(*** -----Labyrinthe----- **)

(* EDIT: nous avons voulu implémenter un systeme de son, avant de se rendre compte que celui ci ne marchait pas. *)

open Graphics;;
open Unix;;

Random.self_init();; (* Init du random *)

module UF = struct

  type t = {
    mutable parent: int array;
    mutable rank : int array;
  }

  let create n = {parent = Array.init n (fun i -> i); rank = Array.init n (fun i -> 0)}

  let rec find uf i =
    let pn = uf.parent.(i) in
      if pn = i then i
      else begin
        let cn = find uf pn in
          uf.parent.(i) <- cn;
          cn
      end

  let union uf i j =
    let x = find uf i in
    let y = find uf j in
    if x = y then () else
    if uf.rank.(x) > uf.rank.(y) then uf.parent.(x) <- y
    else uf.parent.(x) <- y; 
    if uf.rank.(x) = uf.rank.(y) then uf.rank.(x) <- uf.rank.(y) + 1
;;
end

let cases_adjacentes l h (d,x,y) = match d, x, y with 
(* on a d = 0 si vertical, d = 1 si horizontal*)
  | 0, x, y when (x < l && y < h) -> (y*l + x , y*l + x + 1)
  | 1, x, y when (x < l && y < h) -> (y*l + x , (y + 1)*l + x)
  | 0, x, y when (x > l || y > h) -> invalid_arg "Erreur : Coordonnées en dehors !"
  | 1, x, y when (x > l || y > h) -> invalid_arg "Erreur : Coordonnées en dehors !"
  | _, _, _ -> invalid_arg "Erreur (d != 0/1)";;

let mur_au_hasard l h = (* renvoie un tableau de triplets (d, x, y) et un tableau de liste avec les cases accessibles *)
  let n = Random.int ((l-1) * h + l * (h-1)) in
  if n < (l-1) * h
  then (0, n mod (l-1), n / (l-1))
  else let n2 = n - (l-1) * h in
  (1, n2 mod l, n2 / l);;
  let generate_lab l h =
  let mur_present = Array.init 2 (fun _ -> (Array.init l (fun _ -> (Array.init h (fun _-> true))))) in
  let voisine = Array.init (l*h) (fun _ -> [] ) in
  let uf = UF.create (l*h) in
  let i = ref 1 in
  while (!i <= l*h-1) do
    let m = mur_au_hasard l h in
    let (k,j) = cases_adjacentes l h m in
    if UF.find uf k <> UF.find uf j then
    begin
    UF.union uf k j;
    let (d,x,y) = m in
    mur_present.(d).(x).(y) <- false;
    voisine.(k) <- (j :: voisine.(k));
    voisine.(j) <- (k :: voisine.(j));
    i := !i + 1 ;
    end
  done;
  (mur_present, voisine);;

let trace_pourtour upleftx uplefty taille_case l h = (* trace le cadre du labyrinthe *)
  Graphics.moveto upleftx uplefty;
  Graphics.lineto (upleftx + l * taille_case) (uplefty);
  Graphics.lineto (upleftx + l * taille_case) (uplefty + taille_case * (1-h));
  Graphics.moveto (upleftx + l * taille_case) (uplefty - h * taille_case);
  Graphics.lineto (upleftx) (uplefty - h * taille_case);
  Graphics.lineto (upleftx) (uplefty - taille_case);;
  
let trace_mur upleftx uplefty taille_case (d,x,y)=
  let x,y = upleftx + (x+1) * taille_case, uplefty - y * taille_case in
    match d, x, y with
      | 0, x, y -> (* murs verticaux *)
      Graphics.moveto x y; Graphics.lineto x (y-taille_case);
      | 1, x, y -> (* murs horizontaux *)
      Graphics.moveto x (y-taille_case); Graphics.lineto (x-taille_case) (y-taille_case);
      | _, _, _ -> invalid_arg "Erreur";;
    
let trace_lab upleftx uplefty taille_case l h mur_present =
  Printf.printf"%d %d %d\n" upleftx uplefty taille_case;
  Graphics.set_color blue;
  Graphics.set_line_width 3;
  trace_pourtour upleftx uplefty taille_case l h;
  mur_present.(0).(l-1).(h-1) <- false;
  for d=0 to 1 do 
    for x=0 to l-1 do
      for y=0 to h-1 do
        if (mur_present.(d).(x).(y)) then trace_mur upleftx uplefty taille_case (d,x,y)
      done
    done
  done;;

(* -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_- *)
(* _-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_ *)
(* -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_- *)

    (* INITIALISATION DES VARIABLES *)

Graphics.open_graph " 1920x1080";; (* ne pas changer la résolution, nous n'avons pas eu le temps d'automatiser l'affichage... *)
Graphics.set_window_title "Pacman Remastered 2020";;

let width = Graphics.size_x ();;
let height = Graphics.size_y ();;

let l = 10;; (* largeur *)
let h = 10;; (* hauteur *)
let taille_case = min ((width-100)/l) ((height-200)/h) ;; (* Calcule la taille d'une case en fonction de la taille de la fenetre *)
let rayon = taille_case /3;; (* rayon des cercles du pacman et du fantome *)
let case_pacman = ref 0;; (* en haut à gauche *)
let case_fantome = ref (l-1);; (* en haut à droite *)

let redo = ref true;; (* Savoir si on joue ou pas *)

Graphics.set_color black;;
Graphics.fill_rect 0 0 1920 1080;; (* fond noir *)
let upleftx = Graphics.size_x()/10;; (* Coordonnees de depart du lab *)
let uplefty = 9*Graphics.size_y()/10;;
let mur_present, voisines = generate_lab l h;;

let larg_button = 250;; (* Taille des boutons *)
let haut_button = 50;;

let pos_x n = upleftx + taille_case/2 + (n mod l) * taille_case;;
let pos_y n = uplefty - taille_case/2 - (n / l)* taille_case;;
(* renvoient les coordonées du centre de la case n*)

let gray = Graphics.rgb 140 140 140;;
Graphics.set_font "-*-helvetica-medium-r-*-*-24-*";;

let time = ref 0;;(* Variable du timer *)

(* -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_- *)
(* _-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_ *)
(* -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_- *)


let gradient arr w h = (* Matrice correspondant a un degrade noir / blanc en rgb *)
  for y = 0 to h-1 do 
      for x = 0 to w-1 do 
          let s = 255 * (x+y) / (w+h-2) 
          in arr.(y).(x) <- rgb s s s 
      done 
  done

let draw_gradient x y w h = (* Dessiner l'image correspondant a la matrice *)
  let arr = Array.make_matrix h w white
  in 
    gradient arr w h;
    draw_image (make_image arr) 0 0

let rec choix_menu() = (* Verifier que l'on clique sur les boutons *)
  let attend = wait_next_event [Button_down] in
  let abscisse = attend.mouse_x in 
  let ordonnee = attend.mouse_y in
  match (abscisse, ordonnee) with
    | (abs,ord) when ((abs<630) && (abs>450) && (ord<460) && (ord>370)) (* bouton jouer *)
      -> redo := true
    | (abs,ord) when ((abs<1005) && (abs>900) && (ord<575) && (ord>370)) (* bouton quitter *)
      -> Graphics.close_graph()
    | _ -> choix_menu()

let menu() =  (* Menu de depart *)
  Graphics.clear_graph ();
  draw_gradient 0 0 width height;
  Graphics.set_color blue;
  Graphics.fill_rect 230 40 1040 720;
  Graphics.set_color gray;
  Graphics.fill_rect 250 50 1000 700;
  Graphics.moveto 600 600;
  Graphics.set_text_size 500;
  Graphics.set_color black;
  Graphics.set_line_width 5;
  Graphics.draw_string "PACMAN REMASTERED 2020";
  Graphics.moveto 590 595;
  Graphics.lineto 950 595; (* On souligne le titre*)

  Graphics.fill_rect 450 370 180 90; (* Bouton JOUER *)
  Graphics.set_color (Graphics.rgb 100 100 100);
  Graphics.fill_rect 455 375 170 80;
  Graphics.set_color black;
  Graphics.moveto 500 400;
  Graphics.draw_string "JOUER";

  Graphics.fill_rect 900 370 205 90; (* Bouton QUITTER  *)
  Graphics.set_color (Graphics.rgb 100 100 100);
  Graphics.fill_rect 905 375 195 80;
  Graphics.set_color (Graphics.rgb 128 0 0);
  Graphics.moveto 950 400;
  Graphics.draw_string "QUITTER";
  
  choix_menu();;


let afficheTimer () = (* Affiche le timer en bas a droite de l'ecran *)
  Graphics.set_color black;
  Graphics.fill_rect (5*width/6-50) (height/4-50) 250 150;
  Graphics.set_color white; 
  Graphics.moveto (5*width/6) (height/4);
  Graphics.draw_string ("Timer : " ^ (string_of_int !time));;

let majTimer () = (* Incrementer le timer toutes les secondes / dans un thread different *)
  while ((!case_fantome <> !case_pacman) && (!case_pacman <>((l * h)-1))) do 
    Unix.sleep 1;
    time := !time + 1;
    afficheTimer();
  done;;

let rec verif_souris () = (* Verification du clic sur le bouton quitter a la fin  *)
  let attend = wait_next_event [Button_down] in
  let abscisse = attend.mouse_x in 
  let ordonnee = attend.mouse_y in
  match (abscisse, ordonnee) with
    | (abs,ord) when (abs<(3*width/4+(larg_button/2)) && (abs>3*width/4-(larg_button/2)) && (ord<height/4+haut_button) && (ord>height/4)) (* bouton quitter *)
     -> Graphics.close_graph()
    | _ -> verif_souris()

let boutons_fin () = (* Bouton de fin de partie *)
  Graphics.set_color white;
  Graphics.fill_rect (3*width/4-(larg_button/2)-5) (height/4-5) (larg_button+10) (haut_button+10);
  Graphics.set_color (Graphics.rgb 0 153 255);
  Graphics.fill_rect (3*width/4-(larg_button/2)) (height/4) larg_button haut_button;
  Graphics.set_color black;
  Graphics.set_text_size 500;
  Graphics.moveto (3*width/4-40) (2*height/8+10);
  Graphics.draw_string "Quitter";
  Graphics.set_color (Graphics.rgb 255 51 0);
  Graphics.moveto (width/3) (height/3);
  verif_souris();; 

let fin_gagnante () =  (* Ecran de fin lorsque l'on gagne *)
  Graphics.clear_graph();
  Unix.sleepf 1.; (* il faut attendre avant de clear car sinon on affiche le timer par dessus / on met 1s pour etre sur *)
  Graphics.clear_graph ();
  draw_gradient 0 0 width height;
  Graphics.set_color white;
  Graphics.set_text_size 500;
  Graphics.moveto ((width/2)-50) (3*height/4);
  Graphics.draw_string "Trop fort !";
  Graphics.sound 440 200;
  Graphics.set_color yellow;
  Graphics.moveto (width/2-150) (height/2);
  Graphics.draw_string ("Vous avez fini le labyrinthe en " ^ (string_of_int !time) ^ "s !");
  boutons_fin();;

let fin_perdante () = (* Ecran de fin lorsque l'on perd *)
  Graphics.clear_graph();
  Unix.sleepf 1.; 
  Graphics.clear_graph ();
  draw_gradient 0 0 width height;
  Graphics.set_color white;
  Graphics.set_text_size 500;
  Graphics.moveto ((width/2)-50) (3*height/4);
  Graphics.draw_string "Perdu !";
  Graphics.sound 200 100;
  Unix.sleepf 0.1;
  Graphics.sound 200 500;
  Graphics.set_color yellow;
  Graphics.moveto (width/2-125) (height/2);
  Graphics.draw_string ("Vous avez perdu en " ^ (string_of_int !time) ^ "s !");
  boutons_fin();;


(** ----- Pacman ----- **)


let autorisation l h mur_present d case = match d with (* on regarde si le mouvement est possible *)
  | 'q' -> if ( case mod l == 0 || mur_present.(0).((case-1)mod l).((case-1)/l)= true) then false else true
  | 'd' -> if ((case+1) mod l == 0 || mur_present.(0).(case mod l).(case/l) = true) then false else true
  | 's' ->  if (case + l >= l * h || mur_present.(1).(case mod l).(case/l) = true) then false else true
  | 'z' -> if (case - l < 0 || mur_present.(1).(case mod l).((case/l)-1) = true) then false else true
  | _ -> false;;


(* dessiner le pacman *)
let boucle_pacman l h rayon mur_present =
  Graphics.set_color yellow;
  Graphics.fill_circle (pos_x !case_pacman) (pos_y !case_pacman) rayon;
  while !case_pacman <> !case_fantome && !case_pacman <> !case_fantome do
    let old_pos_x = pos_x !case_pacman in
    let old_pos_y = pos_y !case_pacman in (* mémorise la derniere position du pacman *)
    
    let key = Graphics.read_key () in
    Graphics.set_color black;
    match key with (* efface la position précédente et réaffiche la nouvelle *)
      
      | 's' when autorisation l h mur_present 's' !case_pacman -> (* descendre *)
      case_pacman := !case_pacman + l ; Graphics.fill_circle (old_pos_x) (old_pos_y) rayon; Graphics.set_color yellow; Graphics.fill_circle (pos_x !case_pacman) (pos_y !case_pacman) rayon;

      | 'z' when autorisation l h mur_present 'z' !case_pacman -> (* monter *)
      case_pacman := !case_pacman - l; Graphics.fill_circle (old_pos_x) (old_pos_y) rayon; Graphics.set_color yellow; Graphics.fill_circle (pos_x !case_pacman) (pos_y !case_pacman) rayon;

      | 'd' when  autorisation l h mur_present 'd' !case_pacman -> (* droite *)
      case_pacman := !case_pacman + 1 ; Graphics.fill_circle (old_pos_x) (old_pos_y) rayon; Graphics.set_color yellow; Graphics.fill_circle (pos_x !case_pacman) (pos_y !case_pacman) rayon;

      | 'q' when autorisation l h mur_present 'q' !case_pacman -> (* gauche *)
      case_pacman := !case_pacman - 1; Graphics.fill_circle (old_pos_x) (old_pos_y) rayon; Graphics.set_color yellow; Graphics.fill_circle (pos_x !case_pacman) (pos_y !case_pacman) rayon;

      | 'k' -> Graphics.close_graph(); (* permet de quitter le jeu *)

      | _ -> case_pacman := !case_pacman; Graphics.sound 50 500; (* tape un mur *)

      if !case_pacman = ((l * h)-1) then fin_gagnante () (* Arrive sur la dernière case *)
      else if !case_fantome = !case_pacman then fin_perdante (); (* pacman se fait manger *)
  done;;

(* -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_- *)

(* ----- Fantome ----- *)

let rec est_relie src dst evite voisines =  (* Savoir quelles cases sont reliees a pacman *)
  if src = dst then true
  else let rec aux l = match l with 
  | [] -> false 
  | hd :: tl -> if hd = evite then aux tl else (aux tl || est_relie hd dst src voisines)
  in aux voisines.(src);;

let prochaine_case casef casep tab voisins = (* Prochaine case du fantome *)
  let rec aux liste = match liste with
  | [] -> casef
  | hd :: tl -> if est_relie hd casep casef voisins then hd else aux tl
  in aux tab;;

let boucle_fantome (l, h, rayon, voisins) = (* Deplacements du fantome *)
  Graphics.set_color red;
  Graphics.fill_circle (pos_x !case_fantome) (pos_y !case_fantome) rayon; (* depart du fantome *)
  while (!case_pacman <> (l*h)-1 && !case_pacman <> !case_fantome) do
    Unix.sleepf 1.;
    let old_pos_x_f = pos_x !case_fantome in
    let old_pos_y_f = pos_y !case_fantome in (* ancienne position *)
    case_fantome := prochaine_case !case_fantome !case_pacman voisins.(!case_fantome) voisins;
    let pos_x_f = pos_x !case_fantome in
    let pos_y_f = pos_y !case_fantome in (* changement de case *)
    Graphics.set_color black;
    Graphics.fill_circle old_pos_x_f old_pos_y_f rayon; (* efface l'ancienne position *)
    Graphics.set_color red;
    Graphics.fill_circle pos_x_f pos_y_f rayon; (*affiche la nouvelle *)
    if !case_fantome = !case_pacman then fin_perdante () ;  (* pacman se fait manger *)
  done;;

let main () = 
  if !redo then begin
    menu();
    Graphics.set_color black; (* un fond noir *)
    Graphics.fill_rect 0 0 1920 1080;
    trace_lab upleftx uplefty taille_case l h mur_present;
    let _ = Thread.create majTimer () in (* On traite l'actualisation du timer a part *)
    let _ = Thread.create boucle_fantome (l, h, rayon, voisines) in (* Les deplacements du fantome sont a part egalement *)
    boucle_pacman l h rayon mur_present;
  end;;

while !redo do  (* Lancement de la partie (non optimisé) *)
  main();
done