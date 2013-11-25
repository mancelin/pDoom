open Open_bmp;;
open Fonte_bmp;;
open Graphics;;

type scene = {
	title : string;
	code : (unit -> unit) array;
};;

(* list_links contient l' ensemble des zones cliquables et le titre de la zone vers lequel les liens pointent *)
let list_links = ref [];; (* ( [| xDeb ; yDeb ; xFin ; yFin |] , "titre")  *)

(* list_items contient l' ensemble des objets rammasés par le joueur *)
let list_items = ref [""];;	(* shotgun, blue_keycard *)

(* santé du personage *)
let life = ref 100;;

(* est a true si les monstres sont toujours vivants *)
let monsters_alive = ref true;;

(* indique les directions ou le joueur peur aller *)
let make_triangle x1 y1 x2 y2 x3 y3 =
	set_color green;
	set_line_width 2;
	draw_segments [| (x1,y1,x2,y2) ; (x2,y2,x3,y3) ; (x3,y3,x1,y1) |];;

let interaction = ref "main";;

let clear () =
	set_color black;
	fill_rect 0 0 (size_x ()) (size_y ());
(*	Printf.printf "clear %!\n";	*)
	list_links := [];;
	
(* affiche une image correspondant a une zone de jeu *)
let image bmp () =
(*	Printf.printf "a_image %s %!\n" bmp;	*)
	a_image bmp 0 94;;

(* vide la zone texte de l' ecran *)
let clear_msg () =
	set_color black;
	fill_rect 0 0 (size_x ()) 16;;
	
let message msg () =
	clear_msg ();
(*	Printf.printf "message %s %!\n" msg;	*)
	Fonte_bmp.draw_string msg;;
	
let barrer_texte xDeb xFin () =
	set_color red;
	set_line_width 5;
	draw_segments [| (xDeb,78+17, xFin, 78+0) ; (xDeb, 78+0, xFin,78+17) |];;
	
(*	ajoute l'objet "item" a l'inventaire *)
let grab_item item () =
(*	Printf.printf "grab_item : %s %!\n" item; *)
	list_items := item :: !list_items;;

(*	supprime l'objet "item" a l'inventaire *)	
let drop_item item () =
	let f v = if v = item then false else true in
	list_items := List.filter f !list_items;;

(* affiche vie, visage, arme et clé *)
let hud () =
	set_color (rgb 45 45 45);	
	fill_rect 0 0 (size_x ()) 78;
	let vie_p = Printf.sprintf "pDooM/hud/h%d.bmp" !life and
		vie_f = Printf.sprintf "pDooM/hud/f%d.bmp" !life in
	let img_arme = ref "" and img_key = ref "" in
	if (List.mem "shotgun" !list_items) then
		img_arme := "pDooM/hud/shotgun.bmp" 
	else img_arme := "pDooM/hud/no_shotgun.bmp";
	if (List.mem "blue_keycard" !list_items) then
		img_key := "pDooM/hud/key.bmp" 
	else img_key := "pDooM/hud/no_key.bmp";
	a_image vie_p 28 0;
	a_image vie_f 145 0;
	a_image !img_arme 217 0;
	a_image !img_key 346 0;;
	
let	sleep t () =
	let t0 = Unix.gettimeofday () in
(*	Printf.printf "sleep %f %!\n" t; *)
	while Unix.gettimeofday () -. t0 < t do
		try Thread.delay 0.01 with _ -> ()
	done;;

(*

type rect = {xDeb:int ; yDeb:int ; xFin:int ; yFin:int};;
*)
type rect = Rect of int * int * int * int;;

let putlink (Rect (xDeb,yDeb,xFin,yFin)) str_title () =
(*
	(* pour debuguer *)
	set_line_width 2;
	set_color red;
	draw_rect xDeb yDeb (xFin - xDeb) (yFin - yDeb);	
*)	
	list_links := ( [|xDeb;yDeb;xFin;yFin|] , str_title ) :: !list_links;;

let require liste_objets code_sans_objet code_avec_objet () =
	let res = ref false in
	let rec aux liste_objets =
		match liste_objets with
		| t::[] -> res := (List.mem t !list_items)
		| t::q	-> if (List.mem t !list_items) then aux q else res := false
		| _ -> ()
	in aux liste_objets;
	let ff f = f () in
	if !res = true then Array.iter ff code_avec_objet else Array.iter ff code_sans_objet;;

(*
let require liste_objets code_sans_objet code_avec_objet () =
	
	let rec aux liste_objets =
		match liste_objets with
		| t::[] ->  (List.mem t !list_items)
		| t::q	-> if (List.mem t !list_items) then aux q else false
	in if (aux liste_objets) then code_avec_objet else code_sans_objet;;
	*)
	

let zone_click x y =
	let zone = ref "none" in
	let l = !list_links in
	let rec aux l =
	match l with
		| t::q -> (	let e_t = fst t and e_s = snd t in
					if x >= e_t.(0) && x <= e_t.(2) && y >= e_t.(1) && y <= e_t.(3) then (* x y est compris dans la zone cliquable e_t de nom e_s *)
						(zone := e_s; !zone)
					else (aux q)
					)
		| [] -> !zone
	in aux l;;

(* attent un click, et met a jour "interaction" en fonction de la zone cliquée *)
let interact () =
	let _ = wait_next_event [Button_down] in
	let m_pos = mouse_pos () in
	(* debug
	Printf.printf "(x = %d, y = %d)\n" (fst m_pos) (snd m_pos);*)
	let z_c = (zone_click (fst m_pos) (snd m_pos)) in
	(* debug
	Printf.printf "z_c : %s %!\n" z_c;*)
	if z_c <> "none" then interaction := z_c else interaction := "none"	;;
	
(* affichage des fleches pour naviguer entre les zones du niveau *)
let fleche_go_back () =
	make_triangle 555 128 565 108 575 128;;
	
let fleche_gauche () =
	make_triangle 80 238 100 248 100 228;;
	
let fleche_droite () =
	make_triangle 550 238 530 248 530 228;;	
	
let fleche_haut () =
	make_triangle 305 290 315 303 325 290;;	
	
	
(* fait baisser la barre de vie de notre personnage *)
let degats_joueur () =
	life := !life - 25;;
	
(* pour memoriser que les monstres ont été detruits, le heros prends l' item "scalp_monstres" *)
let tuer_monstres () =
	grab_item "scalp_monstres" ();;

(* fin du jeu, on quite *)	
let quit () =
	exit 0;;

(* rend l' interaction "death" si la vie du heros est a 0 *)
let check_death () =
	if !life = 0 then (
		clear ();
		message "Game Over !" ();
		image "pDooM/salles/salle_key_dead.bmp" ();
		hud ();
		interact ();
		quit ();
		);;
	
(* definition du scenario du jeu *)	
let scenar =
	[
	{title = "main";
		code = [|
		clear;
		image "intro.bmp";
		message "Welcome to the castle of DooM !" ;

		sleep 3. ;
		barrer_texte 180 390;
		sleep 4.;	

		clear;
		image "pDooM/doom-pnc.bmp";
		putlink (Rect (0, 0, 642, 498)) "debut";
		interact |]} ;
	{title = "debut" ;
		code = [|
		clear ;
		require ["shotgun"]
			[| 	image "pDooM/salles/debut.bmp" ;
				message "J'vais crever ici sans une arme !";
				
			|]
			[| image "pDooM/salles/debut_w.bmp" |];
			hud;
			fleche_gauche;
			putlink (Rect (75,220,105,255)) "salle_shotgun" ;
			fleche_haut;
			putlink (Rect (285,275,340,320)) "exit" ;
			fleche_droite;
			putlink (Rect (525,220,555,255)) "salle_key" ;
			interact
		 |] } ;
	{title = "salle_shotgun" ;
		code = [|
		clear ;
		require ["shotgun"]
			[|	image "pDooM/salles/salle_shotgun.bmp" ;
				message "Cool! Un shotgun !";
				putlink (Rect (215,143,350,203)) "get_shotgun" ;
			|]
			[|	
				image "pDooM/salles/salle_shotgun_w.bmp" ;
				message "Je n' ai plus rien a faire ici !";
			|];
		hud;
		fleche_go_back;
		putlink (Rect (550,100,580,130)) "debut" ;
		interact |] } ;	
	{title = "get_shotgun" ;
		code = [|
		clear ;
		grab_item "shotgun";
		image "pDooM/salles/salle_shotgun_w.bmp" ;
		hud;
		fleche_go_back;
		putlink (Rect (550,100,580,130)) "debut" ;
		interact |] } ;	
	{title = "exit" ;
		code = [|
		clear ;
		require ["shotgun"]
			[|	image "pDooM/salles/exit_nw.bmp" ;
				putlink (Rect (125,133,510,458)) "no_key" |]
			[|	image "pDooM/salles/exit.bmp";
				require ["blue_keycard"]
					[| 	putlink (Rect (125,133,510,458)) "no_key" |]
					[| 	putlink (Rect (125,133,510,458)) "open_door"|] |];
		hud;
		fleche_go_back;
		putlink (Rect (550,100,580,130)) "debut" ;
		interact |] } ;
	{title = "no_key" ;
		code = [|
		message "J'ai besoin d' une cle !";
		hud;
		interact
		|]
	};
	{title = "open_door";
		code = [|
		drop_item "blue_keycard";
		image "pDooM/salles/end.bmp" ;
		message "Gagne !";
		hud;
		sleep 1.;
		message "Mais deriere cette porte ...";
		hud;
		sleep 1.5;
		message "... il y a encore plus de monstres";
		hud;
		sleep 2.;
		clear_msg;
		hud;
		interact;
		quit;
		|]
	};
	{title = "salle_key" ;
		code = [|
		clear ;
		require ["blue_keycard"]
			[|
			require ["scalp_monstres"]
				[| require ["shotgun"]
					[|	image "pDooM/salles/salle_key_nw.bmp" ;
						hud;
						sleep 0.5;
						message "Arg! J'ai aucune chance sans une arme !";
						degats_joueur;
						hud;	
						check_death	|]
					[|	image "pDooM/salles/salle_key_monster.bmp" ;
						putlink (Rect (0,0,642,498) ) "rater_monstres" ;
						putlink (Rect (180,208,335,378) ) "monstres_morts" ;
						message "Je dois degommer ces monstres !";
						hud	|]
				|]
				[|	message "Maintenant je peux prendre la cle !";
					image "pDooM/salles/salle_key.bmp" ;
					putlink (Rect (300,258,320,283) ) "get_key" ;
					hud;
				|]
			|]
			[|
				image "pDooM/salles/salle_key_nk.bmp" ;
				message "Je n' ai plus rien a faire ici !";
				fleche_go_back;
				putlink (Rect (550,100,580,130)) "debut" ;
				hud;
				interact
			|];
		fleche_go_back;
		putlink (Rect (550,100,580,130)) "debut" ;
		interact |] 
	};
	{title = "rater_monstres";
		code = [|
			degats_joueur;
			check_death;
			hud;
			sleep 1.;
			clear_msg;
			message "Je dois degommer ces monstres !";
			hud;
			interact|]
	};
	{title = "monstres_morts";
		code = [|
			clear ;
			grab_item "scalp_monstres";
			image "pDooM/salles/salle_key.bmp" ;
			message "Maintenant je peux prendre la cle !";
			hud;
			putlink (Rect (300,258,320,283) ) "get_key" ;
			fleche_go_back;
			putlink (Rect (550,100,580,130)) "debut" ;
			interact
			 |]
	};
	{title = "get_key";
		code = [|
			clear ;
			grab_item "blue_keycard";
			image "pDooM/salles/salle_key_nk.bmp" ;
			message "Je n' ai plus rien a faire ici !";
			fleche_go_back;
			putlink (Rect (550,100,580,130)) "debut" ;
			hud;
			interact
			 |]
	}
	];;

(* rend le code qui correspond au titre "titre" dans "scenario" *)
let code_from_title titre scenario =
	let rec aux l =
		match l with 
		| t::q -> if t.title = titre then t.code else aux q
		| [] -> [|quit|]
	in aux scenario;;
			
(* execution du scenario *)
let run scenario =
	open_graph " 642x498";
	while true do
		Printf.printf "interaction : %s %!\n" !interaction;
		let code = code_from_title !interaction scenar in
		let ff f = f () in
		Array.iter ff code;
		while !interaction = "none" do
			Printf.printf "interaction : %s %!\n" !interaction;
			Array.iter ff code;
		done;
	done;;
		
run scenar ;;
		
		
