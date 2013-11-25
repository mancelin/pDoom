open Graphics;;
open Util_bmp;;

(* construit un Array de matrices *)
let matrix3 l n m init =
  let result = Array.make l (Array.make_matrix n m init) in
    for i=0 to l-1 do
		for j=0 to n-1 do
			result.(i) <- Array.make_matrix n m init
		done;
	done;
    result;;
		

(* construit un tableau de sous matrices de la matrice m ou les sous matrices ont pour dimension taille*taille   *)
let make_tab_sous_matrices m taille =
	let h = Array.length m.(0) in
	let t = (h/taille) in
	let t2 = t * t in
	let t_sm =  matrix3 t2 taille taille m.(0).(0) in 
	for j=0 to h-1 do
		for i=0 to h-1 do
			t_sm.(i/taille + t *(j/taille) ).(j mod taille).(i mod taille) <- m.(j).(i)
		done;
	done;
	t_sm;;
	
(* creer un Array de matrices images, de telle sorte que l'élement a l'indice "n" dans ce tableau corresponde a l'image du caractére 
	de code ascii "n"	*)
let decoupe_image arg =
	let im = open_bmp arg in
	match im with
		(image, largeur, hauteur) -> (
			let t_sm_img = make_tab_sous_matrices (dump_image image) 16 in
			let t_im = Array.map make_image t_sm_img in
			t_im;			
		);;
	

(* affiche une chaine de charactéres a partir d'un Array d'images passé en paramétre aux coordonées (x,y) *)
let string_bmp t_im s x y =
	let l = String.length s in
	for i=0 to l-1 do
		let i_c = (int_of_char (s.[i])) in
		Graphics.draw_image t_im.(i_c) (x + 16*i) y;
	done;;
	
(* affiche la chaine de charactéres "s" aux coordonnées (x,y)
	les fontes sont recupérées a partir du fichier "sfont.bmp"  *)
let draw_string s =
	let t_im = decoupe_image "sfont.bmp" in
	string_bmp t_im s 0 78;;
