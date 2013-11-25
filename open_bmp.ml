open Graphics;;
open Util_bmp;;
	
		
(* affiche une image, a partir d'un triplet (image, hauteur_image, largeur_image) passé en paramétre, aux coordonées (x,y)  *)
let afficher_image triplet_im x y=
	match triplet_im with
		(image, largeur, hauteur) -> (
			Graphics.draw_image image x y;
		);;


(* affiche l'image "arg" (nom de fichier bmp) aux coordonnées (x,y) *)
let a_image arg x y=
	let im_t = open_bmp arg in
	afficher_image im_t x y;;
