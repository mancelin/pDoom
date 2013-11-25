open Graphics;;
	
(* charge l'image "filename" et rend un triplet (image, hauteur_image, largeur_image)  *)
let open_bmp filename =
  let ic = open_in filename in
  let b = input_byte ic in
	if ( b = 0x42 ) then
		if ( (input_byte ic) = 0x4d ) then (* verification magic number ok *)
			seek_in ic 18;
			let lar0 = input_byte ic in
			let lar1 = input_byte ic in
			let lar2 = input_byte ic in
			let lar3 = input_byte ic in
			let largeur = lar0 + lar1 * 256 + lar2 *256 * 256 + lar3 * 256 * 256 * 256 in
			let hau0 = input_byte ic in
			let hau1 = input_byte ic in
			let hau2 = input_byte ic in
			let hau3 = input_byte ic in
			let hauteur = hau0 + hau1 * 256 + hau2 * 256 * 256 + hau3 * 256 * 256 * 256 in
			seek_in ic 54;
			let m_couleurs = Array.make_matrix hauteur largeur (Graphics.rgb 0 0 0) in
						
			let l3 = 3 * largeur in
			let decalage = (4-(l3 mod 4)) mod 4 in
			let h = ref 0 and l = ref 0 in
			while !h < hauteur do
				while !l < largeur do
					let b = input_byte ic in
					let g = input_byte ic in
					let r = input_byte ic in
					Array.set m_couleurs.(hauteur - !h - 1) !l (Graphics.rgb r g b); (* l' image bmp est codée de gauche a droite, de bas en haut *)
					l := !l + 1;
				done;
				l := 0;
				while !l < decalage do
					let _ = input_byte ic in
					l := !l + 1;
				done;
				h := !h + 1;
				l := 0;
			done;
			let image = Graphics.make_image m_couleurs in
			(image, largeur, hauteur);;
