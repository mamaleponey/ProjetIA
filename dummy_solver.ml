(*---------- ouverture des modules ----------*)

open Modules ;;  
open Random  ;;  
Random.self_init ;;        

let n_pop = 25 and
n_gen = 200 and
cr = 40 and 
f = 0.6;;     

(*---------- fonctions auxiliaires ----------*)
 
let deep_copy_pop = fun pop ->
	let taille_pop = Array.length pop in
	let taille_individu = Array.length pop.(0) in
	let copy = Array.make_matrix taille_pop taille_individu pop.(0).(0) in
	for k = 0 to taille_pop-1 do
		for i = 0 to taille_individu-1 do
			copy.(k).(i)<- pop.(k).(i)
		done;
	done;
	copy

let to_destination pln f =
  Geo.V2D.make (F.position f) (P.arr pln)


let signe = fun x -> if x < 0. then - 1. else 1.;;

let make_pop = fun taille_pop taille_individu ->
	let pop = Array.make_matrix taille_pop taille_individu 0.0 in
	for k=0 to taille_pop-1 do
		for i=0 to taille_individu-1 do
			let aux = Random.float 6.28 in 
			pop.(k).(i) <- aux;
		done;
	done;
	pop
	
let to_array liste =
	let n = List.length liste in  
	let out = Array.make n (List.hd liste) in 
	let rec aux i l  =  
		match l with 
		|[] -> out
		|hd::tl -> out.(i)<- hd; aux (i+1) tl in 
	aux 0 liste

(*---------- évolution différentielle ----------*)
    
let one_step_evol_diff = fun varmax dirvect pop fobj cr f dirobj f_array ->
	(* fait une génération d'évolution différentielle *)
	let nb_individu = Array.length pop in
  let taille_individu = Array.length pop.(0) in
  let chosen = Array.make 3 0 in
  for k=0 to (nb_individu-1) do
		let trial = Array.make taille_individu 0.0 in
		chosen.(0)<-Random.int (nb_individu-1);
    chosen.(1)<-Random.int (nb_individu-1);
    chosen.(2)<-Random.int (nb_individu-1);
      
		while chosen.(0)=k do 
			chosen.(0)<- Random.int (nb_individu-1);	
	  done;
	   
    while chosen.(1)=chosen.(0) || chosen.(1)=k do 
			chosen.(1)<-Random.int (nb_individu-1);
	  done;
	   
	  while chosen.(2) = chosen.(0) || chosen.(2) = chosen.(1) || chosen.(2) = k do
			chosen.(2) <- Random.int (nb_individu-1)
		done;
	   
    let a = chosen.(0) in 
	  let b = chosen.(1) in 
	  let c = chosen.(2) in
    for i=0 to (taille_individu-1) do
			if (Random.int 100)<cr then
				let change = (pop.(a).(i) +. f*.(pop.(b).(i)-.pop.(c).(i)) -. dirvect.(i)) in
				if abs_float(change) > varmax 
					then 
						trial.(i) <- dirvect.(i) +. varmax*.signe(change)
					else  
						trial.(i) <- pop.(a).(i) +. f*.(pop.(b).(i)-.pop.(c).(i))
         	else
            trial.(i) <- pop.(k).(i)
    done;
    
    if (fobj trial dirobj f_array)<(fobj pop.(k) dirobj f_array) then pop.(k) <- trial
    
    done;
	pop
    
let evol_diff = fun varmax dirvect pop gen_max fobj cr f dirobj f_array ->
	(* lance l'évolution différentielle sur gen_max générations *)
  let aux_pop = ref (deep_copy_pop pop) in
  for k=0 to gen_max do
		aux_pop := one_step_evol_diff varmax dirvect (!aux_pop) fobj cr f dirobj f_array;
  done;
  !aux_pop
    
let best_individu = fun pop fobj dirobj_vect f_array->
	(* renvoie l'indice du meilleur individu de la population *)
	let nb_individu = Array.length pop in 
	let min = ref (fobj pop.(0) dirobj_vect f_array) in
	let indice_min = ref 0 in 
	for k=1 to nb_individu-1 do 
		let aux = (fobj pop.(k) dirobj_vect f_array) in
		if aux <(!min) then 
			begin min := aux; indice_min := k end
	done;
	!indice_min
    
(*---------- récupération information de vol ----------*)
	
let gather_dir= fun flying ->
	(* récupère les directions des avions *) 
	let n = List.length flying in 
	let dir_vect = Array.make n 0. in 
	let rec aux flying i= 
		match flying with
		|[]-> dir_vect
		|f::tl-> dir_vect.(i)<-F.vdir f;
			 aux tl (i+1) in 
	aux flying 0
	
let gather_pos= fun flying ->
	(* récupère les positions des avions *) 
	let n = List.length flying in 
	let pos_vect = Array.make n {Geo.P2D.x=0.;y=0.} in
	let rec aux flying i= 
		match flying with
		|[]-> pos_vect
		|f::tl-> pos_vect.(i)<-F.position f;
			 aux tl (i+1) in 
	aux flying 0

let gather_norm= fun flying ->
	(* récupère les normes des avions *) 
	let n = List.length flying in 
	let norm_vect = Array.make n 0. in
	let rec aux flying i= 
		match flying with
		|[]-> norm_vect
		|f::tl-> norm_vect.(i)<-F.vnorm f;
			 aux tl (i+1) in 
	aux flying 0
	
let gather_dirobj= fun flying plns ->
	(* récupère les directions objectif des avions *) 
	let n = List.length flying in 
	let dirobj_vect = Array.make n 0. in
	let rec aux flying i= 
		match flying with
		|[]-> dirobj_vect
		|f::tl-> let pln= P.find (F.id f) plns in
         let v_to_dest= to_destination pln f in dirobj_vect.(i)<-(Geo.V2D.direction v_to_dest);
			 aux tl (i+1) in 
	aux flying 0
	
(*---------- détection de conflits ----------*)
	
(* on fait appel à conflit pour chaque individu de la génération considérée, renvoie le nombre de conflits futurs en conservant les trajectoire données *)
			
let conflicts = fun flights_array dmin nb_iter -> 				(* flights_array est un tableau contenant les vols;   nb_iter est le nombre de pas de temps  *)
	let nb_conflicts = ref 0. and flights_number = Array.length flights_array in 				(* flights_number = nombre de vols dans la simulation lors de l'appel à conflicts *)
	if flights_number < 2 then 0.
	else ( 
		for i=0 to (flights_number-2) do
			let vect_nul =  {Geo.P2D.x=0.;y=0.} in 				(* creation du vecteur nul de P2D pour la fonction make *)
			for j=(i+1) to (flights_number-1) do
				let flight_i = flights_array.(i) and flight_j = flights_array.(j) in
				let pos_i = ref (Geo.V2D.make vect_nul (F.position flight_i)) and pos_j = ref (Geo.V2D.make vect_nul (F.position flight_j)) in (* conversion de la position en vecteur V2D *)
				let velocity_i = F.velocity flight_i and velocity_j = F.velocity flight_j in  (* ligne précédente pour compatibilité avec le vecteur vitesse *)
				let t=ref 1 and no_conflict = ref true in
				while !t<=nb_iter && !no_conflict do 
					let dx = !pos_i.x -. !pos_j.x and dy = !pos_i.y -. !pos_j.y in
					let dist = sqrt (dx*.dx  +. dy*.dy) in  (* calcul de la distance séparant les avions à un temps donné *)
					if dist > 2. *. (float_of_int nb_iter) *. (max (Geo.V2D.norm velocity_i) (Geo.V2D.norm velocity_j)) then no_conflict := false (* si les avions
					 sont trop éloignés, on arrête immédiatement la recherche de conflits *)
					else (
						if dist <= dmin then (                  (* en cas de conflit, on interrompt la recherche et on passe au(x) vol(s) suivant(s) *)
							nb_conflicts := !nb_conflicts +. (1. /. (float_of_int !t)); 
							no_conflict := false;
						)
						else ( 
							t:=!t+1;
							pos_i := Geo.V2D.(+) !pos_i velocity_i;  (* déplacement de la position *)
							pos_j := Geo.V2D.(+) !pos_j velocity_j;
						)
					)
					done;
			done;
		done;
		!nb_conflicts
	)

(*---------- fonctions objectif ----------*)

let fobj = fun trial dirobj f_array ->
	(*fonction objectif*) 
	let n = Array.length dirobj in 
	let dmin= 2. *. Util.nm2meter in
	let ecart = ref 0. in 
	for i=0 to (n-1) do 
		ecart := !ecart +. (abs_float (trial.(i) -. dirobj.(i)))**2.
	done; 
	let n_conflits = (conflicts f_array dmin 50) in 
	(* ancien
	if n_conflits >0 then 
		max_float
	else 
		!ecart
	*)
	(n_conflits,!ecart)
(*---------- découpage en 4 zones ----------*)

let fl_in_z1 f = 
	let pos = F.position f in 
	(pos.x>300. && pos.y>300.)
	
let fl_in_z2 f = 
	let pos = F.position f in 
	(pos.x<=300. && pos.y<=300.)
	
let fl_in_z3 f = 
	let pos = F.position f in 
	(pos.x>300. && pos.y<=300.)
	
let fl_in_z4 f = 
	let pos = F.position f in 
	(pos.x<=300. && pos.y>300.)
	
(*---------- calcul des nouveaux vecteurs ----------*)

let compute_velocities = fun max_turn_angle dmin plns flying ->
  
  let n = List.length flying in 
	let norm_vect = gather_norm flying in  
	if n>0 then
  	let split_zones flying = 
  		let rec aux z1 z2 z3 z4 fl=
  		match fl with
			|[]-> z1,z2,z3,z4
			|f::tl-> if fl_in_z1 f then aux (f::z1) z2 z3 z4 tl 
							else 
								if fl_in_z2 f then aux z1 (f::z2) z3 z4 tl
								else 
									if fl_in_z3 f then aux z1 z2 (f::z3) z4 tl 
									else 
										aux z1 z2 z3 (f::z4) tl
								
			in aux [] [] [] [] flying
			in
		
		
		let z1,z2,z3,z4 = split_zones flying in
		let n1 = List.length z1 in
		let n2 = List.length z2 in 
		let n3 = List.length z3 in
		let n4 = List.length z4 in
		
		let compute z n= 
			if n>0 then 
				let dirobj_vect = gather_dirobj z plns in (* directions objectif mises à jour des avions *)
				let dir_vect = gather_dir z in (* directions actuelles *)
				let f_array = to_array z in (* tableau des vols *)
				let pop = make_pop n_pop n in (* population aléatoire *)
				let out = evol_diff max_turn_angle dir_vect pop n_gen fobj cr f dirobj_vect f_array in 
				let best = best_individu out fobj dirobj_vect f_array in 
				out,best;
				
			else [||],0  in 
			
		let new_v1,best1 = compute z1 n1 in
		let new_v2,best2 = compute z2 n2 in
		let new_v3,best3 = compute z3 n3 in
		let new_v4,best4 = compute z4 n4 in
		 
  	let rec loop = fun l acc i res best ->
  	match l with 
  	| [] -> acc
  	| f:: tl -> 
  		try 
  			let new_dir = res.(best).(i) in 
  			let new_norm = norm_vect.(i) in 
  			let new_acc= Util.IntMap.add (F.id f) (new_dir, new_norm) acc in (* mise à jour du vecteur direction de l'avion f *)
  			loop tl new_acc (i+1) res best
  		with Not_found -> failwith "compute_velocities" in
  	let acc1 = loop z1 Util.IntMap.empty 0 new_v1 best1 in
		let acc2 = loop z2 acc1 0 new_v2 best2 in
		let acc3 = loop z3 acc2 0 new_v3 best3 in
		loop z4 acc3 0 new_v4 best4;
		
	else 
		Util.IntMap.empty
		
		

  

