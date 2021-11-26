open Random;;

Random.self_init;;



let fmin_un_terme = fun tab ->
	let m = ref 0 in 
	for i=0 to Array.length tab-1 do
		if tab.(i)< !m then 
			m := tab.(i)
	done;
	!m;;
	
let fmin_global = fun tab ->
	let m = ref 0 in
	for i = 0 to Array.length tab-1 do 
		m:= !m + tab.(i)
	done;
	!m;;
	
let fmax_global = fun tab ->
	let m = ref 0 in
	for i = 0 to Array.length tab-1 do 
		m:= !m - tab.(i)
	done;
	!m;;
	
let close_zero = fun tab ->
	let m = ref 0 in
	for i=0 to Array.length tab-1 do
		if tab.(i)<0 then m:= !m - tab.(i) else m:= !m + tab.(i)
	done;
	!m ;;
	
	
let close_zero_float = fun tab ->
	let m = ref 0.0 in
	for i=0 to Array.length tab-1 do
		if tab.(i)< 0.0 then m:= !m -. tab.(i) else m:= !m +. tab.(i)
	done;
	!m ;;


let one_step_evol_diff = fun pop fobj cr f ->
    let nb_individu = Array.length pop in
    let taille_individu = Array.length pop.(0) in
    let chosen = Array.make 3 0 in
    for k=0 to (nb_individu-1) do
		let trial = Array.make taille_individu 0 in
		chosen.(0)<-Random.int (nb_individu-1);
      chosen.(1)<-Random.int (nb_individu-1);
      chosen.(2)<-Random.int (nb_individu-1);
      
		while chosen.(0)=k do 
			chosen.(0)<- Random.int (nb_individu-1);	
	   done;
	   
	   (*Printf.printf "\n \n k = %d \n" k;
	   Printf.printf "C0 = %d \n" (chosen.(0));*)
	   
      while chosen.(1)=chosen.(0) || chosen.(1)=k do 
			chosen.(1)<-Random.int (nb_individu-1);
	   done;
	   
	   (*Printf.printf "C1 = %d \n" (chosen.(1));*)
	   
	   while chosen.(2) = chosen.(0) || chosen.(2) = chosen.(1) || chosen.(2) = k do
			chosen.(2) <- Random.int (nb_individu-1)
		done;
		
	   (*Printf.printf "C2 = %d \n" (chosen.(2));*)
	   
      let a = chosen.(0) in 
	   let b = chosen.(1) in 
	   let c = chosen.(2) in
      for i=0 to (taille_individu-1) do
			if (Random.int 100)<cr then
				trial.(i) <- pop.(a).(i) + f*(pop.(b).(i)-pop.(c).(i))
         else
            trial.(i) <- pop.(k).(i)
    done;
    if (fobj trial)<(fobj pop.(k)) then pop.(k) <- trial
    done;
    pop;;
    

let one_step_evol_diff_float = fun pop fobj cr f ->
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
	   
	   (*Printf.printf "\n \n k = %d \n" k;
	   Printf.printf "C0 = %d \n" (chosen.(0));*)
	   
      while chosen.(1)=chosen.(0) || chosen.(1)=k do 
			chosen.(1)<-Random.int (nb_individu-1);
	   done;
	   
	   (*Printf.printf "C1 = %d \n" (chosen.(1));*)
	   
	   while chosen.(2) = chosen.(0) || chosen.(2) = chosen.(1) || chosen.(2) = k do
			chosen.(2) <- Random.int (nb_individu-1)
		done;
		
	   (*Printf.printf "C2 = %d \n" (chosen.(2));*)
	   
      let a = chosen.(0) in 
	   let b = chosen.(1) in 
	   let c = chosen.(2) in
      for i=0 to (taille_individu-1) do
			if (Random.int 100)<cr then
				trial.(i) <- pop.(a).(i) +. f*.(pop.(b).(i)-.pop.(c).(i))
         else
            trial.(i) <- pop.(k).(i)
    done;
    if (fobj trial)<(fobj pop.(k)) then pop.(k) <- trial
    done;
    pop;;

let evol_diff = fun pop gen_max fobj cr f ->
    let aux_pop = ref pop in
    for k=0 to gen_max do
        aux_pop := one_step_evol_diff (!aux_pop) fobj cr f;
    done;
    !aux_pop;;
    
let evol_diff_float = fun pop gen_max fobj cr f ->
    let aux_pop = ref pop in
    for k=0 to gen_max do
        aux_pop := one_step_evol_diff_float (!aux_pop) fobj cr f;
    done;
    !aux_pop;;

let make_pop_test = fun taille_pop taille_individu ->
	let pop = Array.make_matrix taille_pop taille_individu 0 in
	for k=0 to taille_pop-1 do
		for i=0 to taille_individu-1 do
			let aux = Random.int 50000 in 
			pop.(k).(i) <- aux;
		done;
	done;
	pop;;

let make_pop_test_float = fun taille_pop taille_individu ->
	let pop = Array.make_matrix taille_pop taille_individu 0.0 in
	for k=0 to taille_pop-1 do
		for i=0 to taille_individu-1 do
			let aux = Random.float 50000.0 in 
			pop.(k).(i) <- aux;
		done;
	done;
	pop;;
	
let pop_test = make_pop_test 10 10;;

let test = evol_diff pop_test 1000 close_zero 5 2;;

let pop_test_float = make_pop_test_float 10 10;;

let test = evol_diff_float pop_test_float 1000 close_zero_float 5 0.1;;

(* let optim_cr_f = fun nb_gen taille_pop taille_individu ->
	let pop = ref make_pop_test_float taille_pop taille_individu in
	let score = ref 
	let cr = ref 0.1 in
	let bestcr = ref 0.1 in 
	let f = ref 0.1 in
	let bestf = ref 0.1 in 
	for pas_cr = 0 to 9 do 
		cr := !cr +. 0.1*.pas_cr;
		for pas_f = 0 to 19 do
			f := !f +. 0.1*.pas_f;
			let aux = evol_diff_float pop 1000 close_zero_float !cr !f in *)
			
		





