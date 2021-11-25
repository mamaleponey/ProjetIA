open Random;;

Random.init;;



let fmin = fun tab ->
	let m = ref 0 in 
	for i=0 to Array.length tab-1 do
		if tab.(i)< !m then 
			m := tab.(i)
	done;
	!m;;

let one_step_evol_diff = fun pop fobj cr f ->
    let nb_individu = Array.length pop in
    let taille_individu = Array.length pop.(0) in
    let chosen = Array.make 3 0 in
    for k=0 to (nb_individu-1) do
		let trial = Array.make taille_individu 0 in
		chosen.(0)<-Random.int (nb_individu-1);
      chosen.(1)<-Random.int (nb_individu-1);
      chosen.(2)<-Random.int (taille_individu-1);
		while chosen.(1)=chosen.(0)||chosen.(1)=k do 
			chosen.(1)<- Random.int (nb_individu-1)
	   done;	
      while chosen.(2)=chosen.(1) || chosen.(2)=chosen.(0)||chosen.(2)=k||chosen.(0)=k do 
			chosen.(2)<-Random.int (nb_individu-1);
         chosen.(0)<- Random.int (nb_individu-1);
	   done;
      let a = chosen.(0) in 
	   let b = chosen.(1) in 
	   let c = chosen.(2) in
      for i=0 to taille_individu-1 do
			if (Random.int 100)<cr then
				trial.(i) <- pop.(a).(i) + f*(pop.(b).(i)-pop.(c).(i))
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

let pop_test = 
