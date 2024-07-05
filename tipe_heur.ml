#use"tipe.ml"

    (* 1er Heuristique : la situation *)

(* recup donnÃ©e *)

let nb_troops map = (* renvois un tab *)
  let tab = Array.make 7 0 in
  let troop_total = ref 0 in (* ne prends pas en compte celle des pays neutres *)
  for i = 0 to (Array.length map)-1 do
    if (map.(i).industry)&&(map.(i).ownership != (-1)) then (
      let joueur_troop = map.(i).ownership in
      tab.(joueur_troop) <- tab.(joueur_troop) +1;
      troop_total := !troop_total +1
    )
  done;
  (!troop_total,tab)
;;
    
(* analyse *)

let hegemonie map = 
  let (tt,tab) = nb_troops map in
  let acc = ref 0 in

  let j_heg = ref (-1) in (* -1 : aucun *)
  
  for i = 0 to (Array.length tab) -1 do
    if !acc != tab.(i) then (
      acc := max !acc tab.(i);
      j_heg := i
    )
    else (
      j_heg := -1
    )
  done;
  
  if (not (!acc > tt/3)&&(!j_heg != (-1)))  then
    j_heg := -1; (*NB : condition de victoire : 17 cases d'industrie*)
  
  !j_heg
;;

    (* 2eme Heuristique : la situation locale *)

let voisin map j rela_mat = (*pays frontalier*)
  let tab = Array.make (Array.length rela_mat.(0)) false in
  tab.(j) <- true;

  let voisin_case link =
    for i = 0 to (Array.length link)-1 do
      if map.(link.(i)).ownership != (-1) then
        tab.(map.(link.(i)).ownership) <- true
    done;
  in
  
  for i = 0 to (Array.length map)-1 do
    if map.(i).ownership = j then
      voisin_case map.(i).link 
  done;

  tab.(j) <- false; (*n'est pas voisin de lui-mÃªme*)

  tab
;;

let score_acceptance rela_mat j map = (* potentielle alliance *)
  let tab_voisin = voisin map j rela_mat in
  let n = Array.length tab_voisin in
  let tab_accept = Array.make n max_int in (* max_int arbitraire *)

  let a = 1 in (*coeff Ã  potentiellement redefinir *)
  let b = 1 in
  
  for i = 0 to n-1 do
    if tab_voisin.(i) then (
      let trust = a*rela_mat.(j).(i).trust in
      let interest = -b*rela_mat.(j).(i).interest in
        
      tab_accept.(i) <- trust + interest;
    )
  done;
  
  tab_accept    
;;

let preparation j1 j2 map rela_mat = (* chance de reussite d'invasion (type blitz) *)
  let tab1 = find_troops map j1 j2 in
  let tab2 = find_troops map j2 j1 in
  
  let a = 5 in (*coeff*)
  let b = 3 in
  let c = 1 in

  let troops_front = (- a*(tab1.(0))) - (b*(tab1.(1))) + a*(tab2.(0)) + b*(tab2.(1)) in
  let score = troops_front + c*rela_mat.(j1).(j2).powdiff in

  score (* positif j1 gagne en blitz *)
;;

let mat_blitz map rela_mat = (* matrice du blitz *)
  let n = Array.length rela_mat.(0) in

  let mat = Array.make_matrix n n 0 in
  
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      if i != j then
        mat.(i).(j) <- preparation i j map rela_mat
    done;
  done;

  mat
;;

let score_acceptance_mat rela_mat map = (* potentielle alliance *)

  let n = Array.length rela_mat in
  let tab_max = Array.make n Int.max_int in
  let mat_accept = Array.make n tab_max in (* max_int arbitraire *)
  
  for i = 0 to n-1 do
      mat_accept.(i) <- score_acceptance rela_mat i map;
  done;
  
  mat_accept
;;



    (* 3eme Heuristique : la situation globale *)

type strat = A(*lliance*) | F(*ake A*)| T(*rahison*) | N(*egociation*) | D(*efence*) | W(*ar*) ;;

let resultat_arbre rela_mat map j1 j2 = (* parcourt arbre + score absolue *)

  let all_mat = score_acceptance_mat rela_mat map in
  let b_mat = mat_blitz map rela_mat in

  let score = (Int.abs b_mat.(j1).(j2) ) + (Int.abs all_mat.(j1).(j2) ) + (Int.abs all_mat.(j2).(j1) ) in

  let action = ref A in

  let modifiable = ref false in

  (*
  if b_mat.(j1).(j2) >= 0 then
    (

      modifiable := true ;
      
      if all_mat.(j1).(j2) >= 0 then
        (
          
          if all_mat.(j2).(j1) >= 0 then
            (
            (*alliance normale*)
            )
          else
            (
            (* ici on force l'alliance *)
            )
        
        )
      else
        (
          
          if all_mat.(j2).(j1) >= 0 then
            (
              action := T; (*ici on attacke F on se prépare *)
            )
          else
            (
              action := W;
            )
        
        )
    
    )
  else
    (
      
      if all_mat.(j1).(j2) >= 0 then
        (
          
          if all_mat.(j2).(j1) >= 0 then
            (
            (*Alliance*)
            )
          else
            (
              action := N; (*Ici on est en defence/compromis : neccessite un rÃ©ussite des nÃ©gociations pour retirer les troupes *)
            )
        
        )
      else
        (
          
          if all_mat.(j2).(j1) >= 0 then
            (
              action := F;
            )
          else
            (
              action := D;
            )
        
        )
    
    )
   *)

  let a = -5 in (*coeff pour critère d'alliance *)
  (
    match b_mat.(j1).(j2) >= 0 with
    | true -> modifiable := true;
              (
                match all_mat.(j1).(j2) >= a with
                | true -> (
                  match all_mat.(j2).(j1) >= a with
                  | true -> action := A
                  | _ -> action := A (*ici pacte*)
                )
                | _ -> (
                  match all_mat.(j2).(j1) >= a with
                  | true -> action := T
                  | _ -> action := W
                )
              )
    | _ -> ();
           (
             match all_mat.(j1).(j2) >= a with
             | true -> (
               match all_mat.(j2).(j1) >= a with
               | true -> action := A
               | _ -> action := D
             )
             | _ -> (
               match all_mat.(j2).(j1) >= a with
               | true -> action := F
               | _ -> action := D
             )
           )
  );
         
         
         
  (score,!action,!modifiable) (*Score d'importance , l'action envisagÃ©e , si cette action est modifiable *)
;;

let resultat_arbre2 rela_mat map j1 j2 = (* parcourt arbre + score absolu *)

  let all_mat = score_acceptance_mat rela_mat map in

  let action = ref A in
  (*
  if all_mat.(j1).(j2) >= 0 then
    (
      if all_mat.(j2).(j1) >= 0 then
        (
          action := F
        )
      else
        (
          action := W 
        )
    )
  else
    (
      if all_mat.(j2).(j1) >= 0 then
        (
          action := A
        )
      else
        (
          action := A (* Ici pacte de Non aggression *)
        )
    )
   *)

  let a = -5 in (*coeff d'alliance*)
  
  (
    match all_mat.(j1).(j2) >= a with
    | true -> (
      match all_mat.(j2).(j1) >= a with
      | true -> action := T
      | _ -> action := W
    )
    | _ -> (
      match all_mat.(j2).(j1) >= a with
      | true -> action := A
      | _ -> action := A (* ici pacte *)
    )
  );
  
  !action
;;


let importance_score j map rela_mat = (* ordre de prioritÃ© : renvoie une liste *)
  let l = ref [] in

  let n = Array.length rela_mat in
  let tab_score = Array.make n Int.max_int in

  let voisin_bool = voisin map j rela_mat in
  
  for i = 0 to n-1 do
    if voisin_bool.(i) then
      (
        let (v,_,_) = resultat_arbre rela_mat map j i in
        tab_score.(i) <- v;
      )
  done;

  let idx_min_find tab =
    let idx = ref (-1) in
    let min = ref Int.max_int in
    
    for i = 0 to (Array.length tab)-1 do
      Printf.printf"idx : %d\n" (!idx);
      Printf.printf"i : %d\n" (i);
      if (tab.(i) < !min) then
        (
          min := tab.(i);
          idx := i;
        )
    done;

    if !idx <> -1 then
      tab.(!idx) <- Int.max_int;
    
    !idx
  in
  
  let rec liste_create () =
    Printf.printf"why";
    match idx_min_find tab_score with
    | n when n = -1 -> ()
    | i -> l := i::(!l);
           liste_create ()
    | _ -> failwith"error dans importance score"
      
  in

  liste_create ();

  !l
;;


(*type strat = A(*lliance*) | F(*ake A*)| T(*rahison*) | N(*egociation*) | D(*efence*) | W(*ar*) ;; Negociation demande des troupes, si celle-ci réussi, alors on peut les recuperer*)

let action_is_hostile action =
  let respond = ref true in
  
  if action = A then
    respond := false;
  
  !respond
  
;;


let nb_troops_nec map j1 j2 =

  let nm = Array.length map in

  let mat_dist = Array.make_matrix nm nm (100) in


  for i = 0 to nm-1 do (* mise a jour de tous les links : on met 1 entre chaque voisin *)
    let tabmap = map.(i).link in
    for j = 0 to Array.length tabmap -1 do
      mat_dist.(i).(tabmap.(j)) <- 1
    done;
  done;

 
  
  for i = 0 to nm-1 do
    for j = 0 to nm-1 do
      for k = 0 to nm-1 do
        let d = mat_dist.(i).(k) + mat_dist.(k).(j) in
        if mat_dist.(i).(j) > d then
          (
            mat_dist.(i).(j) <- d;
            mat_dist.(j).(i) <- d;
          )
      done;
    done;
  done;

  
  let tab_pays = Array.make (nm) (-1) in
  for j = 0 to nm-1 do
    let cmp = ref 0 in
    if map.(j).ownership = j1 then      (*On recup les cases du pays*)
      (
        tab_pays.(!cmp) <- j;
        cmp := !cmp + 1;
      )
  done;


  let nb_troops = ref 0 in

  (*let a = 1 in      (* coeff, à revoir *)*)
  
  for j = 0 to nm-1 do
    if (map.(j).ownership = j2) && (map.(j).garrison != E) then
      (
        let min = ref 100 in
        let k = ref 0 in
        while (tab_pays.(!k) <> -1) && (!k < (Array.length tab_pays) - 1) do             (* la on recup le min entre le pays et la case *)
          if mat_dist.(tab_pays.(!k)).(j) < !min then
            (
              min := mat_dist.(tab_pays.(!k)).(j)
            );
          k := !k + 1;
        done;

        if !min < 5 then
          nb_troops := !nb_troops + 1;
      )
  done;

  !nb_troops

;;

let rec inverse_liste l =
  match l with
  | [] -> []
  | (h::t) -> (inverse_liste t)@[h]
;;







let action_realiste j map rela_mat =

  let n = Array.length rela_mat in
  let nb_troops_tab = Array.make n 0 in

  let strategie = Array.make n A in

  let calcul_troops () = (* cacule de nombre de troupes *)
    for i = 0 to (Array.length map)-1 do
      if (map.(i).ownership >= 0) && (map.(i).garrison != E) then
        nb_troops_tab.(map.(i).ownership) <-  nb_troops_tab.(map.(i).ownership) +1;

    done;
  in
  
  calcul_troops ();

  let jtroops = ref nb_troops_tab.(j) in

  let b_mat = mat_blitz map rela_mat in

  Printf.printf("good");

  let action_veri1 j2 troops_restantes troops_necessaire = (* permet de verifier que chacune de nos actions sont crédible en terme de nombre de troupes *)
    match b_mat.(j).(j2) > 0 with
    | false -> let (_,s0,_) = resultat_arbre rela_mat map j j2 in
               s0
    | _ -> let (_,s,_) = resultat_arbre rela_mat map j j2 in
           match action_is_hostile s with
           |false -> let (_,s2,_) = resultat_arbre rela_mat map j j2 in
                     s2
           | _ -> match troops_restantes >= troops_necessaire with
                  | false -> let s3 = resultat_arbre2 rela_mat map j j2 in
                             s3
                  | _ -> jtroops := !jtroops - troops_necessaire;
                         let (_,s4,_) = resultat_arbre rela_mat map j j2 in
                         s4
  in
      
  let rec parcours_list l = (* parcours la liste qui donne l'ordre de priorité *)
    match l with
    | [] -> ()
    | (h::t) -> let troops_nece = nb_troops_nec map j h in
                strategie.(h) <- action_veri1 h !jtroops troops_nece;
                parcours_list t
      
  in

  let l = importance_score j map rela_mat in

  Printf.printf("good");

  parcours_list l;

  Printf.printf("good");
  
(* maintenant on verifie que tout est bon aussi dans l'autre sens *)

  let l2 = inverse_liste l in

  Printf.printf("good");

  let action_veri2 j2 troops_restantes troops_necessaire  = (* permet de verifier que chacune de nos actions sont crédible en terme de nombre de troupes *)
    (
      match b_mat.(j).(j2) > 0 with
      | false -> () 
      | _ -> let (_,s,_) = resultat_arbre rela_mat map j j2 in
             (
               match action_is_hostile s with
               |true -> () 
               | _ -> ();
                      (
                        match troops_restantes >= troops_necessaire with
                        | false -> () 
                        | _ -> jtroops := !jtroops - troops_necessaire;
                               let s2 = resultat_arbre2 rela_mat map j j2 in
                               strategie.(j2) <- s2
                      )
             )
    )
  in
      
  let rec parcours_list2 l = (* parcours la liste qui donne l'ordre de priorité *)
    match l with
    | [] -> ()
    | (h::t) -> let troops_nece = nb_troops_nec map j h in
                action_veri2 h !jtroops troops_nece;
                parcours_list2 t

  in
  
  parcours_list2 l2;
  
  strategie (*Alliance envers soi même : arbitraire et ne sert a rien *)

;;
