(*type country = {
    id : int;
    danger : int;
    power : int;
  };;
 *)

type relation = {
    source : int;
    dest : int;
    mutable trust : int;
    mutable interest : int;
    mutable threat : int;
    mutable powdiff : int;
  };;

(* *)

type troop = Ship | Arti | E;;

type landscape = W(*ater*) | G(*round*) | C(*oast*)

type case_data = {
    ownership : int;
    garrison : troop;
    industry : bool;
    terrain : landscape;
    link : int array;
  };;

type board = case_data array;;

(* Création des trucs de base ----------------------------------------------- *)
(*
let create_country a =
  {id=a;danger=0;power=0}
;;
 *)
(* *)

let create_relation s d tr i th p =
  let r = {source=s;dest=d;trust=tr;interest=i;threat=th;powdiff=p} in
  r
;;

let create_relation0 a b =
  let r = {source=a;dest=b;trust=0;interest=0;threat=0;powdiff=0} in
  r
;;

let create_graph nb_country = (* graphe de relation *)
  let r = create_relation0 0 0 in
  let g = Array.make_matrix nb_country nb_country r in
  for i=0 to nb_country-1 do
    for j=0 to nb_country-1 do
      g.(i).(j) <- create_relation0 i j;
    done;  
  done;
  g
;;

(* Affichage ------------------------ *)

let aff_rela mat_rela =
  let n = Array.length mat_rela.(0) in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      if i != j then
        Printf.printf"Pays %d par rapport à Pays %d : \n Confiance : %d\n Intérêt : %d\n Menace : %d\n Ecart de Puissance : %d\n\n" i j (mat_rela.(i).(j).trust) (mat_rela.(i).(j).interest) (mat_rela.(i).(j).threat) (mat_rela.(i).(j).powdiff) 
    done;
  done
;;


(* Recherche -------------------------- *)

let find_troops map j1 j2 =
  let l = Array.length map in
  let visited = Array.make l false in (*evite les doublons*)
  let listevoisin = ref [] in (*pour la deuxième boucle*)

  let danger = Array.make 2 0 in
  (* tab de taille 2, case 0 nb d'unit à la frontière, et case 2, celle qui sont à 2 cases de distance *)  (* on néglige après 2 cases *)


  (*Autour trouve si il y a des cases enemies autour de celle ci*)
  let autour i tabvisited distance =
    for j = 0 to ((Array.length map.(i).link) -1) do
      let voisin = map.(i).link.(j) in
      if ((not tabvisited.(voisin)) && map.(voisin).ownership = j2 && map.(voisin).garrison != E) then begin
          danger.(distance) <- danger.(distance) +1;
          listevoisin := voisin::!listevoisin;
        end;
      tabvisited.(voisin) <- true;
    done;
  in
    
    
  for i = 0 to l-1 do
    if map.(i).ownership = j1 then autour i visited 0;
  done;
  (* danger.(0)*)


  let tabvoisin = Array.of_list !listevoisin in
  for i = 0 to (Array.length tabvoisin -1) do
    autour tabvoisin.(i) visited 1
  done;

  danger
;;

(* Mise à jour ----------------------- *)

let powerdiff_update map rela_mat =
  let n = Array.length rela_mat.(0) in
  let industrie = Array.make n 0 in
  let nm = Array.length map in
  for j=0 to nm-1 do
    if map.(j).industry && map.(j).ownership != (-1) then industrie.(map.(j).ownership) <- industrie.(map.(j).ownership) +1
  done;

  let a = 1 in (* coeff de l'impact de diff d'industrie *)
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      if i != j then
        rela_mat.(i).(j).powdiff <- a*(industrie.(i)-industrie.(j))
    done;
  done
;;

let threat_update map rela_mat =
  let n = Array.length rela_mat.(0) in
  let adjacent_troops_mat = Array.make_matrix n n [|0;0|] in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      adjacent_troops_mat.(i).(j) <- find_troops map i j
    done;
  done;

  (* coeff à ajuster *)
  let a = -1 in
  let b = 1 in
  let c = -1 in
  let d1 = 3 in
  let d2 = 1 in
  
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      if i != j then
        rela_mat.(i).(j).threat <- a*rela_mat.(i).(j).trust + b*rela_mat.(j).(i).interest + c*rela_mat.(i).(j).powdiff + d1*adjacent_troops_mat.(i).(j).(0) + d2*adjacent_troops_mat.(i).(j).(1)
    (* plus il y a de confiance, moins on est menacé
       plus il y de l'intérêt de j par rapport à i, plus on est menacé
       plus il y a de difference de puissance, plus on se sent menacé 
       les troupes ennemies proches nous menace *)
    done;
  done
;;






let interest_update map rela_mat =
  let n = Array.length rela_mat.(0) in
  let nm = Array.length map in

  for i = 0 to n-1 do (* réinialisation des interêts *)
    for j = 0 to n-1 do
      rela_mat.(i).(j).interest <- 0;
    done;
  done;

  let mat_dist = Array.make_matrix nm nm (100) in


  for i = 0 to nm-1 do (* mise a jour de tous les links : on met 1 entre chaque voisin *)
    let tabmap = map.(i).link in
    for j = 0 to Array.length tabmap -1 do
      mat_dist.(i).(tabmap.(j)) <- 1
    done;
  done;

  (* A* ça marche aussi ?, à voir --------------------------- *) 
  
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

  
  for i = 0 to n-1 do
    
    let tab_pays = Array.make (nm/2) (-1) in
    for j = 0 to nm-1 do
      let cmp = ref 0 in
      if map.(j).ownership = i then      (*On recup les cases du pays*)
        (
          tab_pays.(!cmp) <- j;
          cmp := !cmp + 1;
        )
    done;




    let a = 10 in      (* coeff, à revoir *)
      
    for j = 0 to nm-1 do
      if (map.(j).ownership != i) && (map.(j).ownership != (-1)) && (map.(j).industry) then
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

          let pays2 = map.(j).ownership in
          rela_mat.(i).(pays2).interest <- rela_mat.(i).(pays2).interest + (a/(!min));
        )
    done;
  done
;;



let relation_update map rela_mat =
  interest_update map rela_mat;
  powerdiff_update map rela_mat;
  threat_update map rela_mat;
;;

