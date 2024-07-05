#use"affichage.ml"
(*
let fr = create_country 0;;
let it = create_country 1;;
let au = create_country 2;;
let al = create_country 3;;
let ru = create_country 4;;
let ot = create_country 5;;
let an = create_country 6;;
 *)

let g = create_graph 7;; (*mat des relation*)

let map1ww = [|  {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 1;3;10;69;71|]} ; (*0*)
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 0;2;5;68;69;70|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 1;50;68|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 0;4;10;71;73|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 3;10;25;28;64;73;74|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 1;4;6;7;64;65;66;68;70;72;74|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 5;8;43;65;66|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 5;8;66;67;68|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 6;7;9;43;44;46;48;66;67|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 8;48;49;50;67|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 0;3;4;11;19;21;22;23;25|]} ; (*10*)
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 10;12;13;19;20;22|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 11;13;22;24;29;30|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 11;12;14;20;30;32;34|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 13;15;16;17;20;33;34;59;63|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 14;31;33;40;63|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 14;17;18;56;58;59;60|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 14;16;55;56|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 16;53;54;57;58;60;61|]} ;

                 
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = C ; link = [| 10;11;20|]} ;
                 {ownership = -1 ; garrison = E    ; industry = true ; terrain = C ; link = [| 11;13;14;19|]} ; (*20*)
                 {ownership = -1 ; garrison = E    ; industry = true ; terrain = C ; link = [| 10;22|]} ;
                 {ownership = -1 ; garrison = E    ; industry = true ; terrain = C ; link = [| 10;11;12;21;23;24|]} ;

                 
                 {ownership = 0 ; garrison = E    ; industry = false; terrain = C ; link = [| 10;22;24;25;26;27|]} ;
                 {ownership = 0 ; garrison = Arti ; industry = true ; terrain = C ; link = [| 12;22;23;27;29|]} ;
                 {ownership = 0 ; garrison = Ship ; industry = true ; terrain = C ; link = [| 4;10;23;26;28|]} ;
                 {ownership = 0 ; garrison = Arti ; industry = true ; terrain = G ; link = [| 23;25;27;28|]} ;
                 {ownership = 0 ; garrison = E    ; industry = false; terrain = G ; link = [| 23;24;26;28;41;42;64|]} ;
                 {ownership = 0 ; garrison = E    ; industry = false; terrain = C ; link = [| 4;25;26;27;64|]} ;

                 
                 {ownership = 1 ; garrison = E    ; industry = false; terrain = C ; link = [| 12;24;30;31;35|]} ;
                 {ownership = 1 ; garrison = E    ; industry = false; terrain = C ; link = [| 12;13;29;31;32|]} ; (*30*)
                 {ownership = 1 ; garrison = Arti ; industry = true ; terrain = C ; link = [| 15;29;30;32;33;35;40|]} ;
                 {ownership = 1 ; garrison = Arti ; industry = true ; terrain = C ; link = [| 13;30;31;33;34|]} ;
                 {ownership = 1 ; garrison = E    ; industry = false; terrain = C ; link = [| 14;15;31;32;34|]} ;
                 {ownership = 1 ; garrison = Ship ; industry = true ; terrain = C ; link = [| 13;14;32;33|]} ;

                 
                 {ownership = 2 ; garrison = E    ; industry = false; terrain = G ; link = [| 29;31;36;37;40;42|]} ;
                 {ownership = 2 ; garrison = E    ; industry = false; terrain = G ; link = [| 35;37;39;42;45|]} ;
                 {ownership = 2 ; garrison = Arti ; industry = true ; terrain = G ; link = [| 35;36;38;39;40|]} ;
                 {ownership = 2 ; garrison = Arti ; industry = true ; terrain = G ; link = [| 37;39;40;61;62|]} ;
                 {ownership = 2 ; garrison = E    ; industry = false; terrain = G ; link = [| 36;37;38;45;47;52;61|]} ;
                 {ownership = 2 ; garrison = Ship ; industry = true ; terrain = C ; link = [| 15;31;35;37;38;62;63|]} ; (*40*)

                 
                 {ownership = 3 ; garrison = E    ; industry = false; terrain = G ; link = [| 27;42;43;64;65|]} ;
                 {ownership = 3 ; garrison = Arti ; industry = true ; terrain = G ; link = [| 27;35;36;41;43;44;45|]} ;
                 {ownership = 3 ; garrison = Ship ; industry = true ; terrain = C ; link = [| 6;8;41;42;44;65;66|]} ;
                 {ownership = 3 ; garrison = Arti ; industry = true ; terrain = C ; link = [| 8;42;43;45;46|]} ;
                 {ownership = 3 ; garrison = E    ; industry = false; terrain = G ; link = [| 36;39;42;44;46;47|]} ;
                 {ownership = 3 ; garrison = E    ; industry = false; terrain = C ; link = [| 8;44;45;47;48|]} ;

                 
                 {ownership = 4 ; garrison = Arti ; industry = true ; terrain = G ; link = [| 39;45;46;48;51;52|]} ;
                 {ownership = 4 ; garrison = E    ; industry = false; terrain = C ; link = [| 8;9;46;47;50;51|]} ;
                 {ownership = 4 ; garrison = E    ; industry = false; terrain = C ; link = [| 9;50;67|]} ;
                 {ownership = 4 ; garrison = Ship ; industry = true ; terrain = C ; link = [| 2;48;49;51|]} ; (*50*)
                 {ownership = 4 ; garrison = Arti ; industry = true ; terrain = G ; link = [| 47;48;50;52;53|]} ;
                 {ownership = 4 ; garrison = E    ; industry = false; terrain = G ; link = [| 39;47;51;53;61|]} ;
                 {ownership = 4 ; garrison = Ship ; industry = true ; terrain = C ; link = [| 18;51;52;54;61|]} ;

                 
                 {ownership = 5 ; garrison = E    ; industry = false; terrain = C ; link = [| 18;53;55;56;57|]} ;
                 {ownership = 5 ; garrison = E    ; industry = false; terrain = C ; link = [| 17;54;56|]} ;
                 {ownership = 5 ; garrison = Arti ; industry = true ; terrain = C ; link = [| 16;17;54;55;57;58|]} ;
                 {ownership = 5 ; garrison = Ship ; industry = true ; terrain = C ; link = [| 18;54;56;58|]} ;
                 {ownership = 5 ; garrison = Arti ; industry = true ; terrain = C ; link = [| 16;18;56;57;60|]} ;

                 
                 {ownership = -1 ; garrison = E    ; industry = true ; terrain = C ; link = [| 14;16;60;62;63|]} ;
                 {ownership = -1 ; garrison = E    ; industry = true ; terrain = C ; link = [| 16;18;58;59;61;62|]} ; (*60*)
                 {ownership = -1 ; garrison = E    ; industry = true ; terrain = C ; link = [| 18;38;39;52;53;60;62|]} ;
                 {ownership = -1 ; garrison = E    ; industry = true ; terrain = G ; link = [| 38;40;59;60;61;63|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = C ; link = [| 14;15;40;59;62|]} ;

                 
                 {ownership = -1 ; garrison = E    ; industry = true ; terrain = C ; link = [| 4;5;28;27;41;65|]} ;
                 {ownership = -1 ; garrison = E    ; industry = true ; terrain = C ; link = [| 5;6;41;43;64|]} ;
                 {ownership = -1 ; garrison = E    ; industry = true ; terrain = C ; link = [| 5;6;7;8;43;67|]} ;
                 {ownership = -1 ; garrison = E    ; industry = true ; terrain = C ; link = [| 7;8;9;49;66;68|]} ;
                 {ownership = -1 ; garrison = E    ; industry = true ; terrain = C ; link = [| 1;2;5;7;49;50;67|]} ;

                 
                 {ownership = 6 ; garrison = E    ; industry = false; terrain = C ; link = [| 0;1;70;71|]} ;
                 {ownership = 6 ; garrison = Ship ; industry = true ; terrain = C ; link = [| 1;5;69;71;72|]} ; (*70*)
                 {ownership = 6 ; garrison = Arti ; industry = true ; terrain = C ; link = [| 0;3;69;70;72;73|]} ;
                 {ownership = 6 ; garrison = E    ; industry = false; terrain = C ; link = [| 5;70;71;73;74|]} ;
                 {ownership = 6 ; garrison = E    ; industry = false; terrain = C ; link = [| 3;4;71;72;74|]} ;
                 {ownership = 6 ; garrison = Ship ; industry = true ; terrain = C ; link = [| 4;5;72;73|]}           
             |];;









let map2ww = [|  {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 1;3;10;69;71|]} ; (*0*)
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 0;2;5;68;69;70|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 1;50;68|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 0;4;10;71;73|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 3;10;25;28;64;73;74|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 1;4;6;7;64;65;66;68;70;72;74|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 5;8;43;65;66|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 5;8;66;67;68|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 6;7;9;43;44;46;48;66;67|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 8;48;49;50;67|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 0;3;4;11;19;21;22;23;25|]} ; (*10*)
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 10;12;13;19;20;22|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 11;13;22;24;29;30|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 11;12;14;20;30;32;34|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 13;15;16;17;20;33;34;59;63|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 14;31;33;40;63|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 14;17;18;56;58;59;60|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 14;16;55;56|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = W ; link = [| 16;53;54;57;58;60;61|]} ;

                 
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = C ; link = [| 10;11;20|]} ;
                 {ownership = -1 ; garrison = E    ; industry = true ; terrain = C ; link = [| 11;13;14;19|]} ; (*20*)
                 {ownership = -1 ; garrison = E    ; industry = true ; terrain = C ; link = [| 10;22|]} ;
                 {ownership = -1 ; garrison = E    ; industry = true ; terrain = C ; link = [| 10;11;12;21;23;24|]} ;

                 
                 {ownership = 0 ; garrison = E    ; industry = false; terrain = C ; link = [| 10;22;24;25;26;27|]} ;
                 {ownership = 0 ; garrison = Arti ; industry = true ; terrain = C ; link = [| 12;22;23;27;29|]} ;
                 {ownership = 0 ; garrison = Ship ; industry = true ; terrain = C ; link = [| 4;10;23;26;28|]} ;
                 {ownership = 0 ; garrison = Arti ; industry = true ; terrain = G ; link = [| 23;25;27;28|]} ;
                 {ownership = 0 ; garrison = E    ; industry = false; terrain = G ; link = [| 23;24;26;28;41;42;64|]} ;
                 {ownership = 0 ; garrison = E    ; industry = false; terrain = C ; link = [| 4;25;26;27;64|]} ;

                 
                 {ownership = 1 ; garrison = E    ; industry = false; terrain = C ; link = [| 12;24;30;31;35|]} ;
                 {ownership = 1 ; garrison = E    ; industry = false; terrain = C ; link = [| 12;13;29;31;32|]} ; (*30*)
                 {ownership = 1 ; garrison = Arti ; industry = true ; terrain = C ; link = [| 15;29;30;32;33;35;40|]} ;
                 {ownership = 1 ; garrison = Arti ; industry = true ; terrain = C ; link = [| 13;30;31;33;34|]} ;
                 {ownership = 1 ; garrison = E    ; industry = false; terrain = C ; link = [| 14;15;31;32;34|]} ;
                 {ownership = 1 ; garrison = Ship ; industry = true ; terrain = C ; link = [| 13;14;32;33|]} ;

                 
                 {ownership = 2 ; garrison = E    ; industry = false; terrain = G ; link = [| 29;31;36;37;40;42|]} ;
                 {ownership = 2 ; garrison = E    ; industry = false; terrain = G ; link = [| 35;37;39;42;45|]} ;
                 {ownership = 2 ; garrison = Arti ; industry = true ; terrain = G ; link = [| 35;36;38;39;40|]} ;
                 {ownership = 2 ; garrison = Arti ; industry = true ; terrain = G ; link = [| 37;39;40;61;62|]} ;
                 {ownership = 2 ; garrison = E    ; industry = false; terrain = G ; link = [| 36;37;38;45;47;52;61|]} ;
                 {ownership = 2 ; garrison = Ship ; industry = true ; terrain = C ; link = [| 15;31;35;37;38;62;63|]} ; (*40*)

                 
                 {ownership = 3 ; garrison = E    ; industry = false; terrain = G ; link = [| 27;42;43;64;65|]} ;
                 {ownership = 3 ; garrison = Arti ; industry = true ; terrain = G ; link = [| 27;35;36;41;43;44;45|]} ;
                 {ownership = 3 ; garrison = Ship ; industry = true ; terrain = C ; link = [| 6;8;41;42;44;65;66|]} ;
                 {ownership = 3 ; garrison = E    ; industry = true ; terrain = C ; link = [| 8;42;43;45;46|]} ;
                 {ownership = 3 ; garrison = Arti ; industry = false; terrain = G ; link = [| 36;39;42;44;46;47|]} ;
                 {ownership = 3 ; garrison = Arti ; industry = false; terrain = C ; link = [| 8;44;45;47;48|]} ;

                 
                 {ownership = 4 ; garrison = Arti ; industry = true ; terrain = G ; link = [| 39;45;46;48;51;52|]} ;
                 {ownership = 4 ; garrison = E    ; industry = false; terrain = C ; link = [| 8;9;46;47;50;51|]} ;
                 {ownership = 4 ; garrison = E    ; industry = false; terrain = C ; link = [| 9;50;67|]} ;
                 {ownership = 4 ; garrison = Ship ; industry = true ; terrain = C ; link = [| 2;48;49;51|]} ; (*50*)
                 {ownership = 4 ; garrison = Arti ; industry = true ; terrain = G ; link = [| 47;48;50;52;53|]} ;
                 {ownership = 4 ; garrison = E    ; industry = false; terrain = G ; link = [| 39;47;51;53;61|]} ;
                 {ownership = 4 ; garrison = Ship ; industry = true ; terrain = C ; link = [| 18;51;52;54;61|]} ;

                 
                 {ownership = 5 ; garrison = E    ; industry = false; terrain = C ; link = [| 18;53;55;56;57|]} ;
                 {ownership = 5 ; garrison = E    ; industry = false; terrain = C ; link = [| 17;54;56|]} ;
                 {ownership = 5 ; garrison = Arti ; industry = true ; terrain = C ; link = [| 16;17;54;55;57;58|]} ;
                 {ownership = 5 ; garrison = Ship ; industry = true ; terrain = C ; link = [| 18;54;56;58|]} ;
                 {ownership = 5 ; garrison = Arti ; industry = true ; terrain = C ; link = [| 16;18;56;57;60|]} ;

                 
                 {ownership = -1 ; garrison = E    ; industry = true ; terrain = C ; link = [| 14;16;60;62;63|]} ;
                 {ownership = -1 ; garrison = E    ; industry = true ; terrain = C ; link = [| 16;18;58;59;61;62|]} ; (*60*)
                 {ownership = -1 ; garrison = E    ; industry = true ; terrain = C ; link = [| 18;38;39;52;53;60;62|]} ;
                 {ownership = -1 ; garrison = E    ; industry = true ; terrain = G ; link = [| 38;40;59;60;61;63|]} ;
                 {ownership = -1 ; garrison = E    ; industry = false; terrain = C ; link = [| 14;15;40;59;62|]} ;

                 
                 {ownership = -1 ; garrison = E    ; industry = true ; terrain = C ; link = [| 4;5;28;27;41;65|]} ;
                 {ownership = -1 ; garrison = E    ; industry = true ; terrain = C ; link = [| 5;6;41;43;64|]} ;
                 {ownership = 3  ; garrison = E    ; industry = true ; terrain = C ; link = [| 5;6;7;8;43;67|]} ;
                 {ownership = -1 ; garrison = E    ; industry = true ; terrain = C ; link = [| 7;8;9;49;66;68|]} ;
                 {ownership = -1 ; garrison = E    ; industry = true ; terrain = C ; link = [| 1;2;5;7;49;50;67|]} ;

                 
                 {ownership = 6 ; garrison = E    ; industry = false; terrain = C ; link = [| 0;1;70;71|]} ;
                 {ownership = 6 ; garrison = Ship ; industry = true ; terrain = C ; link = [| 1;5;69;71;72|]} ; (*70*)
                 {ownership = 6 ; garrison = Arti ; industry = true ; terrain = C ; link = [| 0;3;69;70;72;73|]} ;
                 {ownership = 6 ; garrison = E    ; industry = false; terrain = C ; link = [| 5;70;71;73;74|]} ;
                 {ownership = 6 ; garrison = E    ; industry = false; terrain = C ; link = [| 3;4;71;72;74|]} ;
                 {ownership = 6 ; garrison = Ship ; industry = true ; terrain = C ; link = [| 4;5;72;73|]}           
             |];;

create_csv map1ww;;

(*find_troops map1ww 2 1;;

powerdiff_update map1ww g;;

aff_rela g;;

threat_update map1ww g;;

interest_update map1ww g;;

nb_troops map1ww;;

hegemonie map1ww;;

voisin map1ww 0;;

let affi () =
  let a = score_acceptance g in
  
  for i = 0 to (Array.length a)-1 do
    Printf.printf("%s", a.(i))
  done;;

score_acceptance g 0 map1ww;;
score_acceptance g 3 map1ww;;

preparation 0 3 map1ww g;;

score_acceptance g 0 map1ww;;
score_acceptance g 1 map1ww;;

preparation 0 1 map1ww g;;

resultat_arbre g map1ww 0 1;;
resultat_arbre g map1ww 1 0;;
resultat_arbre g map1ww 4 2;;

inverse_liste [1;2;3;4;5;6];; *)

importance_score 0 map1ww g;;

action_realiste 3 map1ww g;;



create_csv map2ww;;

powerdiff_update map2ww g;;

threat_update map2ww g;;

interest_update map2ww g;;

preparation 3 4 map2ww g;;
score_acceptance g 3 map2ww;;

resultat_arbre g map2ww 3 0;;
resultat_arbre g map2ww 3 1;;
resultat_arbre g map2ww 3 2;;
resultat_arbre g map2ww 3 4;;
resultat_arbre g map2ww 3 5;;
resultat_arbre g map2ww 3 6;;

action_realiste 3 map1ww g;;
