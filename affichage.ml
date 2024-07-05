#use"tipe_heur.ml"

let create_csv map =
  let l = Array.length map in
  Printf.printf "%d" l;

  let texte = open_out "map.csv" in
  
  for j=0 to l-1 do
    let o = (map.(j)).ownership in
    let g = (map.(j)).garrison in
    let i = (map.(j)).industry in
    let t = (map.(j)).terrain in
    let l = (map.(j)).link in

    Printf.fprintf texte "%d," o;
    
    (match g with
     |Ship -> output_string texte "S,"
     |Arti -> output_string texte "A,"
     |E -> output_string texte "E,"
    );
    
    (match i with
     |true -> output_string texte "T,"
     |false -> output_string texte "F,"
    );
    
    (match t with
     |W -> output_string texte "W,"
     |G -> output_string texte "G,"
     |C -> output_string texte "C,"
    );

    if (Array.length l) >= 1 then
      begin
        if (Array.length l) >= 2 then
          begin
            for k=0 to ((Array.length l)-2) do
              Printf.fprintf texte "%d," l.(k);
            done;
          end;
        Printf.fprintf texte "%d\n" l.((Array.length l)-1);
      end;
    if (Array.length l) = 0 then
      Printf.fprintf texte "\n";
  done;
  close_out texte;
  ()
;;

