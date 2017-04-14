type formule =
  Var of char
| Vrai
| Faux
| Non of formule
| Et of formule * formule
| Ou of formule * formule
| Imp of formule * formule
| Ssi of formule * formule
;;

type interpretation == (char * bool) list
;;

type sequent == (formule list * formule list)
;;

exception Echec
;;

let ajoute p val (I:interpretation) =
  try if (assoc p I)=val then I else raise Echec
  with Not_found -> (p,val)::I
;;

let rec beth (I:interpretation) (seq:sequent) =
  match seq with
  ([],[])              -> I
| ([],(Var p)::l)      -> beth (ajoute p false I) ([],l)
| ([],Vrai::l)         -> raise Echec
| ([],Faux::l)         -> beth I ([],l)
| ([],(Non f)::l)      -> beth I ([f],l)
| ([],(Et(f1,f2))::l)  -> begin try beth I ([],f1::l) 
                          with Echec -> beth I ([],f2::l) end
| ([],(Ou(f1,f2))::l)  -> beth I ([],f1::f2::l)
| ([],(Imp(f1,f2))::l) -> beth I ([f1],f2::l)
| ([],(Ssi(f1,f2))::l) -> begin try beth I ([f1],f2::l) 
                          with Echec -> beth I ([f2],f1::l) end
| ((Var p)::l,l2)      -> beth (ajoute p true I) (l,l2)
| (Vrai::l,l2)         -> beth I (l,l2)
| (Faux::l,l2)         -> raise Echec
| ((Non f)::l,l2)      -> beth I (l,f::l2)
| ((Et(f1,f2))::l,l2)  -> beth I (f1::f2::l,l2)
| ((Ou(f1,f2))::l,l2)  -> begin try beth I (f1::l,l2)
                          with Echec -> beth I (f2::l,l2) end
| ((Imp(f1,f2))::l,l2) -> begin try beth I (l,f1::l2)
                          with Echec -> beth I (f2::l,l2) end
| ((Ssi(f1,f2))::l,l2) -> begin try beth I (f1::f2::l,l2) 
                          with Echec -> beth I (l,f1::f2::l2) end
;;

let rec saute_blancs s =
  match s with
  [< '(` ` | `\t`) >] -> saute_blancs s
| [< 'c >]            -> [< 'c; saute_blancs s>]
| [< >]               -> s
;;

let rec connecteur s =
  match s with
  [< '`/`; '`\\` >]      -> (fun f1 f2 -> Et (f1,f2))
| [< '`\\`; '`/` >]      -> (fun f1 f2 -> Ou (f1,f2))
| [< '`=`; '`>` >]       -> (fun f1 f2 -> Imp (f1,f2))
| [< '`<`; '`=`; '`>` >] -> (fun f1 f2 -> Ssi (f1,f2))
;;

let rec formule s =
  match s with
  [< '`(`; formule f1; connecteur c; formule f2; '`)` >] -> c f1 f2
| [< '`~`; formule f >]                                  -> Non f
| [< '`1` >]                                             -> Vrai
| [< '`0` >]                                             -> Faux
| [< '(`a`..`z` as p) >]                                 -> Var p
;;

let traduit s =
  match (saute_blancs s) with
  [< formule f; '`#` >] -> f
;;

let rec affiche (I:interpretation) =
  match I with
  (c,val)::I' -> printf__printf "%c=%d " c (if val then 1 else 0); affiche I'
| []          -> print_string "\n"
;;

let main () =
  print_string "Bonjour...\n";
  print_string "Pour quitter tapez Ctrl-D en guise de formule\n";
  begin try 
    while true do 
      print_string "Entrez une formule : ";
      let s = read_line () in
        begin try
          let f = traduit (stream_of_string (s^"#")) in
          try
            let satisf = beth [] ([f],[]) in
            try
              let falsif = beth [] ([],[f]) in
              print_string (s^" est contingente\n");
              print_string ("Voici une interprétation satisfiante : ");
              affiche satisf;
              print_string ("Voici une interprétation falsifiante : ");
              affiche falsif
            with Echec -> print_string (s^" est une tautologie\n")
          with Echec -> print_string (s^" est une antilogie\n")
        with Parse_failure | Parse_error ->
          print_string (s^" ne représente pas une formule correcte\n") end;
      print_string "\n"
    done
  with End_of_file -> print_string "\nAu revoir..."
    |  _           -> print_string "\nVous venez de tomber sur un bug... Bye"
  end;
  print_newline ()
;;

if sys__interactive then () else main ();;