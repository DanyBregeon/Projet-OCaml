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

type interpretation = (char * bool) list;;

type sequent = (formule list * formule list);;

exception Echec;;

let ajoute = fun p b (i:interpretation) ->
  try if (List.assoc p i)=b then i else raise Echec
  with Not_found -> (p,b)::i;;

(*tout ce qui est dans la premiere liste est à valider et tout ce qui est dans la deuxieme est à falsifier *)
let rec beth = fun (i:interpretation) (seq:sequent) ->
  match seq with
  ([],[])              -> i
| ([],(Var p)::l)      -> beth (ajoute p false i) ([],l)
| ([],Vrai::l)         -> raise Echec
| ([],Faux::l)         -> beth i ([],l)
| ([],(Non f)::l)      -> beth i ([f],l)
| ([],(Et(f1,f2))::l)  -> begin try beth i ([],f1::l) 
                          with Echec -> beth i ([],f2::l) end
| ([],(Ou(f1,f2))::l)  -> beth i ([],f1::f2::l)
| ([],(Imp(f1,f2))::l) -> beth i ([f1],f2::l)
| ([],(Ssi(f1,f2))::l) -> begin try beth i ([f1],f2::l) 
                          with Echec -> beth i ([f2],f1::l) end
| ((Var p)::l,l2)      -> beth (ajoute p true i) (l,l2)
| (Vrai::l,l2)         -> beth i (l,l2)
| (Faux::l,l2)         -> raise Echec
| ((Non f)::l,l2)      -> beth i (l,f::l2)
| ((Et(f1,f2))::l,l2)  -> beth i (f1::f2::l,l2)
| ((Ou(f1,f2))::l,l2)  -> begin try beth i (f1::l,l2)
                          with Echec -> beth i (f2::l,l2) end
| ((Imp(f1,f2))::l,l2) -> begin try beth i (l,f1::l2)
                          with Echec -> beth i (f2::l,l2) end
| ((Ssi(f1,f2))::l,l2) -> begin try beth i (f1::f2::l,l2) 
                          with Echec -> beth i (l,f1::f2::l2) end
;;


let delimiterString = fun s ->
      let rec aux = fun s1 s2 p ->
            match (String.sub s1 0 1) with          
            | ")" -> if (p-1) = 0 then s2 ^ ")"
            else aux (String.sub s1 1 ((String.length s1) - 1)) (s2 ^ ")") (p-1)
 				| "(" -> aux (String.sub s1 1 ((String.length s1) - 1)) (s2 ^ "(") (p+1)
 				| v -> aux (String.sub s1 1 ((String.length s1) - 1)) (s2 ^ v) p
      in aux s "" 0;;
delimiterString "& (& (a) (~b)) (c)";;


let rec notation = fun s ->
	match (String.sub s 0 1) with
	"~" -> Non (notation (String.sub s 1 ((String.length s)-1)))
	|"&" -> let s1 = delimiterString (String.sub s 1 ((String.length s)-1)) in
	let f1 = notation s1 in
	let s2 = delimiterString (String.sub s ((String.length s1)+1) ((String.length s)-(String.length s1)-1)) in
	let f2 = notation s2 in
	Et (f1,f2)
	|"V" -> let s1 = delimiterString (String.sub s 1 ((String.length s)-1)) in
	let f1 = notation s1 in
	let s2 = delimiterString (String.sub s ((String.length s1)+1) ((String.length s)-(String.length s1)-1)) in
	let f2 = notation s2 in
	Ou (f1,f2)
	|"=" -> let s1 = delimiterString (String.sub s 1 ((String.length s)-1)) in
	let f1 = notation s1 in
	let s2 = delimiterString (String.sub s ((String.length s1)+1) ((String.length s)-(String.length s1)-1)) in
	let f2 = notation s2 in
	Imp (f1,f2)
	|"<" -> let s1 = delimiterString (String.sub s 2 ((String.length s)-2)) in
	let f1 = notation s1 in
	let s2 = delimiterString (String.sub s ((String.length s1)+2) ((String.length s)-(String.length s1)-2)) in
	let f2 = notation s2 in
	Ssi (f1,f2)
	|">" -> notation (String.sub s 1 ((String.length s)-1))
	|"(" -> notation (String.sub s 1 ((String.length s)-1))
	|")" -> notation (String.sub s 1 ((String.length s)-1))
	|"0" -> Faux
	|"1" -> Vrai
	|" " -> notation (String.sub s 1 ((String.length s)-1))
	(* v.[0] retourne le premier caractere de v sous la forme d'un char)*)
	|v -> Var v.[0];;

notation "<=> (& (=> (V (a) (c)) (~b)) (c)) (1)";;

let rec affiche (i:interpretation) =
  match i with
  (c,b):: i1 -> if b then (print_string ((String.make 1 c)^" vrai \n"); affiche i1)
  else (print_string ((String.make 1 c)^" faux  \n"); affiche i1)
| | []  -> print_string "\n"
;;



let rec lireFormule () =
   begin
      print_string "Veuillez inserer votre formule:";
      print_newline ();
      let formuleString = read_line () in
         let formule = notation formuleString in
            begin
               print_newline (); print_string "Que voulez vous faire ? satisfaire/falsifier/valider/insatisfaire";
               print_newline ();
               let choix = read_line () in
                  match choix with
                  "insatisfaire" ->
                     begin
                        try
                           let insatisfaireFormule = beth [] ([formule], []) in
                              begin
                                 print_string ("La formule " ^ formuleString ^ " n'est pas insatisfiable; voici une interprétation satisfiante : \n");
                                 affiche insatisfaireFormule
                              end
                        with Echec -> print_string ("La forumule " ^ formuleString ^ " est insatisfiable.")
                     end
                  | "valider" ->
                     begin
                        try
                           let validerFormule = beth [] ([], [formule]) in
                              begin
                                 print_string ("La formule " ^ formuleString ^ " n'est pas valide; voici une interprétation falsifiante : \n");
                                 affiche validerFormule
                              end
                        with Echec -> print_string ("La forumule " ^ formuleString ^ " est valide.")
                     end
                  | "satisfaire" ->
                     begin
                        try
                           let satisfaireFormule = beth [] ([formule], []) in
                              begin
                                 print_string ("Voici une interprétation satisfiante : \n");
                                 affiche satisfaireFormule
                              end
                        with Echec -> print_string ("La forumule " ^ formuleString ^ " n'est pas satisfiable.")
                     end
                  | "falsifier" ->
                     begin
                        try
                           let falsifierFormule = beth [] ([], [formule]) in
                              begin
                                 print_string ("Voici une interprétation falsifiante : \n");
                                 affiche falsifierFormule
                              end
                        with Echec -> print_string ("La forumule " ^ formuleString ^ " n'est pas falsifiable.")
                     end
                  | _ -> print_string ("Choix non valide \n");
                     print_newline ()
            end;
            print_string "Voulez-vous recommencer ? si oui o sinon n";
            print_newline ();
            let recommencer = read_line () in
               if recommencer = "o" then lireFormule ()
               else print_string "Au revoir !";
   end;;

let main () =
	begin
	print_string "Bonjour !";
	print_newline();
	lireFormule()
	end;;

(*  lireFormule();;  *)
main();;
<=> (V (a) (b)) (& (a) (b))
insatisfaire
valider
falsifier
satisfaire
no