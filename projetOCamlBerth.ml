type formule =
  Var of char
| Vrai
| Faux
| Non of formule
| Et of formule * formule
| Ou of formule * formule
| Imp of formule * formule
| Eq of formule * formule
;;

type variables = (char * bool) list;;

exception Echec;;

let ajoute = fun p b (v:variables) ->
  try if (List.assoc p v)=b then v else raise Echec
  with Not_found -> (p,b)::v;;

(*tout ce qui est dans la premiere liste est à valider et tout ce qui est dans la deuxieme est à falsifier *)
let rec beth = fun (v:variables) l1 l2 ->
  match (l1,l2) with
  ([],[])              -> v
| ([],(Var p)::l1)      -> beth (ajoute p false v) [] l1
| ([],Vrai::l1)         -> raise Echec
| ([],Faux::l1)         -> beth v [] l1
| ([],(Non f)::l1)      -> beth v [f] l1
| ([],(Et(f1,f2))::l1)  -> begin try beth v [] (f1::l1)
                          with Echec -> beth v [] (f2::l1) end
| ([],(Ou(f1,f2))::l1)  -> beth v [] (f1::f2::l1)
| ([],(Imp(f1,f2))::l1) -> beth v [f1] (f2::l1) 
| ([],(Eq(f1,f2))::l1) -> begin try beth v [f1] (f2::l1)
                          with Echec -> beth v [f2] (f1::l1) end
| ((Var p)::l1,l2)      -> beth (ajoute p true v) l1 l2
| (Vrai::l1,l2)         -> beth v l1 l2
| (Faux::l1,l2)         -> raise Echec
| ((Non f)::l1,l2)      -> beth v l1 (f::l2)
| ((Et(f1,f2))::l1,l2)  -> beth v (f1::f2::l1) l2
| ((Ou(f1,f2))::l1,l2)  -> begin try beth v (f1::l1) l2
                          with Echec -> beth v (f2::l1) l2 end
| ((Imp(f1,f2))::l1,l2) -> begin try beth v l1 (f1::l2)
                          with Echec -> beth v (f2::l1) l2 end
| ((Eq(f1,f2))::l1,l2) -> begin try beth v (f1::f2::l1) l2
                          with Echec -> beth v l1 (f1::f2::l2) end
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



let string_of_formule = fun f ->
	let rec aux = fun f res ->
	match f with
	  Var c -> res^(String.make 1 c)
	| Vrai -> res^"1"
	| Faux -> res^"0"
	| Non f -> aux f res^"~"
	| Et (f1,f2) -> "& ("^(aux f1 res)^") ("^(aux f2 res)^")"
	| Ou (f1,f2) -> "V ("^(aux f1 res)^") ("^(aux f2 res)^")"
	| Imp (f1,f2) -> "=> ("^(aux f1 res)^") ("^(aux f2 res)^")"
	| Eq (f1,f2) -> "<=> ("^(aux f1 res)^") ("^(aux f2 res)^")"
	in aux f "";;
	
	string_of_formule (Eq((Ou (Var 'a',Var 'b')),Et(Var 'a',Var 'b')));;

let rec formule_of_string = fun s ->
	match (String.sub s 0 1) with
	"~" -> Non (formule_of_string (String.sub s 1 ((String.length s)-1)))
	|"&" -> let s1 = delimiterString (String.sub s 1 ((String.length s)-1)) in
	let f1 = formule_of_string s1 in
	let s2 = delimiterString (String.sub s ((String.length s1)+1) ((String.length s)-(String.length s1)-1)) in
	let f2 = formule_of_string s2 in
	Et (f1,f2)
	|"V" -> let s1 = delimiterString (String.sub s 1 ((String.length s)-1)) in
	let f1 = formule_of_string s1 in
	let s2 = delimiterString (String.sub s ((String.length s1)+1) ((String.length s)-(String.length s1)-1)) in
	let f2 = formule_of_string s2 in
	Ou (f1,f2)
	|"=" -> let s1 = delimiterString (String.sub s 1 ((String.length s)-1)) in
	let f1 = formule_of_string s1 in
	let s2 = delimiterString (String.sub s ((String.length s1)+1) ((String.length s)-(String.length s1)-1)) in
	let f2 = formule_of_string s2 in
	Imp (f1,f2)
	|"<" -> let s1 = delimiterString (String.sub s 2 ((String.length s)-2)) in
	let f1 = formule_of_string s1 in
	let s2 = delimiterString (String.sub s ((String.length s1)+2) ((String.length s)-(String.length s1)-2)) in
	let f2 = formule_of_string s2 in
	Eq (f1,f2)
	|">" -> formule_of_string (String.sub s 1 ((String.length s)-1))
	|"(" -> formule_of_string (String.sub s 1 ((String.length s)-1))
	|")" -> formule_of_string (String.sub s 1 ((String.length s)-1))
	|"0" -> Faux
	|"1" -> Vrai
	|" " -> formule_of_string (String.sub s 1 ((String.length s)-1))
	(* v.[0] retourne le premier caractere de v sous la forme d'un char)*)
	|v -> Var v.[0];;

formule_of_string "<=> (& (=> (V (a) (c)) (~b)) (c)) (1)";;

let rec affiche (v:variables) =
  match v with
  (c,b)::i1 -> if b then (print_string ((String.make 1 c)^" vrai \n"); affiche i1)
  else (print_string ((String.make 1 c)^" faux  \n"); affiche i1)
 | []  -> print_string "\n"
;;



let rec lireFormule () =
   begin
      print_string "Veuillez inserer votre formule:";
      print_newline ();
      let formuleString = read_line () in
         let formule = formule_of_string formuleString in
            begin
               print_newline (); print_string "Que voulez vous faire ? satisfaire/falsifier/valider/insatisfaire";
               print_newline ();
               let choix = read_line () in
                  match choix with
                  "insatisfaire" ->
                     begin
                        try
                           let insatisfaireFormule = beth [] [formule] [] in
                              begin
                                 print_string ("La formule " ^ formuleString ^ " n'est pas insatisfiable; voici une interpretation satisfiante : \n");
                                 affiche insatisfaireFormule
                              end
                        with Echec -> print_string ("La forumule " ^ formuleString ^ " est insatisfiable.")
                     end
                  | "valider" ->
                     begin
                        try
                           let validerFormule = beth [] [] [formule] in
                              begin
                                 print_string ("La formule " ^ formuleString ^ " n'est pas valide; voici une interpretation falsifiante : \n");
                                 affiche validerFormule
                              end
                        with Echec -> print_string ("La forumule " ^ formuleString ^ " est valide.")
                     end
                  | "satisfaire" ->
                     begin
                        try
                           let satisfaireFormule = beth [] [formule] [] in
                              begin
                                 print_string ("Voici une interpretation satisfiante : \n");
                                 affiche satisfaireFormule
                              end
                        with Echec -> print_string ("La forumule " ^ formuleString ^ " n'est pas satisfiable.")
                     end
                  | "falsifier" ->
                     begin
                        try
                           let falsifierFormule = beth [] [] [formule] in
                              begin
                                 print_string ("Voici une interpretation falsifiante : \n");
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