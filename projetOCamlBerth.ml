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





let rec lireFormule() =
	begin
	print_string "Veuillez inserer votre formule:";
	print_newline();
	let formuleString = read_line () in
	let formule = notation formuleString in
	begin
	print_newline(); print_string "Que voulez vous faire ? Satisfaire/Falsifier/Valider/Insatisfaire";
	print_newline();
	let choix = read_line () in
	print_string (formuleString^"   "^choix);
	print_newline();
	end;
	print_string "Voulez-vous recommencer ? si oui o sinon n";
	print_newline();
	let recommencer = read_line () in
	if recommencer = "o" then lireFormule()
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