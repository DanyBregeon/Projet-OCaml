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

let rec lireFormule() =
	begin
	print_string "Veuillez inserer votre formule:";
	print_newline();
	let formule = read_line () in
	begin
	print_newline(); print_string "Que voulez vous faire ? Satisfaire/Falsifier/Valider/Insatisfaire";
	print_newline();
	let choix = read_line () in
	print_string (formule^"   "^choix);
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