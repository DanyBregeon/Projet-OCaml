

type f = Non of f | Et of f*f | V of f*f | Imp of f*f | Eq of f*f | Zero | Un ;;

let rec evaluer = fun f ->
      match f with
      | Zero -> Zero
      | Un -> Un
	  | Non Un -> Zero
	  | Non Zero -> Un
      | Non f -> let x = evaluer f in evaluer (Non(x))
      | Et(_, Zero) -> Zero
      | Et(Zero,_) -> Zero
      | Et(Un, Un) -> Un
      | Et(f,g) -> let x1 = evaluer f and x2 = evaluer g in evaluer (Et(x1, x2))
	  | V(_, Un) -> Un
      | V(Un,_) -> Un
      | V(Zero, Zero) -> Zero
      | V(f,g) -> let x1 = evaluer f and x2 = evaluer g in evaluer (V(x1, x2));;
	  
	  let formule = V(Non(Un),Et(Un,Zero));;
	  evaluer formule;;