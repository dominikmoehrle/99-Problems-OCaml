(* Extract a given number of randomly selected elements from a list *)

(* Deterministic, for testing. Replace random with Random.int to get true random results *)
let random n = 1337 mod n ;;

let rec rand_select list n =
    let i = random (List.length list) in
      if n < 0 || n > (List.length list) then raise Not_found
      else if n > 0 then (List.nth list i) :: rand_select (remove_at i list) (n-1)  else [] (*remove_at from exercise twenty*)
;;

assert (rand_select [`a;`b;`c;`d;`e;`f;`g;`h] 3 = [`b;`a;`h]) ;; 
