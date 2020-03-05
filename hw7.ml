(* Question 1.1 *)
let rec occurCheck (v: char) (tau: typExp) : bool =
  match tau with
  |TypInt -> false
  |TypVar char -> char == v
  |Arrow (typE1, typE2) -> (occurCheck v typE1) || (occurCheck v typE2)
  |Lst typE -> occurCheck v typE
;;

(* Question 1.2 *)
let rec substitute (tau1 : typExp) (v : char) (tau2 : typExp) : typExp =
  match tau2 with
  |TypInt -> TypInt
  |TypVar char -> if (char == v) then tau1 else TypVar char
  |Arrow (t1,t2) -> Arrow ((substitute tau1 v t1), (substitute tau1 v t2))
  |Lst t -> Lst (substitute tau1 v t)
;;

(* Question 1.3 *)
let applySubst (sigma: substitution) (tau: typExp) : typExp =
  List.fold_right(fun (a,b) -> substitute b a) sigma tau
;;

(* Question 2 *)
let rec unify (tau1: typExp) (tau2:typExp) : substitution = 
  match tau1 with
  |TypInt -> 
      (match tau2 with 
       |TypInt -> []
       |TypVar x -> [x, TypInt]
       |_ -> failwith "Not unifiable"
      )
      
  |TypVar x -> 
      (match tau2 with
       |TypInt -> [x, TypInt]
       |TypVar x1 -> if (x = x1) then [] else [x1, TypVar x]
       |Arrow (x1,y1) -> if (occurCheck x tau2) then failwith "Not unifiable"
           else [x, Arrow (x1,y1)]
       |Lst _ -> if (occurCheck x tau2) then failwith "Not unifiable"
           else [x, tau2]
      )
      
  |Arrow (x,y) -> 
      (match tau2 with 
       |TypInt -> failwith "Not unifiable"
       |TypVar x1 -> if (occurCheck x1 tau1) then failwith "Not unifiable"
           else [x1, Arrow (x,y)]
       |Arrow (x1,y1) -> let subs1:substitution = (unify x x1) in
           (unify (applySubst subs1 y) (applySubst subs1 y1)) @ subs1 
       |Lst _ -> failwith "Not unifiable"
      )
      
  |Lst x -> 
      (match tau2 with
       |TypInt -> failwith "Not unifiable"
       |TypVar x1 -> if (occurCheck x1 tau1) then failwith "Not unifiable"
           else [x1, tau1]
       |Arrow (_) -> failwith "Not unifiable"
       |Lst x1 -> unify x x1
      ) 
;; 