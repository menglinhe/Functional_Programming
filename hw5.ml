(* Q1 Polynomials TODO: Implement the following four functions *)

let multiplyPolyByTerm (Term(c,e), Poly p) = 
  if p = [] then raise EmptyList 
  else if 
    c = 0.0 then Poly ([(0., 0)]) 
  else
    Poly (List.map(fun (a,b) -> (a *. c, b + e)) p);;
        

let addTermToPoly (Term(c,e), Poly p) =
  let rec helper l = 
    match l with 
    | [] -> if c <> 0.0 then [(c,e)] 
        else []
    |(x,y)::xs as l' -> 
        if (y > e) then (x,y)::(helper xs)
        else if (y = e) 
        then (
          if (c=0.0 -. x) then xs
          else 
            (c +. x,y)::xs)
        else 
          (c,e)::l' in 
  if p = [] then raise EmptyList else 
              
    let ans = helper p in 
    match ans with
    | [] -> Poly ([(0., 0)]) 
    | _ -> Poly ans
;;


let rec addPolys (Poly p1, Poly p2) =
  match p1, p2 with
  |[], [] -> raise EmptyList
  |[], _ -> Poly p2
  |_, [] -> Poly p1
  |(c,e)::xs,p2 -> (addPolys(Poly(xs), addTermToPoly(Term(c,e),Poly (p2))))
;; 

let rec multPolys (Poly p1, Poly p2) =
  match p1, p2 with
  |([], []) -> raise EmptyList
  |([], _) -> Poly []
  |(_, []) -> Poly []
  |((c,e)::xs, _)-> addPolys(multiplyPolyByTerm(Term(c,e),Poly (p2)), multPolys(Poly xs,Poly p2 ))
;;
   


(* Q2 References TODO: implement the `insert` function *)

let rec insert comp (item: int) (list: rlist) =
  match !list with 
  |None -> list := Some {data = item; next = ref None} 
  |Some {data = d; next = n} -> 
      if comp (item, d) 
      then
        list := Some {data = item; next = ref (Some {data = d; next = n})}
      else 
        insert comp item n 
;;

let acc (comb, f, lo, hi, inc, unit)=
  let  rec helper (a,tmp) =
    if a>hi then tmp
    else helper ((inc a), comb(tmp, (f a)))
  in
  helper(lo,unit) 
