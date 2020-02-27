(* Question 1 *)
(* ========= DONE======== *) 
let mapTree_tests =
  [
    (((fun x -> x), Empty), Empty);
    (((fun x -> x+1), Node(Empty, 0, (Node(Empty,1,Empty)))), Node(Empty,1,(Node(Empty,2,Empty))));
    (((fun x -> x+1), Node(Node(Empty,1,Empty),0,Empty))), Node(Node(Empty,2,Empty),1,(Empty)); 
  ]
    (* mapTree : ('a -> 'b) * 'a tree -> 'b tree *)
let rec mapTree (f, (t: 'a tree)) = 
  match t with 
  |Empty -> Empty 
  |Node (l, n, r) -> Node(mapTree(f,l), (f n), mapTree(f,r))
;;


(* Question 2. *)
(* ========= DONE======== *)
let halfint_tests =
  [
    (((fun x -> x +. 1.0), 9.0, -6.0, 0.0001), -1.0000762939453125);
  ] 
    
(*  takes too long to grade -> note that the [a, b] is passed in a reverse order *)
let rec halfint ((f: float -> float), (posValue : float), (negValue : float), (epsilon : float)) = 
  let midPoint = (posValue +. negValue) /. 2.0 in
  if abs_float (f midPoint) < epsilon then midPoint
  else if (f midPoint) < 0.0 then halfint (f, posValue, midPoint, epsilon)
  else halfint (f, midPoint, negValue, epsilon) 
;;


(* Question 3. *)
(* ========= DONE======== *)
let newton_tests =
  [
    ((sin, 5.0, 0.0001, 0.0001), 9.42477);
  ]
  
let rec newton ((f: float -> float),  (guess:float), (epsilon:float), (dx:float)) =
  let close((x:float), (y:float), (epsilon:float)) = abs_float(x-.y) < epsilon in
  let improve((guess:float),f,(dx:float)) = guess -. (f guess) /. (deriv(f,dx) guess) in
  if close((f guess), 0.0, epsilon)
  then
    guess
  else
    newton (f, improve(guess,f,dx), epsilon, dx)
;;
  
(* Question 4. *)
(* ========= DONE======== *) 
let indIntegral (f, (dx:float)) =
  fun x -> integral (f, 0.0, x, dx)
;;

