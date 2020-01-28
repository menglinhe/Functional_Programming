(* Q1a: Implement pairlists. *)
let pairlists_tests = [
  (* Your test cases go here. *)
  (([],[]),[]); 
  (([1;2;3],[4;5;6]),[(1,4);(2,5);(3,6)]);
]

let rec pairlists (l1, l2) =
  match l1, l2 with
  |[],[] -> [] 
  |x::xs,y::ys -> (x,y)::pairlists(xs,ys);;

(* Q1b: Implement w_mean.*)
let w_mean_tests = [
  (* Your test cases go here. *)
  (([1.0;1.0],[1.0;1.0]),1.0);
  (([1.0],[2.0]),2.0);
  (([1.0;1.0],[3.0;3.0]),3.0);
  (([1.0;2.0],[0.0;0.0]),0.0);
  (([1.0;3.0],[-3.0;-1.0]),-1.5);
]

let w_mean weights data =
  (sumlist(List.map( fun (x,y) -> x *. y)(pairlists(weights, data)))) /.(sumlist(weights))
;;

(* Q2: Implement memberof. *)
let memberof_tests = [
  (* Your test cases go here. *)
  ((1,[]),false);
  ((1,[1;2]), true);
  ((1,[2]), false);
]

let rec memberof (x, list) =
  match list with
  | [] -> false
  | y::ys -> if y = x then true
      else memberof (x, ys)
;;

(* Q2: Implement remove.*)
let remove_tests = [
  (* Your test cases go here. *)
  ((1,[]),[]);
  ((1,[1;3;5;2]),[3;5;2]);
  ((1,[1;2;4;1;1]),[2;4]);
  ((2,[1;1]),[1;1]);
]

let rec remove (item, lst) =
  match lst with
  |[] -> [] 
  |x::xs -> if x != item then x::remove(item,xs)
      else remove(item,xs)
;;

(* Q3: Implement find_max. *)
let find_max_tests = [
  (* Your test cases go here.*)
  ([1;2;3;4;5],5);
  ([2],2);
  ([5;1;2;7;1],7);
  ([-1;-2], -1);
]

let find_max l = match l with 
  | [] -> 0
  | x::xs -> List.fold_left max x xs 
;;

(* Q4: Implement selsort. *)
let selsort_tests = [
  (* Your test cases go here. *)
  (*missing one bug*)
  ([3;2;5;4;1],[5;4;3;2;1]);
  ([],[]);
  ([3;2;1],[3;2;1]);
  ([-2;-1;-3],[-1;-2;-3]);
]

let rec selsort l =
  match l with 
  |[] -> []
  |xs -> find_max xs::(selsort (remove (find_max xs,xs)))
;;
