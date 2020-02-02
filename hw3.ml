(* Question 1. *)

let common_tests = [
  (([],[]),[]);
  (([1],[2;3;4]),[]);
  (([1;2;3],[4]),[]);
  (([1;2;3;2],[1;2;3]),[1;2;3]);
  (([1;2;3],[3;4;5]),[3]); 
]

let rec memberof (x, list) =
  match list with
  | [] -> false
  | y::ys -> if y = x then true
      else memberof (x, ys)
;; 

let rec common twolists =
  match twolists with 
  |([],[]) -> []
  |(x,[]) -> []
  |([],y) -> []
  |(x::xs,ys) ->  if memberof(x,ys) then x::common(xs,remove(x,ys))
      else common(xs,ys)
;;

(* Question 2. Mergesort requires that you use recursion.  Using List.sort or
some other sort defeats the whole purpose.  This question is for the
implementation of split.*)

let split_tests = [
  ([],([],[]));
  ([1],([1],[]));
  ([1;2],([1],[2]))
]

let rec split l =
  match l with
  |[] -> ([],[])
  |[x] -> ([x],[])
  |(x0::x1::xs) -> let (l1, l2) = split xs in ((x0::l1), (x1::l2))
;;

(* Question 3 Here you implement merge. *)

let merge_tests = [
  (([],[]),[]);
  (([1],[]),[1]);
  (([1;2;3;5],[4]),[1;2;3;4;5])
]

let rec merge twolists =
  match twolists with
  |([],[]) -> []
  |([], list) -> list
  |(list, []) -> list
  | (x::xs, y::ys) -> if x <= y then x::merge(xs,y::ys)
      else y::merge(x::xs,ys)
;;

(* Question 4 Finally you combine split and merge and use them to implement mergesort. *)

let mergesort_tests = [
  ([],[]);
  ([1],[1]);
  ([5;2;5;1;3],[1;2;3;5;5])
]

let rec mergesort l =
  match l with 
  |[] -> []
  |[x] -> [x]
  |(x::xs) -> let l1, l2 = split l in merge(mergesort l1, mergesort l2)
;;
