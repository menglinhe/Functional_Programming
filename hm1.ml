:Homework 1
(* -----Q1----- *)
let double_tests = [
  (0, 0);
  (1, 2);
  (3, 6);
]

let rec double n = match n with
  | 0 -> 0 
  | n -> 2 + double (n - 1)


let fact_tests = [
  (0,1.0);
  (1,1.0);
  (2,2.0);
  (3,6.0); 
]


let rec fact (n: int): float = match n with
  | 0 -> 1.0
  | _ -> float_of_int n *. fact (n - 1)

(* -----Q2----- *)
let mysqrt_tests = [
  (* Your test cases go here. *)
  (0.0, 0.0);
  (1.0, 1.0);
  (4.0, 2.0);
]

let mysqrt (x:float) = 
  let rec helper g =
    if close(x,square g) then g
    else helper((g +. (x /.g)) /. 2.0)
  in 
  helper(x /. 2.0)

(* -----Q3----- *)
let cube_root_tests = [
  (* Your test cases go here. *)
  (0.0, 0.0);
  (1.0, 1.0);
  (8.0, 2.0);
]

let cube_root (x:float) = 
  let rec helper g = 
    if close(x, cube g) then g
    else helper((2.0 *. g +. (x /. square(g))) /. 3.0)
  in 
  helper(x /. 2.0)

(* -----Q4----- *)
let fast_exp_tests = [
  (* Your test cases go here. *)
  ((0,0),0); 
  ((5,0),1);
  ((1,3),1);
]

let rec fast_exp_aux (base, power, acc) = 
  if base = 0 then 0
  else if power = 0 then acc
  else if ( power mod 2 = 1) then 
    fast_exp_aux (base * base, (power-1)/2, acc * base)
  else
    fast_exp_aux(base * base, power/2, acc)
    

let fast_exp (base, power) = 
  fast_exp_aux (base, power, 1)
    
      
                                         

                           

