(* Question 1: Parsing *)
let parse (inputexp: string): exptree =
  let sym = ref inputexp.[0] in
  let cursor = ref 0 in

  let getsym () =
    cursor := !cursor + 1;
    sym := inputexp.[!cursor]
  in

  let rec expr (): exptree = 
    let l = term() in
    if !sym <> '+' then l
    else begin
      getsym ();
      Expr('+', l, expr())
    end
    
  and term (): exptree =
    let l = primary() in
    if !sym <> '*' then l
    else begin
      getsym ();
      Expr('*', l, term())
    end

  and primary (): exptree =
    if !sym = '('
    then begin
      getsym ();
      let result = expr () in
      if !sym <> ')' then
        failwith "Mismatched parens"
      else if !cursor = (String.length inputexp) - 1  then
        result
      else begin
        getsym ();
        result
      end
    end
    else
    if isin !sym charSet then
      if !cursor = (String.length inputexp) - 1 then
        Var !sym
      else
        let result = Var !sym in
        begin
          getsym ();
          result
        end
    else
      failwith "In primary"
  in
  expr ()

(* Question 2: Code Generation *)
let tempstore = ref 0

let codegen (e: exptree) =
  let rec helper ((e: exptree), (tag: char)) =
    match e with
    | Var c -> 
        (match tag with 
         |'=' -> Printf.printf "LOAD %c\n" c
         |'+' -> Printf.printf "ADD %c\n" c
         |'*' -> Printf.printf "MUL %c\n" c) 
    | Expr(op, l, r) ->
        if tag = '=' then
          match op with
          |'+' -> helper (l, '='); helper (r, '+')
          |'*' -> helper (l, '='); helper (r, '*')
        else begin
          tempstore := !tempstore + 1;
          (* Your code for dealing with STORE goes here *)
          Printf.printf "STORE %i\n" !tempstore;
          helper (l, '='); helper (r, op);
          (if (tag = '+') then
             Printf.printf "ADD %i\n" !tempstore
           else
             Printf.printf "MUL %i\n" !tempstore);
          tempstore := !tempstore - 1
        end
  in
  helper (e, '=')
