(* Question 1 *) 
let makeProtectedAccount ((openingBalance: int), (password: string)) =
  let balance = ref openingBalance in
  let pass = ref password in
  let accountClosed = ref true in
  
  fun ((pwd:string),(t:transaction)) -> 
    if (pwd = !pass && !accountClosed = true)
    then 
      match t with
      |Withdraw(amount) -> if (!balance > amount || amount = 0)
          then ((balance := !balance - amount);
                (Printf.printf "The new balance is: %i." !balance))
          else
            (Printf.printf "Insufficient funds.")
      |Deposit(amount) -> if (amount > 0)
          then
            ((balance := !balance + amount);
             (Printf.printf "The new balance is: %i." !balance))
          else
            (Printf.printf "Hey! What do you want me to do?.")
      |CheckBalance -> (Printf.printf "The balance is: %i." !balance)
      |ChangePassword(newPass) -> ((pass := newPass); 
                                   (Printf.printf "Password changed."))
      |Close -> ((accountClosed := false);
                 (Printf.printf "Account successfully closed."))
    else if (pwd <> !pass) then
      (Printf.printf "Incorrect password.")
    else (Printf.printf "Account closed.")
;;

  
  
  
  
  
  
  