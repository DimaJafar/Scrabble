module internal Eval

    open StateMonad
    

    (* Code for testing *)

    let hello = ('H', 4) :: ('E', 1) :: ('L', 1) :: ('L', 1) :: ('O', 1) :: []  
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    let add (a: SM<int>) (b: SM<int>) = 
        a >>= fun a1 -> 
        b >>= fun b1 -> 
        ret (a1+b1)

    let div (a: SM<int>) (b: SM<int>) = 
        a >>= (fun a1 -> 
        b >>= (fun b1 -> 
        if b1 = 0 then fail DivisionByZero else ret(a1/b1)))    

    let sub (a: SM<int>) (b: SM<int>) = 
        a >>= (fun a1 ->
        b >>= (fun b1 -> 
        ret (a1-b1)))

    let mul (a: SM<int>) (b: SM<int>) = 
        a >>= (fun a1 ->
        b >>= (fun b1 -> 
        ret (a1*b1)))

    let modulo (a: SM<int>) (b: SM<int>) = 
        a >>= (fun a1 -> 
        b >>= (fun b1 -> 
        if b1 <> 0 then ret (a1 % b1) else fail DivisionByZero))

    let aeq (a: SM<int>) (b:SM<int>)  =
       a >>= fun x ->  
       b >>= fun y -> 
        ret (x = y)

    let alt (a: SM<int>) (b:SM<int>)  =
       a >>= fun x ->  
       b >>= fun y -> 
        ret (x < y)

    let no (b:SM<bool>) = 
     b >>= fun x -> 
      ret( not x)
      
    let conj (a: SM<bool>) (b:SM<bool>)  =
       a >>= fun x ->  
       b >>= fun y -> 
        ret (x&&y)



    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    
   
    let rec arithEval (a:aExp) : SM<int> = 
     match a with 
        | N n -> ret(n)
        | V v -> lookup v
        | WL -> wordLength 
        | PV pv -> arithEval pv >>= fun x -> pointValue x
        | Add (a1, a2) -> add(arithEval a1) (arithEval a2)
        | Sub (a1, a2) -> sub(arithEval a1) (arithEval a2)
        | Mul (a1, a2) -> mul(arithEval a1) (arithEval a2)
        | Div (a1, a2) -> div(arithEval a1) (arithEval a2)
        | Mod (a1, a2) -> modulo(arithEval a1) (arithEval a2)
        | CharToInt c -> charEval c >>= fun x -> ret(int x)                                           
                                                
    and charEval (c:cExp) : SM<char> = 
        match c with
        |C c -> ret c 
        |CV cv -> arithEval cv >>= characterValue 
        |ToUpper c -> charEval c >>= (fun vc -> ret (System.Char.ToUpper(vc)))  
        |ToLower c -> charEval c >>= (fun vc -> ret (System.Char.ToLower(vc)))
        |IntToChar c -> arithEval c >>= (fun vc -> ret (char vc))


    let rec boolEval (b:bExp) : SM<bool> =
     match b with
       | TT -> ret true 
       | FF -> ret false                
       | AEq (b1, b2) -> aeq(arithEval b1) (arithEval b2)
       | ALt (b1, b2) -> alt(arithEval b1) (arithEval b2)
       | Not not -> no(boolEval not)
       | Conj (b1, b2) -> conj(boolEval b1) (boolEval b2)
       | IsVowel v -> charEval v >>= fun x -> ret (System.Char.IsDigit x)
       | IsLetter l -> charEval l  >>= fun x ->  ret (System.Char.ToLower(x) |> "aeiouyæøå".Contains)
       | IsDigit d -> charEval d >>= fun x ->  ret (System.Char.IsLetter(x)) 


        

    type stmnt =                  (* statements *)
    | Declare of string           (* variable declaration *)
    | Ass of string * aExp        (* variable assignment *)
    | Skip                        (* nop *)
    | Seq of stmnt * stmnt        (* sequential composition *)
    | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
    | While of bExp * stmnt       (* while statement *)

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *) 

(*
    let stmntToSquareFun stm = failwith "Not implemented"

    let stmntToBoardFun stm m = failwith "Not implemented"

    type squareStmnt = Map<int, stmnt>
    let stmntsToSquare stms = failwith "Not implemented"

    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"
    
    *)