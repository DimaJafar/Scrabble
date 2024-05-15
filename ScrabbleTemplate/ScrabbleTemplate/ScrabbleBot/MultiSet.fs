module internal MultiSet

    type MultiSet<'a when 'a : comparison> = R of Map<'a, uint32> // replace with your type

    let empty = R (Map.empty)

    let isEmpty (s: MultiSet<'a>) = 
        match s with
        R (s: Map<'a,uint32>) -> Map.isEmpty s 

    let size (R (s)) = 
        match R(s) with 
        |R(s) -> Map.values s |> Seq.sum

    let contains (a: 'a) (R (s): MultiSet<'a>) = 
        match s with
        |s -> Map.containsKey a s


    let firstKey (R (s): MultiSet<'a>) = 
        match s with 
        | s -> List.head (List.ofSeq s.Keys)


    let numItems (a: 'a) (R (s): MultiSet<'a>) =
        match Map.tryFind a s with 
        |Some a -> a
        |None -> 0u    
        

    let add (a : 'a) (n: uint32) (s : MultiSet<'a>) : MultiSet<'a> = 
       match s with 
       |R(s) -> R(Map.add a ((numItems a (R(s)))+n )s)

    let addSingle a (R(s)) =
        match s with 
        |s -> add a 1u (R(s))

    
    let remove (a : 'a) (n : uint32) (s  : MultiSet<'a>) : MultiSet<'a> = 
        match s with
        | R(s) ->
             let occ = int (numItems a (R(s))) - int n
             if occ <= 0 then
                 R(Map.remove a s)
             else
                 R(Map.add a (uint32 occ) s)

    let removeSingle (a : 'a) (R(s) : MultiSet<'a>) : MultiSet<'a> = 
        match s with
        |s -> remove a 1u (R(s))

    let fold f acc s = 
        match s with |R e -> Map.fold f acc e
    let foldBack f s acc = 
        match s with |R e -> Map.foldBack f e acc 
        
    let ofList (_ : 'a list) : MultiSet<'a> = R (Map.empty)

    let ofList1 lst = List.fold (fun acc elem -> addSingle elem acc) empty lst
    
    let toList (_ : MultiSet<'a>) : 'a list = []

    let toList1 (R(s) : MultiSet<'a>) : ('a * uint32) list = s |> Map.toList


    let map (_ : 'a -> 'b) (_ : MultiSet<'a>) : MultiSet<'b> = R (Map.empty)

    let union (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = R (Map.empty)
    let sum (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = R (Map.empty)
    
    let subtract (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = R (Map.empty)
    
    let intersection (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = R (Map.empty)
       
    