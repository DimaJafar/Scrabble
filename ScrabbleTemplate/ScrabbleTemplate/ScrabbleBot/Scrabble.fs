namespace Cadaanka

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open MultiSet

open StateMonad

open ScrabbleUtil.DebugPrint

open ScrabbleLib

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

module Print =
    let printString str = forcePrint str

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    type state = {
        board         : Parser.board//int * int -> bool
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        moves         : list<coord * (uint32 * (char * int))>
    }

    let mkState b d pn h pm = {board = b; dict = d;  playerNumber = pn; hand = h; moves = pm}
    
    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand

    let moves st         = st.moves



    let removeTiles (hand: MultiSet<uint32>) (ms: (coord * (uint32 * (char * int))) list) =
        let rec removeTile  (tiles: (coord * (uint32 * (char * int))) list) (hand: MultiSet<uint32>)= 
            match tiles with 
            |[] -> hand 
            |(_, (tileId, (_))) :: tail -> 
            let updatedhand = removeSingle tileId hand
            removeTile tail updatedhand 
        removeTile ms hand 

    let addTiles (hand: MultiSet<uint32>) (newPieces: (uint32 * uint32) list) =
        let rec addTile (piece: (uint32 * uint32) list) (hand: MultiSet<uint32>) = 
            match piece with 
            | [] -> hand 
            | (tileId, count)  :: tail -> 
            let updatedHand = add tileId count hand
            addTile tail updatedHand
        addTile newPieces hand   

    let updateState (st: state) (ms: (coord * (uint32 * (char * int))) list) (newPieces: (uint32 * uint32) list) =
        //Print.printString (sprintf "\nLetters used %A \n" ms.Length)
        let removeTilesFromHand = removeTiles st.hand ms
        //Print.printString (sprintf "\nCurrent hand is of length %A\n" (removeTilesFromHand|> MultiSet.toList1 |> List.length))
        let updateHandWithNewTiles = addTiles removeTilesFromHand newPieces
        //Print.printString (sprintf "\nGot back %A tiles %A \n" newPieces.Length newPieces)
        let addUsedMoves = List.append st.moves ms
        { st with 
            hand = updateHandWithNewTiles 
            moves = addUsedMoves
        }

    let remove (hand: MultiSet<uint32>) =
        let rec removeAll (hand: MultiSet<uint32>) = 
            let l = MultiSet.toList1 hand
            match l with
            | [] -> hand
            | (id, count)::tail ->  
                let newHand = removeSingle id hand
                removeAll newHand  
        removeAll hand

            
    let updateChange (st: state) (newPieces: (uint32 * uint32) list) (hand: MultiSet<uint32>) = 
        let removeTiles = remove hand
        let updateHand = addTiles removeTiles newPieces
        { st with hand = updateHand}

module Word =
    let rec findWord (listOfChars: list<char>) dict (word:string) : string = 
        match listOfChars with
        | [] -> ""
        | head::x::tail ->
            let mutable newWord = word + head.ToString()
            match Dictionary.step head dict with
            | Some (true, newDict) ->
                newWord
            | Some (false, newDict) ->
                findWord (x::tail) newDict newWord
            | None ->
                newWord
        | head::tail ->
            let mutable newWord = word + head.ToString()
            match Dictionary.step head dict with
            | Some (true, newDict) ->
                newWord
            | Some (false, newDict) ->
                newWord
            | None ->
                newWord

    let permute list : list<list<'a>> =
        let rec inserts e = function
            | [] -> [[e]]
            | x::xs as list -> (e::list)::(inserts e xs |> List.map (fun xs' -> x::xs'))

        List.fold (fun accum x -> List.collect (inserts x) accum) [[]] list

    let traverse (st: State.state) (fstChar: char) (listChars: list<char>) (dict: Dictionary.Dict) : string =
        if st.moves.IsEmpty then //Find ord fra ens hånd kun, fordi der ikke er blevet spillet før
            match findWord listChars dict "" with
            | word when (Dictionary.lookup word dict) -> 
                word
            | word when not (Dictionary.lookup word dict) -> 
                ""
            | _ -> ""
        else //Find et ord ud fra sidste bogstav spillet. 
            //Print.printString (sprintf "[2. MAKING WORD FROM] (%A)\n" fstChar)
            let dictN = 
                match Dictionary.step fstChar dict with
                | Some (_ , dictN) -> dictN
                | None -> failwith "no dict"
            
            match findWord listChars (dictN) "" with
            | word when (Dictionary.lookup (sprintf "%c%s" fstChar  word) dict) -> (sprintf "%c%s" fstChar  word)
            | word when not (Dictionary.lookup (sprintf "%c%s" fstChar  word) dict) -> ""
            | _ -> ""

module Scrabble =
    open System.Threading

    /// Given a response, state and an auxiliary function, it manages the state accordingly with RCM / CM objects
    ///
    /// #### Parameters
    /// * msg (Response)               : Output from trying to run moves
    /// * st (State.state)             : State of the game
    /// * auxFun (State.state -> unit) : A function that runs the game logic
    let handleClientMessage (msg: Response, st: State.state, auxFun: (State.state -> unit)) =
        match msg with
        | RCM (CMPlaySuccess(ms, points, newPieces)) ->
            (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
            let st' =  State.updateState st ms newPieces
            auxFun st'

        | RCM (CMPlayed (pid, ms, points)) ->
            (* Successful play by other player. Update your state *)
            let st' = st // This state needs to be updated
            auxFun st'
        | RCM (CMChangeSuccess(newTiles)) ->
            printfn "NEWTILES :%A" newTiles;
            let st' = State.updateChange st newTiles st.hand
            auxFun st'
        | RCM (CMChange(playerId, numberOfTiles)) ->
            printfn "Tryna %A change tiles" numberOfTiles;
            let st' = st 
            auxFun st'
        | RCM (CMPlayFailed (pid, ms)) -> 
            (* Failed play. Update your state *)
            let st' = st // This state needs to be updated
            auxFun st'
        | RCM (CMGameOver _) -> ()
        | RCM a -> failwith (sprintf "not implmented: %A" a)
        | RGPE err -> printfn "Gameplay Error:\n%A" err; auxFun st

    /// Given the hand in its raw dat format and the dictionary that transcode an id into a Set<char * int>
    ///
    /// #### Parameters
    /// * m (MultiSet<uint32>)     : MultiSet representing the hand of a player w/ ids + no occurences
    /// * pieces (Map<uint32, 'a>) : Dictionary transcoding an id to a set of characters and the associated points
    /// 
    /// #### Returns
    /// (list<uint32 * uint32 * Set<char * int>>) List of a tuple containing (id, no. tiles, Set of (character, point))
    let preprocessHand (m: MultiSet<uint32>)(pieces: Map<uint32, 'a>): list<uint32 * uint32 * 'a> =
        let idsOccurence: list<uint32 * uint32> = MultiSet.toList1 m
        List.map (fun (id, occ) -> (id, occ, Map.find id pieces)) idsOccurence

    /// Find possible words from a hand and returns them as a list of string.
    /// 
    /// #### Parameters
    /// * st (State.state)                             : State of the game
    /// * hand (list<uint32 * uint32 * Set<char*int>>) : Hand of the player as a list of a tuple containing (id, no. tiles, Set of (character, point))
    /// * dict (dict: Dictionary.Dict)                 : Dictionary of words that exists for this game
    /// 
    /// #### Returns
    /// (list<string>) List of words that can be placed
    let findWords (st: State.state) (hand: list<uint32 * uint32 * Set<char*int>>) (dict: Dictionary.Dict): list<string> = 
        /// Reduce n occurences into a list of occurence of 1
        let expandWithDuplicate (id: uint32, count: uint32, a) =
            match count with
            | count when count = 1u -> [( id , count , a)]
            | count                 -> List.replicate (int count) (id , 1u, a)
        
        //Helper for checking if a letter element is a wildcard -> for when we need to consider all the letters in the wildcard
        let isWildcard (element: uint32 * uint32 * Set<char*int>): bool = 
            match element with
            | id, _, _ when id = 0u -> true
            | _ -> false
        
        /// Get the char of a Set<char*int>. Only used for non-wildcard char
        let getNormalChar (element: uint32 * uint32 * Set<char*int>): char = match element with (_, _, charSet) ->  charSet |> Set.minElement |> fst
        
        /// Get all chars of the wildcard in a list.
        let getWildCardChars (element: uint32 * uint32 * Set<char*int>): list<char> = 
            match element with (_, _, charSet: Set<char * int>) ->  
                charSet |> Set.toList |> List.map (fun x -> fst x)

        /// Build stuff
        let rec buildPossibilities (h: list<uint32 * uint32 * Set<char*int>>) (acc: list<list<char>>): list<list<char>> =
            match h with
            | [] -> acc
            | head::tail ->
                match isWildcard head with
                | true  ->
                    let lettersWildcard : list<char> = getWildCardChars head
                    let appendCharList (x: char) (l: list<char>): list<char> = List.append l [x]
                    let newAcc: list<list<char>> = List.collect (fun (x: char) -> List.map (fun (y: list<char>) -> appendCharList x y) acc) lettersWildcard
                    
                    buildPossibilities (tail) (newAcc)

                | false -> 
                    let currentChar : char = getNormalChar head
                    let newAcc      : list<list<char>> = List.map (fun x -> List.append x [currentChar]) acc
                    buildPossibilities (tail) (newAcc)

        // 1. Get a list<list<char>> -> possibilities taking wildcard into account
        let handFlattened : list<uint32 * uint32 * Set<char*int>> = List.collect expandWithDuplicate hand
        // If there is a wildCard, theres 26 lists of chars inside the list. If no wildcard, theres just 1 list of chars.
        let possibilites  : list<list<char>>                      = buildPossibilities handFlattened [[]]
        
        // 2. | a. Generate permutation for each list in the list<char> -> it create a list<list<list<char>>>
        let permutationsPossibilities: list<list<list<char>>>     = List.map (fun x -> Word.permute x) possibilites
        
        // 2. | b. Flatten the list<list<list<char>>> into a list<list<char>>
        let possibilitesFlattened: list<list<char>>             = List.concat permutationsPossibilities
        
        // 3. Traverse and filter to return the final list<string>
        //Print.printString (sprintf "[2. BEFORE]\n" )
        let latestMoveChar = 
            if st.moves.IsEmpty then
                //Print.printString (sprintf "[2. IS EMPTY] (%A)\n" st.moves)
                '-'
            else 
                //Print.printString (sprintf "[2. MAKE WORD FROM] (%A)\n" (List.last st.moves |> snd |> snd |> fst))
                List.last st.moves |> snd |> snd |> fst //Get the char of the last used move
    
        let possibleWords: list<string> = Set.ofList [for permutation: list<char> in possibilitesFlattened -> Word.traverse st latestMoveChar (permutation) (dict)] |> Set.toList
        //Print.printString (sprintf "[2. Possible?] (%A)\n" possibleWords)
        if possibleWords.IsEmpty then
            //Print.printString (sprintf "[2. NO WORDS] (%A)\n" possibleWords)
            ["HELLO"]
        else 
            //Print.printString (sprintf "[2. WORDS] (%A)\n" (List.filter (fun x -> x <> "") possibleWords))
            List.filter (fun x -> x <> "") possibleWords


    let findWordsAgain (st: State.state) (hand: list<uint32 * uint32 * Set<char*int>>) (dict: Dictionary.Dict): list<string> = 
        /// Reduce n occurences into a list of occurence of 1
        let expandWithDuplicate (id: uint32, count: uint32, a) =
            match count with
            | count when count = 1u -> [( id , count , a)]
            | count                 -> List.replicate (int count) (id , 1u, a)
        
        //Helper for checking if a letter element is a wildcard -> for when we need to consider all the letters in the wildcard
        let isWildcard (element: uint32 * uint32 * Set<char*int>): bool = 
            match element with
            | id, _, _ when id = 0u -> true
            | _ -> false
        
        /// Get the char of a Set<char*int>. Only used for non-wildcard char
        let getNormalChar (element: uint32 * uint32 * Set<char*int>): char = match element with (_, _, charSet) ->  charSet |> Set.minElement |> fst
        
        /// Get all chars of the wildcard in a list.
        let getWildCardChars (element: uint32 * uint32 * Set<char*int>): list<char> = 
            match element with (_, _, charSet: Set<char * int>) ->  
                charSet |> Set.toList |> List.map (fun x -> fst x)

        /// Build stuff
        let rec buildPossibilities (h: list<uint32 * uint32 * Set<char*int>>) (acc: list<list<char>>): list<list<char>> =
            match h with
            | [] -> acc
            | head::tail ->
                match isWildcard head with
                | true  ->
                    let lettersWildcard : list<char> = getWildCardChars head
                    let appendCharList (x: char) (l: list<char>): list<char> = List.append l [x]
                    let newAcc: list<list<char>> = List.collect (fun (x: char) -> List.map (fun (y: list<char>) -> appendCharList x y) acc) lettersWildcard
                    
                    buildPossibilities (tail) (newAcc)

                | false -> 
                    let currentChar : char = getNormalChar head
                    let newAcc      : list<list<char>> = List.map (fun x -> List.append x [currentChar]) acc
                    buildPossibilities (tail) (newAcc)

        // 1. Get a list<list<char>> -> possibilities taking wildcard into account
        let handFlattened : list<uint32 * uint32 * Set<char*int>> = List.collect expandWithDuplicate hand
        // If there is a wildCard, theres 26 lists of chars inside the list. If no wildcard, theres just 1 list of chars.
        let possibilites  : list<list<char>>                      = buildPossibilities handFlattened [[]]
        
        // 2. | a. Generate permutation for each list in the list<char> -> it create a list<list<list<char>>>
        let permutationsPossibilities: list<list<list<char>>>     = List.map (fun x -> Word.permute x) possibilites
        
        // 2. | b. Flatten the list<list<list<char>>> into a list<list<char>>
        let possibilitesFlattened: list<list<char>>             = List.concat permutationsPossibilities
        
        // 3. Traverse and filter to return the final list<string>
        //Print.printString (sprintf "[2. BEFORE]\n" )
        let latestMoveChar = 
            if st.moves.IsEmpty then
                //Print.printString (sprintf "[2. IS EMPTY] (%A)\n" st.moves)
                '-'
            else 
                List.rev st.moves |> List.item 1 |> snd |> snd |> fst
        
        let possibleWords: list<string> = Set.ofList [for permutation: list<char> in possibilitesFlattened -> Word.traverse st latestMoveChar (permutation) (dict)] |> Set.toList
        //Print.printString (sprintf "[2. Possible?] (%A)\n" possibleWords)
        if possibleWords.IsEmpty then
            //Print.printString (sprintf "[2. NO WORDS] (%A)\n" possibleWords)
            ["HELLO"]
        else 
            //Print.printString (sprintf "[2. WORDS] (%A)\n" (List.filter (fun x -> x <> "") possibleWords))
            List.filter (fun x -> x <> "") possibleWords

    /// Filter and returns the letters in the hand that can be used to buid the given word.
    /// 
    /// #### Parameters
    /// * st /State.state)                                      : State of the game
    /// * ourWord (string)                                      : Word to build
    /// * lettersInHand (list<uint32 * uint32 * Set<char*int>>) : Hand to use
    /// * accList (list<uint32 * uint32 * Set<char*int>>)       : Accumulator building the result
    /// 
    /// ### Returns
    /// (list<uint32 * uint32 * Set<char*int>>) Like the hand but only with the elements building the word
    let rec findCorrespondingPoint (st: State.state) (ourWord: string) (lettersInHand: list<uint32 * uint32 * Set<char*int>>) (accList: list<uint32 * uint32 * Set<char*int>>): list<uint32 * uint32 * Set<char*int>> =
        let findWildCardMatch (listTuple:list<char*int>) (letter:char) : (char * int) = List.filter (fun pair -> fst pair = letter) listTuple |> List.head
        let lettersReversed = List.rev lettersInHand
        //Print.printString (sprintf "[2.Before MATCH] Tiles in hand %A\n" lettersReversed)
        match ourWord with
        | "" -> accList
        | _  -> 
            //Print.printString (sprintf "[2.Current word = %A \n" ourWord)
            //Takes the letters of the word that we want to match with our tiles. First letter and then the rest.
            //If the word is continuing from another, start from the second letter of the word index 1.
            //let isFirstMove : int = if st.moves.IsEmpty || ourWord.Length <= 2 then 0 else 1 //ourWord.Length <= 2 
            //Print.printString (sprintf "[2.isfirstmove] %A\n" isFirstMove)
            let ourLetter   : char   = ourWord.[0]
            //Print.printString (sprintf "[2.Letter of word] %A\n" ourLetter)
            let restOurWord : string = ourWord.Substring(1)
            match lettersReversed with
            | [] ->
                accList

            | (id, occ, set)::tail when id <> 0u && (set |> Set.toList |> List.head |> fst) = ourLetter  ->
                //Print.printString (sprintf "[2.MATCHING not wildcard] %A\n" (id, occ, set))
                let newAccList: list<uint32 * uint32 * Set<char*int>> =
                    match occ with
                    | 1u -> List.append accList [(id, occ,    set)]
                    | _  -> List.append accList [(id, occ-1u, set)]
                
                let newHand: list<uint32 * uint32 * Set<char*int>> =
                    match occ with
                    | 1u -> tail
                    | _  -> List.append tail [(id, occ-1u, set)]

                findCorrespondingPoint (st) (restOurWord) (List.rev newHand) (newAccList)
            
            | (id, occ, set)::tail when id  = 0u && (fst (findWildCardMatch (Set.toList set) ourLetter) = ourLetter) ->
                Print.printString (sprintf "[2.MATCHING IS wildcard] %A\n" (id, occ, set))
                let matched : char * int                               =  findWildCardMatch (Set.toList set) ourLetter
                let newSet  : Set<char * int>                          = Set.singleton matched
                let newList : (uint32 * uint32 * Set<char * int>) list = List.append accList [(id, occ, newSet)]
                findCorrespondingPoint (st) (restOurWord) (List.rev tail) (newList)
            
            | head::tail ->
                // Put the word back behind
                let newHand: (uint32 * uint32 * Set<char * int>) list = List.append tail [head]
                findCorrespondingPoint (st) ourWord (List.rev newHand) accList
    
    /// Adds the coordinates to the hand where the hand can be placed according to the game state.
    /// Note : *the word is placed at the end of one another.*
    /// 
    /// #### Parameters
    /// * st (State.state)                             : State of the game
    /// * hand (list<uint32 * uint32 * Set<char*int>>) : Hand of the player
    /// 
    /// ### Returns
    /// (: list<coord * (uint32 * (char * int))>) 
    let addCoordinates (st: State.state) (hand: list<uint32 * uint32 * Set<char*int>>) : list<coord * (uint32 * (char * int))> =
         /// Returns the direction to take from the list of moves. The result is represented as a coord but used as a vector.
        let getDirection (moves: list<coord * (uint32 * (char * int))>): coord =
            let (.-.) (a:coord) (b:coord) : coord = (fst a - fst b , snd a - snd b)
            let rev (a:coord) : coord = coord (snd a, fst a)
            match moves.Length with
            | 0 -> coord (1, 0)
            | _ ->
                let (coord1, _) = List.last moves
                let (coord2, _) = moves |> List.rev |> List.tail |> List.rev |> List.last
                rev (coord1 .-. coord2)
        
        let handToMatchCoords: list<uint32 * (char * int)> = List.map (fun (id, occ, set) -> (id, (set |> Set.minElement)) ) hand
        //Print.printString (sprintf "[3.handMatchCoords %A\n" handToMatchCoords)
        
        let direction     : coord = getDirection st.moves
        //Print.printString (sprintf "[3.getDirection %A\n" direction)

        let coordAdd (a: coord) (b: coord) :coord = (fst a + fst b, snd a + snd b)

        let rec buildPath (latest: coord) (length: int) (acc: list<coord>): list<coord> =
            //Print.printString (sprintf "[3.handCoords Buildpath %A %A\n" length acc)    
            if length = 0 then 
                acc
            else
                buildPath ( coordAdd latest direction) (length - 1) (List.append acc [latest])
        
        let handCoords =
            if st.moves.IsEmpty then
                let handSeq = {0..handToMatchCoords.Length-1} |> Seq.toList
                List.map (fun x -> coord (x, 0)) handSeq
            else 
                let lastCoord: coord = List.last st.moves |> fst
                //Print.printString (sprintf "[3.lastCoord %A\n" lastCoord)
                buildPath (coordAdd lastCoord direction) (handToMatchCoords.Length) ([])
                
        //Print.printString (sprintf "[3.handCoords %A\n"  handCoords)
   
        List.zip handCoords handToMatchCoords
    
    let addCoordinatesAgain (st: State.state) (hand: list<uint32 * uint32 * Set<char*int>>) : list<coord * (uint32 * (char * int))> =
        /// Returns the direction to take from the list of moves. The result is represented as a coord but used as a vector.
        let getDirection (moves: list<coord * (uint32 * (char * int))>): coord =
            //Print.printString (sprintf "[3.LIST OF USED COORDS %A\n" moves)
            let (.-.) (a:coord) (b:coord) : coord = (fst a - fst b , snd a - snd b)
            let rev (a:coord) : coord = coord (snd a, fst a)
            match moves.Length with
            | 0 -> coord (1, 0)
            | _ ->
                let (coord1, _) = List.last moves
                //Print.printString (sprintf "[3.COORD TO MINUS 1 %A\n" coord1)
                let (coord2, _) = moves |> List.rev |> List.tail |> List.rev |> List.last
                //Print.printString (sprintf "[3.COORD TO MINUS 2 %A\n" coord2)
                rev (coord1 .-. coord2)
        
        let handToMatchCoords: list<uint32 * (char * int)> = List.map (fun (id, occ, set) -> (id, (set |> Set.minElement)) ) hand
        //Print.printString (sprintf "[3.handMatchCoords %A\n" handToMatchCoords)
        

        let direction     : coord = getDirection (st.moves) //(List.rev st.moves |> List.tail |> List.rev)
        //Print.printString (sprintf "[3.getDirection %A\n" direction)

        let coordAdd (a: coord) (b: coord) :coord = (fst a + fst b, snd a + snd b)

        let rec buildPath (latest: coord) (length: int) (acc: list<coord>): list<coord> =
            //Print.printString (sprintf "[3.handCoords Buildpath %A %A\n" length acc)    
            if length = 0 then 
                acc
            else
                buildPath ( coordAdd latest direction) (length - 1) (List.append acc [latest])
        
        let handCoords =
            if st.moves.IsEmpty then
                let handSeq = {0..handToMatchCoords.Length-1} |> Seq.toList
                List.map (fun x -> coord (x, 0)) handSeq
            else 
                let lastCoord: coord = List.rev st.moves |> List.tail |> List.head |> fst
                //Print.printString (sprintf "[3.lastCoord %A\n" lastCoord)
                buildPath (coordAdd lastCoord direction) (handToMatchCoords.Length) ([])
                
        //Print.printString (sprintf "[3.handCoords %A\n"  handCoords)

        List.zip handCoords handToMatchCoords

    /// Run the game
    /// 
    /// #### Parameters
    /// * cstream : Iterator-like datastructure holding the player's choice
    /// * pieces  : Data that transcodes IDs to set of corresponding characters
    /// * st      : State of the game
    let playGame (cstream: Stream) (pieces: Map<uint32, 'a>) (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (st.hand)

            // 1. Preprocessing unexploiteable datastructures
            let idsOccurenceSets: list<uint32 * uint32 * 'a> = preprocessHand st.hand pieces
            //Print.printString (sprintf "[1.PREPROCESS] %A\n" idsOccurenceSets)
            //Print.printString (sprintf "\n[HAND LENGTH %A\n" idsOccurenceSets.Length)

            
            // 2. Find words that can be buildt and placed
            let words       : list<string>               = findWords st idsOccurenceSets st.dict
            let wordsAgain = 
                if words.IsEmpty && (not st.moves.IsEmpty) then
                    findWordsAgain st idsOccurenceSets st.dict
                else 
                    []

            if words.IsEmpty && wordsAgain.IsEmpty && (not st.moves.IsEmpty) then
                // Print.printString (sprintf "[2.Words && WordsAgain EMPTY]\n")
                //Replicated from findWords helper function, because its easier...
                let expandWithDuplicate (id: uint32, count: uint32, a) =
                    match count with
                    | count when count = 1u -> [( id , count , a)]
                    | count                 -> List.replicate (int count) (id , 1u, a)
                let flattenedhand =  List.collect expandWithDuplicate idsOccurenceSets
                let listOfIDs = List.map (fun (id, _ , _ ) -> (id)) flattenedhand

                send cstream (SMChange listOfIDs)
            
            else if (not words.IsEmpty) then
                // Print.printString (sprintf "[2.Words HAS WORD]\n")
                let chosenWord  : string                     = words |> List.head //First word in list
                // Print.printString (sprintf "[2.CHOSEN] %A\n" chosenWord)
                let handMatched : list<uint32 * uint32 * Set<char * int>> = 
                    if st.moves.IsEmpty then
                        findCorrespondingPoint st chosenWord idsOccurenceSets []
                    else 
                        findCorrespondingPoint st (chosenWord.Substring(1)) idsOccurenceSets []
                // Print.printString (sprintf "[2.FIND-WORDS] (%A) %A\n" words.Length words)
                // Print.printString (sprintf "[2.FIND-WORDS] (CORRESPONDING '%A') %A\n" chosenWord handMatched)
                
                // 3. Build the next move
                let move: list<coord * (uint32 * (char * int))> = addCoordinates st handMatched
                
                // 4. Send "Move" variable to the stream
                //Print.printString (sprintf "[3.EXCISTING MOVES %A\n" st.moves)
                // Print.printString (sprintf "[3.Used tiles %A\n" st.moves.Length)
                // Print.printString (sprintf "[3.SENDING MOVES %A\n" move)
                // Print.printString (sprintf "[3. NR LETTERS USED] %A\n" move.Length)
                send cstream (SMPlay move)
            else if words.IsEmpty && (not wordsAgain.IsEmpty) then 
                // Print.printString (sprintf "[2.Words EMPTY, wordsAgain HAS WORD]\n")
                let chosenWord  : string                     = wordsAgain |> List.head //First word in list
                // Print.printString (sprintf "[2.CHOSEN] %A\n" chosenWord)
                let handMatched : list<uint32 * uint32 * Set<char * int>> = 
                    if st.moves.IsEmpty then
                        findCorrespondingPoint st chosenWord idsOccurenceSets []
                    else 
                        findCorrespondingPoint st (chosenWord.Substring(1)) idsOccurenceSets []
                // Print.printString (sprintf "[2.FIND-WORDS] (%A) %A\n" words.Length words)
                // Print.printString (sprintf "[2.FIND-WORDS] (CORRESPONDING '%A') %A\n" chosenWord handMatched)
                
                // 3. Build the next move
                let move: list<coord * (uint32 * (char * int))> = addCoordinatesAgain st handMatched
                
                let expandWithDuplicate (id: uint32, count: uint32, a) =
                    match count with
                    | count when count = 1u -> [( id , count , a)]
                    | count                 -> List.replicate (int count) (id , 1u, a)
                let flattenedhand =  List.collect expandWithDuplicate idsOccurenceSets
                let listOfIDs = List.map (fun (id, _ , _ ) -> (id)) flattenedhand


                let rec checkCoordInList (moves) (matchCoord: coord) = 
                    match moves with
                    | [] -> false
                    | (listCoord, _ )::tail when listCoord = matchCoord -> 
                        true
                    | (listCoord, _ )::tail -> checkCoordInList tail matchCoord

                let getDirection (moves: list<coord * (uint32 * (char * int))>): coord =
                    // Print.printString (sprintf "[3.LIST OF USED COORDS %A\n" moves)
                    let (.-.) (a:coord) (b:coord) : coord = (fst a - fst b , snd a - snd b)
                    let rev (a:coord) : coord = coord (snd a, fst a)
                    match moves.Length with
                    | 0 -> coord (1, 0)
                    | _ ->
                        let (coord1, _) = List.last moves
                        // Print.printString (sprintf "[3.COORD TO MINUS 1 %A\n" coord1)
                        let (coord2, _) = moves |> List.rev |> List.tail |> List.rev |> List.last
                        // Print.printString (sprintf "[3.COORD TO MINUS 2 %A\n" coord2)
                        (coord1 .-. coord2)
               
                // 4. Check for surroundings
                let sendThisSM = 
                    match move with
                    | head::tail ->
                            let ((coordX, coordY) : coord) = fst head 

                            let direction = getDirection move
                            // Print.printString (sprintf "[4.1] DIRECTION %A\n" direction)

                            if (direction = (0,1)) then
                                let checkThisCoord = (coordX - 1, coordY)
                                if checkCoordInList st.moves checkThisCoord then
                                    // Print.printString (sprintf "[4. CANT PLACE -> CHANGE ]\n")
                                    (SMChange listOfIDs)
                                else
                                    // Print.printString (sprintf "[4. CAN PLACE SECOND LAST ]\n")
                                    (SMPlay move)
                            else if (direction = (1,0)) then
                                let checkThisCoord = (coordX, coordY-1)
                                if checkCoordInList st.moves checkThisCoord then
                                    // Print.printString (sprintf "[4. CANT PLACE -> CHANGE ]\n")
                                    (SMChange listOfIDs)
                                else
                                    // Print.printString (sprintf "[4. CAN PLACE SECOND LAST ]\n")
                                    (SMPlay move)
                            else 
                                    (SMChange listOfIDs)


                // 5. Send "Move" variable to the stream
                //Print.printString (sprintf "[3.EXCISTING MOVES %A\n" st.moves)
                // Print.printString (sprintf "[3.Used tiles %A\n" st.moves.Length)
                // Print.printString (sprintf "[3.SENDING MOVES %A\n" move)
                // Print.printString (sprintf "[3. NR LETTERS USED] %A\n" move.Length)
                send cstream (sendThisSM)
           
            let msg: Response = recv cstream
            handleClientMessage(msg, st, aux)

        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        // debugPrint 
        //     (sprintf "Starting game!
        //               number of players = %d
        //               player id = %d
        //               player turn = %d
        //               hand =  %A
        //               timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
        //let board = ScrabbleLib.simpleBoardLangParser.parseSimpleBoardProg boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
        
        let playedMoves: list<coord * (uint32 * (char * int))> = []

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet playedMoves)
        