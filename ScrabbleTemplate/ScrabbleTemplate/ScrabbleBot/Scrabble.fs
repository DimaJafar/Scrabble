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
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board//int * int -> bool
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
    }

    let mkState b d pn h = {board = b; dict = d;  playerNumber = pn; hand = h }
    
    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand


module Word =
    let rec findWord (listOfChars: list<char>) dict (word:string) = 
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

    let permute list =
        let rec inserts e = function
            | [] -> [[e]]
            | x::xs as list -> (e::list)::(inserts e xs |> List.map (fun xs' -> x::xs'))

        List.fold (fun accum x -> List.collect (inserts x) accum) [[]] list

    let traverse fstChar listChars dict =
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

    let playGame cstream pieces (st : State.state) =

        let (coordChar: list<(coord * char)>) = []

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            // Goal: Make a first move from hand starting on coords 0,0. 
            // Steps
                // - Converting the multiset to a list, in order to access the items easilier with an index. 
            let ourHand: MultiSet<uint32> = st.hand
            let listOfMultiset = MultiSet.toList1 ourHand
            //printfn "Vores hand består af disse (ID, ANTAL) \n %A" listOfMultiset
            //[(0u, 1u); (4u, 1u); (5u, 1u); (8u, 1u); (15u, 1u); (18u, 1u); (20u, 1u)]


                // - Getting the corresponding letter (char) from the ID (uint32)
                    //This one takes a tuple (ID, count) and converts the ID to a corresponding char, and returns a new tuple (char, count)
            let tempIntToCharFun ( ID:uint32 , count:uint32 ) : (char * uint32) = ( char (int 'A' + (int ID) - 1) , count )
                    
                    //This one uses the function above on every element of the listOfMultiset, and returns a new list where ID's are now chars instead.
            let listWithCharPointTuple = List.map tempIntToCharFun listOfMultiset
            //printfn "Vores hand består af disse (CHAR, ANTAL) \n %A" listWithCharPointTuple


                // - Check the counts for possible duplicate letters and seperate them to individual items
                    // - ('A', 2) -> ('A', 1) ('A', 1)
            let expandWithDuplicate ( letter:char , count: uint32 ) =
                match count with
                | count when count = 1u -> [( letter , count )] //Do nothing to the tuple if there is no duplicate
                | count                  -> List.replicate (int count) (letter , 1u)
                //Replicate the tuple "count" times, where the count of a letter is now 1 

                    //This one applies the expand function to all elements.
            let charListWithDuplicates = 
                List.collect expandWithDuplicate listWithCharPointTuple
                
            //printfn "Det her er den nye liste med ingen char dupes %A" charListWithDuplicates
                
                    //Make a list only consisting of chars
            let listOnlyChars = charListWithDuplicates |> List.map fst
            //printfn "Liste kun med chars %A" listOnlyChars

            let ListOfPossibleCombinations = Word.permute listOnlyChars
            let unique = 
                Set.ofList [for permutation: char list in ListOfPossibleCombinations -> 
                            Word.traverse 'L' (permutation) (st.dict)]

            let toList s = Set.fold (fun l se -> se::l) [] s
            
            let finalWordList = List.filter (fun x -> x <> "") (toList unique)

            let fi = 
                if (toList unique).IsEmpty then
                    "no words"
                else
                    sprintf "There is %A elements: %A but we go with the first: %A\n" finalWordList.Length finalWordList finalWordList.Head

            Print.printString fi 
            
            let wordToUse = finalWordList.Head



            // - Start gathering info to make a move. 
            
            //Coords
            let evenOddCounter = 0
            let isEven = evenOddCounter % 2 = 0
            
            
            let mutable firstMoveHasBeenPlaced: bool = false

                    //Start coords
            let (FirstMoveCoord:ScrabbleUtil.coord) =  (0,0)


            let CoordToLetter: Map<coord, char> = Map.empty

                //Plus two coords together like a tuple (1,0) + (2,1) = (3,1)
            let addCoords (t1:coord) (t2:coord) = coord (fst t1 + fst t2 , snd t1 + snd t2 )


            //ON EVEN 0 , 2 , 4 , 6 , 8
                // + (1,0)

            //ON ODD 1 , 3 , 5 , 7 , 9



            let rec MapCoordToLetter (CoordMap:Map<coord,char>) (StartCoord:coord) (charList:list<char>) = 
                match charList with
                | [] -> CoordMap
                | firstLetter::remaining when firstMoveHasBeenPlaced=false -> 
                    let firstCoord = (coord (0,0))
                    let updatedCoordMap = CoordMap |> Map.add firstCoord  firstLetter
                    let nextCoord = addCoords firstCoord (coord (1 ,0))
                    firstMoveHasBeenPlaced <- true
                    MapCoordToLetter updatedCoordMap nextCoord remaining

                | firstLetter::remaining when firstMoveHasBeenPlaced=true && isEven=false -> //ON ODD LEVEL
                    let updatedCoordMap = CoordMap |> Map.add StartCoord firstLetter
                    let nextCoord = addCoords StartCoord (coord (0 ,1))
                    MapCoordToLetter updatedCoordMap nextCoord remaining


                | firstLetter::remaining when firstMoveHasBeenPlaced=true && isEven=true -> //ON EVEN LEVEL
                    let updatedCoordMap = CoordMap |> Map.add StartCoord firstLetter
                    let nextCoord = addCoords StartCoord (coord (1 ,0))
                    MapCoordToLetter updatedCoordMap nextCoord remaining



            let WordToList = Seq.toList wordToUse


            let returnedMapOfCoords = MapCoordToLetter CoordToLetter FirstMoveCoord WordToList

            forcePrint (sprintf "%A\n" returnedMapOfCoords)



            //St.Hand = (ID, antal:uint32)
            printfn "%A" st.hand

            //Pieces = Map <  uint32 , Set<char,int> >

            // HEJ
            // 'H'::'E'::'J'
            // 0,0 'H'
            // 1,0 'E'
            // 2,0 'J'
            // Vil have point til et bogstav:
                // Vil matche med tryFind for at få fat i pieces' set som indeholder point som dens second value. 
                    //Dette skal bruge et ID, for man skal matche med key, og pieces key er = ID af en brik
                // Vil gerne have tilsvarende value i det set, givet en char //this has a method




            let findPoint charList =
                for char in charList do
                    match Map.tryFind char pieces with
                    | 
           
            
            //(<x-coordinate> <y-coordinate> <piece id><character><point-value> )
           
            //let move = RegEx.parseMove input


            

            //RemoveTiles function, where we remove ms from hand 
            let removeTiles (hand: MultiSet<uint32>) (ms: (coord * (uint32 * (char * int))) list) =
               let rec removeTile  (tiles: (coord * (uint32 * (char * int))) list) (hand: MultiSet<uint32>)= 
                 match tiles with 
                 |[] -> hand 
                 |(_, (tileId, (_))) :: tail -> 
                 let updatedhand = removeSingle tileId hand
                 removeTile tail updatedhand 
               removeTile ms hand 

            //AddTiles function, where we add newPieces to hand
            let addTiles (hand: MultiSet<uint32>) (newPieces: (uint32 * uint32) list) =
              let rec addTile (piece: (uint32 * uint32) list) (hand: MultiSet<uint32>) = 
                 match piece with 
                 | [] -> hand 
                 | (tileId, count)  :: tail -> 
                 let updatedHand = addSingle tileId hand
                 addTile tail updatedHand  
              addTile newPieces hand   
 

                // List.fold (fun updatedHand (tileId, _)  -> addSingle tileId updatedHand ) hand newPieces
           

            //UpdateSate function, where we combine the two functions and the result is what we update st with hand with
            let updateState (st: State.state) (ms: (coord * (uint32 * (char * int))) list) (newPieces: (uint32 * uint32) list) =
                let removeTilesFromHand = removeTiles st.hand ms
                let updateHandWithNewTiles = addTiles removeTilesFromHand newPieces
                { st with hand = updateHandWithNewTiles }


            // END THE METHOD

            //Send "Move" variable to the stream 
            //send cstream (SMPlay move)


            //PRINTS
            //debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            //Receive the message 
            //Now we use the UpdateState function and use st, ms and newPieces
            let msg = recv cstream
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' = updateState st ms newPieces// This state needs to be updated
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st

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

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet)
        