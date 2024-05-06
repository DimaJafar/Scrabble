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

    let traverse fstChar listChars dict firstMovePlayed=
        if firstMovePlayed=true then //Find ord ud fra et bogstav og ens hånd
            let dictN = 
                match Dictionary.step fstChar dict with
                | Some (_ , dictN) -> dictN
                | None -> failwith "no dict"
            
            match findWord listChars (dictN) "" with
            | word when (Dictionary.lookup (sprintf "%c%s" fstChar  word) dict) -> (sprintf "%c%s" fstChar  word)
            | word when not (Dictionary.lookup (sprintf "%c%s" fstChar  word) dict) -> ""
            | _ -> ""
        else //Find et ord ud fra ens hånd kun
            match findWord listChars dict "" with
            | word when (Dictionary.lookup word dict) -> 
                word
            | word when not (Dictionary.lookup word dict) -> 
                ""
            | _ -> ""

module Scrabble =
    open System.Threading

    let playGame cstream pieces (st : State.state) =


        let mutable evenOddCounter = 0
        let mutable isEven = evenOddCounter % 2 = 0

        let mutable firstMoveHasBeenPlaced: bool = false
        
        let mutable CoordToLetter: Map<coord, char> = Map.empty

        let mutable lastUsedChar = 'A'
        
        let mutable lastUsedCoord = (coord (0,0))

        let mutable accListOfListCharInt = List.Empty

        let mutable listOfAllLastUsedCoordTuples = List.Empty


        let rec aux (st : State.state) =

            Print.printHand pieces (State.hand st)

            let (FirstMoveCoord:ScrabbleUtil.coord) =  (0,0)

            let listOfMultiset = MultiSet.toList1 st.hand
 
            let tempIntToCharFun ( ID:uint32 , count:uint32 ) : (char * uint32) = ( char (int 'A' + (int ID) - 1) , count )
                    
            let expandWithDuplicate1 ( ID:uint32 , count: uint32 ) =
                match count with
                | count when count = 1u -> [( ID , count )] //Do nothing to the tuple if there is no duplicate
                | count                  -> List.replicate (int count) (ID , 1u)

            let listOfHand = List.collect expandWithDuplicate1 (MultiSet.toList1 st.hand)
            
            let getPiecesInHand =  List.map (fun x -> (fst x, Map.find (fst x) pieces)) listOfHand
            
            Print.printString (sprintf "%A" getPiecesInHand)
                    
            let listWithCharCountTuple = List.map tempIntToCharFun listOfMultiset
    
                // - Check the counts for possible duplicate letters and seperate them to individual items
                    // - ('A', 2) -> ('A', 1) ('A', 1)
            let expandWithDuplicate ( letter:char , count: uint32 ) =
                match count with
                | count when count = 1u -> [( letter , count )] //Do nothing to the tuple if there is no duplicate
                | count                  -> List.replicate (int count) (letter , 1u)

                    //This one applies the expand function to all elements.
            let charListWithDuplicates = List.collect expandWithDuplicate listWithCharCountTuple
 
                    //Make a list only consisting of chars
            let listOnlyChars = charListWithDuplicates |> List.map fst
            
            let ListOfPossibleCombinations = Word.permute listOnlyChars
            
            let ListOfWordsWithEmpyLists = 
                Set.ofList [for permutation: char list in ListOfPossibleCombinations -> 
                            Word.traverse lastUsedChar (permutation) (st.dict) (firstMoveHasBeenPlaced)] |> Set.toList

            
            let finalWordList = List.filter (fun x -> x <> "") ListOfWordsWithEmpyLists
    
            let wordToUse = 
                if (finalWordList).IsEmpty then
                    ""
                else
                    finalWordList.Head


            lastUsedChar <- wordToUse |> Seq.toList |> List.last

                //Plus two coords together like a tuple (1,0) + (2,1) = (3,1)
            let addCoords (t1:coord) (t2:coord) = coord (fst t1 + fst t2 , snd t1 + snd t2 )

            let rec MapCoordToLetter (coordMap:Map<coord,char>) (startCoord:coord) (charList:list<char>) = 
                match charList with
                | [] -> coordMap
                | firstLetter::remaining when (firstMoveHasBeenPlaced=false) -> 
                    let firstCoord = (coord (0,0))
                    let updatedCoordMap = coordMap |> Map.add firstCoord  firstLetter
                    let nextCoord = addCoords firstCoord (coord (1 ,0))
                    firstMoveHasBeenPlaced <-true
                    MapCoordToLetter updatedCoordMap nextCoord remaining

                | firstLetter::remaining when firstMoveHasBeenPlaced=true && isEven=false -> //ON ODD LEVEL
                    let updatedCoordMap = coordMap |> Map.add startCoord firstLetter
                    let nextCoord = addCoords startCoord (coord (0 ,1))
                    MapCoordToLetter updatedCoordMap nextCoord remaining

                | firstLetter::remaining when firstMoveHasBeenPlaced=true && isEven=true -> //ON EVEN LEVEL
                    let updatedCoordMap = coordMap |> Map.add startCoord firstLetter
                    let nextCoord = addCoords startCoord (coord (1 ,0))
                    MapCoordToLetter updatedCoordMap nextCoord remaining
                
                | _ -> coordMap
            

            let WordToList = Seq.toList wordToUse
               
            let returnedMapOfCoords =
                if firstMoveHasBeenPlaced=false then
                    MapCoordToLetter CoordToLetter FirstMoveCoord WordToList
                else
                    MapCoordToLetter CoordToLetter lastUsedCoord WordToList


            lastUsedCoord <-  fst (returnedMapOfCoords |> Map.toList |> List.last)

            let tupleToSave = fst (returnedMapOfCoords |> Map.toList |> List.last)
            listOfAllLastUsedCoordTuples <- List.append listOfAllLastUsedCoordTuples [tupleToSave]

            
            
            let piecesValues = pieces.Values |> List.ofSeq

            

            let headOfPieces = piecesValues.Head


            let newPiecesValues = List.append piecesValues.Tail [headOfPieces]


            let rec findCorrespondingPoint (ourLetter:char) (allLetters:list< Set<char * int> >) (l:list<char*int>)=
                match allLetters with
                | [] -> l
                | head::tail when (head |> Set.toList |> List.head |> fst) = ourLetter  -> 
                    let newList = List.append l [head |> Set.toList |> List.head] //This is a tuple
                    newList
                | head::tail -> 
                    findCorrespondingPoint ourLetter tail l

        
            let getPointsForAllLetters (ourWord:list<char>) = List.filter (fun x -> x <> []) [for letter in ourWord do findCorrespondingPoint letter newPiecesValues accListOfListCharInt]

            let PointsForLettersInWord = getPointsForAllLetters WordToList



            
            let rec getCharPointTuple (list:list<list<char * int>>) accumList= 
                match list with 
                | [] -> accumList
                | head::tail -> 
                    let newAccumList = List.append accumList [(head |> List.head)] 
                    getCharPointTuple tail newAccumList

            let ListOfCharPointTuples = getCharPointTuple PointsForLettersInWord List.Empty
            

            
            let rec getIDToCharPointTuple (charPointList:list<char*int>) accumList=
                match charPointList with
                | [] -> accumList
                | head::tail -> 
                    let charToId = 
                        if (snd head = 0) then
                            uint32 0
                        else
                            uint32 ((int (fst head)) - int 'A' + 1) //Change 'char' to ID

                    let addIdToCharIntTuple = (charToId, head)
                    let newAccumList = List.append accumList [addIdToCharIntTuple]
                    getIDToCharPointTuple tail newAccumList
            
            let ListOfIDCharPointTuple = getIDToCharPointTuple ListOfCharPointTuples List.Empty


            let listOfCoordTuples = Map.toList returnedMapOfCoords
            let rec getCoordToIdCharPointTuple (coordMap:list<coord * char>) (iDCharPointList:list<uint32 * (char * int)>) accumList = 
                match coordMap, iDCharPointList with
                | [], id -> accumList
                | coord, [] -> accumList
                | coordHead::coordTail , idHead::idTail -> 
                    let addCoordToIdTuple = ((fst coordHead),idHead)
                    let newAccumList = List.append accumList [addCoordToIdTuple]
                    getCoordToIdCharPointTuple coordTail idTail newAccumList

            let listOfCoordIdCharPointTuples = getCoordToIdCharPointTuple listOfCoordTuples ListOfIDCharPointTuple List.Empty

            let firstMoveTuple = List.head listOfCoordIdCharPointTuples
            let firstMoveCoord = fst firstMoveTuple

            

            let move = 
                if (List.contains firstMoveCoord listOfAllLastUsedCoordTuples) then //Dont play the first letter, as it is already on the board
                    listOfCoordIdCharPointTuples |> List.tail
                else 
                    listOfCoordIdCharPointTuples
        

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
                 let updatedHand = addSingle tileId hand
                 addTile tail updatedHand  
              addTile newPieces hand   
 
            let updateState (st: State.state) (ms: (coord * (uint32 * (char * int))) list) (newPieces: (uint32 * uint32) list) =
                let removeTilesFromHand = removeTiles st.hand ms
                let updateHandWithNewTiles = addTiles removeTilesFromHand newPieces
                { st with hand = updateHandWithNewTiles }

            
            send cstream (SMPlay move)


            //PRINTS
            //debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

   
            let msg = recv cstream
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                
                let st' = updateState st ms newPieces// This state needs to be updated
                
                firstMoveHasBeenPlaced <- true
                evenOddCounter <- evenOddCounter + 1
                isEven <- evenOddCounter % 2 = 0
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
        