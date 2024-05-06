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
        if firstMovePlayed then //Find ord ud fra et bogstav og ens hånd
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

        let (coordChar: list<(coord * char)>) = []

        let evenOddCounter = 0
        let isEven = evenOddCounter % 2 = 0

        let mutable firstMoveHasBeenPlaced: bool = false
        let CoordToLetter: Map<coord, char> = Map.empty


        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            //Variables
            //Coords
            

            let listOfMultiset = MultiSet.toList1 st.hand
 
            let tempIntToCharFun ( ID:uint32 , count:uint32 ) : (char * uint32) = ( char (int 'A' + (int ID) - 1) , count )
                    
                    
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
            
            let ListOfWordsWithEmpyLists = Set.ofList [for permutation: char list in ListOfPossibleCombinations -> Word.traverse 'L' (permutation) (st.dict) (firstMoveHasBeenPlaced)] |> Set.toList

            //let toList s = Set.fold (fun l se -> se::l) [] s
            
            
            let finalWordList = List.filter (fun x -> x <> "") ListOfWordsWithEmpyLists


            let wordToUse = 
                if (finalWordList).IsEmpty then
                    ""
                else
                    finalWordList.Head


            //Method to retrieve last played word, and the last character in that word
            let lastUsedChar = List.last finalWordList |> Seq.toList |> List.last



            
            

                    //Start coords
            let (FirstMoveCoord:ScrabbleUtil.coord) =  (0,0)

                //Plus two coords together like a tuple (1,0) + (2,1) = (3,1)
            let addCoords (t1:coord) (t2:coord) = coord (fst t1 + fst t2 , snd t1 + snd t2 )


            //ON EVEN 0 , 2 , 4 , 6 , 8
                // + (1,0)

            //ON ODD 1 , 3 , 5 , 7 , 9


            let rec MapCoordToLetter (coordMap:Map<coord,char>) (startCoord:coord) (charList:list<char>) = 
                match charList with
                | [] -> coordMap
                | firstLetter::remaining when firstMoveHasBeenPlaced=false -> 
                    let firstCoord = (coord (0,0))
                    let updatedCoordMap = coordMap |> Map.add firstCoord  firstLetter
                    let nextCoord = addCoords firstCoord (coord (1 ,0))
                    firstMoveHasBeenPlaced <- true
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

            let returnedMapOfCoords = MapCoordToLetter CoordToLetter FirstMoveCoord WordToList

            forcePrint (sprintf "Map of coords %A\n" returnedMapOfCoords)
            
            let piecesValues = pieces.Values |> List.ofSeq
            let accumulatingList = List.Empty

            let rec findCorrespondingPoint (ourWord:list<char>) (letter:Set<char * int>) (l:list<char*int>)=
                let t = letter |> Set.toList |> List.head //Tuple af bogstav og point
                match ourWord with
                    | [] -> l
                    | head::tail when (fst t) = head -> 
                        let newList = List.append l [t]
                        newList
                    | head::tail -> 
                        findCorrespondingPoint tail letter l

            //Den her metode havde det problem at den matche vores bogstaver i alfabetisk rækkefølge, så vi går fra et ord "GAY" til "AGY" hvilket er et problem
            let getPointsForAllLetters (ourWord:list<char>) = List.filter (fun x -> x <> []) [for eachSet in piecesValues do findCorrespondingPoint ourWord eachSet accumulatingList]


            //Den her fikser problemet fordi den nu tager 1 af vores word letter af gangen, og matcher den med piecesvalues. Men nu matcher A med wildcard A. 
            //
            let rec findCorrespondingPoint1 (ourLetter:char) (allLetters:list< Set<char * int> >) (l:list<char*int>)=
                match allLetters with
                | [] -> l
                | head::tail when (head |> Set.toList |> List.head |> fst) = ourLetter  -> 
                    let newList = List.append l [head |> Set.toList |> List.head] //This is a tuple
                    newList
                | head::tail -> 
                    findCorrespondingPoint1 ourLetter tail l


            //This is in the right order, but it matches the wildcard first?? Even though we dont have a wildcard
            let getPointsForAllLetters1 (ourWord:list<char>) = List.filter (fun x -> x <> []) [for letter in ourWord do findCorrespondingPoint1 letter piecesValues accumulatingList]

            //Det her bytter om på rækkefølgen....
            let PointsForLettersInWord = getPointsForAllLetters1 WordToList
            forcePrint (sprintf "Points for letters: \n%A\n" PointsForLettersInWord) 

            
            //DEN HER HAR EN BUG??? ignorere nogen gange duplicate letters og andre gange har den et ekstra bogstav tilføjet??
            //map [((0, 0), 'A'); ((1, 0), 'D'); ((2, 0), 'O')]  - det her er hvordan det skal se ud
            //CharPointTuples [('A', 0); ('A', 1); ('D', 2); ('O', 1)] - det her er hvordan den ser ud efter getCharPointTuple??
            //ELLERS er det findCorrespondingPoint funktionen der er buggy? 

            let rec getCharPointTuple (list:list<list<char * int>>) accumList= 
                match list with 
                | [] -> accumList
                | head::tail -> 
                    let newAccumList = List.append accumList [(head |> List.head)] 
                    getCharPointTuple tail newAccumList

            let ListOfCharPointTuples = getCharPointTuple PointsForLettersInWord List.Empty
            Print.printString (sprintf "CharPointTuples %A \n" ListOfCharPointTuples)

            
            let rec getIDToCharPointTuple (charPointList:list<char*int>) accumList=
                match charPointList with
                | [] -> accumList
                | head::tail -> 
                    let charToId = 
                        if (snd head = 0) then //Hvis en brik giver 0 point, så er den eneste brik det kan være = wildcard, med id 0
                            uint32 0
                        else
                            uint32 ((int (fst head)) - int 'A' + 1)

                    let addIdToCharIntTuple = (charToId, head)
                    let newAccumList = List.append accumList [addIdToCharIntTuple]
                    getIDToCharPointTuple tail newAccumList
            
            let ListOfIDCharPointTuple = getIDToCharPointTuple ListOfCharPointTuples List.Empty
            Print.printString (sprintf "ID added to char point %A \n" ListOfIDCharPointTuple)


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

            Print.printString (sprintf "\n Final string? \n %A " listOfCoordIdCharPointTuples )
            

            //(<x-coordinate> <y-coordinate> <piece id><character><point-value> )
           
            //let move = RegEx.parseMove input
            //let move = listOfCoordIdCharPointTuples

            

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
        