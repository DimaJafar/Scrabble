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

module Scrabble =
    open System.Threading

    let playGame cstream pieces (st : State.state) =

        let (coordChar: list<(coord * char)>) = []

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            //forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            
            // ______________START THE NEW METHOD______________


            //Make a first move from hand starting on coords 0,0. 

            






            //Put coords and char in our list




            //Figuring out how step works
            let (chars:list<char>) = 'A'::'A'::'H'::[]

            let findword (chars: char list) =
                let rec wordfinder (chars:char list) (word: string)  =
                  match chars with 
                   |[] -> printfn "Found word: %s" word
                   |head::tail ->
                    match Dictionary.step head st.dict with
                        | Some (true, _) -> //det skal måske være anerledes, når den har fundet et ord skal den stop he
                            let newWord = word + string head
                            wordfinder tail newWord
                        | Some (false, _) -> wordfinder tail word
                        | None -> ()
                wordfinder chars 
                


            let result = findword chars                 
            printfn "Step result: %A"  result


            //val step : char -> Dict -> (bool * Dict) option





























            //_____________RANDOM PRINT_________
            
            //Få fat i brikker med st.hand
            //Multiset (ID, antal); (ID, antal); (ID, antal); (ID, antal); (ID, antal);  
            let ourhand: MultiSet<uint32> = st.hand
            //printfn "Vores hand består af disse (ID, ANTAL) \n %A" ourhand
            
            //Få ID af det første bogstav af vores brikker
            let (FirstID:uint32) = firstKey ourhand
            //printfn "ID af første brik i vores hand: %A" FirstID

            let NumberOf = numItems FirstID ourhand
            //printfn "Antallet af den første brik: %A" NumberOf

            //Få char af første brik, DEN HER TILGNGS MÅDE SKAL ÆNDRES, måske find ud af hvordan vi bruger characterValue?
            let tempIntToCharFun (n:int) :char = char (int 'A' + n - 1)
            let charint = tempIntToCharFun (int FirstID)
            //printfn "Bogstav i første brik i vores hand: %A" charint

          
            //Pieces contains all possible tiles and their points
            // uint32 = ID     'a = set [char, point]
            let printthis = pieces
            //printfn "Print this here: %A" printthis
            
            //Få key til pieces. Keys er ID'erne
            let piecesKeys = pieces.Keys 
            //printfn "Keys af alle brikker %A" piecesKeys

            //Alle values. Values i "pieces" er set. Et set er [('A',1)]
            let piecesValues = pieces.Values |> List.ofSeq
            //printfn "Values af alle brikker %A" piecesValues

            //Board? Se hvad board indeholder og hvad det er?
            let readBoard = st.board
            debugPrint (sprintf "Board information: %A" readBoard )


            //Få point value af en brik, ved at tage den anden værdi i et set. 
            let setValue s = 
                match Set.toList s with
                | [(_, point)] -> point

            //En liste af alle values uden den første, fordi den første er Wild Card
            let RemoveWildCard listOfValues = List.tail listOfValues
            let valuesNoWildCard = RemoveWildCard piecesValues

            //Point af en brik givet i settet.
            let PointValues = valuesNoWildCard |> List.map setValue
            //printfn "Point til hver brik i set: %A" PointValues

            //Look at a starting position on the board and a direction you want to type in.
            //Is there a tile on the board, add that tile and increase the coordinate to the next one and repeat.
            //If there is no tile on the board pick a tile from your hand







            // (coord * (uint32 * (char * int)))
            let input =  System.Console.ReadLine()

            //Save some coordinate in a variable for easier writing later
            let (FirstMoveCoord:ScrabbleUtil.coord) =  (0,0)

            
            //(<x-coordinate> <y-coordinate> <piece id><character><point-value> )
           
            let move = RegEx.parseMove input


            

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
            send cstream (SMPlay move)


            //PRINTS
            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

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
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
        //let board = ScrabbleLib.simpleBoardLangParser.parseSimpleBoardProg boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet)
        