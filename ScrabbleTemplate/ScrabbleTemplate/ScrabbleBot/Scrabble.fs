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

            
            //send cstream (SMPlay move)


            //PRINTS
            //debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

           
            let msg = recv cstream
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' = st
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
        