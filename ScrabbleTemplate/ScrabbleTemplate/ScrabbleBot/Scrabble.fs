﻿namespace Cadaanka

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

    let traverse (fstChar: char) (listChars: list<char>) (dict: Dictionary.Dict) (firstMovePlayed: bool) =
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

    /// TODO Given a response, state and an auxiliary function;
    /// Manages the state accordingly with RCM / CM objects
    let handleClientMessage(msg: Response, st: State.state, auxFun: (State.state -> unit)) =
        match msg with
        | RCM (CMPlaySuccess(ms, points, newPieces)) ->
            (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
            let st' = st
            auxFun st'

        | RCM (CMPlayed (pid, ms, points)) ->
            (* Successful play by other player. Update your state *)
            let st' = st // This state needs to be updated
            auxFun st'
        | RCM (CMPlayFailed (pid, ms)) ->
            (* Failed play. Update your state *)
            let st' = st // This state needs to be updated
            auxFun st'
        | RCM (CMGameOver _) -> ()
        | RCM a -> failwith (sprintf "not implmented: %A" a)
        | RGPE err -> printfn "Gameplay Error:\n%A" err; auxFun st

    /// TODO Return a List of <ids:uint32 * occurence:uint32 * Set<char*int>>
    /// * m      : MultiSet representing the hand of a player w/ ids + no occurences
    /// * pieces : mapping id to a set of characters that can be used
    let preprocessHand (m: MultiSet<uint32>)(pieces: Map<uint32, 'a>): list<uint32 * uint32 * 'a> =
        let idsOccurence: list<uint32 * uint32> = MultiSet.toList1 m
        List.map (fun (id, occ) -> (id, occ, Map.find id pieces)) idsOccurence

    /// TODO Find possible words from a hand and returns them as a list of string.
    /// hand : list of ids/occurences/chars
    let findWords (hand: list<uint32 * uint32 * Set<char*int>>) (dict: Dictionary.Dict): list<string> = 
        /// Reduce n occurences into a list of occurence of 1
        let expandWithDuplicate (id: uint32, count: uint32, a) =
            match count with
            | count when count = 1u -> [( id , count , a)]
            | count                 -> List.replicate (int count) (id , 1u, a)
        

        let isWildcard (element: uint32 * uint32 * Set<char*int>): bool = 
            match element with
            | id, _, _ when id = 0u -> true
            | _ -> false
        
        /// Get the char of a Set<char*int>. Only used for non-wildcard char
        let getNormalChar (element: uint32 * uint32 * Set<char*int>): char = match element with (_, _, charSet) ->  charSet |> Set.minElement |> fst
        
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
        let handFlattened: list<uint32 * uint32 * Set<char*int>> = List.collect expandWithDuplicate hand
        Print.printString (sprintf "[2.FIND-WORDS][listOfHand] %A\n" handFlattened)
        
        let possibilites: list<list<char>> = buildPossibilities handFlattened [[]]
        Print.printString (sprintf "[2.FIND-WORDS][buildPossibilities] %A\n" possibilites)
            //List.map (fun (id, occ, s: Set<char * int>) -> (s |> Set.toList |> (fun x -> fst ) )) handFlattened


        
        // 2. Build permutations
        // 2. | a. Generate permutation for each list in the list<char> -> it create a list<list<list<char>>>
        // 2. | b. Flatten the list<list<list<char>>> into a list<list<char>> 
        // 3. Traverse and filter to return the final list<string>

        // N. Working code
        let startingChar : char             = 'A'
        let listChar     : list<char>       = 'A'::'P'::'H'::'N'::'A'::[]
        let permutations : list<list<char>> = Word.permute listChar
        let final        : list<string>     = Set.ofList [for permutation: char list in permutations -> Word.traverse 'A' (permutation) (dict) (true)] |> Set.toList
        List.filter (fun x -> x <> "") final

    /// Run the game
    /// * cstream : idk
    /// * pieces  : constant data transcoding IDs to set of corresponding characters
    /// * st      : game state
    let playGame (cstream: Stream) (pieces: Map<uint32, 'a>) (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (st.hand)
            
            // 1. Preprocessing: _ -> <ids:uint32 * occurence:uint32 * Set<char*int>
            let idsOccurenceSets: list<uint32 * uint32 * 'a> = preprocessHand st.hand pieces
            Print.printString (sprintf "[1.PREPROCESS] %A\n" idsOccurenceSets)
            
            // 2. Find words -- depending on my hand: <ids:uint32 * occurence:uint32 * Set<char*int> -> list<string>
            let words: list<string> = findWords idsOccurenceSets st.dict
            Print.printString (sprintf "[2.FIND-WORDS] %A\n" [])
            
            // 3. Which words can be put ? -- depending on the board
            // : (list<string>, board, optional:latest) -> list<string>
            
            // 4. Decide where to put the words (starting coord of the word + direction exemple:up=(0,1)) -- depending on the board
            // : list<string> -> list<coord * coord>
            
            // 5. Build the move
            // : ? -> list<coord * (uint32 * (char * int))>
            
            /// List of coordinates for each characters to build a word
            /// * coord  : (x,y) on the board
            /// * uint32 : ID of a tile (represents a character or wildcard)
            /// * char   : chosen character (id=0 -> could be 'a')
            /// * int    : points for the character
            let move: list<coord * (uint32 * (char * int))> = []
            
            // N. Send "Move" variable to the stream
            send cstream (SMPlay move)
           
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

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet)
        