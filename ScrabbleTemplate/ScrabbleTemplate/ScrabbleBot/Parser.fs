// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open Eval

    //Uses FParsecLight
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "not implemented"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "space"
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 .>> spaces >>. p2

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'

    let parenthesise2 p = pchar '{' .>*> p >*>. pchar '}'

    let charListToStr (x: char list) = System.String(x |> List.toArray)

    let pid = pletter <|> pchar '_' .>>. many (palphanumeric <|> pchar '_') |>> fun (a,b) -> charListToStr(a::b)

    let unop op a = op >*>. a  
    let binop op a b = a .>*> op .>*>. b

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"

    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"

    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"

    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let AexpParse = TermParse 

    let NParse = pint32 |>> N <?> "Int"
    let VParse = pid |>> V <?> "V"
    let NegParse = unop (pchar '-') AtomParse |>> (fun x -> Mul ((N -1) , x)) <?> "Neg"
    let ParParse = parenthesise TermParse
    let PVParse = unop pPointValue (parenthesise TermParse) |>> PV <?> "PV"

    let CParse, cref = createParserForwardedToRef<cExp>()

    let CharParse = pchar ''' >>. anyChar .>> pchar ''' |>> C <?> "C"

    let toUppperParse = unop pToUpper (parenthesise CParse) |>> ToUpper <?> "ToUpper"

    let toLowerParse = unop pToLower (parenthesise CParse) |>> ToLower <?> "ToLower"

    let CharValParse = unop pCharValue (parenthesise TermParse) |>> CV <?> "CV" 

    let IntToCharParse = unop pIntToChar (parenthesise TermParse) |>> IntToChar <?> "IntToChar"
    do cref := choice [CharValParse; IntToCharParse; toUppperParse; toLowerParse; CharParse]

    let CharToIntParse = unop pCharToInt (parenthesise CParse) |>> CharToInt <?> "CharToInt"
    do aref := choice [CharToIntParse; NegParse; PVParse; VParse; NParse; ParParse]

    let CexpParse = CParse

    let BexpParse = pstring "not implemented"

    let stmntParse = pstring "not implemented"


    let parseSquareProg _ = failwith "not implemented"

    let parseBoardProg _ = failwith "not implemented"

  
    (* The rest of your parser goes here *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }

    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
