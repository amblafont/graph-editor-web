module QuickInput exposing (equalityParser, Orient,  
   orientToPoint, orientEquation, Equation, defOrient, NonEmptyChain(..), Edge(..))
-- This is not used anymore
import Parser exposing (Parser, variable, map, (|.), (|=), succeed, symbol, spaces, oneOf, end, lazy, andThen, backtrackable)
import ParserExtra as Parser
import Set
import Html exposing (source)
import Geometry.Point as Point exposing (Point)

endArrowChar = '⟩'
type alias Vertex = String
type OrientDir = Up | Down | Left | Right
type alias Orient = { dir : OrientDir, strength : Float }

defOrient : Orient
defOrient = { dir = Right, strength = 1 }

type Edge = Edge (Maybe String) (Maybe Orient)
type NonEmptyChain = Singleton Vertex
           | Cons Vertex Edge NonEmptyChain 

{- reverseChainAcc : NonEmptyChain -> NonEmptyChain -> Edge -> NonEmptyChain
reverseChainAcc c acc edgeAcc = case c of
              Singleton v -> Cons v edgeAcc acc
              Cons v e tail -> reverseChainAcc tail (Cons v edgeAcc acc) e 


reverseChain : NonEmptyChain -> NonEmptyChain
reverseChain c = case c of
              Singleton _ -> c
              Cons v e tail -> reverseChainAcc tail (Singleton v) e
 -}
type alias Equation = (NonEmptyChain, NonEmptyChain)
lengthChain : NonEmptyChain -> Int
lengthChain c = case c of
   Singleton _ -> 0
   Cons _ _ tail -> 1 + lengthChain tail

orientChainSplitAt : Int -> Orient -> Orient -> NonEmptyChain -> NonEmptyChain 
orientChainSplitAt n firsts lasts c = case c of 
   Singleton s -> Singleton s
   Cons v (Edge s o) ch -> 
      let o2 = if n > 0 then firsts else lasts in
      Cons v (Edge s (Just (Maybe.withDefault o2 o)))
      <| orientChainSplitAt (n - 1) firsts lasts ch

{- orientChainSplit : Orient -> Orient -> NonEmptyChain -> NonEmptyChain 
orientChainSplit firsts lasts chain =
   let n = lengthChain chain in
   orientChainSplitAt (n // 2) firsts lasts chain
 -}
orientEquation : Equation -> Equation
orientEquation (source, but) =
   let nsource = lengthChain source 
       nbut    = lengthChain but
   in
   let ni1 = nsource // 2
       nf1 = nbut // 2
   in
   let ni2 = nsource - ni1
       nf2 = nbut - nf1
   in
   let maxud = max ni1 nf2 
       maxlr = max ni2 nf1
   in
   let mkOri d m n = { dir = d, strength = toFloat m / toFloat n } in
   
   (orientChainSplitAt ni1 (mkOri Right maxud ni1) (mkOri Down maxlr ni2) source,
    orientChainSplitAt nf1 (mkOri Down maxlr nf1)   (mkOri Right maxud nf2) but
    )


orientToPoint : Orient -> Point
orientToPoint {strength, dir} =
    Point.resize (Debug.log "strength" strength) <| case dir of
        Up   -> (0, -1)
        Down -> (0, 1)
        Left -> (-1, 0)
        Right -> (1, 0)

-- specialChars = ['|', '-', '<', '>'] 
correctLabelChar : List Char -> Char -> Bool
correctLabelChar fb c = not <| List.member c <| fb -- ++ ['|', '-', '<', '>'] 

labelParser : List Char -> Parser String
labelParser fb = 
    oneOf [
     succeed identity |. symbol "\"" |= 
      variable { start = \ c -> c /= '\"',
                 inner = \ c -> c /= '\"',
                 reserved = Set.empty} |. symbol "\""
    , succeed String.trim |=
            variable
              { start = correctLabelChar fb,
                inner = correctLabelChar fb,
                reserved = Set.empty}
        ]


vertexParser : Parser Vertex
vertexParser = labelParser ['-', '='] -- specialChars

{- 
orientParser : Parser Orient
orientParser = oneOf
               [succeed Left  |. symbol "l", 
                succeed Right |. symbol "r", 
                succeed Up    |. symbol "u", 
                succeed Down  |. symbol "d" ] -}

edgeParser : Parser Edge
edgeParser = 
             oneOf[
               succeed Edge
           |. symbol "-"
           |. spaces
              -- some label?
           |= Parser.maybe (labelParser [endArrowChar, '-'])
           |. spaces
           |. symbol (endArrowChar |> String.fromChar)
           |= succeed Nothing
           -- |= Parser.maybe (succeed identity |. backtrackable (symbol "@") |= orientParser)
         {-   , succeed Edge
           |. symbol "-"
           |. spaces
              -- some label?
           |= Parser.maybe (labelParser specialChars)
           |. spaces
           |. symbol ">"
           |= Parser.maybe (succeed identity |. backtrackable (symbol "@") |= orientParser)

           ,  succeed Edge
           |. symbol "|"
           |. spaces
              -- some label?
           |= Parser.maybe (labelParser (['v', '^'] ++ specialChars)) 
           |. spaces
           |= oneOf [
                succeed (Just Down) |. symbol "v",
                succeed (Just Up) |. symbol "^"
               ]
           
 -}
                 ]

nonEmptyChainParser : Parser NonEmptyChain
nonEmptyChainParser =
        vertexParser |> andThen
            (\v ->
                 succeed identity |. spaces |=
                 oneOf [ 
                  succeed (Cons v) |= backtrackable edgeParser |. spaces
                      |= (oneOf [lazy (\_ -> nonEmptyChainParser), succeed <| Singleton "" ])
                  , succeed <| Singleton v ]
            )


chainParser : Parser (Maybe NonEmptyChain)
chainParser =
    succeed identity |. spaces |=
    oneOf [ succeed Nothing |. end,
            map Just nonEmptyChainParser 
          ]

equalityParser : Parser (Maybe (NonEmptyChain, NonEmptyChain))
equalityParser =
     succeed identity |. spaces |=
     oneOf [ succeed Nothing |. end,
      succeed (\a b -> Just (a, b))      
            |= nonEmptyChainParser 
            |. spaces
            |. symbol "="
            |. spaces 
            |= nonEmptyChainParser
           ]
