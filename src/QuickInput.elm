module QuickInput exposing (..)
-- This is not used anymore
import Parser exposing (Parser, variable, map, (|.), (|=), succeed, symbol, spaces, oneOf, end, lazy, andThen, backtrackable)

import Set

maybeParser : Parser a -> Parser (Maybe a)
maybeParser p = oneOf [
               map Just p, succeed Nothing
              ]

type alias Vertex = String
type Orient = Up | Down | Left | Right
type Edge = Edge (Maybe String) (Maybe Orient)
type NonEmptyChain = Singleton Vertex
           | Cons Vertex Edge NonEmptyChain 


orientToPoint : Orient -> (Float, Float)
orientToPoint o =
    case o of
        Up   -> (0, -1)
        Down -> (0, 1)
        Left -> (-1, 0)
        Right -> (1, 0)

correctLabelChar : List Char -> Char -> Bool
correctLabelChar fb c = not <| List.member c <| fb ++ ['|', '-', '<', '>'] 

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
vertexParser = labelParser []


orientParser : Parser Orient
orientParser = oneOf
               [succeed Left  |. symbol "l", 
                succeed Right |. symbol "r", 
                succeed Up    |. symbol "u", 
                succeed Down  |. symbol "d" ]

edgeParser : Parser Edge
edgeParser = 
             oneOf[
           succeed Edge
           |. symbol "-"
           |. spaces
              -- some label?
           |= maybeParser (labelParser [])
           |. spaces
           |. symbol ">"
           |= maybeParser (succeed identity |. backtrackable (symbol "@") |= orientParser)

           ,  succeed Edge
           |. symbol "|"
           |. spaces
              -- some label?
           |= maybeParser (labelParser ['v', '^']) 
           |. spaces
           |= oneOf [
                succeed (Just Down) |. symbol "v",
                succeed (Just Up) |. symbol "^"
               ]
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

