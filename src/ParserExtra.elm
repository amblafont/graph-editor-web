module ParserExtra exposing (untilConsume, maybe, repeat, repeatSpaces)
import Parser exposing (Parser, token, oneOf, succeed, end, spaces, lazy, 
       map, (|.), (|=))

untilConsume : String -> Parser ()
untilConsume s = Parser.chompUntil s |. token s

maybe : Parser a -> Parser (Maybe a)
maybe p = oneOf [
               map Just p, succeed Nothing
              ]

-- repeat until it fails
repeat p = oneOf [ -- succeed [] |. end,
                   succeed (::) |= p |= lazy (\ _ -> repeat p)
              ,    succeed [] ]

repeatSpaces p = repeat (succeed identity |. spaces |= p |. spaces)