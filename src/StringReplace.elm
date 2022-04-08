module StringReplace exposing (replaceIn)

import Parser exposing (Parser, oneOf, token, succeed, (|.), 
  (|=), chompIf, getChompedString, lazy, end)
import Dict exposing (Dict)

replaceIn : Dict String String -> String -> String
replaceIn d s = 
   Parser.run (parserReplace d) s |>
   Result.withDefault s
   
-- not very efficient but hey

parserReplace : Dict String String -> Parser String
parserReplace d =
   let ks = Dict.toList d in
   let mkParser (s1, s2) = succeed s2 |. token s1 in
   let parsers = (succeed "" |. end) :: List.map mkParser ks in
   let final _ = 
        oneOf <| parsers    
         ++ [ 
              succeed String.append 
              |= (chompIf (always True) |> getChompedString)
              |= lazy final ]
   in
   final ()
   -- Parser.succeed ""
   