module ListExtraExtra exposing (succCyclePairs, toMaybe)
import List.Extra as List

permute : List a -> List a
permute l =
  case l of 
   [] -> []
   t :: q -> q ++ [t]

succCyclePairs : List a -> List (a, a)
succCyclePairs l =
   case List.zip l (permute l) of
       [ _ ] -> []
       r -> r

toMaybe : List a -> Maybe a
toMaybe l = 
  case l of
      [ t ] -> Just t
      _  -> Nothing