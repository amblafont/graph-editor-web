module ListExtraExtra exposing (succCyclePairs)
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