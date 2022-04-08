module ListExtraExtra exposing (succCyclePairs, nextInList)
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

prevInList : List a -> a -> a
prevInList l a = case l of
      [] -> a
      [c] -> c
      b :: c :: t ->
         if a == c then
            b
         else
            prevInList (c :: t) a

nextInList : List a -> a -> a
nextInList l a = prevInList (List.reverse l) a
