module ListExtraExtra exposing (succCyclePairs, nextInList, moveLeftCycle, moveRightCycle, prevInList, move)
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

move : Bool -> (a -> Bool) -> List a -> List a
move isRight f l =
   case l of
      [] -> []
      [c] -> [c]
      b :: c :: q ->
         if (if isRight then f b else f c) then
            c :: b :: q
         else
            b :: move isRight f (c :: q)
moveRightCycle : (a -> Bool) -> List a -> List a
moveRightCycle f l =
   case List.last l of
      Just c -> 
          move True f (c :: l) |> List.unconsLast |> Maybe.map Tuple.second 
          |> Maybe.withDefault l
      Nothing -> []

moveLeftCycle : (a -> Bool) -> List a -> List a
moveLeftCycle f l =
   case List.head l of
      Just c -> 
          move False f (l ++ [c]) |> List.tail
          |> Maybe.withDefault l
      Nothing -> []