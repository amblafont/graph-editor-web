module ListExtraExtra exposing (succPairs, succCyclePairs, nextInList, moveLeftCycle, moveRightCycle, prevInList, move)
import List.Extra as List

permute : List a -> List a
permute l =
  case l of 
   [] -> []
   t :: q -> q ++ [t]

succPairs : List a -> List (a, a)
succPairs l =
   case l of
       t1 :: t2 :: q -> (t1, t2) :: succPairs (t2 :: q)
       _ -> []


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
   case List.unconsLast l of
      Just (t,q) -> if f t then t :: q else
          move True f l
      Nothing -> []

moveLeftCycle : (a -> Bool) -> List a -> List a
moveLeftCycle f l =
   case l of     
      t :: q -> if f t then q ++ [t] else
                 move False f l
      [] -> []
