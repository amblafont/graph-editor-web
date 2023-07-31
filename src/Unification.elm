module Unification exposing (unify, Config)
import List.Extra

type alias Config a = {
    isMetavariable : a -> Bool
    -- eq : a -> b -> Bool 
    }

-- metavariables only occurs in the first argument
-- returns the list of substitutions
-- let us assume that the metavariable occurs only at the beginning
-- and at the end
unify : Config a -> List a -> List b -> Result String (List (a, List b))
unify cfg l1 l2 =
    if List.Extra.count cfg.isMetavariable l1 > 1 then
      Err "Only one hole at most is supported"
    else
     let length1 = List.length l1 in
     let length2 = List.length l2 in
     if length2 < length1 then Err "The solved handside cannot be smaller" else
     unifyAux cfg (1 + length2 - length1) l1 l2

unifyAux : Config a -> Int -> List a -> List b -> Result String (List (a, List b))
unifyAux cfg length l1 l2 =
     case l1 of
      [] -> Ok []
      t1 :: q1 ->
         if cfg.isMetavariable t1 then            
            let t2 = List.take length l2 in
            Ok [(t1, t2)] 
         else
            unifyAux cfg length q1 (List.drop 1 l2)
