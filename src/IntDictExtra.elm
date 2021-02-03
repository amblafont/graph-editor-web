module IntDictExtra exposing (filterMap, removeList)

import IntDict exposing (IntDict)

filterMap : (Int -> a -> Maybe b) -> IntDict a -> IntDict b
filterMap f d =
      IntDict.toList d
         |> List.filterMap (\(i, o) -> Maybe.map (\b -> (i, b)) (f i o))                              
         |> IntDict.fromList

removeList : List Int -> IntDict a -> IntDict a
removeList l d = List.foldl IntDict.remove d l