module IntDictExtra exposing (filterMap, removeList, getList, any)

import IntDict exposing (IntDict)

filterMap : (Int -> a -> Maybe b) -> IntDict a -> IntDict b
filterMap f d =
      IntDict.toList d
         |> List.filterMap (\(i, o) -> Maybe.map (\b -> (i, b)) (f i o))                              
         |> IntDict.fromList

removeList : List Int -> IntDict a -> IntDict a
removeList l d = List.foldl IntDict.remove d l

getList : List Int -> IntDict a -> List (Int, a)
getList l d =
   let d2 = IntDict.map Tuple.pair d in
   List.filterMap (\i -> IntDict.get i d2) l

any : (a -> Bool) -> IntDict a -> Bool
any f d = IntDict.filter (always f) d |> IntDict.isEmpty |> not