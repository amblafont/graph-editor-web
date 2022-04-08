module UnifyDiag exposing (unifyStringList)
import Dict exposing (Dict)
import MyDiff exposing (Change)


unifyStrings : String -> String -> Dict String String -> Maybe (Dict String String)
unifyStrings s1 s2 d = 
   MyDiff.diff (String.toList s1) (String.toList s2) 
   |> MyDiff.removeAdds (String.toList s1) 
   |> Maybe.andThen (\ delta ->            
   buildReplaceDict s1 delta d)

unifyStringList : List String -> List String -> Dict String String -> Maybe (Dict String String)
unifyStringList l1 l2 d =
  case (l1, l2) of
     ([], []) -> Just d
     (t1 :: q1, t2 :: q2) ->
        unifyStrings t1 t2 d |>
        Maybe.andThen 
           (unifyStringList q1 q2)
     _ -> Nothing
   

buildReplaceDictChange : String -> Change Char -> Dict String String -> Maybe (String, Dict String String)
buildReplaceDictChange s c d = 
   let finalIdx = c.index + c.length in
   let finalStr = MyDiff.applyStr c s
                -- ++ String.right (String.length s - finalIdx) s 
   in

   let key = String.slice c.index finalIdx s in
   let value = c.rep |> String.fromList in
   let retf = Just (finalStr, Dict.insert key value d) in
   case Dict.get key d of
     Nothing -> retf
     Just value2 -> if value == value2 then retf else Nothing

   


buildReplaceDict : String -> List (Change Char) -> Dict String String -> Maybe (Dict String String)
buildReplaceDict l cs d =
   case cs of
       [] -> Just d
       c :: tail ->
          buildReplaceDictChange l c d 
          |> Maybe.andThen (\ (finalStr, d2) ->
               buildReplaceDict finalStr tail d2)




   

   