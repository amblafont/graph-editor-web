module MyDiff exposing (swapDiff)

import List.Extra as List
import Diff





type alias Change a = { index : Int, length : Int, rep : List a }


compile : Int -> List (Diff.Change a) -> List (Change a)
compile i l =
    case l of
       [] -> []
       Diff.NoChange _ :: q -> compile (i + 1) q
       _ -> compileChange i l { index = i, length = 0, rep = []}
        

compileChange : Int -> List (Diff.Change a) -> Change a -> List (Change a)
compileChange i l c =
    case l of
       [] -> [c]
       Diff.Added x   :: q  -> compileChange (i + 1) q { c | rep = x :: c.rep} 
       Diff.Removed _ :: q  -> compileChange i q { c | length = c.length + 1} 
       Diff.NoChange _ :: q -> c :: compile (i + 1) q  

flip : (a, b) -> (b, a)
flip (x, y) = (y, x)

apply : Change a -> List a -> List a
apply c l =    
          let (l1, l2) = List.splitAt c.index l in
           l1 ++ List.reverse c.rep ++ List.drop c.length l2

applyAll : List a -> List (Change a) -> List a
applyAll = List.foldl apply

commute : Change a -> Change b -> Maybe (Change b, Change a)
commute c1 c2 = 
    -- TODO: verifeir que ca commute bien
    let ret p2 q2 = Just ({c2 | index = q2}, { c1 | index = p2}) in
    if c1.index < c2.index then
       ret c1.index (c2.index + c1.length - List.length c1.rep)
    else
       ret (c1.index + List.length c2.rep - c2.length) c2.index
   
    

commuteList : Change a -> List (Change b) -> Maybe (List (Change b))
commuteList c l =
   case l of
      [] -> Just []
      t :: q ->
          commute c t |>  
          Maybe.andThen 
          (\(t2, c2) -> commuteList c2 q |> Maybe.map ((::) t2))

commuteAll : List (Change a) -> List (Change b) -> Maybe (List (Change a))
commuteAll l cl =
   List.foldl (Maybe.andThen << commuteList) (Just l) (List.reverse cl)

swapDiff : List a -> List a -> List a -> Maybe (List a)
swapDiff l1 l2 l3 = 
     let cl1 = Diff.diff l1 l2 |> compile 0
         cl2 = Diff.diff l2 l3 |> compile 0
     in 
      commuteAll cl2 cl1 |> Maybe.map (applyAll l1)
                   
