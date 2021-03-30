module MyDiff exposing (swapDiff, swapDiffStr, failingTests)

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
{- 
flip : (a, b) -> (b, a)
flip (x, y) = (y, x) -}

apply : Change a -> List a -> List a
apply c l =    
          let (l1, l2) = List.splitAt c.index l in
           l1 ++ List.reverse c.rep ++ List.drop c.length l2

applyAll : List a -> List (Change a) -> List a
applyAll = List.foldl apply

offset : Change a -> Int
offset c = List.length c.rep - c.length

changeIndices : Change a -> Change b -> Int -> Int -> (Change b, Change a)
changeIndices c1 c2 p q = 
   ({c2 | index = q}, { c1 | index = p})

-- could be indeterministic, see tests below
{-

-----
     |
     |
     |

-}

commuteRL : Change a -> Change b -> List (Change b, Change a)
commuteRL c1 c2 = 
    (if c1.index + List.length c1.rep <= c2.index then [ changeIndices c1 c2 c1.index (c2.index - offset c1)] else [])
    ++
    (if c2.index + c2.length <= c1.index then [ changeIndices c1 c2 (c1.index + offset c2) c2.index] else [])

commuteLR : Change a -> Change b -> List (Change b, Change a)
commuteLR c1 c2 = 
    (if c1.index + c1.length <= c2.index then [ changeIndices c1 c2 c1.index (c2.index + offset c1)] else [])
    ++
    (if c2.index + c2.length <= c1.index then [ changeIndices c1 c2 (c1.index + offset c2) c2.index] else [])

commute : Bool ->  Change a -> Change b -> List (Change b, Change a)
commute lr = if lr then commuteLR else commuteRL


commuteList : Bool ->
               Change a -> List (Change b) -> List (List (Change b))
commuteList lr c l =
   case l of
      [] -> [[]]
      t :: q ->
          commute lr c t
          |> List.andThen (\(t2, c2) -> commuteList lr c2 q |> List.map ((::) t2))
          



commuteAll : Bool ->
              List (Change a) -> List (Change b) -> List (List (Change a))
commuteAll lr l cl =
   List.foldl (List.andThen << commuteList lr) [l] (List.reverse cl)

swapDiff : Bool -> List a -> List a -> List a -> List (List a)
swapDiff lr l1 l2 l3 =           
     let d2 = Diff.diff l2 l3 in
     let d1 = if lr then Diff.diff l2 l1 else Diff.diff l1 l2 in
     let cl1 =  d1 |> compile 0
         cl2 = d2 |> compile 0
     in 
      commuteAll lr cl2 cl1
       |> List.map (applyAll l1)

swapDiffStr : Bool -> String -> String -> String -> List String
swapDiffStr lr s1 s2 s3 =
   swapDiff lr (String.toList s1) (String.toList s2) (String.toList s3) |> List.map String.fromList


{-

These are tests to ensure that it works fine

-}


type alias Test = { lr : Bool, s1 : String, s2 : String, s3 : String, r : List String }
invTest : Test -> Test
invTest t =
   { t | s1 = t.s3, s3 = t.s1}
{-

a   b
c   d
-}
testsCommutatif : String -> String -> String -> String -> List Test
testsCommutatif a b c d =
   let l =  [
         Test False a b d [ c ],
         Test False a c d [ b ],
         Test True c a b [ d ] ,
         Test True b d c [ a ]
          ]
   in 
   l ++ List.map invTest l



tests : List Test
tests =  
   let l = 
        [ Test False "A" "AB" "ACB" ["AC"],
        Test False "A" "AC" "ACB" ["AB"],
        Test True "AB" "A" "AC" ["ABC", "ACB"],
        Test False "AB" "A" "AC" ["ABC", "ACB"] ]
   in
    testsCommutatif "X" "TX" "Y" "TY" ++
               l ++ List.map invTest l


checkTest : Test -> Maybe (Test, List String)
checkTest t = 
    let r = swapDiffStr t.lr t.s1 t.s2 t.s3 |> List.sort in
    if r == List.sort t.r then
       Nothing
    else
       Just (t, r)


failingTests : List (Test, List String)
failingTests = List.filterMap checkTest tests