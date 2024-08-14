module Modif exposing (Result, map, fold)

type alias Result a b = Maybe { next : a, undo : b }

map : (a1 -> a2) -> (b1 -> b2) -> Result a1 b1 -> Result a2 b2
map f g = Maybe.map (\ { next, undo } -> { next = f next, undo = g undo })

fold : (a -> b -> Result a b) -> a -> List b -> Result a (List b)
fold f c cs =
   if cs == [] then Nothing else
   let aux a bs acc =
            case bs of
                [] ->
                    { next = a, undo = acc } 
                b :: bs2 ->
                    case f a b of
                        Just { next, undo } ->
                               aux next bs2 (undo :: acc)
                        Nothing -> aux a bs2 acc
   in
   let r = aux c cs [] in
   if r.undo == [] then Nothing else Just r