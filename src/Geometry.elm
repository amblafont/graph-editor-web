module Geometry exposing (raytraceRect, PosDims, Rect, pad, segmentRect)

import Point exposing (Point)

type alias PosDims = { pos : Point, dims : Point }
type alias Rect = { topLeft : Point, bottomRight : Point }

rectFromPosDims : PosDims -> Rect
rectFromPosDims { pos, dims } =
   let dims2 = Point.resize 0.5 dims in
   Rect 
   (Point.subtract pos dims2)
   (Point.add pos dims2)

pad : Float -> PosDims -> PosDims 
pad n {pos , dims } = 
   let n2 = n * 2 in
  { pos = pos, dims = Point.add dims (n2, n2) }


segmentRect : PosDims -> PosDims -> (Point, Point)
segmentRect r1 r2 = 
    let p1 =
             raytraceRect r2.pos r1.pos (rectFromPosDims r1) |>
             Maybe.withDefault r1.pos
        p2 =
             raytraceRect r1.pos r2.pos (rectFromPosDims r2) |>
             Maybe.withDefault r2.pos 
    in
        (p1, p2)


raytraceRect : Point -> Point -> Rect -> Maybe Point
-- raytraceRect ((px, py) as p) ((x, y) as d) { toTop, toBottom, toRight, toLeft} =
raytraceRect p1 p2 { topLeft, bottomRight } =
    let v = Point.subtract p2 p1 |> Point.normalise 1 in
    let l = Point.toList in
    case intersection (l p1) (l v) (l topLeft, l bottomRight) of
        Just [ix, iy] -> Just (ix, iy)
        _ -> Nothing



-- from https://github.com/stackgl/ray-aabb-intersection
--
-- First argument, origin
-- second argument: direction (normalized)
-- third argument: boxed
-- usage: intersection([1,2] , [0,2], [[1,2],[3,4]])
intersection : List Float -> List Float -> (List Float, List Float) -> Maybe (List Float)
intersection ro rd aabb =
    distance ro rd aabb |>
    Maybe.andThen
        (\ d -> Just <| List.map2 (\ roi rdi -> roi + rdi * d) ro rd)


distance : List Float -> List Float -> (List Float, List Float) -> Maybe Float
distance ro rd (aa,bb) =
    let f x roi rdi = (x - roi) / rdi in
    let dimLo = List.map3 f aa ro rd in
    let dimHi = List.map3 f bb ro rd in
    let dimLo2 = List.map2 min dimLo dimHi in
    let dimHi2 = List.map2 max dimLo dimHi in
       case (List.maximum dimLo2, List.minimum dimHi2) of
           (Just maxLo, Just minHi) ->
               if minHi < maxLo then
                   Nothing
               else
                   Just maxLo
           _ -> Nothing
