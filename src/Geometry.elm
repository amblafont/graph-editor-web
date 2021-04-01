module Geometry exposing (raytraceRect, PosDims, Rect, pad, makeRect,
    centerRect, 
    rectEnveloppe,
   -- segmentRect, 
   segmentRectBent, isInRect, isInPosDims -- , diamondPointPx
  )

import Geometry.Point as Point exposing (Point)
import Geometry.QuadraticBezier exposing (QuadraticBezier)
import Collage.Layout exposing (bottomRight)
import Collage.Layout exposing (topLeft)


type alias PosDims = { pos : Point, dims : Point }
type alias Rect = { topLeft : Point, bottomRight : Point }

rectEnveloppe : List Point -> Rect
rectEnveloppe l =
   let (xs, ys) = List.unzip l in
   let lmin = List.minimum >> Maybe.withDefault 0
       lmax = List.maximum >> Maybe.withDefault 0
   in
   { topLeft = (lmin xs, lmin ys), bottomRight = (lmax xs, lmax ys) }

makeRect : Point -> Point -> Rect
makeRect p1 p2  =
   rectEnveloppe [ p1, p2 ]   

centerRect : Rect -> Point
centerRect { bottomRight, topLeft} = Point.middle bottomRight topLeft

rectFromPosDims : PosDims -> Rect
rectFromPosDims { pos, dims } =
   let dims2 = Point.resize 0.5 dims in
   Rect 
   (Point.subtract pos dims2)
   (Point.add pos dims2)

isInRect : Rect -> Point -> Bool 
isInRect {topLeft , bottomRight} (x, y) =
  let (x1, y1) = topLeft
      (x2, y2) = bottomRight
   in
     x1 < x && x < x2 &&  y1 < y && y  < y2

isInPosDims : PosDims -> Point -> Bool
isInPosDims dims p = isInRect (rectFromPosDims dims) p

pad : Float -> PosDims -> PosDims 
pad n {pos , dims } = 
   let n2 = n * 2 in
  { pos = pos, dims = Point.add dims (n2, n2) }





pxFromRatio : Point -> Point -> Float -> Float
pxFromRatio p1 p2 r =
  r * Point.distance p2 p1

  





segmentRectBent : PosDims -> PosDims -> Float -> QuadraticBezier
segmentRectBent r1 r2 bent = 
    let controlPoint = Point.diamondPx r1.pos r2.pos 
                        <| pxFromRatio r1.pos r2.pos bent 
    in
   
    let p1 =
             raytraceRect controlPoint r1.pos (rectFromPosDims r1) |>
             Maybe.withDefault r1.pos
        p2 =
             raytraceRect controlPoint r2.pos (rectFromPosDims r2) |>
             Maybe.withDefault r2.pos 
    in
        { from = p1, to = p2, controlPoint = controlPoint }


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
