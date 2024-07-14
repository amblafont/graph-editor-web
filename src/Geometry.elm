module Geometry exposing (raytraceRect, PosDims, Rect, pad, makeRect,
    centerRect, 
    rectEnveloppe,
   -- segmentRect, 
   segmentRectBent, isInRect, isInPosDims
   -- , diamondPointPx 
   -- from Quiver
   , determine_label_position
   , rectFromPosDims
   , posDimsFromRect
   , distanceToRect
   , LabelAlignment(..)
  )

import Geometry.Point as Point exposing (Point)
import Geometry.QuadraticBezier exposing (QuadraticBezier)
import Geometry.Bezier as Bezier exposing (Bezier)
-- import ArrowStyle exposing (PosLabel(..))
import Geometry.RoundedRectangle exposing (RoundedRectangle)


type alias PosDims = { pos : Point, dims : Point }
type alias Rect = { topLeft : Point, bottomRight : Point }

-- https://stackoverflow.com/questions/5254838/calculating-distance-between-a-point-and-a-rectangular-box-nearest-point
distanceToRect : Point -> Rect -> Float
distanceToRect (px, py) r =
    let (minx, miny) = r.topLeft 
        (maxx, maxy) = r.bottomRight
    in
    let dx = max (minx - px) <| max 0 (px - maxx)
        dy = max (miny - py) <| max 0 (py - maxy)
    in
      sqrt (dx * dx + dy * dy)
  

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

posDimsFromRect : Rect -> PosDims
posDimsFromRect { topLeft, bottomRight } =
   let center = Point.add topLeft bottomRight |> Point.resize 0.5 in
   PosDims center <| Point.subtract bottomRight topLeft

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
    let (r1_bis, r2_bis, bent_bis) =
             if r1.pos /= r2.pos then 
              (r1, r2, bent)
             else 
               let (w1, h1) = r1.dims
                   (w2, h2) = r2.dims
               in               
              --  let offset = max ((min w1 w2) / 3) 7 in
              let offset = 7 in
               let new_w w = 2 in -- max 1 (w - offset) in
               let newBent = -40 / offset in
               ({pos = Point.add r1.pos (-offset, 0), dims = (new_w w1, h1)},
                {pos = Point.add r2.pos ( offset, 0), dims = (new_w w2, h2)},
                newBent)
    in
      segmentRectBent_aux r1_bis r2_bis bent_bis


segmentRectBent_aux : PosDims -> PosDims -> Float -> QuadraticBezier
segmentRectBent_aux r1 r2 bent = 
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
    -- we recompute the controlPoint to avoid
    -- glitch (e.g., if the source is much bigger than the target,
    -- there the control point could be in a vert wrong location,
    -- resulting in inversion of the arrow head or strange double arrows)
    let betterControlPoint =
          Point.diamondPx p1 p2
          <| pxFromRatio p1 p2 bent 
    in
        { from = p1, to = p2, controlPoint = betterControlPoint }


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

-- from Quiver
type LabelAlignment =
    Centre
  | Over
  | Left 
  | Right

-- translated from https://github.com/varkor/quiver/blob/2c62d40b820cadc3c7f9d0816a33121f389b6240/src/arrow.js#L1247
-- determine_label_position :
-- length: the distance from the source to the target
-- angle The angle of the straight line connecting the source to the target.
-- label_position: between 0 and 1
determine_label_position length angle edge_width start end curve label_position label_alignement label_size =
   let bezier = Bezier.new (0, 0) length -- curve 
         --(Debug.log "curve" curve) 
           curve
         --(Debug.log "angle" angle)
           angle
   in
   let centre = Bezier.point bezier (start + (end - start) * label_position)
   in
   if label_alignement == Over then centre else
   let offset_angle = case label_alignement of
            Centre -> 0
            Over -> 0
            Left -> 0 - pi / 2
            Right -> pi / 2
   in
   let offset_allowance = 4 in
   
   let bail_out = 1024 in
   let while i offset_min offset_max =
        let label_offset = (offset_min + offset_max) / 2 in 
        if i == 0 then 
          -- Debug.log "Had to bail out from determining label offset." 
          label_offset
        else
        let nexti = i - 1 in
        let rect_centre = Point.rotate angle centre
                |> Point.add (Point.lendir label_offset (angle + offset_angle))
        in
        let intersections = Bezier.intersections_with_rounded_rectangle
                         bezier 
                         (RoundedRectangle rect_centre
                            (Point.add (edge_width, edge_width)
                                       label_size
                            )
                            (edge_width / 2)
                         )
                         True
        in
        if intersections == [] then
           if offset_max - offset_min < 1 then
              label_offset
           else              
              while nexti offset_min label_offset
        else   
          while nexti label_offset offset_max
   in
   let offset_min = 0 in
   let offset_max = offset_allowance + abs curve / 2 +
                     (Point.radius <| Point.resize 0.5 
                     <| Point.add label_size (edge_width, edge_width) )
   in
   let label_offset = while bail_out offset_min offset_max in
   Point.add centre <| Point.lendir label_offset offset_angle

