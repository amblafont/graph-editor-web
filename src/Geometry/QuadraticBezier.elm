-- deprecated: we now use Bezier (from Quiver)
-- TODO: delete this file
module Geometry.QuadraticBezier exposing (QuadraticBezier, middle, isLine, orthoVectPx, dummy, toCubic
 , shiftFrom, shiftTo)
import Geometry.Point as Point exposing (Point)
import Geometry.Point exposing (orthoVectPx)

type alias QuadraticBezier = 
  { from : Point, to : Point, controlPoint : Point}

dummy : QuadraticBezier
dummy = { from = (0,0), to = (0,0), controlPoint = (0,0) }

{-
https://latex.org/forum/viewtopic.php?t=4424
Formula to turn a quadratic into a cubic bezier
Q0 = P0
Q1 = 1/3 P0 + 2/3 P1
Q2 = 2/3 P1 + 1/3 P2
Q3 = P2
-}
toCubic : QuadraticBezier -> {from : Point, to : Point, controlPoint1 : Point, controlPoint2 : Point}
toCubic {from, to, controlPoint} =
  let rescale n = Point.scale (n / 3) (n / 3) in
  let q1 = Point.add (rescale 1 from) (rescale 2 controlPoint) in
  let q2 = Point.add (rescale 2 controlPoint) (rescale 1 to)
  in
  {from = from, to = to, controlPoint1 = q1, controlPoint2 = q2}


middle : QuadraticBezier -> Point
middle {from, to, controlPoint } =
  Point.middle controlPoint <| Point.middle from to



shiftFrom : QuadraticBezier -> Float -> Point
shiftFrom ({from, controlPoint} as q) shift =
  Point.towardsBentDiagonal from (middle q) controlPoint shift

shiftTo : QuadraticBezier -> Float -> Point
shiftTo ({to, controlPoint} as q) shift =
  Point.towardsBentDiagonal to (middle q) controlPoint shift



isLine : QuadraticBezier -> Bool
isLine {from, to, controlPoint} =
   let (x1, y1) = Point.subtract to from |> Point.orthogonal
       (x2, y2) = Point.subtract controlPoint to       
    in    
       abs (x1 * x2 + y1 * y2) < 1e-10 -- scalar product



orthoVectPx : Float -> QuadraticBezier -> QuadraticBezier
orthoVectPx px {from, to, controlPoint} = 
  let deltaFrom = Point.orthoVectPx from controlPoint px in
  let deltaTo = Point.orthoVectPx controlPoint to px in
  let deltaCp = Point.normalise (abs px) <| Point.add deltaFrom deltaTo in
  
  { from = Point.add deltaFrom from, 
    to = Point.add deltaTo to, 
    controlPoint = Point.add controlPoint deltaCp }



