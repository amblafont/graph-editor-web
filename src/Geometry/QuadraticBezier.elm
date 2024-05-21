-- deprecated: we now use Bezier (from Quiver)
-- TODO: delete this file
module Geometry.QuadraticBezier exposing (QuadraticBezier, middle, isLine, orthoVectPx, dummy)
import Geometry.Point as Point exposing (Point)

type alias QuadraticBezier = 
  { from : Point, to : Point, controlPoint : Point}

dummy : QuadraticBezier
dummy = { from = (0,0), to = (0,0), controlPoint = (0,0) }

middle : QuadraticBezier -> Point
middle {from, to, controlPoint } =
  Point.middle controlPoint <| Point.middle from to


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



