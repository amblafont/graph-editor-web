-- translated from https://github.com/varkor/quiver/blob/2c62d40b820cadc3c7f9d0816a33121f389b6240/src/bezier.js#L303
module Geometry.RoundedRectangle exposing (RoundedRectangle, points5)

import Geometry.Point as Point exposing (Point)
import Geometry.Epsilon exposing (epsilon)

-- /// A very small value we use to determine fuzzy equality of points. Floating-point arithmetic is
-- /// imprecise, so we have to take slight inequalities into account when computing.

type alias RoundedRectangle = { centre : Point, size : Point, radius : Float }

{-   /// Returns the points forming the rounded rectangle (with an approximation for the rounded
    /// corners).
    /// `min_segment_length` specifies the precision of the approximation, as the maximum length of
    /// any straight line used to approximate a curve. This must be greater than zero. -}

points5 : RoundedRectangle -> List Point
points5 this = points this 5

points : RoundedRectangle -> Int -> List Point
points this max_segment_length =
    let n = if this.radius == 0 then 0
            else pi / atan (toFloat max_segment_length / (2 * this.radius))
    in
    let sides = ceiling n in
    let r = this.radius / cos (pi / toFloat sides) in
    let add_corner_points sx sy (angle_offset, pts) =
            let pts2 = 
                 
                 (Point.subtract (Point.resize 0.5 this.size) (this.radius, this.radius)
                 |> Point.scale sx sy
                 |> Point.add (Point.lendir this.radius angle_offset)
                 |> Point.add this.centre)
                 :: pts
            in
            let for i pts_for =
                  if toFloat i >= toFloat sides / 4 then pts_for else
                  let angle = (toFloat i + 0.5) / toFloat sides * 2 * pi + angle_offset in
                  let pt = 
                         Point.subtract (Point.resize 0.5 this.size) (this.radius, this.radius)
                         |> Point.scale sx sy
                         |> Point.add (Point.lendir r angle)
                         |> Point.add this.centre
                   in
                    for (i + 1) <| pt :: pts_for
            in
            let pts3 = for 0 pts2 in
            let angle_offset2 = angle_offset + pi / 2 in
            let pt = 
                 Point.subtract (Point.resize 0.5 this.size) (this.radius, this.radius)
                  |> Point.scale sx sy
                  |> Point.add (Point.lendir this.radius angle_offset2)
                  |> Point.add this.centre
            in
            (angle_offset2, pt :: pts3)
    in
    let (_, pts) =
             (0, [])
             |> add_corner_points  1 1
             |> add_corner_points  -1 1
             |> add_corner_points  -1 -1
             |> add_corner_points  1 -1
    in
    let elim0 l acc = case l of
             (x1, y1) :: (x2, y2) :: t ->
                
                  elim0 ((x2, y2) :: t) 
                  <| if abs (x2 - x1) <= epsilon && abs (y2 - y1) <= epsilon then
                      acc
                    else 
                      (x1, y1) :: acc
             _ -> List.reverse acc
    in
      elim0 pts []
                