module Geometry.Bezier exposing (Bezier, new, point,
   intersections_with_rounded_rectangle)
-- translated from https://github.com/varkor/quiver/blob/2c62d40b820cadc3c7f9d0816a33121f389b6240/src/bezier.js#L9
import Geometry.Point as Point exposing (Point, NamedPoint)
import Geometry.RoundedRectangle as RoundedRectangle exposing (RoundedRectangle)
import Geometry.Epsilon exposing (inv_epsilon, epsilon)
import Set exposing (Set)


type alias Bezier = { origin : Point, w : Float, h : Float, 
                      angle : Float,
                      end : Point, control : Point}

new : Point -> Float -> Float -> Float -> Bezier
new origin w h angle =
   { origin = origin, w = w, h = h, 
     angle = angle,
     end = Point.add origin (w, 0),     
     control = Point.add origin (w / 2, h)
   }

-- Returns the (x, y)-point at t = `t`.
point : Bezier -> Float -> Point
--  return this.origin.lerp(this.control, t).lerp(this.control.lerp(this.end, t), t);
point this t = Point.lerp 
            (Point.lerp this.origin this.control t)
            (Point.lerp this.control this.end t)
             t

-- Returns the `x` co-ordinates of the intersection with the line `y = m x + c`.
x_intersections_with_nonvertical_line : Float -> Float -> List Float
x_intersections_with_nonvertical_line m c =
   let determinant = m * m - 4 * m + 4 - 8 * c in
   if determinant > 0 then
      [(2 - m + sqrt determinant) / 4, (2 - m - sqrt determinant) / 4]
   else if determinant == 0 then
      [(2 - m + sqrt determinant) / 4]
   else 
      []


--  Returns the `y` co-ordinates of the intersection with the line `x = a`.
y_intersection_with_vertical_line : Float -> Float
y_intersection_with_vertical_line a = 2 * a * (1 - a)

-- Returns the angle of the tangent to the curve at t = `t`.
tangent : Bezier -> Float -> Float
tangent this t =
    Point.subtract
      (Point.lerp this.control this.end t)
      (Point.lerp this.origin this.control t)    
    |> Point.pointToAngle   

type alias BezierPoint = { point : Point, t : Float, angle : Float }

{- /// Intersect the Bézier curve with the given rounded rectangle. Note that the general
    /// (analytic) problem of intersecting a Bézier curve with a circle (for the rounded corners) is
    /// very difficult, so we approximate circles with regular polygons. If the rounded rectangle
    /// entirely contains the Bézier curve, and `permit_containment` is true, a single intersection
    /// point (the centre of the rectangle) is returned; otherwise, an error is thrown.
 -}
 
intersections_with_rounded_rectangle : Bezier -> RoundedRectangle -> Bool -> List Point
intersections_with_rounded_rectangle this rect permit_containment =
   let h = if this.h == 0 then 1 else this.h in
   let points = RoundedRectangle.points5 rect
              |> List.map 
              (\p -> Point.subtract p this.origin
                |> Point.rotate -this.angle
                |> Point.inv_scale this.w h
                |> Point.name
              )
   in
   let add_intersection p intersections = 
        Set.insert          
               (toFloat (round (p.x * inv_epsilon)) / inv_epsilon, 
                toFloat (round (p.y * inv_epsilon)) / inv_epsilon)                
                intersections 
   in
   let m_c (endpoint0, endpoint1) =
            let m = (endpoint1.y - endpoint0.y) / (endpoint1.x - endpoint0.x) in
            (m, endpoint0.y - m * endpoint0.x)
   in
   let intersections2 = 
        if this.h == 0 then
           let for pt0 pts intersections =
                case pts of
                    [] -> intersections
                    endpoint0 :: q ->
                       let endpoint1 = List.head q |> Maybe.withDefault pt0 in
                       let endpoints = (endpoint0, endpoint1) in
                       for pt0 q <|
                       if (abs (endpoint0.x - endpoint1.x) <= epsilon) then
                          if (endpoint0.x >= 0 && endpoint0.x <= 1
                             && min endpoint0.y endpoint1.y <= 0
                             && max endpoint0.y endpoint1.y >= 0) then
                             add_intersection (NamedPoint endpoint0.x 0) intersections
                          else
                             intersections
                       else
                         let (m, c) = m_c endpoints in
                         if abs m > epsilon then
                           let x = -c / m in
                           if x >= 0 && x <= 1
                             && x >= min endpoint0.x endpoint1.x
                             && x <= max endpoint0.x endpoint1.x
                           then add_intersection (NamedPoint x 0) intersections
                           else intersections
                         else if abs endpoint0.y <= epsilon then
                           let minx = min endpoint0.x endpoint1.x
                               maxx = min endpoint0.x endpoint1.x
                           in
                           if minx <= 1 && maxx >= 0 then
                             intersections 
                             |> add_intersection (NamedPoint (max minx 0) 0)
                             |> add_intersection (NamedPoint (max maxx 1) 0)
                           else
                             intersections
                         else intersections
           in
           for (List.head points |> Maybe.withDefault (NamedPoint 0 0))
                points Set.empty      
        
        else
        -- // The usual case: when we have a nontrivial Bézier curve.
        let for pt0 pts intersections =
                case pts of
                    [] -> intersections
                    endpoint0 :: q ->
                       let endpoint1 = List.head q |> Maybe.withDefault pt0 in
                       let endpoints = (endpoint0, endpoint1) in
                       for pt0 q <| 
                       if (abs (endpoint0.x - endpoint1.x) <= epsilon) then
                          let y = y_intersection_with_vertical_line endpoint0.x in
                          if (y >= 0
                             && y >= min endpoint0.y endpoint1.y
                             && y <= max endpoint0.y endpoint1.y) then
                             add_intersection (NamedPoint endpoint0.x y) intersections
                          else
                             intersections
                       else
                         let (m, c) = m_c endpoints in
                         let inters = x_intersections_with_nonvertical_line m c 
                                   |> List.filter(\ x -> x >= 0 && x <= 1 
                                              && x >= min endpoint0.x endpoint1.x
                                              && x <= max endpoint0.x endpoint1.x )
                                   |> List.map (\ x -> NamedPoint x (m * x + c))
                         in
                           List.foldl add_intersection 
                         intersections inters

         in
            for (List.head points |> Maybe.withDefault (NamedPoint 0 0))
                points Set.empty 
    in
    let intersections3 =
         -- // If there are no intersections, check whether the rectangle entirely contains the curve.
         if Set.isEmpty intersections2 then
           let sharp_rect = RoundedRectangle rect.centre rect.size 0 in
           if Point.isInPoly this.origin (RoundedRectangle.points5 sharp_rect) then
             if permit_containment then
                 -- new BezierPoint(Point.zero(), 0, this.tangent(0))
                 (0, 0) --, 0, (tangent this 0))
                 |> Set.singleton
             else
                 Debug.log "Bézier curve was entirely contained by rounded rectangle."
                 intersections2
           else
             intersections2
         else
           intersections2
    in
    intersections3 |> Set.toList |>
    List.map 
    -- return new BezierPoint(p.scale(this.w, h), p.x, Math.atan2((2 - 4 * p.x) * h, this.w));
            (Point.scale this.w h)         
    
   
   -- ICI