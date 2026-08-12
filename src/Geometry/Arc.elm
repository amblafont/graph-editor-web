module Geometry.Arc exposing (Arc, new, point,
   intersections_with_rounded_rectangle)
-- translated from https://github.com/varkor/quiver/blob/1816fb788e4d315bf1dc30053a5e1646eb0af9b8/src/curve.mjs#L427
import Geometry.Point as Point exposing (Point, NamedPoint)
import Geometry.RoundedRectangle as RoundedRectangle exposing (RoundedRectangle)
import Geometry.Epsilon exposing (inv_epsilon, epsilon)

{-
// Round a number to the nearest `EPSILON` to avoid floating point precision issues.
function round_to_epsilon(x) {
    return Math.round(x * INV_EPSILON) / INV_EPSILON;
}
-}
round_to_epsilon : Float -> Float
round_to_epsilon x =
    (round (x * inv_epsilon)) / inv_epsilon

sign : Float -> Float
sign x =
    if x < 0 then -1 else if x > 0 then 1 else 0

clockwise : Float -> Float
clockwise radius = if radius >= 0 then 1 else -1

type alias Arc = { origin : Point, chord : Float, major : Float, 
                      radius : Float,
                      angle : Float, 
                      -- computed properties
                      sagitta : Float,
                      centre_normalised : Point,
                      sweep_angle : Float,
                      centre : Point,
                      start_angle : Float }

{-

constructor(origin, chord, major, radius, angle) {
        super();
        this.origin = origin;
        this.chord = chord;
        this.major = major;
        this.radius = radius;
        this.angle = angle;

        // Computed properties.
        this.sagitta = this.radius
            - Math.sign(this.radius) * (this.radius ** 2 - this.chord ** 2 / 4) ** 0.5;
        // The normalised circle centre, not taking into account the origin or angle.
        this.centre_normalised = new Point(
            this.chord / 2,
            (this.radius - this.sagitta) * (this.major ? -1 : 1),
        );
        const start_angle = mod(this.centre_normalised.neg().angle(), 2 * Math.PI);
        this.sweep_angle = Math.PI + (2 * Math.PI - 2 * start_angle) * this.clockwise,
        this.centre = this.origin.add(this.centre_normalised.rotate(this.angle));
        this.start_angle = mod(start_angle + this.angle, 2 * Math.PI);
    }

-}
new : Point -> Float -> Float -> Float -> Float -> Arc
new origin chord major radius angle =
   let sagitta = radius - (sign radius) * sqrt (radius * radius - (chord * chord) / 4) in
   let centre_normalised = 
          ((chord / 2), ((radius - sagitta) * (if major /= 0 then -1 else 1)))
   in
   let start_angle = Point.flip centre_normalised |> Point.quiverAngle |> Point.normaliseAngle02pi in
   let sweep_angle = 
        pi + (2 * pi - 2 * start_angle) * clockwise radius
   in
   let centre = 
        Point.add origin 
          (Point.rotate angle centre_normalised)
   in
   { origin = origin
   , chord = chord
   , major = major
   , radius = radius
   , angle = angle
   , sagitta = sagitta
    , centre_normalised = centre_normalised
    , sweep_angle = sweep_angle
    , centre = centre
    , start_angle = start_angle + angle |> Point.normaliseAngle02pi
   }

-- Returns the (x, y)-point at t = `t`.
{-

      return this.centre_normalised.add(this.origin)
            .add(new Point(Math.abs(this.radius), 0)
                .rotate(this.start_angle - this.angle + t * this.sweep_angle * this.clockwise));

-}
point : Arc -> Float -> Point
point this t =
    Point.add this.centre_normalised this.origin
       |> Point.add (abs this.radius, 0)
       |>
        (Point.rotate 
            (this.start_angle - this.angle + t * this.sweep_angle * clockwise this.radius)
        )

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

{- 

/// Intersect the arc with the given rounded rectangle. If the rounded rectangle entirely
    /// contains the arc, and `permit_containment` is true, a single intersection point (the centre
    /// of the rectangle) is returned; otherwise, an error is thrown.
    intersections_with_rounded_rectangle(rect, permit_containment) {
        // If the arc is essentially a straight line, we pass off intersection checking to the
        // Bézier code, which already special cases straight lines. Since the circles involved can
        // be very large, it does not suffice to use `EPSILON` here, so we use `1.0` instead. In any
        // case, we do not care very much about sub-pixel precision.
        if (!this.major && Math.abs(this.sagitta) <= 1.0) {
            return new Bezier(this.origin, this.chord, 0, this.angle)
                .intersections_with_rounded_rectangle(rect, permit_containment);
        }

        // Normalise all the points with respect to the circle.
        const points = rect.points().map((p) => {
            // Translate the point with respect to the centre of the circle.
            p = p.sub(this.centre).map(round_to_epsilon);
            return p;
        });
        // We wish to return points in order of proximity to the origin, so we must reverse the
        // iteration order if we are traversing anticlockwise.
        if (this.radius < 0) {
            points.reverse();
        }
        const intersections = new Set();

        // We need to find the intersections of line segments with a circle. There may be 0, 1 or 2
        // intersections for each segment.
        for (let i = 0; i < points.length; ++i) {
            const endpoints = [points[i], points[(i + 1) % points.length]];
            const d = endpoints[1].sub(endpoints[0]);
            const det = endpoints[0].x * endpoints[1].y - endpoints[1].x * endpoints[0].y;
            const ls = d.length() ** 2;
            const disc = (this.radius ** 2) * ls - (det ** 2);
            if (Math.sign(disc) < 0) {
                // No intersection.
                continue;
            }
            // If the sign of `disc` is 0, then the line segment is tangent to the circle. If the
            // sign is positive, then there are two intersection points on the circle (though not
            // necessarily on the arc).
            for (const s of Math.abs(disc) <= EPSILON ? [0] : [1, -1]) {
                const [x, y] = [
                    (det * d.y + s * d.x * (disc ** 0.5) * (d.y < 0 ? -1 : 1)) / ls,
                    (-det * d.x + s * (disc ** 0.5) * Math.abs(d.y)) / ls,
                ].map(round_to_epsilon);

                // Check that the intersection is on the line segment.
                if (x >= Math.min(endpoints[0].x, endpoints[1].x)
                    && x <= Math.max(endpoints[0].x, endpoints[1].x)
                    && y >= Math.min(endpoints[0].y, endpoints[1].y)
                    && y <= Math.max(endpoints[0].y, endpoints[1].y)
                ) {
                    // Check that the intersection is on the arc.
                    if (this.angle_in_arc(Math.atan2(y, x))) {
                        Curve.add_intersection(intersections, new Point(x, y));
                    }
                }
            }
        }

        // If there are no intersections, check whether the rectangle entirely contains the curve.
        if (intersections.size === 0) {
            return Curve.check_for_containment(this.origin, rect, permit_containment);
        }

        return Array.from(intersections).map((p) => {
            const t = mod((Math.atan2(p.y, p.x) - this.start_angle) * this.clockwise, 2 * Math.PI)
                / this.sweep_angle;
            return new CurvePoint(
                p.add(this.centre).sub(this.origin).rotate(-this.angle),
                t,
                this.tangent(t),
            );
        });
    }

 -}
 
intersections_with_rounded_rectangle : Arc -> RoundedRectangle -> Bool -> List NamedPoint
intersections_with_rounded_rectangle this rect permit_containment =
   if not this.major && abs this.sagitta <= 1.0 then
      -- Arc is essentially a straight line, delegate to Bezier
      -- This would require a Bezier module implementation
      []
       else
      let
          -- Normalise all points with respect to the circle
          points = 
            RoundedRectangle.points rect
             |> List.map (\p -> 
            Point.subtract p this.centre
                |> Point.mapBoth (Point.round_to_epsilon inv_epsilon)
             )
             |> (\pts -> if this.radius < 0 then List.reverse pts else pts)
          
          -- Find intersections for each line segment
          findSegmentIntersections : Int -> List Point -> List Point
          findSegmentIntersections i pts =
         let
             p0 = Maybe.withDefault (0, 0) (List.head (List.drop i pts))
             p1 = Maybe.withDefault (0, 0) (List.head (List.drop ((i + 1) |> modBy (List.length pts)) pts))
             d = Point.subtract p1 p0
             det = (Tuple.first p0) * (Tuple.second p1) - (Tuple.first p1) * (Tuple.second p0)
             ls = Point.lengthSquared d
             disc = (this.radius * this.radius) * ls - (det * det)
         in
         if disc < 0 then
             []
         else
             let
            sValues = if abs disc <= epsilon then [0] else [1, -1]
             in
             List.filterMap (\s ->
            let
                sqrtDisc = sqrt disc
                dx = Tuple.first d
                dy = Tuple.second d
                x = (det * dy + s * dx * sqrtDisc * (if dy < 0 then -1 else 1)) / ls
               |> Point.round_to_epsilon inv_epsilon
                y = (-det * dx + s * sqrtDisc * abs dy) / ls
               |> Point.round_to_epsilon inv_epsilon
                minX = min (Tuple.first p0) (Tuple.first p1)
                maxX = max (Tuple.first p0) (Tuple.first p1)
                minY = min (Tuple.second p0) (Tuple.second p1)
                maxY = max (Tuple.second p0) (Tuple.second p1)
            in
            if x >= minX && x <= maxX && y >= minY && y <= maxY then
                if Point.angleInArc this (atan2 y x) then
               Just (x, y)
                else
               Nothing
            else
                Nothing
             ) sValues
          
          allIntersections =
         List.range 0 (List.length points - 1)
             |> List.concatMap (\i -> findSegmentIntersections i points)
             |> Point.uniquePoints
      in
      if List.isEmpty allIntersections then
          Point.checkForContainment this.origin rect permit_containment
      else
          List.map (\p ->
         let
             t = Point.normaliseAngle02pi ((atan2 (Tuple.second p) (Tuple.first p) - this.start_angle) * clockwise this.radius) / this.sweep_angle
             finalPoint = Point.add p this.centre
            |> Point.subtract this.origin
            |> Point.rotate -this.angle
         in
         { name = ""
         , point = finalPoint
         }
          ) allIntersections