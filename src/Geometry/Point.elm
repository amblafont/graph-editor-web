module Geometry.Point exposing (Point, radius, orthoVectPx, towards, diamondPx, diamondPave,
  normalise, orthogonal, flip, flipY, subtract, add, resize, middle, pointToAngle, toList,
  angleWithInRange, distance, flipAngle, snapToGrid, distanceAngleSigned,
  countRounds, countRoundsAngle, name, unname, NamedPoint, isInPoly,
  -- from quiver
  lerp, lendir, rotate, scale, inv_scale, normaliseAngle, barycenter,
  towardsBentDiagonal, scalarProduct)

import ListExtraExtra as List
-- import Geometry.QuadraticBezier exposing (orthoVectPx)

type alias Point = (Float, Float)
type alias NamedPoint = { x : Float, y : Float}
name : Point -> NamedPoint
name (x, y) = {x = x, y = y}

unname : NamedPoint -> Point
unname {x, y} = (x, y)

scalarProduct : Point -> Point -> Float
scalarProduct (x1, y1) (x2, y2) = x1 * x2 + y1 * y2
-- get the radius of a pont


radius : Point -> Float
radius p = sqrt <| scalarProduct p p



normalise : Float -> Point -> Point
normalise len ( x, y ) =
    let
        r =
            radius ( x, y )
    in
    ( len * x / r, len * y / r )


orthogonal : Point -> Point
orthogonal ( x, y ) =
    ( 0 - y, x )

towards : Point -> Point -> Float -> Point
towards source to shift =
  add source <| normalise shift <| subtract to source 

orthoVectPx : Point -> Point -> Float -> Point
orthoVectPx from to px =
  subtract to from 
   |> orthogonal
   |> normalise px 

diamondPx : Point -> Point -> Float -> Point
diamondPx p1 p2 d =
  let mid = middle p1 p2 in
  add mid <| orthoVectPx p1 p2 d

diamondPave : Point -> Point -> Point -> Point
diamondPave p1 p2 p3 =
  add p1 <| subtract p3 p2
  

flip : Point -> Point
flip ( x, y ) =
    ( 0 - x, 0 - y )


flipY : Point -> Point
flipY ( x, y ) =
    ( x, 0 - y )


subtract : Point -> Point -> Point
subtract ( x1, y1 ) ( x2, y2 ) =
    ( x1 - x2, y1 - y2 )


add : Point -> Point -> Point
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


resize : Float -> Point -> Point
resize s ( x1, y1 ) =
    ( x1 * s, y1 * s )


middle : Point -> Point -> Point
middle ( x1, y1 ) ( x2, y2 ) =
    ( (x1 + x2) / 2, (y1 + y2) / 2 )




distance : Point -> Point -> Float
distance x y = subtract y x |> radius

-- convert a point to an angle


pointToAngle : Point -> Float
pointToAngle ( x, y ) =
    if y == 0 && x <= 0 then
        pi

    else
        2 * atan (y / (x + radius ( x, y )))



toList : Point -> List Float
toList (px, py) = [px, py]


-- return something between -q / 2 and q/2
closeRemainder : Float -> Float -> Float
closeRemainder q a = a - toFloat (round (a / q)) * q

-- returns something between -pi and pi
normaliseAngle : Float -> Float
normaliseAngle alpha = closeRemainder (2 * pi) alpha 

-- normaliseAngle : Float -> Float
-- normaliseAngle alpha = modByFloat (2 * pi) alpha

-- return an angle between 0 and pi
distanceAngle : Float -> Float -> Float 
distanceAngle alpha beta = 
   distanceAngleSigned alpha beta |> abs
   
distanceAngleSigned : Float -> Float -> Float 
distanceAngleSigned alpha beta = 
   normaliseAngle (beta - alpha) 

flipAngle : Float -> Float
flipAngle a = a + pi

angleWithInRange : Float -> Float -> Float -> Bool
angleWithInRange delta alpha beta  =

   distanceAngle alpha beta <= abs delta
   

snapToGrid : Float -> Point -> Point
snapToGrid sizeGrid (px, py) =
   let approx c = toFloat (floor (c / sizeGrid)) * sizeGrid + sizeGrid / 2 in
   (approx px, approx py)

-- distanceToBox : Point -> Distances -> (Point, Point)
-- distanceToBox (px, py) { toTop, toBottom, toRight, toLeft} =
--     ((x - toLeft, y - toBottom), (x + toRight, y + toTop))

sumAngles : List Float -> Float
sumAngles l =
 case l of
      [] -> 0
      [ _ ] -> 0
      a :: b :: tl ->
          let sr = distanceAngleSigned a b in
        --   let _ = Debug.log "r" (a,b, sr) in
          sr + sumAngles (b :: tl)

-- count the number of rounds with respect to the angles
countRounds : List Float -> Int
countRounds l =    
   sumAngles l |> countRoundsAngle

countRoundsAngle : Float -> Int
countRoundsAngle a = a / (2 * pi) |> round


-- Note: Quiver has a more efficient implementation
-- (point_inside_polygon), as it does not use
-- trigonometric functions (I do in Point.pointToAngle)
isInPoly : Point -> List Point -> Bool
isInPoly pos l = 
   let angles = List.map (subtract pos >> pointToAngle) l in
   let anglesLoop = List.succCyclePairs angles
         |> List.map (\ (a, b) -> distanceAngleSigned a b)
         |> List.sum
   in
      --Debug.log "isInPoly" 
      (countRoundsAngle anglesLoop) == 1

-- fron quiver
lerp : Point -> Point -> Float -> Point
lerp this other t =
   add this 
   <| resize t <| subtract other this

lendir : Float -> Float -> Point
lendir length direction = (length * cos direction, length * sin direction)

rotate : Float -> Point -> Point
rotate theta (x, y)  = (x * cos theta - y * sin theta,
                       y * cos theta + x * sin theta
                      )

scale : Float -> Float -> Point -> Point
scale sx sy (x, y) = (x * sx, y * sy)

inv_scale : Float -> Float -> Point -> Point
inv_scale sx sy (x, y) = (x / sx, y / sy)

barycenter : List Point -> Point 
barycenter pts =
   let (xs, ys) = List.unzip pts in
   let length0 = toFloat <| List.length pts in
   let length = if length0 == 0 then 1 else length0 in
   (List.sum xs / length,
    List.sum ys / length)

-- from to1 and to2 defines a triangle.
-- I want to shift px towards somewhere between to1 and to2,
-- depending on how far I am from the start.
-- if I am at the start, I want to go towards to1.
-- if I am at the end, I want to go towards to2.
towardsBentDiagonal : Point -> Point -> Point -> Float -> Point
towardsBentDiagonal from to1 to2 shift =
  let p1 = towards from to1 shift in
  let ratio = shift / distance from to1 in
  let p2 = towards from to2 shift in 
--   let diff = subtract p2 p1 in 

  add p2 <| resize ratio <| subtract p1 p2
