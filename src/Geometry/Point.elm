module Geometry.Point exposing (Point, radius, orthoVectPx, diamondPx,
  normalise, orthogonal, flip, flipY, subtract, add, resize, middle, pointToAngle, toList,
  angleWithInRange, distance, flipAngle, snapToGrid, distanceAngleSigned,
  countRounds, name, unname, NamedPoint, isInPoly,
  -- from quiver
  lerp, lendir, rotate, scale, inv_scale)



type alias Point = (Float, Float)
type alias NamedPoint = { x : Float, y : Float}
name : Point -> NamedPoint
name (x, y) = {x = x, y = y}

unname : NamedPoint -> Point
unname {x, y} = (x, y)


-- get the radius of a pont


radius : Point -> Float
radius ( x, y ) =
    sqrt (x * x + y * y)



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

orthoVectPx : Point -> Point -> Float -> Point
orthoVectPx from to px =
  subtract to from 
   |> orthogonal
   |> normalise px 

diamondPx : Point -> Point -> Float -> Point
diamondPx p1 p2 d =
  let mid = middle p1 p2 in
  add mid <| orthoVectPx p1 p2 d


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
   sumAngles l / (2 * pi) |> round 


-- Note: Quiver has a more efficient implementation
-- (point_inside_polygon), as it does not use
-- trigonometric functions (I do in Point.pointToAngle)
isInPoly : Point -> List Point -> Bool
isInPoly pos l = 
   let angles = List.map (subtract pos >> pointToAngle) l in
   let anglesLoop = 
         case angles of
            t :: _ -> angles ++ [ t ]
            [] -> []
   in
      --Debug.log "isInPoly" 
      (countRounds anglesLoop) == 1

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