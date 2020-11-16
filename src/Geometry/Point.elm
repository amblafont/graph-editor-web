module Geometry.Point exposing (Point, radius, orthoVectPx, diamondPx,
  normalise, orthogonal, flip, flipY, subtract, add, resize, middle, pointToAngle, toList)


type alias Point = (Float, Float)



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



-- convert a point to an angle


pointToAngle : Point -> Float
pointToAngle ( x, y ) =
    if y == 0 && x <= 0 then
        pi

    else
        2 * atan (y / (x + radius ( x, y )))



toList : Point -> List Float
toList (px, py) = [px, py]

-- distanceToBox : Point -> Distances -> (Point, Point)
-- distanceToBox (px, py) { toTop, toBottom, toRight, toLeft} =
--     ((x - toLeft, y - toBottom), (x + toRight, y + toTop))

