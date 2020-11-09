module Point exposing (Point, radius, normalise, orthogonal, flip, flipY, subtract, add, resize, middle, pointToAngle, raytraceRect)


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



pointToList : Point -> List Float
pointToList (px, py) = [px, py]

-- distanceToBox : Point -> Distances -> (Point, Point)
-- distanceToBox (px, py) { toTop, toBottom, toRight, toLeft} =
--     ((x - toLeft, y - toBottom), (x + toRight, y + toTop))

raytraceRect : Point -> Point -> (Point, Point) -> Maybe Point
-- raytraceRect ((px, py) as p) ((x, y) as d) { toTop, toBottom, toRight, toLeft} =
raytraceRect p1 p2 (r1, r2) =
    let v = subtract p2 p1 |> normalise 1 in
    let l = pointToList in
    case intersection (l p1) (l v) (l r1, l r2) of
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

