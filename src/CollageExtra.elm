module CollageExtra exposing (..)

import Collage exposing (..)
import Collage.Events exposing (onClick)
import Collage.Layout exposing (..)
import Color exposing (..)

-- get the radius of a pont
radius : Point -> Float
radius (x, y) = sqrt (x * x + y * y) 

normaliseP : Float -> Point -> Point
normaliseP len (x, y) =
    let r = radius (x, y) in
    (len * x / r, len * y / r)

orthogonalP : Point -> Point
orthogonalP (x, y) = (0 - y , x)

flip : Point -> Point
flip (x, y) = (0 - x, 0 - y)

flipY : Point -> Point
flipY (x, y) = (x, 0 - y)

minusP : Point -> Point -> Point
minusP (x1, y1)(x2, y2) = (x1 - x2, y1 - y2)

addP : Point -> Point -> Point
addP (x1, y1)(x2, y2) = (x1 + x2, y1 + y2)

resizeP : Float -> Point -> Point
resizeP s (x1, y1) = (x1 * s, y1 * s)

middleP : Point -> Point -> Point
middleP (x1, y1) (x2, y2) = ((x1 + x2) / 2, (y1 + y2) / 2)
-- convert a point to an angle 
pointToAngle : Point -> Float
pointToAngle (x, y) =
    if y == 0 && x <= 0 then
      pi
    else
      2 * atan (y / (x + radius (x, y) ))

-- draws an arrow
arrow : Color.Color -> Point -> Point -> Collage a
arrow c from to =
    let
        delta =  minusP to from
        -- pos = to
        offset = 15
        offsetP = normaliseP offset delta
        pos = minusP to offsetP
        fromOffset = addP from offsetP
    in
      Collage.group
         [ triangle 10
        |> filled (uniform c)
           |> rotate (pointToAngle <| flip <| orthogonalP delta)
              |> shift pos, segment fromOffset pos
               |> traced (solid thin (uniform c))
         ]

collageToRect : Collage a -> (Point, Point)
collageToRect c = (Collage.Layout.bottomLeft c, Collage.Layout.topRight c)

-- arrow between two collages
arrowCollage : Color.Color -> Collage a -> Collage a -> Collage a
arrowCollage c from to =
    let pfrom = Collage.Layout.base from
        pto = Collage.Layout.base to
    in
    let (end, start) = case (rayTraceP_rect pfrom pto (collageToRect to),
                             rayTraceP_rect pto pfrom (collageToRect from)) of
            (Just e, Just s) -> (e, s)
            _ -> (pto, pfrom)
    in
        arrow c start end


--

-- px  -- v -->  | x   returns the y (where v is unitary)

-- rayTraceP_lineX : Float -> Slope -> Float -> Float
-- rayTraceP_lineX px s x = s * (x - px) 

-- slope : Vect -> Float
-- slope (x, y) = y / x
   
pointToList : Point -> List Float
pointToList (px, py) = [px, py]

-- distanceToBox : Point -> Distances -> (Point, Point)
-- distanceToBox (px, py) { toTop, toBottom, toRight, toLeft} =
--     ((x - toLeft, y - toBottom), (x + toRight, y + toTop))

rayTraceP_rect : Point -> Point -> (Point, Point) -> Maybe Point
-- rayTraceP_rect ((px, py) as p) ((x, y) as d) { toTop, toBottom, toRight, toLeft} =
rayTraceP_rect p1 p2 (r1, r2) =
    let v = minusP p2 p1 |> normaliseP 1 in
    let l = pointToList in
    case intersection (l p1) (l v) (l r1, l r2) of
        Just [ix, iy] -> Just (ix, iy)
        _ -> Nothing
    -- let lx = x - toLeft
    --     uy = y + toTop
    --     rx = x + toRight
    --     dy = y - toBottom
    -- in
    -- let yt = if y < py then by else uy 
    --     xt = if x < px then lx else rx 
    -- in
    -- if px == x then
    --     (x, yt)
    -- else
    --     let sl = slope <| (minP d p) in
    --     let yf = rayTraceP_lineX px sl xf in
    --     if dy < y && y < uy then
    --         (xf, yf)
    --     else
    --         let yf2 = (if y < py then dy else uy) in


       



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
    let f = (\ x roi rdi -> (x - roi) / rdi) in
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

