module EdgeShape exposing (Hat, EdgeShape(..), pullshoutHat)


import Geometry.Point as Point exposing (Point)
import Geometry.QuadraticBezier as Bez exposing (QuadraticBezier)

type alias Hat = {p1 : Point, p2 : Point, summit : Point}

type EdgeShape =
    Bezier QuadraticBezier
  | HatShape Hat
  | None

-- posOfShape : EdgeShape -> Point
-- posOfShape e = 
--    case e of
--       Bezier b -> Bez.middle b
--       HatShape h -> h.summit
--       None -> (0, 0)

type alias Extrem a =
 { controlPoint : Point,
   fromId : a,
   fromPos : Point,
   toPos : Point}

pullshoutHat : Extrem a -> Extrem a -> Hat
pullshoutHat e1 e2 =
    let vertex =
            if e1.fromId == e2.fromId then
                e1.fromPos
            else 
                e1.toPos
    in
     let shift = 30 in
    --  let shift = 15 in
     let smallshift = 5 in

     let r1 = Point.towards vertex e1.controlPoint shift
         r2 = Point.towards vertex e2.controlPoint shift
     in
     let extrem = Point.diamondPave r1 vertex r2 in
     let s1 = Point.towards r1 extrem smallshift
         s2 = Point.towards r2 extrem smallshift
     in
     { p1 = s1, p2 = s2, summit = extrem }