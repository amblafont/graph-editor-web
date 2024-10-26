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
 { bez : QuadraticBezier,
   fromId : a,
   fromPos : Point,
   toPos : Point
 }

pullshoutHat : Float -> Float -> Extrem a -> Extrem a -> Hat
pullshoutHat shift1 shift2 e1 e2 =
    let ((vertex, vertex1, vertex2), shift) =
            if e1.fromId == e2.fromId then
                ((e1.fromPos, e1.bez.from, e2.bez.from), Bez.shiftFrom)
            else 
                -- this is a pushout
                ((e1.toPos, e1.bez.to, e2.bez.to), Bez.shiftTo)
    in
     let smallshift = 5 in
     

     let r1 = shift e1.bez shift1 -- vertex1 (Bez.middle e1.bez) shift1
         r2 = shift e2.bez shift2
        --  r2 = Point.towards vertex2 (Bez.middle e2.bez) shift2
     in
    --  let vertex = Point.middle vertex1 vertex2 in
     let extrem = Point.diamondPave r1 vertex r2 in
     let s1 = Point.towards r1 extrem smallshift
         s2 = Point.towards r2 extrem smallshift
     in
     { p1 = s1, p2 = s2, summit = extrem }