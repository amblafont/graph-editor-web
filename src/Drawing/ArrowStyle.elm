module Drawing.ArrowStyle exposing (makeHeadTailImgs)
import Geometry.Point as Point exposing (Point)

import String.Svg as Svg
import ArrowStyle exposing (ArrowStyle, makeHeadShape, makeTailShape)
import Geometry.QuadraticBezier exposing (QuadraticBezier)

type alias Style = ArrowStyle

-- from GridCellArrow in tikz-cd editor
imgHeadWidth : Float
imgHeadWidth = 9.764

imgTailWidth : Float
imgTailWidth = 3.089 * 2

imgHeight : Float
imgHeight = 13



type alias Svg a = Svg.Svg a
   
makeTheImg : Point -> Float -> Float -> Svg a -> Svg a
makeTheImg (x,y) angle imgWidth content =
     let (xh, yh) = (x - imgWidth / 2, y - imgHeight / 2) in
     let f = String.fromFloat in
     Svg.g
          ([

           Svg.transform <|         
        
          " rotate(" ++ String.fromFloat angle
          ++ " " ++ String.fromFloat x
          ++ " " ++ String.fromFloat y ++ ")"
          ++ "translate(" ++ f xh ++ ", " ++ f yh ++ ")"
           ++ " scale(2)" 
          ]        
          )
           [content]

makeHeadTailImgs : QuadraticBezier -> Style -> List (Svg a)
makeHeadTailImgs {from, to, controlPoint} style =
   
    let angle delta =  Point.pointToAngle delta * 180 / pi in
     
    -- let mkImg = makeImg from to angle in
    [ makeTheImg to (angle <| Point.subtract to controlPoint) 
       imgHeadWidth <| makeHeadShape style,
      makeTheImg from (angle <| Point.subtract controlPoint from) 
       imgTailWidth <|  makeTailShape style
       ]