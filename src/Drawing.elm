module Drawing exposing (Drawing,   
  fromString, circle, html, group, arrow,
  Attribute, on, onClick, onMouseEnter, onMouseLeave, color,
  svg, Color, red, black, class
  )

import Svg exposing (Svg)
import Svg.Attributes as Svg
import Svg.Events
import Point exposing (Point)
import Json.Decode as D
import Html 
import ArrowStyle

svg : List (Html.Attribute a) -> Drawing a -> Html.Html a
svg l d =
  d |> drawingToSvgs |> Svg.svg l


attrToSvgAttr : (String -> Svg.Attribute a) -> Attribute a -> Maybe (Svg.Attribute a)
attrToSvgAttr col a =
  case a of
     Color c -> c |> colorToString |> col |> Just
     On e d -> Svg.Events.on e d |> Just
     Class s -> Svg.class s |> Just

attrsToSvgAttrs : (String -> Svg.Attribute a) -> List (Attribute a) -> List (Svg.Attribute a)
attrsToSvgAttrs f = List.filterMap (attrToSvgAttr f)

type Attribute msg =
    On String (D.Decoder msg)
    | Color Color
    | Class String

type Color = Black | Red

colorToString : Color -> String
colorToString c = case c of
  Black -> "black"
  Red -> "red"

black : Color
black = Black

red : Color
red = Red

class : String -> Attribute msg
class = Class


on : String -> D.Decoder msg -> Attribute msg
on = On

simpleOn : String -> msg -> Attribute msg
simpleOn s m = on s (D.succeed m)

onClick : msg -> Attribute msg
onClick = simpleOn "click" 

onMouseEnter : msg -> Attribute msg
onMouseEnter = simpleOn "mouseenter" 

onMouseLeave : msg -> Attribute msg
onMouseLeave = simpleOn "mouseleave" 

color : Color -> Attribute msg
color = Color

type Drawing a
    = Drawing (List (Svg a))

ofSvg : Svg a -> Drawing a
ofSvg s = Drawing [ s ]

drawingToSvgs : Drawing a -> List (Svg a)
drawingToSvgs d = case d of 
    Drawing c -> c


mkLine : Bool -> List (Attribute a) -> Point -> Point -> Svg a
mkLine dashed attrs (x1, y1) (x2, y2) =
  let f = String.fromFloat in
    
    Svg.line ([Svg.x1 <| f x1, Svg.x2 <| f x2, Svg.y1 <| f y1, Svg.y2 <| f y2] 
                ++ 
                attrsToSvgAttrs Svg.stroke attrs
                ++
            if dashed then
              [ Svg.strokeDasharray ArrowStyle.dashedStr]
            else 
              []
              ) []
                

arrow : List (Attribute a) -> ArrowStyle.Style -> Point -> Point -> Drawing a
arrow attrs style from to =
    let imgs = ArrowStyle.makeHeadTailImgs from to style in    
    let mkl = mkLine style.dashed attrs in
    let lines = if ArrowStyle.isDouble style then
                let delta = Point.subtract to from 
                            |> Point.orthogonal
                            |> Point.normalise ArrowStyle.doubleSize
                in
              
                [ mkl (Point.add from delta) (Point.add to delta),
                  mkl (Point.subtract from delta) (Point.subtract to delta)
                ]
        
                else
                    [ mkl from to ]
    in lines ++ imgs |> Drawing







    






fromString : List (Attribute msg) -> Point -> String-> Drawing msg
fromString attrs (x,y) str = 
   
  let f = String.fromFloat in
   Svg.text_ 
     ([Svg.x <| f x, Svg.y <| f y, Svg.textAnchor "middle",
      Svg.dominantBaseline "middle"
     ] ++ attrsToSvgAttrs Svg.fill attrs)
     [Svg.text str]      
       |> ofSvg

circle : List (Attribute msg) ->  Point -> Float -> Drawing msg
circle attrs (cx, cy) n = 
  
  let f = String.fromFloat in
  Svg.circle ([Svg.cx <| f cx, Svg.cy <| f cy, Svg.r <| f n ] ++ attrsToSvgAttrs Svg.fill attrs) 
  []
     |> ofSvg


html : Point -> Point -> Html.Html a -> Drawing a
html (x1, y1) (width, height) h = 
  let f = String.fromFloat in
  let x = x1 - width / 2
      y = y1 - height / 2
  in
   Svg.foreignObject 
   [Svg.x <| f x, Svg.y <| f y, Svg.width <| f width, Svg.height <| f height]
   [h]
    |> ofSvg

group : List (Drawing a) -> Drawing a
group l =
  (List.map drawingToSvgs l) |> List.concat |> Drawing
