module Drawing exposing (Drawing,   
  fromString, circle, html, group, arrow,
  Attribute, on, onClick, onMouseEnter, onMouseLeave, color,
  svg, Color, red, black, class
  )

import Svg exposing (Svg)
import Svg.Attributes as Svg
import Svg.Events
import Point exposing (Point)
import Html
import Json.Decode as D

svg : List (Html.Attribute a) -> Drawing a -> Html.Html a
svg l d =
  d |> drawingToSvg |> List.singleton |> Svg.svg l


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
    = Drawing (Svg a)


drawingToSvg : Drawing a -> Svg a
drawingToSvg d = case d of 
    Drawing c -> c






arrow : List (Attribute a) -> Point -> Point -> Drawing a
arrow attrs from to =
    let
    --    c = attrsColor attrs |> Maybe.withDefault Color.black
        delta = Point.subtract to from

        -- pos = to
        offset = 0 -- 15

        offsetP = Point.normalise offset delta

        pos = Point.subtract to offsetP

        fromOffset = Point.add from offsetP

        tailHeadWidth = 9.764
        tailHeadHeight = 13
        -- from GridCellArrow
        

    in
    
    let (x2, y2) = pos
        (x1, y1) = fromOffset
        (xa, ya) = (x2 - tailHeadHeight / 2, y2 - tailHeadHeight / 2)
        f = String.fromFloat        
    in
    let angle = f <| (\ a -> a * 180 / pi) <| Point.pointToAngle <| delta in
    Svg.g [] [
    Svg.line ([Svg.x1 <| f x1, Svg.x2 <| f x2, Svg.y1 <| f y1, Svg.y2 <| f y2] ++ 
                attrsToSvgAttrs Svg.stroke attrs) [],
                Svg.image
                [Svg.xlinkHref "img/arrow/default.svg",
                Svg.x <| f xa,
                Svg.y <| f ya,
                Svg.width <| f tailHeadWidth,
                Svg.height <| f tailHeadHeight,
                Svg.transform <| 
                    -- "translate(" ++ f (tailHeadWidth / 2) ++ ", " ++ f (tailHeadHeight / 2) ++ ")" ++
                    " rotate(" ++ angle 
                      ++ " " ++ f x2
                      ++ " " ++ f y2 ++ ")"
                ] []
    ]

        |> Drawing







    






fromString : List (Attribute msg) -> Point -> String-> Drawing msg
fromString attrs (x,y) str = 
   
  let f = String.fromFloat in
   Svg.text_ 
     ([Svg.x <| f x, Svg.y <| f y, Svg.textAnchor "middle",
      Svg.dominantBaseline "middle"
     ] ++ attrsToSvgAttrs Svg.fill attrs)
     [Svg.text str]      
       |> Drawing

circle : List (Attribute msg) ->  Point -> Float -> Drawing msg
circle attrs (cx, cy) n = 
  
  let f = String.fromFloat in
  Svg.circle ([Svg.cx <| f cx, Svg.cy <| f cy, Svg.r <| f n ] ++ attrsToSvgAttrs Svg.fill attrs) 
  []
     |> Drawing


html : Point -> Point -> Html.Html a -> Drawing a
html (x1, y1) (width, height) h = 
  let f = String.fromFloat in
  let x = x1 - width / 2
      y = y1 - height / 2
  in
   Svg.foreignObject 
   [Svg.x <| f x, Svg.y <| f y, Svg.width <| f width, Svg.height <| f height]
   [h]
    |> Drawing

group : List (Drawing a) -> Drawing a
group l =
  Svg.g [] (List.map drawingToSvg l) |> Drawing
