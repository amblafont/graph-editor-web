module Drawing exposing (Drawing,   
  fromString, circle, html, group, arrow, rect,
  Attribute, simpleOn, on, onClick, onDoubleClick, {- onMouseEnter, onMouseLeave, -} color,
  svg, Color, red, black, class, empty, grid
  )

import Svg exposing (Svg)
import Svg as S
import Svg.Attributes as Svg
import Svg as SvgElts
import Svg.Events
import Geometry.Point exposing (Point)
import Geometry
import Json.Decode as D
import Html 
import ArrowStyle exposing (ArrowStyle)
import Geometry.QuadraticBezier as Bez exposing (QuadraticBezier)
-- import Geometry
import Svg
import Collage.Layout exposing (bottomRight)
import Html.Events.Extra.Mouse as MouseEvents

svg : List (Html.Attribute a) -> Drawing a -> Html.Html a
svg l d =
  d |> drawingToSvgs |> Svg.svg l


attrToSvgAttr : (String -> Svg.Attribute a) -> Attribute a -> Maybe (Svg.Attribute a)
attrToSvgAttr col a =
  case a of
     Color c -> c |> colorToString |> col |> Just     
     Class s -> Svg.class s |> Just
     On e d -> Svg.Events.on e d |> Just
     OnClick f -> MouseEvents.onClick f |> Just
     OnDoubleClick f -> MouseEvents.onDoubleClick f |> Just             

attrsToSvgAttrs : (String -> Svg.Attribute a) -> List (Attribute a) -> List (Svg.Attribute a)
attrsToSvgAttrs f = List.filterMap (attrToSvgAttr f)

type Attribute msg =
    On String (D.Decoder msg)
    | Color Color
    | Class String
    | OnClick (MouseEvents.Event -> msg)
    | OnDoubleClick (MouseEvents.Event -> msg)      

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

onClick : (MouseEvents.Event -> msg) -> Attribute msg
onClick = OnClick 

onDoubleClick : (MouseEvents.Event -> msg) -> Attribute msg
onDoubleClick = OnDoubleClick 

{- onMouseEnter : msg -> Attribute msg
onMouseEnter = simpleOn "mouseenter" 

onMouseLeave : msg -> Attribute msg
onMouseLeave = simpleOn "mouseleave"  -}

color : Color -> Attribute msg
color = Color

type Drawing a
    = Drawing (List (Svg a))

empty : Drawing a
empty = Drawing []

ofSvg : Svg a -> Drawing a
ofSvg s = Drawing [ s ]

drawingToSvgs : Drawing a -> List (Svg a)
drawingToSvgs d = case d of 
    Drawing c -> c

dashedToAttrs : Bool -> List (Svg.Attribute a)
dashedToAttrs dashed =  
            if dashed then
              [ Svg.strokeDasharray ArrowStyle.dashedStr]
            else 
              []


{- mkLine : Bool -> List (Attribute a) -> Point -> Point -> Svg a
mkLine dashed attrs (x1, y1) (x2, y2) =
  
  let f = String.fromFloat in
    
    Svg.line ([Svg.x1 <| f x1, Svg.x2 <| f x2, Svg.y1 <| f y1, Svg.y2 <| f y2] 
                ++ 
                attrsToSvgAttrs Svg.stroke attrs
                ++
                dashedToAttrs dashed
              ) []
 -}
quadraticBezierToAttr : QuadraticBezier -> Svg.Attribute a 
quadraticBezierToAttr  {from, to, controlPoint } =
  let f = String.fromFloat in
  let p (x1, x2) = f x1 ++ " " ++ f x2 in    
    Svg.d  <|
    "M" ++ p from 
    ++ " Q " ++ p controlPoint
    ++ ", " ++ p to

mkPath : Bool -> List (Attribute a) -> QuadraticBezier -> Svg a
mkPath dashed attrs q =
  SvgElts.path 
  ( quadraticBezierToAttr q ::
    Svg.fill "none" ::   
      attrsToSvgAttrs Svg.stroke attrs
      ++
      dashedToAttrs dashed
  )
  []        


arrow : List (Attribute a) -> ArrowStyle -> QuadraticBezier -> Drawing a
arrow attrs style q =
    let imgs = ArrowStyle.makeHeadTailImgs q style in    
    let mkl = mkPath style.dashed attrs in
    let lines = if ArrowStyle.isDouble style then
                -- let delta = Point.subtract q.to q.controlPoint 
                --             |> Point.orthogonal
                --             |> Point.normalise ArrowStyle.doubleSize
                -- in
              
                [ mkl (Bez.orthoVectPx (0 - ArrowStyle.doubleSize ) q),
                  mkl (Bez.orthoVectPx ArrowStyle.doubleSize q)
                ]
        
                else
                    [ mkl q ]
    in lines ++ imgs |> Drawing



rect : Geometry.Rect -> Drawing a
rect { topLeft, bottomRight } =
   let (fromx, fromy) = topLeft
       (tox, toy) = bottomRight
       f = String.fromFloat 
   in
   Svg.rect 
      [Svg.x <| f <| fromx,
       Svg.y <| f <| fromy,
       Svg.width <| f  <| (tox - fromx),
       Svg.height <| f <| (toy - fromy),
       Svg.class "rect-select"
       ]
       [] 
       |> ofSvg


grid : Int -> Drawing a
grid n =
  let sn = String.fromInt n in
  [Svg.defs [] [Svg.pattern [ Svg.id "grid", 
        Svg.width sn,
        Svg.height sn,
        Svg.patternUnits "userSpaceOnUse"] 
        [ -- Svg.rect [Svg.width sn, Svg.height sn] []
         S.path [Svg.d ("M " ++ sn ++ " 0 L 0 0 0 " ++ sn),
          Svg.fill "none", Svg.stroke "gray", Svg.strokeWidth "1px"
          ] []
        ]
        ],
        Svg.rect [Svg.width "100%", Svg.height "100%", Svg.fill "url(#grid)"] []
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
   [Svg.x <| f x, Svg.y <| f y
   -- otherwise, katex breaks lines
     , Svg.width <| "100%"
     , Svg.height <| f height
     -- , Svg.width <| f width, Svg.height <| f height
     ]
   [h]
    |> ofSvg

group : List (Drawing a) -> Drawing a
group l =
  (List.map drawingToSvgs l) |> List.concat |> Drawing
