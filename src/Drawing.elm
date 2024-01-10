module Drawing exposing (Drawing,   
  fromString, circle, group, arrow, rect,
  line,
  Attribute, simpleOn, on, onClick, onDoubleClick, {- onMouseEnter, onMouseLeave, -} color,
  svg,
  class, empty, grid, htmlAnchor,
  zindexAttr, emptyForeign, toString
  )

import Zindex exposing (defaultZ, backgroundZ)
import String.Svg as Svg exposing (Svg)
import Geometry.Point exposing (Point)
import Geometry
import Json.Decode as D
import Html 
import ArrowStyle exposing (ArrowStyle)
import Geometry.QuadraticBezier as Bez exposing (QuadraticBezier)
-- import Geometry
import Svg.Events
import Html.Events.Extra.Mouse as MouseEvents
import List.Extra
import Msg exposing (Msg)
import String.Html exposing (ghostAttribute)
import Drawing.Color as Color exposing (Color)

svgHelper : List (String.Html.Attribute a) -> Drawing a -> Svg a
svgHelper l d =
  d |> drawingToZSvgs
  |> List.sortBy .zindex 
  |> List.map .svg
  |> Svg.svg l

svg : List (Html.Attribute a) -> Drawing a -> Html.Html a
svg l d =
  svgHelper (List.map ghostAttribute l) d
  |> String.Html.toHtml

toString :  List (String.Html.Attribute a) -> Drawing a -> String
toString l d =
    svgHelper l d
  |> String.Html.toString


attrToSvgAttr : (String -> Svg.Attribute a) -> Attribute a -> Maybe (Svg.Attribute a)
attrToSvgAttr col a =
  case a of
     Color c -> c |> Color.toString |> col |> Just     
     Class s -> Svg.class s |> Just
     Style s -> Svg.style s |> Just
     StrokeWidth s -> Svg.strokeWidth s |> Just
     On e d -> Svg.Events.on e d |> ghostAttribute |> Just
     OnClick f -> MouseEvents.onClick f |> ghostAttribute |> Just
     OnDoubleClick f -> MouseEvents.onDoubleClick f |> ghostAttribute |> Just
     ZIndex _ -> Nothing          

attrsToSvgAttrs : (String -> Svg.Attribute a) -> List (Attribute a) -> List (Svg.Attribute a)
attrsToSvgAttrs f = List.filterMap (attrToSvgAttr f)

type Attribute msg =
    On String (D.Decoder msg)
    | Color Color
    | Class String
    | StrokeWidth String
    | Style String
    | OnClick (MouseEvents.Event -> msg)
    | OnDoubleClick (MouseEvents.Event -> msg) 
    | ZIndex Int     

attributeToZIndex : Attribute msg -> Maybe Int
attributeToZIndex a = case a of
      ZIndex n -> Just n
      _ -> Nothing

attributesToZIndex : List (Attribute msg) -> Int
attributesToZIndex =
  List.Extra.findMap attributeToZIndex
  >> Maybe.withDefault defaultZ

class : String -> Attribute msg
class = Class

style : String -> Attribute msg
style = Style

strokeWidth : String -> Attribute msg
strokeWidth = StrokeWidth



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

zindexAttr : Int -> Attribute Msg
zindexAttr = ZIndex

type Drawing a
    = Drawing (List { svg : Svg a, zindex : Int})



empty : Drawing a
empty = Drawing []

ofSvgs : Int -> List (Svg a) -> Drawing a
ofSvgs z l = Drawing <| List.map (\s -> { svg = s, zindex = z }) l 


ofSvg : Int -> Svg a -> Drawing a
ofSvg z s = ofSvgs z [ s ]

drawingToZSvgs : Drawing a -> List { svg : Svg a, zindex : Int}
drawingToZSvgs (Drawing c) = c


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
  Svg.path 
  ( quadraticBezierToAttr q ::
    Svg.fill "none" ::   
      attrsToSvgAttrs Svg.stroke attrs
      ++
      dashedToAttrs dashed
  )
  []        


arrow : List (Attribute a) -> ArrowStyle -> QuadraticBezier -> Drawing a
arrow attrs0 arrowStyle q =
    let attrs = Color arrowStyle.color :: attrs0 in
    let zindex = attributesToZIndex attrs in
    let imgs = ArrowStyle.makeHeadTailImgs q arrowStyle in    
    let mkgen d l = mkPath d (l ++ attrs) in
    let mkl = mkgen arrowStyle.dashed [] in
    -- let mkshadow = mkgen False [class "shadow-line"] in
    
    -- let mkshadow = mkgen False [style "stroke-width : 4;  stroke: white;"] in
    -- overriding the black color with style attribute 
    -- TODO: do it more properly
    let mkshadow = mkgen False [style "stroke: white;", strokeWidth "4"] in
    let mkall l = List.map mkshadow l ++ List.map mkl l in
    let lines = if ArrowStyle.isDouble arrowStyle then
                -- let delta = Point.subtract q.to q.controlPoint 
                --             |> Point.orthogonal
                --             |> Point.normalise ArrowStyle.doubleSize
                -- in
              
                mkall [ (Bez.orthoVectPx (0 - ArrowStyle.doubleSize ) q),
                    (Bez.orthoVectPx ArrowStyle.doubleSize q)
                ]
        
                else
                    mkall [ q ]
    in lines ++ imgs |> ofSvgs zindex

line : List (Attribute a) -> Point -> Point -> Drawing a
line l (fromx, fromy) (tox, toy) = 
   let z = attributesToZIndex l in
   let f = String.fromFloat in
      Svg.line 
      ([Svg.x1 <| f fromx
      , Svg.y1 <| f fromy
      , Svg.x2 <| f tox
      , Svg.y2 <| f toy
      ] ++ attrsToSvgAttrs Svg.stroke l)
      []
      |> ofSvg z



rect : Int -> Geometry.Rect -> Drawing a
rect z { topLeft, bottomRight } =
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
       |> ofSvg z


grid : Int -> Drawing a
grid n =
  let sn = String.fromInt n in
  [Svg.defs [] [Svg.pattern [ Svg.id "grid", 
        Svg.width sn,
        Svg.height sn,
        Svg.patternUnits "userSpaceOnUse"] 
        [ -- Svg.rect [Svg.width sn, Svg.height sn] []
         Svg.path [Svg.d ("M " ++ sn ++ " 0 L 0 0 0 " ++ sn),
          Svg.fill "none", Svg.stroke "gray", Svg.strokeWidth "1px"
          ] []
        ]
        ],
        Svg.rect [Svg.width "100%", Svg.height "100%", Svg.fill "url(#grid)"] []
        ]         
        |> ofSvgs defaultZ







fromString : List (Attribute msg) -> Point -> String-> Drawing msg
fromString attrs (x,y) str = 
  let z = attributesToZIndex attrs in
  let f = String.fromFloat in
   Svg.text_ 
     ([Svg.x <| f x, Svg.y <| f y, Svg.textAnchor "middle",
      Svg.dominantBaseline "middle"
     ] ++ attrsToSvgAttrs Svg.fill attrs)
     [Svg.text str]      
       |> ofSvg z

circle : List (Attribute msg) ->  Point -> Float -> Drawing msg
circle attrs (cx, cy) n = 
  let z = attributesToZIndex attrs in
  let f = String.fromFloat in
  Svg.circle ([Svg.cx <| f cx, Svg.cy <| f cy, Svg.r <| f n ] ++ attrsToSvgAttrs Svg.fill attrs) 
  []
     |> ofSvg z

-- This is a trick to prevent unwanted scrolling on google chrome
-- when editing stuff
emptyForeign : Drawing a
emptyForeign = 
  Svg.foreignObject 
  -- Svg.text_
   [Svg.x "1", Svg.y "1", Svg.width "100%", Svg.height "100%"
     ]
   []
   -- put it in the very background
    |> ofSvg (2 * backgroundZ)


htmlAnchor : Int -> Point -> Point -> Bool -> String -> Html.Html a -> Drawing a
htmlAnchor z (x1, y1) (width, height) center str h = 
  let f = String.fromFloat in
  let (x, y) = if center then (x1 - width / 2, y1 - height / 2) else (x1, y1) in
   Svg.foreignObject 
   [Svg.x <| f x, Svg.y <| f y
   -- otherwise, katex breaks lines
     , Svg.width <| "100%"
     , Svg.height <| f height
     -- , Svg.width <| f width, Svg.height <| f height
     ]
   [String.Html.customNode str h]
    |> ofSvg z

group : List (Drawing a) -> Drawing a
group l =
  (List.map drawingToZSvgs l) |> List.concat |> Drawing
