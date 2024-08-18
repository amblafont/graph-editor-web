module Drawing exposing (Drawing,   
  group, arrow, rect,
  polyLine,
  -- Attribute, simpleOn, on, onClick, onDoubleClick, {- onMouseEnter, onMouseLeave, -} -- color,
  svg,
  -- class, 
  empty, grid, ruler, htmlAnchor,
  makeLatex,
   emptyForeign, toString --, shadowClass
  )

import Zindex exposing (defaultZ, backgroundZ)
import String.Svg as Svg exposing (Svg)
import Geometry.Point exposing (Point)
import Geometry
import Html 
import ArrowStyle exposing (ArrowStyle)
import Drawing.ArrowStyle
import Geometry.QuadraticBezier as Bez exposing (QuadraticBezier)
-- import Geometry
import String.Html exposing (ghostAttribute)
import Drawing.Color as Color exposing (Color)
import Maybe.Extra
import Svg.Attributes exposing (to)
import HtmlDefs
import ArrowStyle exposing (shadow)
import ListExtraExtra as ListExtra

svgHelper : List (String.Html.Attribute a) -> Drawing a -> Svg a
svgHelper l d =
  let (unkeyedList, keyedList) = 
            d |> drawingToZSvgs
            |> List.sortBy .zindex 
            |> List.partition (.key >> Maybe.Extra.isNothing)
  in
  let unkeyedGroup = Svg.g [] (List.map .svg unkeyedList) in
  let keyedGroup = 
        -- Svg.kg []  <|
        List.map (\item -> (Maybe.withDefault "" item.key, item.svg))
        keyedList
  in
  -- Svg.svg l <| (List.map .svg unkeyedList ++ [keyedGroup])
  Svg.ksvg l <| ("unkeyed", unkeyedGroup):: keyedGroup

svg : List (Html.Attribute a) -> Drawing a -> Html.Html a
svg l d =
  svgHelper (List.map ghostAttribute l) d
  |> String.Html.toHtml

toString :  List (String.Html.Attribute a) -> Drawing a -> String
toString l d =
    svgHelper l d
  |> String.Html.toString







{- onMouseEnter : msg -> Attribute msg
onMouseEnter = simpleOn "mouseenter" 

onMouseLeave : msg -> Attribute msg
onMouseLeave = simpleOn "mouseleave"  -}



type Drawing a
    = Drawing (List { shape : Shape a, zindex : Int, key : Maybe String})

type alias LineArg = {from : Point, to : Point, color: Color, strokeWidth : Int}
type alias NodeArg = {label : String, angle : Float, preamble : String, pos : Point, dims : Point}
type alias ArrowArg = {style : ArrowStyle, bezier : QuadraticBezier, strokeWidth : Int}

lineToSvg : LineArg -> List (Html.Attribute a) -> Svg a
lineToSvg arg attrs = 
    let (fromx, fromy) = arg.from in
    let (tox, toy) = arg.to in
    let f = String.fromFloat in
    Svg.line 
            ([Svg.x1 <| f fromx
            , Svg.y1 <| f fromy
            , Svg.x2 <| f tox
            , Svg.y2 <| f toy
            , Svg.strokeFromColor arg.color
            , Svg.strokeWidthPx arg.strokeWidth
      ] ++ List.map ghostAttribute attrs
      ) []

makeLatexString s = "\\(" ++ s ++ "\\)"
withPreamble preamble s = preamble ++ "\n" ++ s

nodeToSvg : NodeArg -> List (Html.Attribute a) -> Svg a
nodeToSvg arg attrs =
   let style = 
        if arg.angle /= 0 then 
          let (x,y) = arg.pos in
          let f = String.fromFloat in
          let angle = f <| arg.angle * 180 / pi in
          [Svg.transform ("rotate(" ++ angle ++ 
               " " ++ f x ++ " " ++ f y ++ ")")]
          -- " " ++ String.fromFloat (fst arg.pos) ++ " " ++ String.fromFloat (snd arg.pos) ++ ")")]
          -- [Svg.style 
          --        <| "transform: "
          --           ++  ("rotate(" ++ String.fromFloat arg.angle ++ "rad)")]
        else
          []
   in
   Svg.g  style [
    htmlAnchorSvg arg.pos arg.dims True
            (makeLatexString arg.label)
            <| HtmlDefs.makeLatex
              attrs
              (withPreamble arg.preamble arg.label)
   ]

arrowToSvg : ArrowArg -> List (Html.Attribute a) -> Svg a
arrowToSvg args attrs0 =
    let arrowStyle = args.style in
    let q = args.bezier in
    let attrs = List.map ghostAttribute attrs0 in
    if ArrowStyle.isNone arrowStyle then
        Svg.g [] []
    else
    -- let zindex = attributesToZIndex attrs in
    let imgs = Drawing.ArrowStyle.makeHeadTailImgs q arrowStyle in    
    let mkgen d l = mkPath {dashed = d, color = arrowStyle.color,
                            strokeWidth = args.strokeWidth }
                      (l ++ attrs) 
    in
    let mkl = mkgen arrowStyle.dashed [] in
    -- let mkshadow = mkgen False [Svg.class shadowClass] in
    
    -- let mkshadow = mkgen False [style "stroke-width : 4;  stroke: white;"] in
    -- overriding the black color with style attribute 
    -- TODO: do it more properly
    -- let mkshadow = mkgen False [style "stroke: white;", strokeWidth "4"] in
    let mkall l = -- List.map mkshadow l ++ 
                  List.map mkl l in
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
    in lines ++ imgs |> Svg.g []

tikzShapeToSvg : TikzShape -> List (Html.Attribute a) -> Svg a
tikzShapeToSvg shape attrs  =
    case shape of
        Node arg -> nodeToSvg arg attrs
        Line arg -> lineToSvg arg attrs
        Arrow arg -> arrowToSvg arg attrs

type TikzShape =
     Node NodeArg
   | Line LineArg
   | Arrow ArrowArg

type Shape a =
      TikzShape (List (Html.Attribute a)) TikzShape
    | SvgShape (Svg a)

shapeToSvg : Shape a -> Svg a
shapeToSvg shape = 
   case shape of
    SvgShape s -> s
    TikzShape attrs s -> tikzShapeToSvg s attrs

empty : Drawing a
empty = Drawing []

ofSvgs : Int -> List (Svg a) -> Drawing a
ofSvgs z l = Drawing <| List.map (\s -> { shape = SvgShape s, zindex = z, key = Nothing }) l 


ofSvg : Int -> Svg a -> Drawing a
ofSvg z s = ofSvgs z [ s ]

ofShapeWithKey : Int -> Maybe String -> Shape a -> Drawing a
ofShapeWithKey z k s = Drawing [{ shape = s, zindex = z, key = k }]


ofShape : Int -> Shape a -> Drawing a
ofShape z s = ofShapeWithKey z Nothing s

ofSvgWithKey : Int -> Maybe String -> Svg a -> Drawing a
ofSvgWithKey z k s = Drawing [{ shape = SvgShape s, zindex = z, key = k }]

drawingToZSvgs : Drawing a -> List { svg : Svg a, zindex : Int, key:Maybe String}
drawingToZSvgs (Drawing c) = 
   List.map (\{shape, zindex, key} -> { svg = shapeToSvg shape, zindex = zindex, key = key }) c


dashedToAttrs : Bool -> List (Svg.Attribute a)
dashedToAttrs dashed =  
            if dashed then
              [ Svg.strokeDasharray ArrowStyle.dashedStr]
            else 
              []


quadraticBezierToAttr : QuadraticBezier -> Svg.Attribute a 
quadraticBezierToAttr  {from, to, controlPoint } =
  let f = String.fromFloat in
  let p (x1, x2) = f x1 ++ " " ++ f x2 in    
    Svg.d  <|
    "M" ++ p from 
    ++ " Q " ++ p controlPoint
    ++ ", " ++ p to

mkPath : {dashed:Bool, color:Color, strokeWidth : Int} -> List (Svg.Attribute a) -> QuadraticBezier -> Svg a
mkPath arg attrs q =
  Svg.path 
  ( quadraticBezierToAttr q ::
    Svg.fill "none" :: 
      Svg.strokeFromColor arg.color
      ::
      attrs
      ++
      dashedToAttrs arg.dashed
      ++
      (if arg.strokeWidth /= 1 then [Svg.strokeWidthPx arg.strokeWidth] else [])
  )
  []

makeLatex : { zindex:Int,
              label : String, preamble : String, pos : Point, dims : Point
              , angle : Float} 
              -> List (Html.Attribute a) -> Drawing a
makeLatex arg attrs = 
  Node {angle = arg.angle, label = arg.label, preamble = arg.preamble, pos = arg.pos, dims = arg.dims}
  |> TikzShape attrs
  |> ofShape arg.zindex 

shadowWidth = 4

polyLine : {zindex:Int, color: Color, points : List Point} -> List (Html.Attribute a) -> Drawing a
polyLine args attrs =
   let pairs = ListExtra.succPairs args.points in
   let normalArg (from, to) = {from = from, to = to, color = args.color, strokeWidth = 1} in
   let shadowArg (from, to) = {from = from, to = to, color = Color.white, strokeWidth = shadowWidth} in
   let makeShape arg = Line arg |> TikzShape attrs |> ofShape args.zindex in
   let shadow = List.map shadowArg pairs |> List.map makeShape in
   let normal = List.map normalArg pairs |> List.map makeShape in
   group <| shadow ++ normal




{-line : {zindex:Int, color: Color} -> List (Html.Attribute a) -> Point -> Point -> Drawing a
line args attrs from to = 
  let normalArg = {from = from, to = to, color = args.color, strokeWidth = 1} in
  let shadowArg = {from = from, to = to, color = Color.white, strokeWidth = shadowWidth} in
  let makeShape arg = Line arg |> TikzShape attrs |> ofShape args.zindex in
  group [makeShape shadowArg, makeShape normalArg]
-}

arrow : {zindex : Int, style : ArrowStyle, bezier : QuadraticBezier} -> List (Html.Attribute a) -> Drawing a
arrow args attrs0 =
    let normalArg = { bezier = args.bezier, style = args.style,  strokeWidth = 1} in
    let shadowArg = { bezier = args.bezier, style = ArrowStyle.shadow args.style, strokeWidth = shadowWidth} in
    let makeShape arg = Arrow arg |> TikzShape attrs0 |> ofShape args.zindex in
    group [makeShape shadowArg, makeShape normalArg]
{-
    let arrowStyle = args.style in
    let q = args.bezier in
    let attrs = List.map ghostAttribute attrs0 in
    let zindex = args.zindex in
    if ArrowStyle.isNone arrowStyle then
        empty
    else
    -- let zindex = attributesToZIndex attrs in
    let imgs = Drawing.ArrowStyle.makeHeadTailImgs q arrowStyle in    
    let mkgen d l = mkPath {dashed = d, color = arrowStyle.color }
                      (l ++ attrs) 
    in
    let mkl = mkgen arrowStyle.dashed [] in
    let mkshadow = mkgen False [Svg.class shadowClass] in
    
    -- let mkshadow = mkgen False [style "stroke-width : 4;  stroke: white;"] in
    -- overriding the black color with style attribute 
    -- TODO: do it more properly
    -- let mkshadow = mkgen False [style "stroke: white;", strokeWidth "4"] in
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
    in lines ++ imgs |> ofSvgs zindex -}





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

ruler : Int -> Drawing a
ruler offset =
-- draw a vertical line at given offset 
  let f = String.fromInt in
  let z = defaultZ in
  Svg.line 
  ([Svg.x1 <| f offset, Svg.x2 <| f offset, Svg.y1 "0", Svg.y2 "100%"]
  ++ [ Svg.strokeFromColor Color.black])
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
          Svg.fill "none", Svg.stroke "gray", Svg.strokeWidthPx 1
          ] []
        ]
        ],
        Svg.rect [Svg.width "100%", Svg.height "100%", Svg.fill "url(#grid)"] []
        ]         
        |> ofSvgs defaultZ








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

htmlAnchorSvg : Point -> Point -> Bool -> String -> Html.Html a -> Svg a
htmlAnchorSvg (x1, y1) (width, height) center str h = 
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
htmlAnchor : Maybe String -> Int -> Point -> Point -> Bool -> String -> Html.Html a -> Drawing a
htmlAnchor key z (x1, y1) (width, height) center str h = 
  htmlAnchorSvg (x1, y1) (width, height) center str h
    |> ofSvgWithKey z key

group : List (Drawing a) -> Drawing a
group l =
  (List.map (\(Drawing d) -> d) l) |> List.concat |> Drawing
