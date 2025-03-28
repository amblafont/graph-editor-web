module Drawing exposing (Drawing,   
  group, arrow, rect,
  polyLine,
  -- Attribute, simpleOn, on, onClick, onDoubleClick, {- onMouseEnter, onMouseLeave, -} -- color,
  svg, tikz,
  -- class, 
  empty, grid, ruler, htmlAnchor,
  makeLatex,
   emptyForeign, toString --, shadowClass
  )

import Zindex exposing (defaultZ, backgroundZ)
import String.Svg as Svg exposing (Svg)
import Geometry.Point as Point exposing (Point)
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

keyPartition (Drawing l) =
    let (unkeyedList, keyedList) =
            List.sortBy .zindex l
            |>  List.partition (.key >> Maybe.Extra.isNothing)
    in
    (unkeyedList, keyedList)

drawingToSvgs : Drawing a -> List { svg : Svg a, key:Maybe String}
drawingToSvgs (Drawing c) = 
   List.map (\{shape, key} -> { svg = shapeToSvg shape, key = key }) c

tikz : Drawing a -> String 
tikz d =
  let (unkeyedList, keyedList) = keyPartition d in
  let data = List.map (.shape >> shapeToTikz) (unkeyedList ++ keyedList)
           |> String.join "\n"
  in
  "\\begin{tikzpicture}\n" ++ data ++ "\n\\end{tikzpicture}"

svgHelper : List (String.Html.Attribute a) -> Drawing a -> Svg a
svgHelper l d =
  let (unkeyedList, keyedList) = 
          let (unkeyed, keyed) = keyPartition d in 
          (drawingToSvgs (Drawing unkeyed), 
           drawingToSvgs (Drawing keyed))
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
type alias NodeArg = {label : String, angle : Float, preamble : String, pos : Point, scale: Float, dims : Point}
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

nodeToTikz : NodeArg -> String
nodeToTikz arg =
    let (x, y) = arg.pos in
    -- TODO: faire la normalisation
    let rotate = 
          if arg.angle == 0 then [] else
          ["rotate=" ++ String.fromFloat (0 - arg.angle * 180 / pi)]
    in
    let scale =
          if arg.scale == 1 then [] else
          ["scale=" ++ String.fromFloat arg.scale]
    in
    let options = 
          let loptions = rotate ++ scale in
          if loptions == [] then "" else
          "[" ++ String.join "," loptions ++ "]"
    in
    "\\node" ++ options ++ " at "
    ++ pointToTikz (x,y)
        ++ " {$"
        ++ arg.label
        ++ "$} ;"


dimToTikz : Float -> String
-- d / 21
-- 17.7667
-- tikz uses 1.2 em size
dimToTikz d = String.fromFloat (d / (16 * 1.2)) ++ "em"

pointToTikz : Point -> String
pointToTikz (x,y) = 
  "(" ++ dimToTikz x ++ "," 
    ++ dimToTikz (0 - y) ++ ")"
    

nodeToSvg : NodeArg -> List (Html.Attribute a) -> Svg a
nodeToSvg arg attrs =
   let f = String.fromFloat in
   let (x, y) = arg.pos in
   let (width, height) = arg.dims in
   let angleOption = 
        if arg.angle == 0 then [] else        
        let angle = f <| arg.angle * 180 / pi in          
        ["rotate(" ++ angle 
                  ++ " " ++ f (width / 2) ++ " " ++ f (height / 2) ++ ")"]
   in
   let rescaleOption = 
        if arg.scale == 1 then [] else
        -- []
        let scale = f arg.scale in
        ["scale(" ++ scale ++ 
                  ")"]
   in 
   let translateOption = -- []
          ["translate(" ++ f (x - width / 2) ++ "," 
                        ++ f (y - height / 2) ++ ")" ]
   in
         
   let style = 
          let list = translateOption ++ angleOption ++ rescaleOption 
          in
          if list == [] then [] else
          [Svg.transform <| String.join " " list]
   in

   Svg.g  style [
    htmlAnchorSvg (0,0) -- arg.pos
           arg.dims False
            (makeLatexString arg.label)
            <| HtmlDefs.makeLatex
              attrs
              (withPreamble arg.preamble arg.label)
   ]




arrowToTikz : ArrowArg -> String
arrowToTikz args =
    let width =  
          if args.strokeWidth == 1 then "" else
          "line width=" ++ dimToTikz (toFloat args.strokeWidth)
    in
    let bez = Bez.toCubic args.bezier in
    "\\draw[" ++ ArrowStyle.tikzStyle args.style ++ width ++ "] "
    ++ pointToTikz bez.from
    ++ " .. controls "
    -- ++ "to[quadratic="
    ++ pointToTikz bez.controlPoint1
    -- ++ "] "
    ++ " and "
    ++ pointToTikz bez.controlPoint2
    ++ " .. "
    ++ pointToTikz bez.to
    ++ ";"


lineToTikz : LineArg -> String
lineToTikz arg =
    "\\draw[" ++ Color.toString arg.color ++ "] "
    ++ pointToTikz arg.from
    ++ " -- "
    ++ pointToTikz arg.to
    ++ ";"


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
    let mkgen l = mkPath {wavy=arrowStyle.wavy, dashed = arrowStyle.dashed, color = arrowStyle.color,
                            strokeWidth = args.strokeWidth }
                      (l ++ attrs) 
    in
    let mkl = mkgen [] in
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

tikzShapeToTikz : TikzShape -> String
tikzShapeToTikz shape =
    case shape of
        Node arg -> nodeToTikz arg
        Line arg -> lineToTikz arg
        Arrow arg -> arrowToTikz arg

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

shapeToTikz : Shape a -> String
shapeToTikz shape = 
   case shape of
    SvgShape s -> ""
    TikzShape _ s -> tikzShapeToTikz s

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



dashedToAttrs : Bool -> List (Svg.Attribute a)
dashedToAttrs dashed =  
            if dashed then
              [ Svg.strokeDasharray ArrowStyle.dashedStr]
            else 
              []


qBezToPath : QuadraticBezier -> String
qBezToPath {from, to, controlPoint} =
  let f = String.fromFloat in
  let p (x1, x2) = f x1 ++ " " ++ f x2 in    
    "M" ++ p from 
    ++ " Q " ++ p controlPoint
    ++ ", " ++ p to

generateWavyPath : QuadraticBezier -> String
generateWavyPath ({from, to, controlPoint} as b) =
  let amplitude = 2 in
  let ratio = 2 in
  -- this offset is to avoid the path to be too close to the end
  let offsetEnd = 3 in
  let pxOffsetEnd = 5 in
  let f = String.fromFloat in
  let p (x1, x2) = f x1 ++ " " ++ f x2 in
  let bUnder = Bez.orthoVectPx amplitude b 
      bOver = Bez.orthoVectPx (0 - amplitude) b
  in
  let n = round (Bez.length b / ratio) in
  let steps = (List.range 1 (n - offsetEnd)) in
  let stepToStr i = 
         let t = toFloat i / toFloat n in
         let bez = 
                let r = modBy 4 i in
                if r == 0 then bUnder 
                else if r == 2 then bOver
                else b
         in
         "L " ++ p (Bez.point bez t) ++ "\n" in
  -- let points = List.map Bez.point steps in  
  
  let path = "M " ++ p from ++ " "
        ++ String.join "" (List.map stepToStr steps) 
        ++ "L " ++ p (Bez.shiftTo b pxOffsetEnd)
        ++ "L " ++ p to
  in
  path
      



wavyQBezToPath : QuadraticBezier -> String
wavyQBezToPath ({from, to, controlPoint} as b) =
  let f = String.fromFloat in
  let p (x1, x2) = f x1 ++ " " ++ f x2 in
  -- Drawing.WavyLine.
  -- generateWavyPath from controlPoint to
  generateWavyPath b
    --  (Point.name from) 
    --  (Point.name controlPoint) 
    --  (Point.name to)
    --  2
     
  -- like qBezToAttr, but make it squiggly


quadraticBezierToAttr : Bool -> QuadraticBezier -> Svg.Attribute a 
quadraticBezierToAttr wavy b =
  let f = String.fromFloat in
  let p (x1, x2) = f x1 ++ " " ++ f x2 in    
    Svg.d  <| if wavy then wavyQBezToPath b else qBezToPath b

mkPath : {dashed:Bool, wavy:Bool, color:Color, strokeWidth : Int} -> List (Svg.Attribute a) -> QuadraticBezier -> Svg a
mkPath arg attrs q =
  Svg.path 
  ( quadraticBezierToAttr arg.wavy q ::
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
              , angle : Float
              , scale : Float
              , key : Maybe String} 
              -> List (Html.Attribute a) -> Drawing a
makeLatex arg attrs = 
  Node {angle = arg.angle, label = arg.label, preamble = arg.preamble, pos = arg.pos, dims = arg.dims
    , scale = arg.scale}
  |> TikzShape attrs
  |> ofShapeWithKey arg.zindex arg.key

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
