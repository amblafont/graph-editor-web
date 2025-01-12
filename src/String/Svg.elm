module String.Svg  exposing (..)
-- Actually, we could spare this work by exporting to svg from js
import String.Html exposing (Html, attribute, attributeNS)
import Geometry
import Geometry exposing (PosDims)
import Drawing.Color as Color

type alias Svg a = Html a
type alias Attribute a = String.Html.Attribute a
node = String.Html.nodeNS "http://www.w3.org/2000/svg"
keyedNode = String.Html.keyedNodeNS "http://www.w3.org/2000/svg"

svg = node "svg"
ksvg = keyedNode "svg"

text = String.Html.text
foreignObject = node "foreignObject"
g = node "g"
kg = keyedNode "g"
image = node "image"
circle = node "circle"
line = node "line"
pattern = node "pattern"
path = node "path"
rect = node "rect"
text_ = node "text"
use = node "use"

-- svg = node "svg"

viewBox : Geometry.Rect -> Attribute a
viewBox {topLeft, bottomRight} =
  let (a1, b1) = topLeft
      (a2, b2) = bottomRight 
  in
  let f = round >> String.fromInt in
   attribute "viewbox" <| 
   f a1 ++ " " ++ f b1 ++ " " ++ f (a2 - a1) ++ " " ++ f (b2 - b1)

textAnchor = attribute "text-anchor"
dominantBaseline = attribute "dominant-baseline"
class = attribute "class"
style = attribute "style"
defs = node "defs"
fill = attribute "fill"
stroke = attribute "stroke"
strokeFromColor = stroke << Color.toString

strokeDasharray = attribute "stroke-dasharray"
strokeLinecap = attribute "stroke-linecap"
strokeLinejoin = attribute "stroke-linejoin"
strokeMiterlimit = attribute "stroke-miterlimit"

d = attribute "d"
id = attribute "id"
x = attribute "x"
y = attribute "y"
cx = attribute "cx"
cy = attribute "cy"
r = attribute "r"


x1 = attribute "x1"
x2 = attribute "x2"
y1 = attribute "y1"
y2 = attribute "y2"
width = attribute "width"
height = attribute "height"
patternUnits = attribute "patternUnits"
strokeWidth = attribute "stroke-width"
strokeWidthPx w = attribute "stroke-width" 
                  <| String.fromInt w ++ "px"
transform = attribute "transform"

xlinkHref = attributeNS "http://www.w3.org/1999/xlink" "xlink:href"



   