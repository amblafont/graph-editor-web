module String.Svg  exposing (..)

import String.Html exposing (Html, attribute, attributeNS)
import Geometry
import Html.Attributes exposing (poster)
import Geometry exposing (PosDims)

type alias Svg a = Html a
type alias Attribute a = String.Html.Attribute a
node = String.Html.nodeNS "http://www.w3.org/2000/svg"

svg = node "svg"

text = String.Html.text
foreignObject = node "foreignObject"
image = node "image"
circle = node "circle"
line = node "line"
pattern = node "pattern"
path = node "path"
rect = node "rect"
text_ = node "text"

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
defs = node "defs"
fill = attribute "fill"
stroke = attribute "stroke"

strokeDasharray = attribute "stroke-dash-array"

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
transform = attribute "transform"

xlinkHref = attributeNS "http://www.w3.org/1999/xlink" "xlink:href"



   