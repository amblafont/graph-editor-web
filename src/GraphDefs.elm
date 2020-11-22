module GraphDefs exposing (EdgeLabel, NodeLabel, EdgeLabelJs, edgeLabelToJs, edgeLabelFromJs)

import Geometry.Point exposing (Point)
import ArrowStyle.Core as ArrowStyle
import ArrowStyle exposing (ArrowStyle)

type alias EdgeLabel = { label : String, style : ArrowStyle}
type alias NodeLabel = { pos : Point , label : String}

type alias EdgeLabelJs = { label : String, style : ArrowStyle.JsStyle, bend : Float}

edgeLabelToJs : EdgeLabel -> EdgeLabelJs
edgeLabelToJs {label, style} = 
  {label = label, style = ArrowStyle.toJsStyle style.s, bend = style.bend}

edgeLabelFromJs : EdgeLabelJs -> EdgeLabel
edgeLabelFromJs {label, style, bend } = 
  EdgeLabel label <| ArrowStyle (ArrowStyle.fromJsStyle style) bend