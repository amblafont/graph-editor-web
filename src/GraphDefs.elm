module GraphDefs exposing (EdgeLabel, NodeLabel,
   newNodeLabel,
   EdgeLabelJs, edgeLabelToJs, edgeLabelFromJs,
   NodeLabelJs, nodeLabelToJs, nodeLabelFromJs,
   getNodeLabelOrCreate, getNodeDims
   )

import Geometry.Point exposing (Point)
import ArrowStyle.Core as ArrowStyle
import ArrowStyle exposing (ArrowStyle)
import Graph exposing (Graph, NodeId)
import GraphExtra as Graph

type alias EdgeLabel = { label : String, style : ArrowStyle}
type alias NodeLabel = { pos : Point , label : String, dims : Maybe Point}

type alias EdgeLabelJs = { label : String, style : ArrowStyle.JsStyle, bend : Float}
type alias NodeLabelJs = { pos : Point , label : String}

newNodeLabel : Point -> String -> NodeLabel
newNodeLabel p s = NodeLabel p s Nothing

nodeLabelToJs : NodeLabel -> NodeLabelJs
nodeLabelToJs {pos, label} = NodeLabelJs pos label

nodeLabelFromJs : NodeLabelJs -> NodeLabel
nodeLabelFromJs {pos, label} = NodeLabel pos label Nothing


edgeLabelToJs : EdgeLabel -> EdgeLabelJs
edgeLabelToJs {label, style} = 
  {label = label, style = ArrowStyle.toJsStyle style.s, bend = style.bend}

edgeLabelFromJs : EdgeLabelJs -> EdgeLabel
edgeLabelFromJs {label, style, bend } = 
  EdgeLabel label <| ArrowStyle (ArrowStyle.fromJsStyle style) bend

createNodeLabel : Graph NodeLabel EdgeLabel -> String -> Point -> (Graph NodeLabel EdgeLabel,
                                                                       NodeId, Point)
createNodeLabel g s p =
    let label = { pos = p, label = s, dims = Nothing} in
    let (g2, id) = Graph.newNode g label in
     (g2, id, p)

getNodeLabelOrCreate : Graph NodeLabel EdgeLabel -> String -> Point -> (Graph NodeLabel EdgeLabel,
                                                                       NodeId, Point)
getNodeLabelOrCreate g s p =
    if s == "" then
       createNodeLabel g s p
    else
        case Graph.filterNodes g (\ l -> l.label == s) of
            [] -> createNodeLabel g s p
            t :: _ -> (g , t.id, t.label.pos)

getNodeDims : NodeLabel -> Point
getNodeDims n =
    case  n.dims of
        Nothing ->
         let height = 16 in
         let size = max 1 (String.length n.label) in
         -- copied from source code of Collage
         (height / 2 * toFloat size, height)
        Just p -> p