module GraphDefs exposing (EdgeLabel, NodeLabel,
   newNodeLabel, newEdgeLabel, emptyEdge,
   EdgeLabelJs, edgeLabelToJs, edgeLabelFromJs,
   NodeLabelJs, nodeLabelToJs, nodeLabelFromJs,
   getNodeLabelOrCreate, getNodeDims, getEdgeDims,
   addNodesSelection, clearSelection, selectedGraph,
   removeSelected,
   getNodesAt, cloneSelected
   )

import Geometry.Point as Point exposing (Point)
import Geometry
import ArrowStyle.Core
import ArrowStyle exposing (ArrowStyle)
import Polygraph as Graph exposing (Graph, NodeId)


type alias EdgeLabel = { label : String, style : ArrowStyle, dims : Maybe Point, selected : Bool}
type alias NodeLabel = { pos : Point , label : String, dims : Maybe Point, selected : Bool}

type alias EdgeLabelJs = { label : String, style : ArrowStyle.Core.JsStyle, bend : Float}
type alias NodeLabelJs = { pos : Point , label : String}

newNodeLabel : Point -> String -> NodeLabel
newNodeLabel p s = NodeLabel p s Nothing False

newEdgeLabel : String -> ArrowStyle -> EdgeLabel
newEdgeLabel s style = { label = s, style = style, dims = Nothing, selected = False}

emptyEdge : EdgeLabel
emptyEdge = newEdgeLabel "" ArrowStyle.empty


nodeLabelToJs : NodeLabel -> NodeLabelJs
nodeLabelToJs {pos, label} = NodeLabelJs pos label

nodeLabelFromJs : NodeLabelJs -> NodeLabel
nodeLabelFromJs {pos, label} = NodeLabel pos label Nothing False


edgeLabelToJs : EdgeLabel -> EdgeLabelJs
edgeLabelToJs {label, style} = 
  {label = label, style = ArrowStyle.Core.toJsStyle style.s, bend = style.bend}

edgeLabelFromJs : EdgeLabelJs -> EdgeLabel
edgeLabelFromJs {label, style, bend } = 
  EdgeLabel label 
     (ArrowStyle (ArrowStyle.Core.fromJsStyle style) bend)
     Nothing False

createNodeLabel : Graph NodeLabel EdgeLabel -> String -> Point -> (Graph NodeLabel EdgeLabel,
                                                                       NodeId, Point)
createNodeLabel g s p =
    let label = { pos = p, label = s, dims = Nothing, selected = False} in
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


defaultDims : String -> Point
defaultDims s = 
  let height = 16 in
  let size = max 1 (String.length s) in
   -- copied from source code of Collage
   (height / 2 * toFloat size, height)

getNodeDims : NodeLabel -> Point
getNodeDims n =
    case  n.dims of
        Nothing -> defaultDims n.label
        Just p -> p

getEdgeDims : EdgeLabel -> Point
getEdgeDims n =
    case  n.dims of
        Nothing -> defaultDims n.label
        Just p -> p

-- select nodes and everything between them
addNodesSelection : Graph NodeLabel EdgeLabel -> (NodeLabel -> Bool) -> Graph NodeLabel EdgeLabel
addNodesSelection g f =
      Graph.mapRecAll .selected
        .selected
       (\_ n -> { n | selected = f n || n.selected})
       (\_ s1 s2 e -> {e | selected = (s1 && s2) || e.selected })
       g
    -- Graph.map 
    --    (\_ n -> { n | selected = f n })(\_ -> identity) g

selectedGraph : Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
selectedGraph = Graph.keepBelow .selected .selected

removeSelected : Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
removeSelected =  Graph.drop .selected .selected

clearSelection : Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
clearSelection g =
  Graph.map (\_ n -> {n | selected = False})
            (\_ e -> {e | selected = False}) g


getNodesAt : Graph NodeLabel e -> Point -> List NodeId
getNodesAt g p =
  Graph.filterNodes g
    (\n -> Geometry.isInPosDims { pos = n.pos, 
                                  dims = getNodeDims n} p)
  |> List.map .id


cloneSelected : Graph NodeLabel EdgeLabel -> Point -> 
                Graph NodeLabel EdgeLabel
cloneSelected g offset =
  let g2 = selectedGraph g |> 
       Graph.map (\_ n -> {n | pos = Point.add n.pos offset, selected = True })
         (\_ e -> {e | selected = True } )
  in
  let gclearSel = clearSelection g in
  Graph.union gclearSel g2

