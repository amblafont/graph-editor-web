module GraphDefs exposing (EdgeLabel, NodeLabel,
   newNodeLabel, newEdgeLabel, emptyEdge,
   EdgeLabelJs, edgeLabelToJs, edgeLabelFromJs,
   NodeLabelJs, nodeLabelToJs, nodeLabelFromJs,
   getNodeLabelOrCreate, getNodeDims, getEdgeDims,
   addNodesSelection, clearSelection, selectedGraph,
   removeSelected,
   getNodesAt, cloneSelected, snapToGrid, snapNodeToGrid, exportQuiver
   )

import Geometry.Point as Point exposing (Point)
import Geometry
import ArrowStyle.Core exposing (HeadStyle(..), TailStyle(..))
import ArrowStyle exposing (ArrowStyle)
import Polygraph as Graph exposing (Graph, NodeId)

import Json.Encode as JEncode


type alias EdgeLabel = { label : String, style : ArrowStyle, dims : Maybe Point, selected : Bool}
type alias NodeLabel = { pos : Point , label : String, dims : Maybe Point, selected : Bool}

type alias EdgeLabelJs = { label : String, style : ArrowStyle.Core.JsStyle, bend : Float}
type alias NodeLabelJs = { pos : Point , label : String}



quiverStyle : ArrowStyle -> List (String, JEncode.Value)
quiverStyle st =
   let { tail, head, double, dashed } = st.s in
   let makeIf b x = if b then [x] else [] in
   let headStyle = case head of 
          DefaultHead -> []       
          TwoHeads -> [("head", [("name", "epi")])]
          NoHead -> [("head", [("name", "none")])]
   in
   let tailStyle = case tail of 
          DefaultTail -> []
          Hook -> [("tail", [("name", "hook"),("side", "top")])]
          HookAlt -> [("tail", [("name", "hook"),("side", "bottom")])]
   in
   let style = List.map (\(x,y) -> (x, JEncode.object <| List.map (\(s, l) -> (s, JEncode.string l)) y)) <|
               headStyle
               ++
               tailStyle ++
               (makeIf dashed ("body", [("name", "dashed")]))
   in
   (makeIf double ("level", JEncode.int 2))  
   ++ [("style", JEncode.object style )]
   ++ (makeIf (st.bend /= 0) ("curve", JEncode.int <| floor (st.bend * 10)))


exportQuiver : Int -> Graph NodeLabel EdgeLabel -> JEncode.Value
exportQuiver sizeGrid g =
  let gnorm = Graph.normalise g in
  let nodes = Graph.nodes gnorm
      edges = Graph.edges gnorm
  in
  let coordInt x = floor (x / toFloat sizeGrid) |> JEncode.int in
  let encodePos (x, y) = [coordInt x, coordInt y] in
  let encodeNode n = JEncode.list identity <| encodePos n.label.pos ++ 
            [ JEncode.string (if n.label.label == "" then "\\bullet" else n.label.label)] in
  let encodeEdge e = JEncode.list identity <| 
               [JEncode.int e.from
               , JEncode.int e.to
               , JEncode.string e.label.label
               , JEncode.int 0 -- alignment
               , JEncode.object <| quiverStyle e.label.style
                  -- [("level", if e.label.style.double then JEncode.int 2 else JEncode.int 1)] --options
                ] in
  let jnodes = nodes |> List.map encodeNode
      jedges = edges |> List.map encodeEdge
  in
  JEncode.list identity <|
  [JEncode.int 0, JEncode.int <| List.length nodes] ++ jnodes ++ jedges

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

snapNodeToGrid : Int -> NodeLabel -> NodeLabel
snapNodeToGrid sizeGrid n =  { n | pos = Point.snapToGrid (toFloat sizeGrid) n.pos }

snapToGrid : Int -> Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
snapToGrid sizeGrid g =
   Graph.map (\_ -> snapNodeToGrid sizeGrid) (\_ -> identity ) g