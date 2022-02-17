module GraphDefs exposing (EdgeLabel, NodeLabel,
   newNodeLabel, newEdgeLabel, emptyEdge,
   EdgeLabelJs, edgeLabelToJs, edgeLabelFromJs,
   NodeLabelJs, nodeLabelToJs, nodeLabelFromJs,
   graphToJs, jsToGraph, GraphJS,
   createNodeLabel,
   getNodeLabelOrCreate, getNodeDims, getEdgeDims,
   addNodesSelection, selectAll, clearSelection, selectedGraph,
   isEmptySelection,
   selectedEdgeId, selectedNode, selectedId,
   removeSelected, getLabelLabel,
   getNodesAt, snapToGrid, snapNodeToGrid, exportQuiver,
   addOrSetSel
   )

import Geometry.Point as Point exposing (Point)
import Geometry
import ArrowStyle.Core exposing (HeadStyle(..), TailStyle(..))
import ArrowStyle exposing (ArrowStyle)
import Polygraph as Graph exposing (Graph, NodeId, EdgeId, Node, Edge)

import Json.Encode as JEncode
import List.Extra as List


type alias EdgeLabel = { label : String, style : ArrowStyle, dims : Maybe Point, selected : Bool}
type alias NodeLabel = { pos : Point , label : String, dims : Maybe Point, selected : Bool}

type alias EdgeLabelJs = { label : String, style : ArrowStyle.Core.JsStyle, bend : Float}
type alias NodeLabelJs = { pos : Point , label : String}

type alias GraphJS = (List (Graph.Node NodeLabelJs) , List (Graph.Edge EdgeLabelJs))

graphToJs : Graph NodeLabel EdgeLabel -> GraphJS
graphToJs g = 
          let gjs = g
                   |> Graph.map 
                    (\_ -> nodeLabelToJs)
                    (\_ -> edgeLabelToJs) 
                   |> Graph.normalise           
          in
          let nodes = Graph.nodes gjs
              edges = Graph.edges gjs
          in
          (nodes, edges)

jsToGraph : GraphJS -> Graph NodeLabel EdgeLabel
jsToGraph (ns,es) = Graph.fromNodesAndEdges ns es
                       |> Graph.map 
                          (\_ -> nodeLabelFromJs)
                          (\_ -> edgeLabelFromJs)

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

selectAll : Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
selectAll g = addNodesSelection g (always True)

selectedGraph : Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
selectedGraph = Graph.keepBelow .selected .selected

selectedNodes : Graph NodeLabel EdgeLabel -> List (Node NodeLabel)
selectedNodes g = Graph.nodes g |> List.filter (.label >> .selected)

selectedEdges : Graph NodeLabel EdgeLabel -> List (Edge EdgeLabel)
selectedEdges g = Graph.edges g |> List.filter (.label >> .selected)

isEmptySelection : Graph NodeLabel EdgeLabel -> Bool
isEmptySelection g = 
   List.isEmpty (selectedNodes g) && List.isEmpty (selectedEdges g)

selectedNode : Graph NodeLabel EdgeLabel -> Maybe (Node NodeLabel)
selectedNode g = 
    case selectedNodes g of
       [ x ] -> Just x
       _ -> Nothing

selectedEdgeId : Graph NodeLabel EdgeLabel -> Maybe EdgeId
selectedEdgeId g = 
    case selectedEdges g of
       [ x ] -> Just x.id
       _ -> Nothing

selectedId : Graph NodeLabel EdgeLabel -> Maybe Graph.Id
selectedId g = 
   case (List.map .id <| selectedNodes g)
          ++ (List.map .id <| selectedEdges g) of
      [ x ] -> Just x
      _ -> Nothing


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


snapNodeToGrid : Int -> NodeLabel -> NodeLabel
snapNodeToGrid sizeGrid n =  { n | pos = Point.snapToGrid (toFloat sizeGrid) n.pos }

snapToGrid : Int -> Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
snapToGrid sizeGrid g =
   Graph.map (\_ -> snapNodeToGrid sizeGrid) (\_ -> identity ) g

getLabelLabel : Graph.Id -> Graph NodeLabel EdgeLabel -> String
getLabelLabel id g = g |> Graph.get id .label .label |> Maybe.withDefault ""

addOrSetSel : Bool -> Graph.Id -> Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
addOrSetSel keep o gi =

    let g = if keep then gi else clearSelection gi in
    let g2 = Graph.update o (\n -> {n | selected = True}) 
                  (\n -> {n | selected = True}) g
    in
       {-   = case o of
          ONothing -> g
          ONode id -> Graph.updateNode id (\n -> {n | selected = True}) g
          OEdge id -> Graph.updateEdge id (\n -> {n | selected = True}) g
    in -}
   g2
