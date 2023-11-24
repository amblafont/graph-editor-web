module Format.Version10 exposing (Graph, Node, Edge, ArrowStyle, fromJSGraph, version)

import Polygraph as Graph exposing (Graph)
import Geometry.Point exposing (Point)
import Format.GraphInfo exposing (GraphInfo)
import GraphDefs exposing (EdgeType(..))
import Format.Version11 as NextVersion
import Zindex exposing (foregroundZ)
import Zindex exposing (defaultZ)

version = 10

type alias Edge = NextVersion.Edge
type alias ArrowStyle = NextVersion.ArrowStyle

type alias Node = { pos : Point , label : String, isMath : Bool}

type alias Tab = { 
      title: String,
      sizeGrid : Int,
      active: Bool,
      nodes: List (Graph.Node Node),
      edges: List (Graph.Edge Edge) 
   }
type alias Graph = { 
      tabs : List Tab,
      latexPreamble : String}

toNextNode : Int -> Node -> NextVersion.Node
toNextNode zindex { pos, label, isMath} =
   { pos = pos, label = label, isMath = isMath,
     zindex = zindex }

biggestZindex : List (Graph.Edge Edge) -> Int
biggestZindex el = 
  List.map (.label >> .zindex) el
  |> List.maximum 
  |> Maybe.withDefault defaultZ

toNextTab : Tab -> NextVersion.Tab
toNextTab {title, sizeGrid, active, nodes, edges} =
  let bigZ = (biggestZindex edges) + 1 in 
  { title = title, 
    sizeGrid = sizeGrid,
    active = active,
    nodes = List.map (Graph.nodeMap (toNextNode bigZ)) nodes,
    edges = edges }

toNextVersion : Graph -> NextVersion.Graph
toNextVersion { tabs, latexPreamble } = 
    { tabs = List.map toNextTab tabs,
      latexPreamble = latexPreamble }


fromJSGraph : Graph -> GraphInfo
fromJSGraph g = 
     g |> toNextVersion |> NextVersion.fromJSGraph