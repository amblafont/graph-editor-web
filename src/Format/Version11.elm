module Format.Version11 exposing (Graph, Node, Tab, ArrowStyle, Edge, fromJSGraph, version)

import Polygraph as Graph exposing (Graph)
import GraphDefs exposing (EdgeType(..))
import Format.Version12 as NextVersion
import Format.GraphInfo exposing (GraphInfo)

version = 11

type alias ArrowStyle = NextVersion.ArrowStyle

type alias Edge = { label : String, style : ArrowStyle, isPullshout : Bool,
       zindex : Int }

type alias Node = NextVersion.Node
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





toNextEdge : Edge -> NextVersion.Edge
toNextEdge e =
  -- { label : String, style : ArrowStyle, kind : String,
  --      zindex : Int }
  { label = e.label,
    style = e.style,
    kind = if e.isPullshout then NextVersion.pullshoutKey else NextVersion.normalKey,
    zindex = e.zindex }

toNextTab : Tab -> NextVersion.Tab
toNextTab {title, sizeGrid, active, nodes, edges} =
  { title = title, 
    sizeGrid = sizeGrid,
    active = active,
    nodes = nodes,
    edges = List.map (Graph.edgeMap toNextEdge) edges }

toNextVersion : Graph -> NextVersion.Graph
toNextVersion { tabs, latexPreamble } = 
    { tabs = List.map toNextTab tabs,
      latexPreamble = latexPreamble }


fromJSGraph : Graph -> GraphInfo
fromJSGraph g = 
     g |> toNextVersion |> NextVersion.fromJSGraph