module Format.Version12 exposing (Graph, Node, Tab, pullshoutKey, normalKey, ArrowStyle, Edge, fromJSGraph, version)

import Polygraph as Graph exposing (Graph)
import Geometry.Point exposing (Point)
import Format.GraphInfo exposing (GraphInfo)
import GraphDefs exposing (EdgeType(..))
import Format.Version13 as NextVersion
import List.Extra

version = 12

pullshoutKey = NextVersion.pullshoutKey
normalKey = NextVersion.normalKey

type alias ArrowStyle = { tail : String, head : String, kind : String
   , dashed : Bool, bend : Float, alignment : String, 
   position : Float, color : String }


type alias Edge = { label : String, style : ArrowStyle, kind : String,
       zindex : Int }

toNextEdge : Edge -> NextVersion.Edge 
toNextEdge {label, style, kind, zindex} = 
  {label = label, style = style, kind = kind, zindex = zindex} --, selected = False}

type alias Node = { pos : Point , label : String, isMath : Bool, zindex: Int}

toNextNode : Node -> NextVersion.Node
toNextNode {pos, label, isMath, zindex} = 
   { pos = pos, label = label, isMath = isMath, zindex = zindex }
     -- selected = False}


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


toNextTab : Int -> Tab -> NextVersion.Tab
toNextTab id {title, sizeGrid, active, nodes, edges} =
  { title = title, 
    id = id,
    sizeGrid = sizeGrid,
    nodes = List.map (Graph.nodeMap toNextNode) nodes,
    edges = List.map (Graph.edgeMap toNextEdge) edges,
    nextGraphId = List.maximum (List.map .id nodes ++ List.map .id edges) 
                  |> Maybe.withDefault -1
                  |> (+) 1
  }

toNextVersion : Graph -> NextVersion.Graph
toNextVersion { tabs, latexPreamble} = 
    let idx = List.Extra.findIndex .active tabs
            |> Maybe.withDefault 0
    in
    { tabs = List.indexedMap toNextTab tabs,
      latexPreamble = latexPreamble,
      nextTabId = List.length tabs,
      activeTabId = idx}


fromJSGraph : Graph -> GraphInfo
fromJSGraph g = 
     g |> toNextVersion |> NextVersion.fromJSGraph
