module Format.Version13 exposing (Graph, Node, Tab, ArrowStyle, Edge, fromJSGraph, version)

import Polygraph as Graph exposing (Graph)
import Geometry.Point exposing (Point)
import ArrowStyle exposing (tailCodec, headCodec, alignmentCodec, kindCodec)
import GraphDefs exposing (EdgeLabel, NodeLabel)
import Format.GraphInfo as GraphInfo exposing (GraphInfo)
import GraphDefs exposing (EdgeType(..))
import Drawing.Color as Color
import Codec exposing (Codec)
import Format.Version14 as NextVersion
import Format.Keys exposing (normalKey, pullshoutKey)

version = 13

type alias ArrowStyle = NextVersion.ArrowStyle


type alias Edge = NextVersion.Edge


type alias Node = { pos : Point , label : String, isMath : Bool, zindex: Int
  -- , selected : Bool
  }
type alias Tab = { 
      title: String,
      sizeGrid : Int,
      id : Int,
      nodes: List (Graph.Node Node),
      edges: List (Graph.Edge Edge),
      nextGraphId : Int
   }
type alias Graph = { 
      tabs : List Tab,
      latexPreamble : String,
      nextTabId : Int,
      activeTabId : Int}

toNextNode : Node -> NextVersion.Node
toNextNode {pos, label, isMath, zindex} = 
   { pos = pos, label = label, isMath = isMath, zindex = zindex, isCoqValidated = False }
     -- selected = False}



toNextTab : Int -> Tab -> NextVersion.Tab
toNextTab id {title, sizeGrid, nodes, edges, nextGraphId} =
  { title = title, 
    id = id,
    sizeGrid = sizeGrid,
    nodes = List.map (Graph.nodeMap toNextNode) nodes,
    edges = edges,
    nextGraphId = nextGraphId
  }

toNextVersion : Graph -> NextVersion.Graph
toNextVersion { tabs, nextTabId, activeTabId, latexPreamble} = 
    
    { tabs = List.indexedMap toNextTab tabs,
      latexPreamble = latexPreamble,
      nextTabId = nextTabId,
      activeTabId = activeTabId}


fromJSGraph : Graph -> GraphInfo
fromJSGraph g = 
     g |> toNextVersion |> NextVersion.fromJSGraph


