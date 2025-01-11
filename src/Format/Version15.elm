module Format.Version15 exposing (Graph, Node, Tab, ArrowStyle, Edge, fromJSGraph, version)
{- 
Changes from Version 14:
- offset1 and offset2 of pullshout edges are saved in the bend and position fields of the style
- style has now a marker field
-}
import Polygraph as Graph exposing (Graph)
import Format.GraphInfo as GraphInfo exposing (GraphInfo)
import GraphDefs exposing (EdgeType(..))
import Format.Version16 as NextVersion


version = 15

type alias ArrowStyle = { tail : String, head : String, kind : String
   , dashed : Bool, bend : Float, alignment : String, 
   position : Float, color : String, marker : String
   }

toNextStyle : ArrowStyle -> NextVersion.ArrowStyle
toNextStyle {tail, head, kind, dashed, bend, alignment, position, color, marker} = 
  {marker = marker, tail = tail, head = head, kind = kind, dashed = dashed, bend = bend, alignment = alignment, position = position, color = color,  
  headColor = color, tailColor = color}


type alias Edge = { label : String, style : ArrowStyle, kind : String,
       zindex : Int
       }


type alias Node = NextVersion.Node
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

toNextEdge : Edge -> NextVersion.Edge 
toNextEdge {label, style, kind, zindex} = 
  {label = label, 
   style = toNextStyle style,
   kind = kind, zindex = zindex} --, selected = False}


toNextTab : Tab -> NextVersion.Tab
toNextTab {id, title, sizeGrid, nodes, edges, nextGraphId} =
  { title = title, 
    id = id,
    sizeGrid = sizeGrid,
    nodes = nodes,
    edges = List.map (Graph.edgeMap toNextEdge) edges,
    nextGraphId = nextGraphId
  }

toNextVersion : Graph -> NextVersion.Graph
toNextVersion { tabs, nextTabId, activeTabId, latexPreamble} = 
    
    { tabs = List.map toNextTab tabs,
      latexPreamble = latexPreamble,
      nextTabId = nextTabId,
      activeTabId = activeTabId}


fromJSGraph : Graph -> GraphInfo
fromJSGraph g = 
     g |> toNextVersion |> NextVersion.fromJSGraph