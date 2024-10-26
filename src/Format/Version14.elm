module Format.Version14 exposing (Graph, Node, normalKey, pullshoutKey, Tab, ArrowStyle, Edge, fromJSGraph, version)

import Polygraph as Graph exposing (Graph)
import Geometry.Point exposing (Point)
import ArrowStyle exposing (tailCodec, headCodec, alignmentCodec, kindCodec)
import GraphDefs exposing (defaultPullshoutShift)
import Format.GraphInfo as GraphInfo exposing (GraphInfo)
import GraphDefs exposing (EdgeType(..))
import Drawing.Color as Color
import Codec exposing (Codec)
import Format.Version15 as NextVersion

version = 14
pullshoutKey = NextVersion.pullshoutKey
normalKey = NextVersion.normalKey

adjunctionKey = "adjunction"

type alias ArrowStyle = { tail : String, head : String, kind : String
   , dashed : Bool, bend : Float, alignment : String, 
   position : Float, color : String }

emptyArrowStyle : Color.Color -> ArrowStyle
emptyArrowStyle color =
  { tail = "", head = "", kind = "normal", 
    dashed = False, bend = 0, alignment = "", position = 0,
    color = Codec.encoder Color.codec color }

type alias Edge = { label : String, style : ArrowStyle, kind : String,
       zindex : Int
      --  , selected : Bool 
       }

pullshoutEdge : Int -> Color.Color -> Edge
pullshoutEdge z color = Edge "" (emptyArrowStyle color) pullshoutKey z -- False

type alias Node = { pos : Point , label : String, isMath : Bool, zindex: Int
  , isCoqValidated : Bool
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

toNextEdge : Edge -> NextVersion.Edge 
toNextEdge {label, style, kind, zindex} = 
  {label = label, 
   style = if kind /= pullshoutKey then style else 
            {style | bend = defaultPullshoutShift, position = defaultPullshoutShift},
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