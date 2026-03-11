module Format.Version17 exposing (Graph, Node,  Tab, Edge, fromJSGraph, version, ArrowStyle)
{- 
Changes from Version 16:
- more compact json format
-}
import Format.Version18 as NextVersion
import Polygraph as Graph exposing (Graph)
import Geometry.Point exposing (Point)
import ArrowStyle exposing (tailCodec, headCodec, alignmentCodec, kindCodec)
import ArrowStyle exposing (TailStyle(..), HeadStyle(..), ArrowKind(..))
import Geometry exposing (LabelAlignment(..))
import GraphDefs exposing (EdgeLabel, NodeLabel)
import Format.GraphInfo as GraphInfo exposing (GraphInfo)
import GraphDefs exposing (EdgeType(..))
import Drawing.Color as Color exposing (Color)
import Codec exposing (Codec)
-- import Codec exposing (FinalCustomCodec)
-- import Format.Keys exposing (normalKey, pullshoutKey, adjunctionKey)

version = 17
-- intCodec = 
--   Codec.build 
--     String.fromInt
--     (String.toInt >> Maybe.withDefault 0)

type alias ArrowStyle = NextVersion.ArrowStyle
type alias Edge = NextVersion.Edge


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




toNextTab : Tab -> NextVersion.Tab
toNextTab {id, title, sizeGrid, nodes, edges, nextGraphId} =
  { title = title, 
    id = id,
    sizeGrid = sizeGrid,
    nodes = nodes,
    edges = edges,
    nextGraphId = nextGraphId,
    freehandDrawings = []
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