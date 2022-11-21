module Format.Version6 exposing (Graph, Node, Edge, fromJSGraph, version)

import Polygraph as Graph exposing (Graph)
import Geometry.Point exposing (Point)
import ArrowStyle
import GraphDefs exposing (EdgeLabel, NodeLabel)
import ArrowStyle exposing (LabelAlignment(..))
import Format.GraphInfo exposing (GraphInfo)
import GraphDefs exposing (EdgeType(..))
import Format.Version7 as NextVersion

version = 6

type alias ArrowStyle = { tail : String, head : String, double : Bool
   , dashed : Bool, bend : Float, alignment : String, 
   position : Float }


type alias Edge = { label : String, style : ArrowStyle, isPullback : Bool }


type alias Node = { pos : Point , label : String, isMath : Bool}
type alias Graph = { 
      nodes: List (Graph.Node Node),
      edges: List (Graph.Edge Edge),
      sizeGrid : Int,
      latexPreamble : String}


toNextEdge : Edge -> NextVersion.Edge
toNextEdge { label, style, isPullback } = 
  { label = label, style = style, isPullback = isPullback,
    zindex = 0 }


toNextVersion : Graph -> NextVersion.Graph
toNextVersion { nodes, edges, sizeGrid, latexPreamble } = 
    { nodes = nodes, 
      edges =  List.map (Graph.edgeMap toNextEdge) edges,
      sizeGrid = sizeGrid,
      latexPreamble = latexPreamble }


fromJSGraph : Graph -> GraphInfo
fromJSGraph g = 
     g |> toNextVersion |> NextVersion.fromJSGraph