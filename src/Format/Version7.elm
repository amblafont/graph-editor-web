module Format.Version7 exposing (Graph, Node, Edge, fromJSGraph, version)

import Polygraph as Graph exposing (Graph)
import Geometry.Point exposing (Point)
import Format.GraphInfo exposing (GraphInfo)
import GraphDefs exposing (EdgeType(..))
import Format.Version8 as NextVersion

version = 7

type alias ArrowStyle = NextVersion.ArrowStyle


type alias Edge = { label : String, style : ArrowStyle, isPullback : Bool,
       zindex : Int }

type alias Node = NextVersion.Node
type alias Graph = { 
      nodes: List (Graph.Node Node),
      edges: List (Graph.Edge Edge),
      sizeGrid : Int,
      latexPreamble : String}

toNextEdge : Edge -> NextVersion.Edge
toNextEdge { label, style, isPullback } = 
  { label = label, style = style, isPullshout = isPullback,
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