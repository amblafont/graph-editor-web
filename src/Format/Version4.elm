module Format.Version4 exposing (Graph, Node, Edge, fromJSGraph, version)

import Polygraph as Graph exposing (Graph)
import Geometry.Point exposing (Point)
import Format.GraphInfo exposing (GraphInfo)
import ArrowStyle exposing (LabelAlignment(..))
import Format.Version5 as NextVersion

version = 4

type alias ArrowStyle = { tail : String, head : String, double : Bool
   , dashed : Bool, bend : Float, alignment : String, 
   position : Float }

type alias Edge = { label : String, style : ArrowStyle }
type alias Node = { pos : Point , label : String, isMath : Bool}
type alias Graph = { 
      nodes: List (Graph.Node Node),
      edges: List (Graph.Edge Edge),
      sizeGrid : Int}



toNextVersion : Graph -> NextVersion.Graph
toNextVersion { nodes, edges, sizeGrid } = 
    { nodes = nodes, 
      edges = edges,
      sizeGrid = sizeGrid,
      latexPreamble = ""}


fromJSGraph : Graph -> GraphInfo
fromJSGraph g = 
     g |> toNextVersion |> NextVersion.fromJSGraph
       