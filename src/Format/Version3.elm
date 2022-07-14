module Format.Version3 exposing (Graph, Edge, fromJSGraph, version, ArrowStyle)

import Polygraph as Graph exposing (Graph)
import Geometry.Point exposing (Point)
import Format.GraphInfo exposing (GraphInfo)
import ArrowStyle exposing (LabelAlignment(..))
import Format.Version4 as NextVersion

version = 3

type alias ArrowStyle = { tail : String, head : String, double : Bool
   , dashed : Bool, bend : Float, alignment : String, 
   position : Float }

type alias Edge = { label : String, style : ArrowStyle }
type alias Node = { pos : Point , label : String}
type alias Graph = { 
      nodes: List (Graph.Node Node),
      edges: List (Graph.Edge Edge),
      sizeGrid : Int}


toNextNode : Node -> NextVersion.Node
toNextNode { label, pos } = 
  { label = label, pos = pos, isMath = True }


toNextVersion : Graph -> NextVersion.Graph
toNextVersion { nodes, edges, sizeGrid } = 
    { nodes = List.map (Graph.nodeMap toNextNode) nodes, 
      edges = edges,
      sizeGrid = sizeGrid }


fromJSGraph : Graph -> GraphInfo
fromJSGraph g = 
     g |> toNextVersion |> NextVersion.fromJSGraph
       