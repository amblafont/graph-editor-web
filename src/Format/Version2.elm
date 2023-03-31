module Format.Version2 exposing (Graph, Edge, fromJSGraph, version)

import Polygraph as Graph exposing (Graph)
import Geometry.Point exposing (Point)
import Format.GraphInfo exposing (GraphInfo, defaultGridSize)
import Format.Version3 as NextVersion

version = 2

type alias ArrowStyle = { tail : String, head : String, double : Bool
   , dashed : Bool, bend : Float, alignment : String, 
   position : Float }

type alias Edge = { label : String, style : ArrowStyle }
type alias Node = { pos : Point , label : String}
type alias Graph = { nodes: List (Graph.Node Node) , edges: List (Graph.Edge Edge)}


toNextVersion : Graph -> NextVersion.Graph
toNextVersion { nodes, edges } = 
    { nodes = nodes, 
      edges = edges,
      sizeGrid = defaultGridSize }



fromJSGraph : Graph -> GraphInfo
fromJSGraph g = g |> toNextVersion |> NextVersion.fromJSGraph
