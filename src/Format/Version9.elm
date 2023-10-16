module Format.Version9 exposing (Graph, Node, ArrowStyle, Edge, fromJSGraph, version)

import Polygraph as Graph exposing (Graph)
import Geometry.Point exposing (Point)
import GraphDefs exposing (EdgeLabel, NodeLabel)
import Format.GraphInfo exposing (GraphInfo)
import GraphDefs exposing (EdgeType(..))
import Format.Version10 as NextVersion

version = 9

type alias Node = NextVersion.Node
type alias Edge = NextVersion.Edge
type alias ArrowStyle = NextVersion.ArrowStyle

type alias Graph = { 
      nodes: List (Graph.Node Node),
      edges: List (Graph.Edge Edge),
      sizeGrid : Int,
      latexPreamble : String}
toNextVersion : Graph -> NextVersion.Graph
toNextVersion { nodes, edges, sizeGrid, latexPreamble } = 
    { tabs = [ { title = "1", 
                 sizeGrid = sizeGrid,
                 nodes = nodes, 
                 edges =  edges,
                 active = True
                 } ],
      latexPreamble = latexPreamble }


fromJSGraph : Graph -> GraphInfo
fromJSGraph g = 
     g |> toNextVersion |> NextVersion.fromJSGraph
