module Format.Version1 exposing (Graph, Edge, fromJSGraph, version)

import Polygraph as Graph
import Geometry.Point exposing (Point)
import Format.Version2 as NextVersion
import GraphDefs exposing (NodeLabel, EdgeLabel)

version = 1

toNextEdge : Edge -> NextVersion.Edge
toNextEdge { label, style } = 
  { label = label, style = 
       { tail = style.tail
       , head = style.head
       , double = style.double
       , dashed = style.dashed 
       , bend = style.bend
       , alignment = "left"
       , position = 0.5 }}

toNextVersion : Graph -> NextVersion.Graph
toNextVersion { nodes, edges } = 
    { nodes = nodes, 
      edges = List.map (Graph.edgeMap toNextEdge) edges }

fromJSGraph : Graph -> Graph.Graph NodeLabel EdgeLabel
fromJSGraph g = g |> toNextVersion |> NextVersion.fromJSGraph

type alias ArrowStyle = { tail : String, head : String, double : Bool
   , dashed : Bool, bend : Float }

type alias Edge = { label : String, style : ArrowStyle }
type alias Node = { pos : Point , label : String}
type alias Graph = { nodes: List (Graph.Node Node) , edges: List (Graph.Edge Edge)}


