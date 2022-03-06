module Format.Version0 exposing (Graph, fromJSGraph, version)

import Polygraph as Graph
import Geometry.Point exposing (Point)
import Format.Version1 as Version1
import GraphDefs exposing (NodeLabel, EdgeLabel)

version = 0

type alias ArrowStyle = { tail : String, head : String, double : Bool, dashed : Bool}

type alias Edge = { label : String, style : ArrowStyle, bend : Float}
type alias Node = { pos : Point , label : String}
type alias Graph = (List (Graph.Node Node) , List (Graph.Edge Edge))


toEdge1 : Edge -> Version1.Edge
toEdge1 { label, style, bend } = 
  { label = label, style = 
       { tail = style.tail
       , head = style.head
       , double = style.double
       , dashed = style.dashed 
       , bend = bend }}

toVersion1 : Graph -> Version1.Graph
toVersion1 (nodes, edges) = 
    { nodes = nodes, 
      edges = List.map (Graph.edgeMap toEdge1) edges }

fromJSGraph : Graph -> Graph.Graph NodeLabel EdgeLabel
fromJSGraph g = g |> toVersion1 |> Version1.fromJSGraph