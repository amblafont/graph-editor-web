module Format.Version0 exposing (Graph, fromJSGraph, version)

import Polygraph as Graph
import Geometry.Point exposing (Point)
import Format.Version1 as NextVersion
import Format.GraphInfo exposing (GraphInfo)

version = 0

type alias ArrowStyle = { tail : String, head : String, double : Bool, dashed : Bool}

type alias Edge = { label : String, style : ArrowStyle, bend : Float}
type alias Node = { pos : Point , label : String}
type alias Graph = (List (Graph.Node Node) , List (Graph.Edge Edge))


toNextEdge : Edge -> NextVersion.Edge
toNextEdge { label, style, bend } = 
  { label = label, style = 
       { tail = style.tail
       , head = style.head
       , double = style.double
       , dashed = style.dashed 
       , bend = bend }}

toNextVersion : Graph -> NextVersion.Graph
toNextVersion (nodes, edges) = 
    { nodes = nodes, 
      edges = List.map (Graph.edgeMap toNextEdge) edges }

fromJSGraph : Graph -> GraphInfo
fromJSGraph g = g |> toNextVersion |> NextVersion.fromJSGraph