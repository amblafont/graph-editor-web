module Format.Version1 exposing (Graph, Node, Edge, toJSGraph, fromJSGraph, version)

import Polygraph as Graph exposing (Graph)
import Geometry.Point exposing (Point)
import ArrowStyle
import GraphDefs exposing (EdgeLabel, NodeLabel)

version = 1

type alias ArrowStyle = { tail : String, head : String, double : Bool
   , dashed : Bool, bend : Float }

type alias Edge = { label : String, style : ArrowStyle }
type alias Node = { pos : Point , label : String}
type alias Graph = { nodes: List (Graph.Node Node) , edges: List (Graph.Edge Edge)}

fromEdgeLabel : EdgeLabel -> Edge
fromEdgeLabel { label, style } = 
     { label = label,       
       style = { tail = ArrowStyle.tailToString style.tail
               , head = ArrowStyle.headToString style.head
               , double = style.double
               , dashed = style.dashed
               , bend = style.bend
               }
     }
toEdgeLabel : Edge -> EdgeLabel
toEdgeLabel { label, style} = 
     { label = label,       
       style = { tail = ArrowStyle.tailFromString style.tail
               , head = ArrowStyle.headFromString style.head
               , double = style.double
               , dashed = style.dashed
               , bend = style.bend
               }
     , dims = Nothing
     , selected = False          
     }



fromNodeLabel : NodeLabel -> Node
fromNodeLabel { pos, label } = { pos = pos, label = label}

toNodeLabel : Node -> NodeLabel
toNodeLabel { pos, label } = { pos = pos, label = label
   , dims = Nothing, selected = False}





toJSGraph : Graph.Graph NodeLabel EdgeLabel -> Graph
toJSGraph g =
          let gjs = g
                   |> Graph.map 
                    (\_ -> fromNodeLabel)
                    (\_ -> fromEdgeLabel) 
                   |> Graph.normalise           
          in
          let nodes = Graph.nodes gjs
              edges = Graph.edges gjs
          in
          {nodes = nodes, edges = edges}



fromJSGraph : Graph -> Graph.Graph NodeLabel EdgeLabel
fromJSGraph { nodes, edges } = Graph.fromNodesAndEdges nodes edges
                       |> Graph.map 
                          (\_ -> toNodeLabel)
                          (\_ -> toEdgeLabel)
