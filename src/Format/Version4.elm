module Format.Version4 exposing (Graph, Node, Edge, toJSGraph, fromJSGraph, version)

import Polygraph as Graph exposing (Graph)
import Geometry.Point exposing (Point)
import ArrowStyle
import GraphDefs exposing (EdgeLabel, NodeLabel)
import ArrowStyle exposing (LabelAlignment(..))
import Format.GraphInfo exposing (GraphInfo)

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

fromEdgeLabel : EdgeLabel -> Edge
fromEdgeLabel { label, style } = 
     { label = label,       
       style = { tail = ArrowStyle.tailToString style.tail
               , head = ArrowStyle.headToString style.head
               , alignment = ArrowStyle.alignmentToString style.labelAlignment
               , double = style.double
               , dashed = style.dashed
               , bend = style.bend
               , position = style.labelPosition
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
               , labelAlignment = ArrowStyle.alignmentFromString style.alignment
               , labelPosition = style.position 
                                 |> min 0.9
                                 |> max 0.1                                 
               }
     , dims = Nothing
     , selected = False
     , weaklySelected = False
     }



fromNodeLabel : NodeLabel -> Node
fromNodeLabel { pos, label, isMath } = { pos = pos, label = label, isMath = isMath}

toNodeLabel : Node -> NodeLabel
toNodeLabel { pos, label, isMath } = { pos = pos, label = label
   , dims = Nothing, selected = False, weaklySelected = False, isMath = isMath}





toJSGraph : GraphInfo -> Graph
toJSGraph m =
          let g = m.graph in
          let gjs = g
                   |> Graph.map 
                    (\_ -> fromNodeLabel)
                    (\_ -> fromEdgeLabel) 
                   |> Graph.normalise           
          in
          let nodes = Graph.nodes gjs
              edges = Graph.edges gjs
          in
          {nodes = nodes, edges = edges, sizeGrid = m.sizeGrid}



fromJSGraph : Graph -> GraphInfo
fromJSGraph { nodes, edges, sizeGrid } = 
       { graph = Graph.fromNodesAndEdges nodes edges
                  |> Graph.map (\_ -> toNodeLabel) (\_ -> toEdgeLabel),
         sizeGrid = sizeGrid }
