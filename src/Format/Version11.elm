module Format.Version11 exposing (Graph, Node, Tab, ArrowStyle, Edge, toJSGraph, fromJSGraph, version)

import Polygraph as Graph exposing (Graph)
import Geometry.Point exposing (Point)
import ArrowStyle
import GraphDefs exposing (EdgeLabel, NodeLabel)
import Format.GraphInfo as GraphInfo exposing (GraphInfo)
import GraphDefs exposing (EdgeType(..))
import Drawing.Color as Color

version = 11

type alias ArrowStyle = { tail : String, head : String, double : Bool
   , dashed : Bool, bend : Float, alignment : String, 
   position : Float, color : String }

emptyArrowStyle : ArrowStyle
emptyArrowStyle = ArrowStyle "" "" False False 0 "" 0 "black"

type alias Edge = { label : String, style : ArrowStyle, isPullshout : Bool,
       zindex : Int }

pullshoutEdge : Int -> Edge
pullshoutEdge z = Edge "" emptyArrowStyle True z

type alias Node = { pos : Point , label : String, isMath : Bool, zindex: Int}
type alias Tab = { 
      title: String,
      sizeGrid : Int,
      active: Bool,
      nodes: List (Graph.Node Node),
      edges: List (Graph.Edge Edge) 
   }
type alias Graph = { 
      tabs : List Tab,
      latexPreamble : String}


fromEdgeLabel : EdgeLabel -> Edge
fromEdgeLabel e = 
   case e.details of
       PullshoutEdge -> pullshoutEdge e.zindex
       NormalEdge {label, style} ->
            { label = label,
              isPullshout = False,       
              style = { tail = ArrowStyle.tailToString style.tail
               , head = ArrowStyle.headToString style.head
               , alignment = ArrowStyle.alignmentToString style.labelAlignment
               , double = style.double
               , dashed = style.dashed
               , bend = style.bend
               , position = style.labelPosition
               , color = Color.toString style.color
               },
               zindex = e.zindex               
            }
     
toEdgeLabel : Edge -> EdgeLabel
toEdgeLabel { label, style, isPullshout, zindex } = 
   { selected = False, weaklySelected = False,
     zindex = zindex,
     details = 
       if isPullshout then PullshoutEdge else
         NormalEdge { label = label,       
           style = { tail = ArrowStyle.tailFromString style.tail
                   , head = ArrowStyle.headFromString style.head
                   , double = style.double
                   , dashed = style.dashed
                   , bend = style.bend
                   , color = Color.fromString style.color
                   , labelAlignment = ArrowStyle.alignmentFromString style.alignment
                   , labelPosition = style.position 
                                     |> min 0.9
                                     |> max 0.1                                 
                   }
         , dims = Nothing
         }
   }




fromNodeLabel : NodeLabel -> Node
fromNodeLabel { pos, label, isMath, zindex } = 
  { pos = pos, label = label, isMath = isMath, zindex = zindex}

toNodeLabel : Node -> NodeLabel
toNodeLabel { pos, label, isMath, zindex } = { pos = pos, label = label
   , dims = Nothing, selected = False, weaklySelected = False, isMath = isMath,
     zindex = zindex}


toJSTab : GraphInfo.Tab -> Tab
toJSTab tab =
          let g = tab.graph in
          let gjs = g
                   |> Graph.map 
                    (\_ -> fromNodeLabel)
                    (\_ -> fromEdgeLabel) 
                   |> Graph.normalise           
          in
          let nodes = Graph.nodes gjs
              edges = Graph.edges gjs
          in
          { nodes = nodes,
            edges = edges,
            sizeGrid = tab.sizeGrid,
            title = tab.title,
            active = tab.active
          }

fromJSTab : Tab -> GraphInfo.Tab
fromJSTab tab = 
     { graph = Graph.fromNodesAndEdges tab.nodes tab.edges
                  |> Graph.map (\_ -> toNodeLabel) (\_ -> toEdgeLabel),
       sizeGrid = tab.sizeGrid,
       title = tab.title,
       active = tab.active}


toJSGraph : GraphInfo -> Graph
toJSGraph m =
          {tabs = List.map toJSTab m.tabs,
          latexPreamble = m.latexPreamble}



fromJSGraph : Graph -> GraphInfo
fromJSGraph { tabs, latexPreamble } = 
       { tabs = List.map fromJSTab tabs,
         latexPreamble = latexPreamble }
