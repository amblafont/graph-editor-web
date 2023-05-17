module Format.Version8 exposing (Graph, Node, Edge, ArrowStyle, fromJSGraph, version)

import Polygraph as Graph exposing (Graph)
import Format.GraphInfo exposing (GraphInfo)
import GraphDefs exposing (EdgeType(..))
import Format.Version9 as NextVersion

version = 8

type alias Node = NextVersion.Node
type alias ArrowStyle = { tail : String, head : String, double : Bool
   , dashed : Bool, bend : Float, alignment : String, 
   position : Float }

toNextStyle : ArrowStyle -> NextVersion.ArrowStyle
toNextStyle style =
   {tail = style.tail, 
    head = style.head,
    double = style.double,
    dashed = style.dashed,
    bend  = style.bend,
    alignment = style.alignment,
    position = style.position,
    color = "black"
   }

type alias Edge = { label : String, style : ArrowStyle, isPullshout : Bool,
       zindex : Int }


type alias Graph = { 
      nodes: List (Graph.Node Node),
      edges: List (Graph.Edge Edge),
      sizeGrid : Int,
      latexPreamble : String}
toNextEdge : Edge -> NextVersion.Edge
toNextEdge { label, style, isPullshout, zindex } = 
  { label = label, style = toNextStyle style, isPullshout = isPullshout,
    zindex = zindex }


toNextVersion : Graph -> NextVersion.Graph
toNextVersion { nodes, edges, sizeGrid, latexPreamble } = 
    { nodes = nodes, 
      edges =  List.map (Graph.edgeMap toNextEdge) edges,
      sizeGrid = sizeGrid,
      latexPreamble = latexPreamble }


fromJSGraph : Graph -> GraphInfo
fromJSGraph g = 
     g |> toNextVersion |> NextVersion.fromJSGraph