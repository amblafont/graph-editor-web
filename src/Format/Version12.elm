module Format.Version12 exposing (Graph, Node, normalKey, pullshoutKey, Tab, ArrowStyle, Edge, toJSGraph, fromJSGraph, version)

import Polygraph as Graph exposing (Graph)
import Geometry.Point exposing (Point)
import ArrowStyle exposing (tailCodec, headCodec, alignmentCodec, kindCodec)
import GraphDefs exposing (EdgeLabel, NodeLabel)
import Format.GraphInfo as GraphInfo exposing (GraphInfo)
import GraphDefs exposing (EdgeType(..))
import Drawing.Color as Color
import Codec exposing (Codec)

version = 12
pullshoutKey = "pullshout"
normalKey = "normal"
adjunctionKey = "adjunction"

type alias ArrowStyle = { tail : String, head : String, kind : String
   , dashed : Bool, bend : Float, alignment : String, 
   position : Float, color : String }

emptyArrowStyle : ArrowStyle
emptyArrowStyle = ArrowStyle "" "" "normal" False 0 "" 0 "black"

type alias Edge = { label : String, style : ArrowStyle, kind : String,
       zindex : Int }

pullshoutEdge : Int -> Edge
pullshoutEdge z = Edge "" emptyArrowStyle pullshoutKey z

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


arrowStyleCodec : Codec ArrowStyle.ArrowStyle ArrowStyle
arrowStyleCodec =
  Codec.object
  (\tail head kind dashed bend alignment position color ->
      { tail = tail, head = head, kind = kind
   , dashed = dashed, bend = bend, labelAlignment = alignment, 
   labelPosition = position, color = color }
    
  )
  (\tail head kind dashed bend alignment position color ->
    { tail = tail, head = head, kind = kind
   , dashed = dashed, bend = bend, alignment = alignment, 
   position = position, color = color }
  )
  |> Codec.bothFields .tail .tail tailCodec
  |> Codec.bothFields .head .head headCodec
  |> Codec.bothFields .kind .kind kindCodec
  |> Codec.bothFields .dashed .dashed Codec.identity
  |> Codec.bothFields .bend .bend Codec.identity
  |> Codec.bothFields .labelAlignment .alignment alignmentCodec
  |> Codec.bothFields .labelPosition (.position >> min 0.9 >> max 0.1) Codec.identity
  |> Codec.bothFields .color .color Color.codec
  |> Codec.buildObject
  


fromEdgeLabel : EdgeLabel -> Edge
fromEdgeLabel e = 
   case e.details of
       PullshoutEdge -> pullshoutEdge e.zindex
       NormalEdge ({label, isAdjunction} as l)->
            let style = ArrowStyle.getStyle l in
            { label = label,
              kind = if isAdjunction then adjunctionKey else normalKey,       
              style = Codec.encoder arrowStyleCodec style,
              zindex = e.zindex               
            }
     
toEdgeLabel : Edge -> EdgeLabel
toEdgeLabel { label, style, kind, zindex } = 
   { selected = False, weaklySelected = False,
     zindex = zindex,
     details = 
       if kind == pullshoutKey then PullshoutEdge else
         NormalEdge { label = label
           , isAdjunction = kind == adjunctionKey      
           , style = Codec.decoder arrowStyleCodec style
         , dims = Nothing
         }
   }

edgeCodec : Codec EdgeLabel Edge
edgeCodec = 
   Codec.build fromEdgeLabel toEdgeLabel

nodeCodec : Codec NodeLabel Node
nodeCodec = 
   Codec.object
   (\ pos label isMath zindex ->
   { pos = pos, label = label
   , dims = Nothing, selected = False, weaklySelected = False, isMath = isMath,
     zindex = zindex, isCoqValidated = False}
    )
    (\ pos label isMath zindex ->
    { pos = pos, label = label, isMath = isMath, zindex = zindex})
    |> Codec.bothFields .pos .pos Codec.identity
    |> Codec.bothFields .label .label Codec.identity
    |> Codec.bothFields .isMath .isMath Codec.identity
    |> Codec.bothFields .zindex .zindex Codec.identity
    |> Codec.buildObject


tabCodec : Codec  GraphInfo.Tab Tab 
tabCodec =
  Codec.object
  (\ graph title active sizeGrid ->
    { graph = graph,
      title = title, active = active, sizeGrid = sizeGrid
    }
  )
  (\ graph title active sizeGrid ->
    { nodes = graph.nodes,
      edges = graph.edges,
      title = title, active = active, sizeGrid = sizeGrid
    }
  )
  |> Codec.bothFields .graph (\e -> { nodes = e.nodes, edges = e.edges}) 
       (Codec.compose Graph.codec (Graph.mapCodec nodeCodec edgeCodec))
  |> Codec.bothFields .title .title Codec.identity
  |> Codec.bothFields .active .active Codec.identity
  |> Codec.bothFields .sizeGrid .sizeGrid Codec.identity
  |> Codec.buildObject

-- TODO: normalisation

toJSTab : GraphInfo.Tab -> Tab
toJSTab = Codec.encoder tabCodec


fromJSTab : Tab -> GraphInfo.Tab
fromJSTab = Codec.decoder tabCodec


toJSGraph : GraphInfo -> Graph
toJSGraph m =
          {tabs = List.map toJSTab m.tabs,
          latexPreamble = m.latexPreamble}



fromJSGraph : Graph -> GraphInfo
fromJSGraph { tabs, latexPreamble } = 
       { tabs = List.map fromJSTab tabs,
         latexPreamble = latexPreamble }
