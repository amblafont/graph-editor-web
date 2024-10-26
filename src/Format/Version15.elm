module Format.Version15 exposing (Graph, Node, pullshoutStyle, nodeCodec, edgeCodec, normalKey, pullshoutKey, Tab, ArrowStyle, Edge, toJSGraph, fromJSGraph, version, tabCodec, graphInfoCodec, defaultGraph)
{- 
Changes from Version 14:
- offset1 and offset2 of pullshout edges are saved in the bend and position fields of the style
- style has now a marker field
-}
import Polygraph as Graph exposing (Graph)
import Geometry.Point exposing (Point)
import ArrowStyle exposing (tailCodec, headCodec, alignmentCodec, kindCodec, markerCodec)
import GraphDefs exposing (EdgeLabel, NodeLabel)
import Format.GraphInfo as GraphInfo exposing (GraphInfo)
import GraphDefs exposing (EdgeType(..))
import Drawing.Color as Color
import Codec exposing (Codec)
import Svg exposing (marker)

version = 15
pullshoutKey = "pullshout"
normalKey = "normal"
adjunctionKey = "adjunction"

type alias ArrowStyle = { tail : String, head : String, kind : String
   , dashed : Bool, bend : Float, alignment : String, 
   position : Float, color : String, marker : String
   }

pullshoutStyle : GraphDefs.PullshoutEdgeLabel -> ArrowStyle
pullshoutStyle {color, offset1, offset2} =
  { tail = "", head = "", kind = "normal", 
    dashed = False, bend = offset1, alignment = "", position = offset2,
    color = Codec.encoder Color.codec color, marker = "" }

type alias Edge = { label : String, style : ArrowStyle, kind : String,
       zindex : Int
      --  , selected : Bool 
       }

pullshoutEdge : Int -> GraphDefs.PullshoutEdgeLabel -> Edge
pullshoutEdge z label = 
    Edge "" (pullshoutStyle label) pullshoutKey z -- False

type alias Node = { pos : Point , label : String, isMath : Bool, zindex: Int
  , isCoqValidated : Bool
  -- , selected : Bool
  }
type alias Tab = { 
      title: String,
      sizeGrid : Int,
      id : Int,
      nodes: List (Graph.Node Node),
      edges: List (Graph.Edge Edge),
      nextGraphId : Int
   }
type alias Graph = { 
      tabs : List Tab,
      latexPreamble : String,
      nextTabId : Int,
      activeTabId : Int}

defaultGraph : Graph
defaultGraph = { tabs = [], latexPreamble = "", nextTabId = 0, activeTabId = 0}

arrowStyleCodec : Codec ArrowStyle.ArrowStyle ArrowStyle
arrowStyleCodec =
  Codec.object
  (\tail head kind dashed bend alignment position color marker ->
      { tail = tail, head = head, kind = kind
   , dashed = dashed, bend = bend, labelAlignment = alignment, 
   labelPosition = position, color = color, marker = marker }
    
  )
  (\tail head kind dashed bend alignment position color marker ->
    { tail = tail, head = head, kind = kind
   , dashed = dashed, bend = bend, alignment = alignment, 
   position = position, color = color, marker = marker }
  )
  |> Codec.fields .tail .tail tailCodec
  |> Codec.fields .head .head headCodec
  |> Codec.fields .kind .kind kindCodec
  |> Codec.fields .dashed .dashed Codec.identity
  |> Codec.fields .bend .bend Codec.identity
  |> Codec.fields .labelAlignment .alignment alignmentCodec
  |> Codec.fields .labelPosition (.position >> min 0.9 >> max 0.1) Codec.identity
  |> Codec.fields .color .color Color.codec
  |> Codec.fields .marker .marker markerCodec
  |> Codec.buildObject
  


fromEdgeLabel : EdgeLabel -> Edge
fromEdgeLabel e = 
   case e.details of
       PullshoutEdge l -> pullshoutEdge e.zindex l
       NormalEdge ({label, isAdjunction} as l)->
            let style = ArrowStyle.getStyle l in
            { label = label,
              kind = if isAdjunction then adjunctionKey else normalKey,       
              style = Codec.encoder arrowStyleCodec style,
              zindex = e.zindex 
              -- , selected = e.selected              
            }
     
toEdgeLabel : Edge -> EdgeLabel
toEdgeLabel { label, style, kind, zindex } = 
   { selected = False -- selected
     , weaklySelected = False,
     zindex = zindex,
     details = 
       if kind == pullshoutKey then 
          PullshoutEdge {color = Codec.decoder Color.codec style.color,
                         offset1 = style.bend, offset2 = style.position}
       else
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
   (\ pos label isMath zindex isCoqValidated ->
   { pos = pos, label = label
   , dims = Nothing,  weaklySelected = False, isMath = isMath,
     zindex = zindex, isCoqValidated = isCoqValidated , selected = False
     }
    )
    (\ pos label isMath zindex isCoqValidated ->
    { pos = pos, label = label, isMath = isMath, zindex = zindex
    , isCoqValidated = isCoqValidated
      --, selected = selected
      })
    |> Codec.fields .pos .pos Codec.identity
    |> Codec.fields .label .label Codec.identity
    |> Codec.fields .isMath .isMath Codec.identity
    |> Codec.fields .zindex .zindex Codec.identity
    |> Codec.fields .isCoqValidated .isCoqValidated Codec.identity
    -- |> Codec.fields .selected .selected Codec.identity
    |> Codec.buildObject


tabCodec : Codec  GraphInfo.Tab Tab 
tabCodec =
  Codec.object
  (\ graph title sizeGrid tabId ->
    { graph = graph,
      title = title, sizeGrid = sizeGrid,
      id = tabId
    }
  )
  (\ graph title sizeGrid tabId ->
    { nodes = graph.nodes,
      edges = graph.edges,
      nextGraphId = graph.nextId,
      title = title, sizeGrid = sizeGrid,
      id = tabId
    }
  )
  |> Codec.fields .graph (\e -> { nextId = e.nextGraphId, nodes = e.nodes, edges = e.edges}) 
     ( -- Codec.compose (Graph.nextIdCodec)
       (Codec.compose Graph.codec (Graph.mapCodec nodeCodec edgeCodec))
        )
  |> Codec.fields .title .title Codec.identity
  -- |> Codec.fields .active .active Codec.identity
  |> Codec.fields .sizeGrid .sizeGrid Codec.identity
  |> Codec.fields .id .id Codec.identity
  |> Codec.buildObject



graphInfoCodec : Codec GraphInfo Graph
graphInfoCodec = 
  Codec.object
  (\tabs nextTabId latexPreamble activeTabId ->
    { tabs = tabs, nextTabId = nextTabId, activeTabId = activeTabId, latexPreamble = latexPreamble }
  )
  (\tabs nextTabId latexPreamble activeTabId ->
    { tabs = tabs, nextTabId = nextTabId, activeTabId = activeTabId, latexPreamble = latexPreamble }
  )
  |> Codec.fields .tabs .tabs (Codec.list tabCodec)
  |> Codec.fields .nextTabId .nextTabId Codec.identity
  |> Codec.fields .latexPreamble .latexPreamble Codec.identity
  |> Codec.fields .activeTabId .activeTabId Codec.identity
  |> Codec.buildObject
toJSGraph : GraphInfo -> Graph
toJSGraph = Codec.encoder graphInfoCodec

fromJSGraph : Graph -> GraphInfo
fromJSGraph = Codec.decoder graphInfoCodec
