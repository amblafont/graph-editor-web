module Format.Version16 exposing (Graph, Node, keys, Tab, ArrowStyle, Edge, fromJSGraph, version)
{- 
Changes from Version 15:
- style has now head / tail colors and a wavy field
-}
import Polygraph as Graph exposing (Graph)
import Geometry.Point exposing (Point)
import Format.GraphInfo exposing (GraphInfo)
import GraphDefs exposing (EdgeType(..))
import Drawing.Color as Color
import Codec
import Format.Version17 as NextVersion exposing
  (dashedFlag, wavyFlag, textFlag, coqValidatedFlag,
  prefixes, bendFlag, positionFlag)
import GraphDefs

version = 16

keys = {
   pullshout = "pullshout",
   normal = "normal",
   adjunction = "adjunction"
  }

type alias ArrowStyle = { tail : String, head : String, kind : String
   , dashed : Bool, bend : Float, alignment : String, 
   position : Float, color : String, marker : String,
   headColor : String, tailColor : String, wavy : Bool
   }

toNextStyle : ArrowStyle -> NextVersion.ArrowStyle
toNextStyle {tail, head, kind, dashed, wavy, bend, alignment, position, color, headColor, tailColor, marker} = 
  let enc2 c = Codec.encoder c in
  let prefix field s = field prefixes ++ s in 
  -- { -- bend = bend,
            enc2 dashedFlag dashed ++
            enc2 wavyFlag wavy ++ 
            enc2 bendFlag bend ++
            enc2 positionFlag position ++
            [prefix .color color, prefix .headColor headColor, 
            prefix .tailColor tailColor,
            prefix .tail tail,
            prefix .head head, 
            prefix .kind kind,
            prefix .alignment alignment,
            prefix .marker marker
            ]

toNextEdge : Edge -> NextVersion.Edge
toNextEdge {label, style, kind, zindex} = 
  let style2 = toNextStyle style in
  let style3 = if kind == keys.pullshout then
                 NextVersion.addFlag 
                 (NextVersion.Pullshout {offset1 = style.position, offset2 = style.bend}) style2 
               else style2 
  in
  let style4 = if kind == keys.adjunction then
                 NextVersion.addFlag NextVersion.Adjunction style3
               else style3
  in
  {label = label, 
   style = 
      -- let _ = Debug.log "flags" <| Codec.decoder (Codec.list edgeFlagCodec) style4 in
      -- let _ = Debug.log "kind" <| Codec.decoder  kindFlag style4 in
        -- Debug.log "nextStyle" 
        style4,
    zindex = zindex} --, selected = False}


type alias Edge = { label : String, style : ArrowStyle, kind : String,
       zindex : Int
      --  , selected : Bool 
       }


type alias Node = { pos : Point , label : String, isMath : Bool, zindex: Int
  , isCoqValidated : Bool
  -- , selected : Bool
  }

toNextNode : Node -> NextVersion.Node
toNextNode {pos, label, isMath, zindex, isCoqValidated} = 
  let enc = Codec.encoder  in
  {pos = pos, label = label, zindex = zindex,
  flags = enc textFlag (not isMath) ++ enc coqValidatedFlag isCoqValidated
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






toNextTab : Tab -> NextVersion.Tab
toNextTab {id, title, sizeGrid, nodes, edges, nextGraphId} =
  { title = title, 
    id = id,
    sizeGrid = sizeGrid,
    nodes = List.map (Graph.nodeMap toNextNode) nodes,
    edges = List.map (Graph.edgeMap toNextEdge) edges,
    nextGraphId = nextGraphId
  }

toNextVersion : Graph -> NextVersion.Graph
toNextVersion { tabs, nextTabId, activeTabId, latexPreamble} = 
    
    { tabs = List.map toNextTab tabs,
      latexPreamble = latexPreamble,
      nextTabId = nextTabId,
      activeTabId = activeTabId}


fromJSGraph : Graph -> GraphInfo
fromJSGraph g = 
    g |> toNextVersion |> NextVersion.fromJSGraph