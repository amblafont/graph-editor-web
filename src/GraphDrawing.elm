
module GraphDrawing exposing(..)

import Graph exposing (..)
import GraphExtra as Graph
import Html 
import Html.Attributes
import Html.Events
import Drawing exposing (Drawing)
import ArrowStyle exposing (ArrowStyle)
import ArrowStyle.Core as ArrowStyle
import Geometry.Point as Point exposing (Point)
import Msg exposing (Msg(..))
import Color exposing (..)
import GraphDefs exposing (NodeLabel, EdgeLabel)
import Geometry 
import Geometry.QuadraticBezier as Bez exposing (QuadraticBezier)
import HtmlDefs



-- these are extended node and edge labels used for drawing (discarded for saving)
type alias EdgeDrawingLabel = 
   { label : String, editable : Bool, isActive : Bool, 
   style : ArrowStyle, dims : Point }
type alias NodeDrawingLabel =
    { pos : Point, label : String, editable : Bool, isActive : Bool,
          dims : Point 
          -- whether I should watch entering and leaving 
          -- node
          -- watchEnterLeave : Bool
    }

make_edgeDrawingLabel : {editable : Bool, isActive : Bool} 
                      -> EdgeLabel-> EdgeDrawingLabel
make_edgeDrawingLabel {editable, isActive} ({label, style} as l) =
    { label = label, editable = editable, isActive = isActive, style = style,
      dims = GraphDefs.getEdgeDims l
    }

make_nodeDrawingLabel : {editable : Bool, isActive : Bool} -> NodeLabel ->  NodeDrawingLabel
make_nodeDrawingLabel {editable, isActive} ({label, pos} as l) =
    { label = label, pos = pos, editable = editable, isActive = isActive,
    dims = GraphDefs.getNodeDims l }


-- create an input with id curIdInput
make_input : Point -> String -> (String -> Msg) -> Drawing Msg
make_input pos label onChange =
         Html.input ([ Html.Attributes.value label ,
                       Html.Events.onInput onChange,
                       Html.Attributes.id HtmlDefs.idInput,
                       Html.Attributes.autofocus True
                    ] ++ 
                    HtmlDefs.onRendered (always <| Do <| Msg.focusId HtmlDefs.idInput ))
                      []
             |> Drawing.html pos (100,16)


nodeLabelDrawing : List (Drawing.Attribute Msg) -> Node NodeDrawingLabel -> Drawing Msg
nodeLabelDrawing attrs node =
    let n = node.label in
    let id = node.id in
    let color = if n.isActive then Drawing.red else Drawing.black in
    (
     if n.editable then
         make_input n.pos n.label (NodeLabelEdit id)
     else
         if  n.label == "" then
             (Drawing.circle (Drawing.color color :: attrs ) n.pos 5)
         else 
            Drawing.html n.pos n.dims
            <| HtmlDefs.makeLatex
            ([   Html.Events.onClick (NodeClick id)            
            ] ++ 
            (if n.isActive then [Html.Attributes.class "active-label" ] else [])
            ++
            HtmlDefs.onRendered (Msg.NodeRendered id)
            )
             n.label
                

             {- Drawing.fromString 
             ([ Drawing.color color,
             Drawing.on "create" (newsizeDecoder id),
             Drawing.on "remove" (nosizeDecoder id),
             Drawing.class "lifecycle"
             ]
             ++ attrs)
             n.pos n.label  -}
             
        ) 

nodeDrawing : Node NodeDrawingLabel -> Drawing Msg
nodeDrawing n =
  {-  let watch = if n.label.watchEnterLeave then
        [Drawing.onMouseEnter (NodeEnter n.id),
         Drawing.onMouseLeave (NodeLeave n.id) ]
         else []
   in  -}
    nodeLabelDrawing
    [Drawing.onClick (NodeClick n.id)]
    
     n
        



segmentLabel : QuadraticBezier -> Graph.EdgeId -> EdgeDrawingLabel -> Drawing Msg
segmentLabel q edgeId label =

    let
        offset = 10 + (if ArrowStyle.isDouble label.style.s then ArrowStyle.doubleSize else 0)
        labelpos =
           if Bez.isLine q then
              Point.diamondPx q.from q.to offset
              
            else 
              let m = Bez.middle q in
              Point.add m <|
              Point.normalise offset <|        
               Point.subtract q.controlPoint <| m
      
    in
        if label.editable then
             make_input labelpos label.label
             (EdgeLabelEdit edgeId)
        else
         if  label.label == "" then
             Drawing.empty
         else 
            Drawing.html labelpos label.dims -- n.dims
            <| HtmlDefs.makeLatex
            ([   Html.Events.onClick (EdgeClick edgeId)            
            ] ++ 
            (if label.isActive then [Html.Attributes.class "active-label" ] else [])
             ++
             HtmlDefs.onRendered (Msg.EdgeRendered edgeId)
            )
             label.label
            {- Drawing.fromString [Drawing.onClick (EdgeClick edgeId)]
              labelpos label.label  -}
         


edgeDrawing : Graph.EdgeNodes (DrawingDims Msg) EdgeDrawingLabel -> Drawing Msg
edgeDrawing {from, to , label } =
    let c = if label.isActive then Drawing.red else Drawing.black in
    let edgeId = (from.id, to.id) in
    
    let q = Geometry.segmentRectBent from.label.posDims to.label.posDims 
             label.style.bend
    in
    Drawing.group [
         Drawing.arrow 
          [Drawing.color c, Drawing.onClick (EdgeClick edgeId)] 
          label.style.s
         q, 
          segmentLabel q edgeId label]

type alias DrawingDims msg =
    { drawing : Drawing msg
    , posDims : Geometry.PosDims    
    }


graphDrawing : Graph NodeDrawingLabel EdgeDrawingLabel -> Drawing Msg
graphDrawing g0 =
     
      let padding = 5 in
      let g = Graph.mapNodesEdges
              (\n -> { drawing = nodeDrawing n, 
                      posDims = {
                      dims = 
                      
                      if n.label.editable then (0, 0) else
                      -- copied from source code of Collage                         
                         n.label.dims, 
                      pos = n.label.pos
                      } |> Geometry.pad padding
                       } )
              .label g0 in
      let nodes = Graph.nodes g
          edges = Graph.edgesWithNodes g
      in
          List.map (.label >> .drawing) nodes ++
          List.map edgeDrawing edges |>
          Drawing.group
