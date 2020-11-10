
module GraphDrawing exposing(..)

import Graph exposing (..)
import GraphExtra as Graph
import Html 
import Html.Attributes
import Html.Events
import Drawing exposing (Drawing)
import Point exposing (Point)
import Msg exposing (..)
import Color exposing (..)
import Json.Decode as D
import Geometry



-- these are extended node and edge labels used for drawing (discarded for saving)
type alias EdgeDrawingLabel = { label : String, editable : Bool, isActive : Bool }
type alias NodeDrawingLabel =
    { pos : Point, label : String, editable : Bool, isActive : Bool,
          dims : Maybe Point
    }

make_edgeDrawingLabel : {editable : Bool, isActive : Bool} 
                      -> EdgeLabel-> EdgeDrawingLabel
make_edgeDrawingLabel {editable, isActive} label =
    { label = label, editable = editable, isActive = isActive}

make_nodeDrawingLabel : {editable : Bool, isActive : Bool, dims : Maybe Point} -> NodeLabel ->  NodeDrawingLabel
make_nodeDrawingLabel {editable, isActive, dims} {label, pos} =
    { label = label, pos = pos, editable = editable, isActive = isActive,
    dims = dims}


-- create an input with id curIdInput
make_input : Point -> String -> (String -> Msg) -> Drawing Msg
make_input pos label onChange =
         Html.input [ Html.Attributes.value label ,
                       Html.Events.onInput onChange,
                       Html.Attributes.id curIdInput,
                       Html.Attributes.autofocus True,
                       Html.Events.on "create" (D.succeed (Do focusLabelInput)),
                       Html.Attributes.class "lifecycle"
                    ]  []
             |> Drawing.html pos (100,50)

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
             Drawing.fromString 
             ([ Drawing.color color,
             Drawing.on "create" (newsizeDecoder id),
             Drawing.on "remove" (nosizeDecoder id),
             Drawing.class "lifecycle"
             ]
             ++ attrs)
             n.pos n.label 
             
        ) 

nodeDrawing : Node NodeDrawingLabel -> Drawing Msg
nodeDrawing n =
    nodeLabelDrawing
    [Drawing.onClick (NodeClick n.id),
    Drawing.onMouseEnter (NodeEnter n.id),
    Drawing.onMouseLeave (NodeLeave n.id)
    ]
     n
        



segmentLabel : Point -> Point -> Graph.EdgeId -> EdgeDrawingLabel -> Drawing Msg
segmentLabel fromP toP edgeId label =

    let
        
      --  fromP = from.label.pos
      --  toP = to.label.pos
        delta = Point.subtract toP fromP
        middle = Point.middle fromP toP
        coef  = 10
        orth = Point.normalise coef <| Point.orthogonal delta
        labelpos = Point.add middle orth
    in
        if label.editable then
             make_input labelpos label.label
             (EdgeLabelEdit edgeId)
        else
            Drawing.fromString [Drawing.onClick (EdgeClick edgeId)]
              labelpos label.label 
         


edgeDrawing : Graph.EdgeNodes (DrawingDims Msg) EdgeDrawingLabel -> Drawing Msg
edgeDrawing ({from, to , label}) =
    let c = if label.isActive then Drawing.red else Drawing.black in
    let edgeId = (from.id, to.id) in
    let (fromP, toP) = Geometry.segmentRect from.label.posDims to.label.posDims in
    Drawing.group [
         Drawing.arrow 
          [Drawing.color c, Drawing.onClick (EdgeClick edgeId)] 
         fromP toP, 
          segmentLabel fromP toP edgeId label]

type alias DrawingDims msg =
    { drawing : Drawing msg
    , posDims : Geometry.PosDims    
    }


graphDrawing : Graph NodeDrawingLabel EdgeDrawingLabel -> Drawing Msg
graphDrawing g0 =
      let height = 16 in
      let padding = 5 in
      let g = Graph.mapNodeEdges
              (\n -> { drawing = nodeDrawing n, 
                      posDims = {
                      dims = 
                      -- copied from source code of collage
                      Maybe.withDefault (height / 2 * toFloat (String.length n.label.label), height)
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
