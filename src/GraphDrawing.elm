
module GraphDrawing exposing(..)

import Graph exposing (..)
import GraphExtra as Graph
import Html 
import Html.Attributes
import Html.Events
import Drawing exposing (Drawing, DrawingDims)
import Point exposing (Point)
import Msg exposing (..)
import Color exposing (..)
-- id of the text input when the user labels an edge or a node
curIdInput : String
curIdInput = "edited_label"

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
make_input : Point -> String -> (String -> a) -> Drawing a
make_input pos label onChange =
         Html.input ([ Html.Attributes.value label ,
                       Html.Events.onInput onChange,
                       Html.Attributes.id curIdInput,
                       Html.Attributes.autofocus True
                    ] ) []
             |> Drawing.html pos (100,50)

nodeLabelDrawing : List (Drawing.Attribute Msg) -> Node NodeDrawingLabel -> Drawing Msg
nodeLabelDrawing attrs node =
    let n = node.label in
    let id = node.id in
    let color = if n.isActive then red else black in
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
             Drawing.on "remove" (nosizeDecoder id)]
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
        



segmentLabel : Graph.EdgeNodes (DrawingDims Msg) EdgeDrawingLabel -> Drawing Msg
segmentLabel {from, to, label} =
    let fromDrawing = from.label.drawing
        toDrawing = to.label.drawing
    in
    let
        edgeId = (from.id, to.id)
        fromP = Drawing.base fromDrawing
        toP = Drawing.base toDrawing
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
edgeDrawing ({from, to , label} as e) =
    let c = if label.isActive then red else black in
    let edgeId = (from.id, to.id) in
    Drawing.group [
         Drawing.arrowDrawing 
          [Drawing.color c, Drawing.onClick (EdgeClick edgeId)] 
         from.label to.label, 
          segmentLabel e]


graphDrawing : Graph NodeDrawingLabel EdgeDrawingLabel -> Drawing Msg
graphDrawing g0 =
      let g = Graph.mapNodeEdges
              (\n -> { drawing = nodeDrawing n, dims = n.label.dims } )
              .label g0 in
      let nodes = Graph.nodes g
          edges = Graph.edgesWithNodes g
      in
          List.map (.label >> .drawing) nodes ++
          List.map edgeDrawing edges |>
          Drawing.group
