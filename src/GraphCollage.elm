
module GraphCollage exposing(..)

import Graph exposing (..)
import GraphExtra as Graph exposing (EdgeId)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Collage exposing (..)
import CollageExtra as Collage exposing (..)
import Collage.Events exposing (onClick)
import Collage.Text exposing (fromString)
import Collage.Layout
import Msg exposing (..)
import Color exposing (..)
import Tuple

-- id of the text input when the user labels an edge or a node
curIdInput : String
curIdInput = "edited_label"

-- these are extended node and edge labels used for drawing (discarded for saving)
type alias EdgeCollageLabel = { label : String, editable : Bool, isActive : Bool }
type alias NodeCollageLabel =
    { pos : Point, label : String, editable : Bool, isActive : Bool,
          dims : Maybe Point
    }

make_edgeCollageLabel : {editable : Bool, isActive : Bool} 
                      -> EdgeLabel-> EdgeCollageLabel
make_edgeCollageLabel {editable, isActive} label =
    { label = label, editable = editable, isActive = isActive}

make_nodeCollageLabel : {editable : Bool, isActive : Bool, dims : Maybe Point} -> NodeLabel ->  NodeCollageLabel
make_nodeCollageLabel {editable, isActive, dims} {label, pos} =
    { label = label, pos = pos, editable = editable, isActive = isActive,
    dims = dims}


-- create an input with id curIdInput
make_input : String -> (String -> a) -> Collage a
make_input label onChange =
         Html.input ([ Html.Attributes.value label ,
                       Html.Events.onInput onChange,
                       Html.Attributes.id curIdInput,
                       Html.Attributes.autofocus True
                    ] ) []
             |> Collage.html (100,50)

nodeLabelCollage : Node NodeCollageLabel -> Collage Msg
nodeLabelCollage node =
    let n = node.label in
    let id = node.id in
    let color = if n.isActive then red else black in
    (
     if n.editable then
         make_input n.label (NodeLabelEdit id)
     else
         if  n.label == "" then
             (circle 5 |> filled (uniform color))
         else
             (Collage.Text.fromString n.label
             |> Collage.Text.color color
             |> rendered
             |> Collage.Events.on "create" (newsizeDecoder id)
             |> Collage.Events.on "remove" (nosizeDecoder id)
             )
        ) |> shift n.pos

nodeCollage : Node NodeCollageLabel -> Collage Msg
nodeCollage n =
    nodeLabelCollage n
        |> Collage.Events.onClick (NodeClick n.id)
        |> Collage.Events.onMouseEnter (\ _ -> NodeEnter n.id)
        |> Collage.Events.onMouseLeave (\ _ -> NodeLeave n.id)



segmentLabel : Graph.EdgeNodes (CollageDims Msg) EdgeCollageLabel -> Collage Msg
segmentLabel {from, to, label} =
    let fromCollage = from.label.collage
        toCollage = to.label.collage
    in
    let
        edgeId = (from.id, to.id)
        fromP = Collage.Layout.base fromCollage
        toP = Collage.Layout.base toCollage
        delta = minusP toP fromP
        middle = middleP fromP toP
        coef  = 10
        orth = normaliseP coef <| orthogonalP delta
        labelpos = addP middle orth
    in
        (if label.editable then
             make_input label.label
             (EdgeLabelEdit edgeId)
        else
            (Collage.Text.fromString label.label |> rendered)
        ) |> shift labelpos


edgeCollage : Graph.EdgeNodes (CollageDims Msg) EdgeCollageLabel -> Collage Msg
edgeCollage ({from, to , label} as e) =
    let c = if label.isActive then red else black in
    let edgeId = (from.id, to.id) in
    Collage.group [
         arrowCollage c from.label to.label, segmentLabel e]
        |>  Collage.Events.onClick (EdgeClick edgeId)


graphCollage : Graph NodeCollageLabel EdgeCollageLabel -> Collage Msg
graphCollage g0 =
      let g = Graph.mapNodeEdges
              (\n -> { collage = nodeCollage n, dims = n.label.dims } )
              .label g0 in
      let nodes = Graph.nodes g
          edges = Graph.edgesWithNodes g
      in
          List.map (.label >> .collage) nodes ++
          List.map edgeCollage edges |>
          Collage.group
