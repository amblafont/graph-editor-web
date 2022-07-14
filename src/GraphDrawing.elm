
module GraphDrawing exposing(..)

import Polygraph as Graph exposing (Graph, Node, Edge)
import Html 
import Html.Attributes
import Html.Events
import Drawing exposing (Drawing)
import ArrowStyle exposing (ArrowStyle)
import Geometry.Point as Point exposing (Point)
import Msg exposing (Msg(..))
import Color exposing (..)
import GraphDefs exposing (NodeLabel, EdgeLabel)
import Geometry 
import Geometry.QuadraticBezier as Bez exposing (QuadraticBezier)
import HtmlDefs
import Json.Decode as D
import Html.Events.Extra.Mouse as MouseEvents


-- these are extended node and edge labels used for drawing (discarded for saving)
type alias EdgeDrawingLabel = 
   { label : String, editable : Bool, isActive : Activity, 
   style : ArrowStyle, dims : Point }
type alias NodeDrawingLabel =
    { pos : Point, label : String, editable : Bool, isActive : Activity,
      isMath : Bool,
          dims : Point 
          -- whether I should watch entering and leaving 
          -- node
          -- watchEnterLeave : Bool
    }

type alias Config = { latexPreamble : String }

type Activity = 
     MainActive
   | WeakActive
   | NoActive

toDrawingGraph : Graph NodeLabel EdgeLabel -> Graph NodeDrawingLabel EdgeDrawingLabel
toDrawingGraph =
    let makeActivity r =
           if r.selected then MainActive
           else if r.weaklySelected then WeakActive
           else NoActive
    in
    Graph.map
        (\ _ n ->  make_nodeDrawingLabel
          { editable = False
         , isActive = makeActivity n
          } n)
        (\ _ e ->  make_edgeDrawingLabel
                    { editable = False, 
                      isActive = makeActivity e
                    } e
        )

makeActive : List Graph.Id -> Graph NodeDrawingLabel EdgeDrawingLabel ->  Graph NodeDrawingLabel EdgeDrawingLabel
makeActive l = Graph.updateList l 
             (\ n -> { n | isActive = MainActive})
             (\ e -> { e | isActive = MainActive})
             

make_edgeDrawingLabel : {editable : Bool, isActive : Activity} 
                      -> EdgeLabel-> EdgeDrawingLabel
make_edgeDrawingLabel {editable, isActive} ({label, style} as l) =
    { label = label, editable = editable, isActive = isActive,
      style = style,
      dims = GraphDefs.getEdgeDims l
    }

make_nodeDrawingLabel : {editable : Bool, isActive : Activity} -> NodeLabel ->  NodeDrawingLabel
make_nodeDrawingLabel {editable, isActive} ({label, pos, isMath} as l) =
    { label = label {- if l.isMath || editable then label else "\\text{" ++ label ++ "}" -}
    , pos = pos, editable = editable, isActive = isActive, isMath = isMath,
    dims = GraphDefs.getNodeDims l }


-- create an input with id curIdInput
make_input : Point -> String -> (String -> Msg) -> Drawing Msg
make_input pos label onChange =
         Html.input ([ Html.Attributes.value label ,
                       Html.Events.onInput onChange,
                       Msg.onTabPreventDefault,
                       Html.Attributes.id HtmlDefs.idInput,
                       Html.Attributes.autofocus True,
                       Html.Attributes.style "width"
                           <| String.fromInt (String.length label + 1) ++ "ch"
                    ] ++ 
                    HtmlDefs.onRendered (always <| Do <| Msg.focusId HtmlDefs.idInput ))
                      []
             |> Drawing.html pos (100,16)

activityToColor : Activity -> Drawing.Color
activityToColor a =
   case a of
     MainActive -> Drawing.red 
     WeakActive -> Drawing.blue
     _ -> Drawing.black

activityToClasses : Activity -> List String
activityToClasses a =
   case a of
     MainActive -> ["active-label"] 
     WeakActive -> ["weak-active-label"]
     _ -> []

nodeLabelDrawing : Config -> List (Drawing.Attribute Msg) -> Node NodeDrawingLabel -> Drawing Msg
nodeLabelDrawing cfg attrs node =
    let n = node.label in
    let _ = Debug.log "pos" n.pos in
    let id = node.id in
    let color = activityToColor node.label.isActive in
    (
     if n.editable then
         make_input n.pos n.label (NodeLabelEdit id)
     else
         if n.label == "" then
             (Drawing.circle (Drawing.color color :: attrs ) n.pos 5)
         else 
            let label = cfg.latexPreamble ++ "\n" ++ if n.isMath then n.label else "\\text{" ++ n.label ++ "}" in
            Drawing.htmlAnchor n.pos n.dims n.isMath            
            <| HtmlDefs.makeLatex
            ([   MouseEvents.onClick (NodeClick id),
                 MouseEvents.onDoubleClick (EltDoubleClick id)
                 -- Html.Events.on "mousemove" (D.succeed (EltHover id))
            ] ++ 
            (activityToClasses n.isActive |> List.map Html.Attributes.class)
           -- ++ (if n.isMath then [] else  [Html.Attributes.class "text-node"])
            ++
            HtmlDefs.onRendered (Msg.NodeRendered id)
            )
             label
                

             {- Drawing.fromString 
             ([ Drawing.color color,
             Drawing.on "create" (newsizeDecoder id),
             Drawing.on "remove" (nosizeDecoder id),
             Drawing.class "lifecycle"
             ]
             ++ attrs)
             n.pos n.label  -}
             
        ) 

nodeDrawing : Config -> Node NodeDrawingLabel -> Drawing Msg
nodeDrawing cfg n =
  {-  let watch = if n.label.watchEnterLeave then
        [Drawing.onMouseEnter (NodeEnter n.id),
         Drawing.onMouseLeave (NodeLeave n.id) ]
         else []
   in  -}
    nodeLabelDrawing cfg
    [Drawing.onClick (NodeClick n.id)]
    
    n
        


segmentLabel : Config -> QuadraticBezier -> Graph.EdgeId -> EdgeDrawingLabel -> Float -> Drawing Msg
segmentLabel cfg q edgeId label curve =
    let offset = 10 + (if ArrowStyle.isDouble label.style then ArrowStyle.doubleSize else 0) in
    let labelpos =              
              -- Quiver algorithm, following redraw_label
              -- https://github.com/varkor/quiver/blob/2c62d40b820cadc3c7f9d0816a33121f389b6240/src/arrow.js#L1219
              let diffP = Point.subtract q.to q.from in
              let angle = Point.pointToAngle diffP in
              let length = Point.radius diffP in
               Geometry.determine_label_position
                 length
                 angle
                 2 -- edge_width
                 0 -- start
                 1 -- end
                 (curve * length)
                 label.style.labelPosition -- label_position
                 label.style.labelAlignment
                 (if label.editable then (2,2) else label.dims)
                 |> Point.rotate angle
                 |> Point.add q.from
         {-  -- previous algorithm 
           if Bez.isLine q then
              Point.diamondPx q.from q.to offset
              
            else 
              let m = Bez.middle q in
              Point.add m <|
              Point.normalise offset <|        
               Point.subtract q.controlPoint <| m
               -}
      
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
            ([   MouseEvents.onClick (EdgeClick edgeId),
                 MouseEvents.onDoubleClick (EltDoubleClick edgeId),
                 MouseEvents.onMove  (always (MouseOn edgeId))
                -- Html.Events.onMouseOver (EltHover edgeId)
            ] ++ 
            (activityToClasses label.isActive |> List.map Html.Attributes.class)        
             ++
             HtmlDefs.onRendered (Msg.EdgeRendered edgeId)
            )
            <| cfg.latexPreamble ++ "\n" ++ label.label
            {- Drawing.fromString [Drawing.onClick (EdgeClick edgeId)]
              labelpos label.label  -}
         

edgeDrawing : Config -> Graph.EdgeId 
-- -> Geometry.PosDims -> Geometry.PosDims
     -> EdgeDrawingLabel -> QuadraticBezier -> Float -> Drawing Msg
edgeDrawing cfg edgeId {- from to -} label q curve =
    let c = activityToColor label.isActive in
    
    
    -- let q = Geometry.segmentRectBent from to 
    --          label.style.bend
    -- in
    Drawing.group [
         Drawing.arrow 
          [Drawing.color c,
           Drawing.onClick (EdgeClick edgeId),
           Drawing.onDoubleClick (EltDoubleClick edgeId),
          -- Drawing.onHover (EltHover edgeId),
           Drawing.simpleOn "mousemove" (MouseOn edgeId)
          ] 
          label.style
         q, 
          segmentLabel cfg q edgeId label curve]

{- type alias DrawingDims msg =
    { drawing : Drawing msg
    , posDims : Geometry.PosDims    
    } -}


graphDrawing : Config -> Graph NodeDrawingLabel EdgeDrawingLabel -> Drawing Msg
graphDrawing cfg g0 =
     
      let padding = 5 in
      let g = Graph.mapRecAll     
              identity identity      
              (\id n -> { drawing = nodeDrawing cfg (Node id n), 
                      posDims = {
                      dims = 
                      
                      if n.editable then (0, 0) else
                      -- copied from source code of Collage                         
                         n.dims, 
                      pos = n.pos
                      } |> Geometry.pad padding
                       } )
               (\id n1 n2 e -> 
                   let q = Geometry.segmentRectBent n1.posDims n2.posDims e.style.bend in
                   { drawing = edgeDrawing cfg id e q e.style.bend,                     
                    -- TODO
                     posDims = {
                         pos = Bez.middle q,
                         dims = (padding, padding) |> Point.resize 4

                     }
                   }
              )
              g0 
      in
      let nodes = Graph.nodes g |> List.map (.label >> .drawing)
          edges = Graph.edges g |> List.map (.label >> .drawing)
      in
      let
          drawings = nodes ++ edges
      in
          Drawing.group drawings
