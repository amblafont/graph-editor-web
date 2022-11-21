
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
import GraphDefs exposing (NodeLabel, EdgeLabel, NormalEdgeLabel)
import Geometry 
import Geometry.QuadraticBezier as Bez exposing (QuadraticBezier)
import HtmlDefs
import Json.Decode as D
import Html.Events.Extra.Mouse as MouseEvents


-- these are extended node and edge labels used for drawing (discarded for saving)
type alias NormalEdgeDrawingLabel = 
   { label : String, editable : Bool,
     style : ArrowStyle, dims : Point }

type EdgeType = 
    PullbackEdge
  | NormalEdge NormalEdgeDrawingLabel

type alias EdgeDrawingLabel = { details : EdgeType, isActive : Activity}

mapNormalEdge : (NormalEdgeDrawingLabel -> NormalEdgeDrawingLabel) -> EdgeDrawingLabel -> EdgeDrawingLabel
mapNormalEdge f e = 
  {isActive = e.isActive,
   details = case e.details of
               PullbackEdge -> PullbackEdge
               NormalEdge l -> NormalEdge <| f l
  }


type alias NodeDrawingLabel =
    { pos : Point,
      inputPos : Point, -- where the input text should be located
      -- (differ from pos in the case the node is a text)
     label : String, editable : Bool, isActive : Activity,
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
                      -> EdgeLabel -> EdgeDrawingLabel
make_edgeDrawingLabel {editable, isActive} e =
   { isActive = isActive,
     details = case e.details of 
        GraphDefs.PullbackEdge -> PullbackEdge
        GraphDefs.NormalEdge ({label, style} as l) ->
           NormalEdge { label = label, editable = editable, 
              style = style,
              dims = GraphDefs.getEdgeDims l
            }
   }
    
        
    

make_nodeDrawingLabel : {editable : Bool, isActive : Activity} -> NodeLabel ->  NodeDrawingLabel
make_nodeDrawingLabel {editable, isActive} ({label, pos, isMath} as l) =
    let nodePos = GraphDefs.getNodePos l in
    { label = label {- if l.isMath || editable then label else "\\text{" ++ label ++ "}" -}
    , pos = nodePos
    , inputPos = pos
    , editable = editable, isActive = isActive, isMath = isMath,
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
                    HtmlDefs.onRendered (always <| Do <| Msg.focusId HtmlDefs.idInput )
                    ++
                    HtmlDefs.onRendered (always <| Do <| HtmlDefs.select HtmlDefs.idInput )
                    ) []                                        
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
    let id = node.id in
    let color = activityToColor node.label.isActive in
    (
     if n.editable then
         make_input n.inputPos n.label (NodeLabelEdit id)
     else
         if n.label == "" then
             (Drawing.circle (Drawing.color color :: attrs ) n.pos 5)
         else 
            let label = cfg.latexPreamble ++ "\n" ++ if n.isMath then n.label else "\\text{" ++ n.label ++ "}" in
            Drawing.htmlAnchor n.pos n.dims True {- n.isMath -}            
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
        


segmentLabel : Config -> QuadraticBezier -> Graph.EdgeId -> Activity -> NormalEdgeDrawingLabel -> Float -> Drawing Msg
segmentLabel cfg q edgeId activity label curve =
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
            (activityToClasses activity |> List.map Html.Attributes.class)        
             ++
             HtmlDefs.onRendered (Msg.EdgeRendered edgeId)
            )
            <| cfg.latexPreamble ++ "\n" ++ label.label
            {- Drawing.fromString [Drawing.onClick (EdgeClick edgeId)]
              labelpos label.label  -}
         

normalEdgeDrawing : Config -> Graph.EdgeId 
-- -> Geometry.PosDims -> Geometry.PosDims
     -> Activity 
     -> NormalEdgeDrawingLabel -> QuadraticBezier -> Float -> Drawing Msg
normalEdgeDrawing cfg edgeId activity {- from to -} label q curve =
    let c = activityToColor activity in
    
    
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
          segmentLabel cfg q edgeId activity label curve]

{- type alias DrawingDims msg =
    { drawing : Drawing msg
    , posDims : Geometry.PosDims    
    } -}

drawPullback : Graph.EdgeId -> Activity -> (Point, Point) -> (Point, Point) -> Drawing Msg
drawPullback edgeId a (p1, p2) (q1, q2) =
     let shift = 30 in
     let smallshift = 5 in

     let r1 = Point.towards p1 p2 shift
         r2 = Point.towards q1 q2 shift
     in
     let extrem = Point.diamondPave r1 p1 r2 in
     let s1 = Point.towards r1 extrem smallshift
         s2 = Point.towards r2 extrem smallshift
     in
     let blackline = Drawing.line 
               [Drawing.color <| activityToColor a,
                Drawing.onClick (EdgeClick edgeId)]
     in
     Drawing.group 
     [blackline s1 extrem, blackline extrem s2] 

{-

Fin

-}

graphDrawing : Config -> Graph NodeDrawingLabel EdgeDrawingLabel -> Drawing Msg
graphDrawing cfg g0 =
     
      let padding = 5 in
      let drawEdge id n1 n2 e = 
             case e.details of
               PullbackEdge -> { drawing = 
                                   Maybe.map2 (drawPullback id e.isActive) n1.extrems n2.extrems
                                   |> Maybe.withDefault Drawing.empty
                               , posDims = { pos = (0, 0), dims = (0, 0)}
                               , extrems = Just (n1.posDims.pos, n2.posDims.pos)
                               }
               NormalEdge l ->
                   let q = Geometry.segmentRectBent n1.posDims n2.posDims l.style.bend in
                       { drawing = normalEdgeDrawing cfg id e.isActive l q l.style.bend,                     
                        -- TODO
                         posDims = {
                             pos = Bez.middle q,
                             dims = (padding, padding) |> Point.resize 4

                         },
                         extrems = Just (n1.posDims.pos, n2.posDims.pos)
                       }
      in
      let g = Graph.mapRecAll     
              identity identity      
              (\id n -> { drawing = nodeDrawing cfg (Node id n), 
                      extrems = Nothing,
                      posDims = {
                      dims = 
                      
                      if n.editable then (0, 0) else
                      -- copied from source code of Collage                         
                         n.dims, 
                      pos = n.pos
                      } |> Geometry.pad padding
                       } )
               drawEdge
              g0 
      in
      let nodes = Graph.nodes g |> List.map (.label >> .drawing)
          edges = Graph.edges g |> List.map (.label >> .drawing)
      in
      let
          drawings = nodes ++ edges
      in
          Drawing.group drawings
