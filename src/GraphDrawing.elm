
module GraphDrawing exposing(..)

import Polygraph as Graph exposing (Graph, Node, Edge, Id)
import Html 
import Html.Attributes
import Html.Events
import Drawing exposing (Drawing)
import Drawing.Color as Color exposing (Color)
import ArrowStyle exposing (ArrowStyle, MarkerStyle)
import Geometry.Point as Point exposing (Point)
import Msg exposing (Msg(..))
import GraphDefs exposing (NodeLabel, EdgeLabel, NormalEdgeLabel)
import Geometry 
import SpecialLabels
import Geometry.QuadraticBezier as Bez exposing (QuadraticBezier)
import HtmlDefs
import Html.Events.Extra.Mouse as MouseEvents
import Zindex exposing (foregroundZ)
import EdgeShape exposing (EdgeShape(..), Hat)
import Json.Decode
import Geometry.Curve as Curve exposing (Curve(..))

-- these are extended node and edge labels used for drawing (discarded for saving)
type alias NormalEdgeDrawingLabel = 
   { label : String, editable : Bool,
     style : ArrowStyle, dims : Point, isAdjunction : Bool,
     isDependency : Bool}

type EdgeType = 
    PullshoutEdge {color : Color.Color}
  | NormalEdge NormalEdgeDrawingLabel

type alias EdgeDrawingLabel = { details : EdgeType, shape : EdgeShape, isActive : Activity, 
    -- this is useful for dependency edges, to know if we should draw them
    sourceIsActive : Activity, 
    zindex : Int}

mapNormalEdge : (NormalEdgeDrawingLabel -> NormalEdgeDrawingLabel) -> EdgeDrawingLabel -> EdgeDrawingLabel
mapNormalEdge f e = 
  {isActive = e.isActive,
   zindex = e.zindex,
   shape = e.shape,
   sourceIsActive = e.sourceIsActive,
   details = case e.details of
               PullshoutEdge x -> PullshoutEdge x
               NormalEdge l -> NormalEdge <| f l
  }


type alias NodeDrawingLabel =
    { pos : Point, 
    -- is it marked as validated
    isValidated : Bool,
      inputPos : Point, -- where the input text should be located
      -- (differ from pos in the case the node is a text)
     label : String, editable : Bool, isActive : Activity,
      isMath : Bool,
          dims : Point,
          zindex : Int,
          color : Color
          -- whether I should watch entering and leaving 
          -- node
          -- watchEnterLeave : Bool
    }

type alias Config = { latexPreamble : String, showDependencies : Bool }

type Activity = 
     MainActive
   | WeakActive
   | NoActive

toDrawingGraph : Graph NodeLabel EdgeLabel -> Graph NodeDrawingLabel EdgeDrawingLabel
toDrawingGraph g =
    let makeActivity r =
           if r.selected then MainActive
           else if r.weaklySelected then WeakActive
           else NoActive
    in
    let graphWithPos = GraphDefs.posGraph g in
    Graph.mapRecAll .isActive .isActive
        (\ _ n ->  make_nodeDrawingLabel
          { editable = False
         , isActive = makeActivity n
          } n)
        (\ _ sourceIsActive _ e ->  make_edgeDrawingLabel
                    { editable = False, 
                      isActive = makeActivity e.label,
                      sourceIsActive = sourceIsActive,
                      shape = e.shape
                    } e.label
        )
        graphWithPos

makeActive : List Graph.Id -> Graph NodeDrawingLabel EdgeDrawingLabel ->  Graph NodeDrawingLabel EdgeDrawingLabel
makeActive l = Graph.updateList l 
             (\ n -> { n | isActive = MainActive})
             (\ e -> { e | isActive = MainActive})
             

make_edgeDrawingLabel : {editable : Bool, isActive : Activity, sourceIsActive : Activity, shape : EdgeShape} 
                      -> EdgeLabel -> EdgeDrawingLabel
make_edgeDrawingLabel {editable, isActive, shape, sourceIsActive} e =
   { isActive = isActive, zindex = e.zindex, shape = shape,
     sourceIsActive = sourceIsActive,
     details = case e.details of 
        GraphDefs.PullshoutEdge x -> PullshoutEdge { color = x.color}
        GraphDefs.NormalEdge ({label, style, isAdjunction} as l) ->
           NormalEdge { label = label, editable = editable, 
              isAdjunction = isAdjunction,
              style = style,
              isDependency = GraphDefs.isDepEdge l,
              dims = GraphDefs.getEdgeDims l
              -- bezier = bezier |> Maybe.withDefault Bez.dummy
            }
   }
    
        
    

make_nodeDrawingLabel : {editable : Bool, isActive : Activity} -> NodeLabel ->  NodeDrawingLabel
make_nodeDrawingLabel {editable, isActive} ({label, pos, isMath} as l) =
    let nodePos = GraphDefs.getNodePos l in
    { label = label {- if l.isMath || editable then label else "\\text{" ++ label ++ "}" -}
    , pos = nodePos
    , inputPos = pos
    , isValidated = l.isCoqValidated
    , editable = editable, isActive = isActive, isMath = isMath,
      dims = if editable then (0,0) else GraphDefs.getNodeDims l
    , zindex = l.zindex
    , color = l.color }


-- create an input with id curIdInput
make_input : Maybe String -> Point -> String -> (String -> Msg) -> Drawing Msg
make_input key pos label onChange =
         Html.input ([ Html.Attributes.value label ,
                       Html.Events.onInput onChange,
                       Msg.onTabPreventDefault,
                       Html.Attributes.id HtmlDefs.idInput,
                       Html.Attributes.autofocus True,
                       Html.Attributes.style "width"
                           <| String.fromInt (String.length label + 1) ++ "ch"
                    ] ++ [class [HtmlDefs.renderedClass]] ++
                    [HtmlDefs.onRendered (always RenderedTextInput)]
                    ) []                                        
             |> Drawing.htmlAnchor key foregroundZ pos (100,16) True ""


activityToClasses : Activity -> List String
activityToClasses a =
   case a of
     MainActive -> ["active-label"] 
     WeakActive -> ["weak-active-label"]
     _ -> []

activityToEdgeClasses : Activity -> List String
activityToEdgeClasses a =
   case a of
     MainActive -> ["active-edge"] 
     WeakActive -> ["weak-active-edge"]
     _ -> []

nodeDrawing : Config -> Node NodeDrawingLabel -> Drawing Msg
nodeDrawing cfg node =
    let n = node.label in    
    let id = node.id in
    (
     if n.editable then
         make_input (idToKey id) n.inputPos n.label (NodeLabelEdit id)
     else
        let attrs = ([   MouseEvents.onClick (NodeClick id),
                    MouseEvents.onDoubleClick (EltDoubleClick id)
                    -- Html.Events.on "mousemove" (D.succeed (EltHover id))
                ] 
                ++ [HtmlDefs.renderedClass :: activityToClasses n.isActive |> class]
                ++ HtmlDefs.dimsAttribute n.dims
                ++
                [HtmlDefs.onRendered (Msg.NodeRendered id)]
                )
        in
        --  if n.label == "" then
            --  (Drawing.circle (Drawing.zindexAttr foregroundZ :: Drawing.color color :: attrs ) n.pos 5)
        --  else 
            case SpecialLabels.extractVerbatim n.label of 
              Just vLabel -> 
                    Drawing.makeVerbatim
                    {
                        zindex = n.zindex,
                        label = vLabel,
                        color = n.color,
                        -- preamble = cfg.latexPreamble,
                        pos = n.pos,
                        dims = n.dims,
                        angle = 0,
                        scale = 1,
                        key = idToKey id
                    } attrs
              Nothing -> 
                let label = (if n.isValidated then "\\color{green}" else "")
                        ++ (if n.label == "" then "\\bullet" else
                        -- let vLabel = verbatimLabel n.isVerbatim n.label in
                        if n.isMath then n.label else "\\text{" ++ n.label ++ "}" )
                in
                -- makeLatex cfg n.pos id n.dims label n.zindex
                Drawing.makeLatex 
                {
                    zindex = n.zindex,
                    label = SpecialLabels.removeAny label ,
                    color = n.color,
                    preamble = cfg.latexPreamble,
                    pos = n.pos,
                    dims = n.dims,
                    angle = 0,
                    scale = 1,
                    key = idToKey id
                } attrs
                
                

             {- Drawing.fromString 
             ([ Drawing.color color,
             Drawing.on "create" (newsizeDecoder id),
             Drawing.on "remove" (nosizeDecoder id),
             Drawing.class "lifecycle"
             ]
             ++ attrs)
             n.pos n.label  -}
             
        ) 

        


drawLabelAt : Config -> Graph.EdgeId -> Activity -> NormalEdgeDrawingLabel -> Point -> Float -> Drawing Msg
drawLabelAt cfg edgeId activity label labelpos angle =
    if label.editable then
         make_input (idToKey edgeId) labelpos label.label
         (EdgeLabelEdit edgeId)
    else
     if  label.label == "" then
         Drawing.empty
     else 
         let finalLabel = label.label in
         let attrs =    [   MouseEvents.onClick (EdgeClick edgeId),
                  MouseEvents.onDoubleClick (EltDoubleClick edgeId),
                  MouseEvents.onMove  (always (MouseOn edgeId))
                , HtmlDefs.renderedClass :: activityToClasses activity |> class                    
                , HtmlDefs.onRendered (Msg.EdgeRendered edgeId)]
                ++ HtmlDefs.dimsAttribute 
                (Point.resize (1 / GraphDefs.edgeScaleFactor) label.dims)
         in
         case SpecialLabels.extractVerbatim finalLabel of 
          Just vLabel -> 
                Drawing.makeVerbatim
                   {
                    color = label.style.labelColor,
                    zindex = foregroundZ,
                    label = vLabel,
                    pos = labelpos,
                    dims = label.dims,
                    angle = angle,
                    scale = GraphDefs.edgeScaleFactor,
                    key = idToKey edgeId
                } attrs
          Nothing ->
                Drawing.makeLatex 
                {
                    color = label.style.labelColor,
                    zindex = foregroundZ,
                    label = SpecialLabels.removeAny finalLabel,
                    preamble = cfg.latexPreamble,
                    pos = labelpos,
                    dims = label.dims,
                    angle = angle,
                    scale = GraphDefs.edgeScaleFactor,
                    key = idToKey edgeId
                } attrs

computeLabelPos : Curve -> NormalEdgeDrawingLabel -> MarkerStyle -> Float -> Point
computeLabelPos q label marker curve =
    case q of
        CurveBezier b -> 
          -- Quiver algorithm, following redraw_label
          -- https://github.com/varkor/quiver/blob/2c62d40b820cadc3c7f9d0816a33121f389b6240/src/arrow.js#L1219
          let offset = 
                if ArrowStyle.isDouble label.style then 2 * ArrowStyle.doubleSize else
                if ArrowStyle.isMarker marker then 5 else
                0
          in
          let edge_width = 2 + offset in
          let diffP = Point.subtract (Curve.to q) (Curve.from q) in
          let baseAngle = Point.pointToAngle diffP in
          let length = Point.radius diffP in
               Geometry.determine_label_position
                 length
                 baseAngle
                 edge_width 
                 0 -- start
                 1 -- end
                 (curve * length)
                 label.style.labelPosition
                 label.style.labelAlignment
                 (if label.editable then (2,2) else label.dims)
                 |> Point.rotate baseAngle
                 |> Point.add (Curve.from q)

        CurveArc loop -> 
            -- generated by copilot
            let t = label.style.labelPosition in
            -- Compute the true SVG arc center from p1, p2, and r.
            -- loop.center is NOT the SVG arc center (p1/p2 are not on that circle).
            -- The SVG arc "M p1 A r r 0 1 sweep p2" has its center determined by the
            -- W3C SVG arc spec formula (F.6.5.2 for rx=ry=r, x-rotation=0):
            --   dx = (x1-x2)/2,  dy = (y1-y2)/2
            --   sign = -1 if sweep==large-arc (both 1 when r>=0, so sign=-1 for r>=0)
            --   factor = sign * sqrt(max 0 ((r²-d²)/d²))
            --   cx = factor*dy + mx,  cy = -factor*dx + my
            -- p1 and p2 are guaranteed on this circle, so gap is exact to endpoints.
            let (x1, y1) = loop.from in
            let (x2, y2) = loop.to in
            let (mx, my) = Point.middle loop.from loop.to in

            let (hdx, hdy) = Point.subtract loop.from loop.to in 
            -- let hdy = (y1 - y2) / 2 in
            let d_sq = hdx * hdx + hdy * hdy in
            let r = abs loop.r in
            let signCenter = if loop.r >= 0 then -1 else 1 in
            let factor = signCenter * sqrt (max 0 ((r * r - d_sq) / d_sq)) in
            let svgCenter = (factor * hdy + mx, -factor * hdx + my) in

            let angle1 = Point.subtract loop.from svgCenter |> Point.pointToAngle in
            let angle2 = Point.subtract loop.to svgCenter |> Point.pointToAngle in
            let spanShort = Point.distanceAngleSigned angle1 angle2 in
            let span = if spanShort >= 0 then spanShort - 2 * pi else spanShort + 2 * pi in
            let theta = angle1 + t * span in
            let outward = (cos theta, sin theta) in
            let curvePos = Point.add svgCenter (Point.resize r outward) in

            -- label.dims is already the SVG-space size (rawDims * edgeScaleFactor, stored in
            -- Main.elm; the label renders with scale=edgeScaleFactor, so SVG size = label.dims).
            let (dw, dh) = if label.editable then (20, 20) else label.dims in
            let (ox, oy) = outward in
            -- Support function of the axis-aligned label box in the outward direction:
            -- ensures a constant 5px gap between the label edge and the arc regardless
            -- of the label's angular position around the loop.
            let halfExtent = (dw / 2) * abs ox + (dh / 2) * abs oy in
            -- before 0 was 5
            let pushDist = 5 + halfExtent in
            let labelpos = Point.add curvePos (Point.resize pushDist outward) in
            labelpos            

segmentLabel : Config -> Curve -> Graph.EdgeId -> Activity -> NormalEdgeDrawingLabel -> MarkerStyle -> Float -> Drawing Msg
segmentLabel cfg q edgeId activity label marker curve =
    let baseAngle = Point.pointToAngle <| Point.subtract (Curve.to q) (Curve.from q) in
    let labelpos = computeLabelPos q label marker curve in
       
    let angle =
         if label.style.labelAlignment == Geometry.Over then
          baseAngle
         else 0
    in
    drawLabelAt cfg edgeId activity label labelpos angle


{-
adjunctionArrow : Graph.EdgeId -> List String -> Int -> NormalEdgeDrawingLabel -> QuadraticBezier -> Drawing Msg
adjunctionArrow id classes z label q =
   let p = Bez.middle q in 
   let angle = Point.pointToAngle <| Point.subtract q.to q.from in
   let attrs = [ Html.Attributes.style "transform"
                   ("rotate(" ++ String.fromFloat angle ++ "rad)"),
                 MouseEvents.onClick (EdgeClick id),
                 MouseEvents.onDoubleClick (EltDoubleClick id)
                 -- Html.Events.on "mousemove" (D.succeed (EltHover id))
            ] ++ (List.map Html.Attributes.class classes)
   in
   makeLatex {latexPreamble = ""} 
        p (12,24) "⊢" z attrs
-}

idToKey : Graph.Id -> Maybe String
idToKey = String.fromInt >> Just

normalEdgeDrawing : Config -> Graph.EdgeId 
-- -> Geometry.PosDims -> Geometry.PosDims
     -> Activity 
     -> Int
     -> NormalEdgeDrawingLabel -> Curve -> Float -> Drawing Msg
normalEdgeDrawing cfg edgeId activity z {- from to -} label q curve =
    let style = ArrowStyle.getStyle label in
    let classes =  
         if label.isAdjunction then 
            activityToClasses activity 
         else
            activityToEdgeClasses activity 
    in
    -- let q = Geometry.segmentRectBent from to 
    --          label.style.bend
    -- in
    let attrs = (class classes ::
            [
          onClick (EdgeClick edgeId),
          onDoubleClick (EltDoubleClick edgeId),
          -- Drawing.onHover (EltHover edgeId),
           HtmlDefs.simpleOn "mousemove" (MouseOn edgeId)
          ] 
          )
    in
    -- if label.isAdjunction then
    --     adjunctionArrow edgeId classes z label q
    --     -- makeLatex {latexPreamble = ""} 
    --     --     pos dims label z attrs
    --     -- Drawing.adjunctionArrow attrs style q
    -- else
        Drawing.group [
         Drawing.arrow {zindex = z, style = style, curve = q}
          attrs,
          segmentLabel cfg q edgeId activity label style.marker curve,
          drawMarker style.color style.marker q]

-- loopEdgeDrawing : Config -> Graph.EdgeId 
--      -> Activity 
--      -> Int
--      -> NormalEdgeDrawingLabel -> EdgeShape.Loop -> Float -> Drawing Msg
-- loopEdgeDrawing cfg edgeId activity z label loop curve =
--     let style = ArrowStyle.getStyle label in
--     let classes =  
--          if label.isAdjunction then 
--             activityToClasses activity 
--          else
--             activityToEdgeClasses activity 
--     in
--     let attrs = (class classes ::
--             [
--           onClick (EdgeClick edgeId),
--           onDoubleClick (EltDoubleClick edgeId),
--            HtmlDefs.simpleOn "mousemove" (MouseOn edgeId)
--           ] 
--           )
--     in
--     let q = loop.q in
--     let t = label.style.labelPosition in
--     -- Compute the true SVG arc center from p1, p2, and r.
--     -- loop.center is NOT the SVG arc center (p1/p2 are not on that circle).
--     -- The SVG arc "M p1 A r r 0 1 sweep p2" has its center determined by the
--     -- W3C SVG arc spec formula (F.6.5.2 for rx=ry=r, x-rotation=0):
--     --   dx = (x1-x2)/2,  dy = (y1-y2)/2
--     --   sign = -1 if sweep==large-arc (both 1 when r>=0, so sign=-1 for r>=0)
--     --   factor = sign * sqrt(max 0 ((r²-d²)/d²))
--     --   cx = factor*dy + mx,  cy = -factor*dx + my
--     -- p1 and p2 are guaranteed on this circle, so gap is exact to endpoints.
--     let (x1, y1) = loop.p1 in
--     let (x2, y2) = loop.p2 in
--     let mx = (x1 + x2) / 2 in
--     let my = (y1 + y2) / 2 in
--     let hdx = (x1 - x2) / 2 in
--     let hdy = (y1 - y2) / 2 in
--     let d_sq = hdx * hdx + hdy * hdy in
--     let r = abs loop.r in
--     let signCenter = if loop.r >= 0 then -1 else 1 in
--     let factor = signCenter * sqrt (max 0 ((r * r - d_sq) / d_sq)) in
--     let svgCenter = (factor * hdy + mx, -factor * hdx + my) in

--     let angle1 = Point.subtract loop.p1 svgCenter |> Point.pointToAngle in
--     let angle2 = Point.subtract loop.p2 svgCenter |> Point.pointToAngle in
--     let spanShort = Point.distanceAngleSigned angle1 angle2 in
--     let span = if spanShort >= 0 then spanShort - 2 * pi else spanShort + 2 * pi in
--     let theta = angle1 + t * span in
--     let outward = (cos theta, sin theta) in
--     let curvePos = Point.add svgCenter (Point.resize r outward) in

--     -- label.dims is already the SVG-space size (rawDims * edgeScaleFactor, stored in
--     -- Main.elm; the label renders with scale=edgeScaleFactor, so SVG size = label.dims).
--     let (dw, dh) = if label.editable then (20, 20) else label.dims in
--     let (ox, oy) = outward in
--     -- Support function of the axis-aligned label box in the outward direction:
--     -- ensures a constant 5px gap between the label edge and the arc regardless
--     -- of the label's angular position around the loop.
--     let halfExtent = (dw / 2) * abs ox + (dh / 2) * abs oy in
--     -- before 0 was 5
--     let pushDist = 5 + halfExtent in
--     let labelpos = Point.add curvePos (Point.resize pushDist outward) in
--     Drawing.group [
--          Drawing.loopArrow {zindex = z, style = style, loop = loop}
--           attrs,
--           drawLabelAt cfg edgeId activity label labelpos 0,
--           drawMarker style.color style.marker q]

{- type alias DrawingDims msg =
    { drawing : Drawing msg
    , posDims : Geometry.PosDims    
    } -}
drawMarker : Color -> MarkerStyle -> Curve -> Drawing Msg
drawMarker color marker q =
   if marker == "" then Drawing.empty else drawStringMarker color marker q
  -- case marker of
  --   NoMarker -> Drawing.empty
  --   BulletMarker -> drawStringMarker color "\\bullet" q
  --   BarMarker -> drawStringMarker color "|" q

drawStringMarker : Color -> String -> Curve -> Drawing Msg
drawStringMarker color marker q =
    let pos = Curve.middle q in
    let angle = Point.pointToAngle <| Point.subtract (Curve.to q) (Curve.from q) in
             
             Drawing.makeLatex 
             {
                zindex = foregroundZ,
                label = marker,
                preamble = "",
                pos = pos,
                color = color,
                dims = (12,18),
                angle = angle,
                scale = GraphDefs.edgeScaleFactor,
                key = Nothing
             } 
             []   


type alias Extrem =
 { bez : QuadraticBezier,
   fromId : Id,
   toId : Id,
   fromPos : Point,
   toPos : Point}


onClick : (MouseEvents.Event -> a) -> Html.Attribute a --  String.Html.Attribute a
onClick = MouseEvents.onClick -- >> ghostAttribute



onDoubleClick : (MouseEvents.Event -> a) -> Html.Attribute a
onDoubleClick = MouseEvents.onDoubleClick -- >> ghostAttribute

-- Html.Attributes.class doesn't work
-- as it creates a property, not an attribute
-- and svg does not support the class attribute
class : List String -> Html.Attribute a
class = Html.Attributes.attribute "class" << String.join " "




drawHat : Graph.EdgeId -> Activity -> Int -> {color: Color.Color} -> Hat -> Drawing Msg
drawHat edgeId a z {color} hat =
    let classes = class <| activityToEdgeClasses a in
    Drawing.polyLine 
                {zindex = z, color = color
                , points = [hat.p1, hat.summit, hat.p2] }
               [classes, onClick (EdgeClick edgeId) 
                 ]

    --  Drawing.group 
    --  [mk_pbk [class <| (Drawing.shadowClass :: classes)], mk_pbk [class classes]] 

{-

Fin

-}

graphDrawing : Config -> Graph NodeDrawingLabel EdgeDrawingLabel -> Drawing Msg
graphDrawing cfg g0 =
     
      let drawEdge id e = 
             case (e.details, e.shape) of
               (PullshoutEdge pullshoutStyle, HatShape hat) -> 
                                drawHat id e.isActive e.zindex pullshoutStyle hat
                              
               (NormalEdge l, ArrowShape q) ->
                       if not cfg.showDependencies && l.isDependency 
                          && e.isActive /= MainActive 
                          && e.sourceIsActive /= MainActive
                          && not l.editable then
                           Drawing.empty                        
                       else normalEdgeDrawing cfg id e.isActive e.zindex l q l.style.bend
              --  (NormalEdge l, EdgeShape.LoopShape loop) ->
              --           loopEdgeDrawing cfg id e.isActive e.zindex l loop l.style.bend
                  
               _ -> Drawing.empty
               
      in
      let g = Graph.map     
              -- identity identity      
              (\id n -> nodeDrawing cfg (Node id n))
               drawEdge
              g0 
      in
      let nodes = Graph.nodes g |> List.map .label
          edges = Graph.edges g |> List.map .label
      in
      let
          drawings = nodes ++ edges
      in
          Drawing.group drawings
