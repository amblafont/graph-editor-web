module Modes.Loop exposing (fixModel, update, graphDrawing, help, initialise, updateLoopStyle)

import Modes exposing (Mode(..), LoopState)
import Model exposing (Model, noCmd, toggleHelpOverlay, getActiveGraph, setMode, collageGraphFromGraph)
import Msg exposing (Msg(..))
import Polygraph as Graph exposing (Graph, Edge, EdgeId)
import GraphDrawing exposing (NodeDrawingLabel, EdgeDrawingLabel)
import Format.GraphInfo exposing (Modif(..))
import HtmlDefs exposing (Key(..))
import GraphDefs exposing (NodeLabel, EdgeLabel)
import Geometry.Point exposing (Point)
import Geometry.Point as Point
import Modes.Lib 
import Model exposing (switch_Default)
import ArrowStyle

-- computeFlags : LoopState -> Model.CmdFlags
-- computeFlags _ = { pointerLock = False }

fixModel : Model -> LoopState -> Model
fixModel m state =
   initialise_with_state m (Just state)
   |> Maybe.withDefault 
        (setMode DefaultMode m)

update : LoopState -> Msg -> Model -> (Model, Cmd Msg)
update state msg m =
    case msg of 
        -- MouseMove p -> noCmd <| setMode (LoopMode { state | mousePos = p }) m
        KeyChanged False _ (Control "Escape") -> switch_Default m
        KeyChanged False _ (Character ' ') -> api.finalise m state
        MouseClick -> api.finalise m state
        KeyChanged False _ (Character '?') -> noCmd <| toggleHelpOverlay m
        _ -> noCmd m

initialise : Model -> Maybe Model
initialise model = initialise_with_state model Nothing

graphDrawing : Model -> LoopState -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawing = api.graphDrawing

updateLoopStyle : Model -> Point -> ArrowStyle.ArrowStyle -> ArrowStyle.ArrowStyle
updateLoopStyle model pos oldStyle =
    let diff = Point.subtract model.mousePos pos in
    let angle = Point.pointToAngle diff in
    let radius = Point.distance (0,0) diff / 2 in
    { oldStyle | loopAngle = angle, loopRadius = radius }

createModif : Model -> LoopState -> Graph.ModifHelper NodeLabel EdgeLabel 
createModif model state =
    let modelGraph = getActiveGraph model in
    -- GraphDefs.updateStyleEdges (updateLoopStyle model state.initialPos >> Just) 
    --          [state.id] modelGraph
    GraphDefs.md_updateNormalEdge state.id
          ( \ l -> { l | style = updateLoopStyle model state.initialPos l.style })
          (Graph.newModif modelGraph)
  

api = Modes.Lib.makeModelApi createModif

initialise_with_state : Model -> Maybe LoopState -> Maybe Model
initialise_with_state model mayState =
    let modelGraph = getActiveGraph model in
    let failedRet = Nothing in
    
    case GraphDefs.selectedEdge modelGraph of
        Nothing -> failedRet
        Just e ->
            if e.from /= e.to then failedRet else
            case e.label.details of
                GraphDefs.PullshoutEdge _ -> failedRet
                GraphDefs.NormalEdge l ->
                    let posGraph = GraphDefs.posGraph modelGraph in
                    case Graph.get e.from .pos .pos posGraph of
                        Nothing -> failedRet
                        Just pos -> 
                            Just (setMode (LoopMode { id = e.id, initialPos = pos }) model)

help : String
help = "Loop mode: move mouse to adjust angle and radius, [Click] or [SPC] to confirm, [ESC] to cancel. " ++ HtmlDefs.overlayHelpMsg
