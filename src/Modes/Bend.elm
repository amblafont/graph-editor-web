module Modes.Bend exposing (fixModel, computeFlags, update, graphDrawing, help, initialise, captureValue, initialiseCapture)
 -- updateComponent,, componentGetBend) -- initialiseComponent

import Modes.Capture exposing (UpdateResult(..))
import GraphDefs exposing (NodeLabel, EdgeLabel)
import Modes exposing (Mode(..), BendState, BendComponentState)
import Model exposing (Model, noCmd, toggleHelpOverlay, getActiveGraph, setMode, collageGraphFromGraph)
import Msg exposing (Msg(..))
import Polygraph as Graph exposing (Graph, Edge, EdgeId)
import InputPosition exposing (InputPosition(..))
import GraphDrawing exposing (NodeDrawingLabel, EdgeDrawingLabel)
import Format.GraphInfo exposing (Modif(..))
import HtmlDefs exposing (Key(..))
import GraphDefs
import Geometry.Point exposing (Point)
import ArrowStyle exposing (ArrowStyle)
import CommandCodec exposing (updateModifHelper)
import Geometry.Point as Point
import Modes.Lib 
import Model exposing (switch_Default)

-- State for BendMode
-- edge: the edge being bent
-- origMouse: where the mouse was at the start
-- origBend: the original bend value
-- pos: current mouse position

computeFlags : BendState -> Model.CmdFlags
computeFlags _ = { pointerLock = True }

fixModel : Model -> BendState -> Model
fixModel m state = initialise_with_state m (Just state)



update : BendState -> Msg -> Model -> (Model, Cmd Msg)
update state msg m =
    let result = Modes.Capture.update  state.captureState msg in
    case result of
        Cancel -> switch_Default m
        NewState newCompState ->
            let newState = { state | captureState = newCompState } in
            noCmd <| setMode (BendMode newState) m
        NoChange ->
            case msg of 
                KeyChanged False _ (Control "Escape") -> switch_Default m
                KeyChanged False _ (Character ' ') -> api.finalise m state
                MouseClick -> api.finalise m state
                KeyChanged False _ (Character '?') -> noCmd <| toggleHelpOverlay m
                _ -> noCmd m


initialise : Model -> Model
initialise model = initialise_with_state model Nothing

graphDrawing : Model -> BendState -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawing = api.graphDrawing



createModif : Graph NodeLabel EdgeLabel -> BendState -> Graph.ModifHelper NodeLabel EdgeLabel 
createModif modelGraph state =

            GraphDefs.updateStyleEdges
            (\ style -> Just <| { style | bend = captureValue state.captureState }) [state.edge] modelGraph


api = Modes.Lib.makeApi createModif 


initialise_with_state : Model -> Maybe BendState -> Model
-- if mayBend is nothing, then we are initialising for the first time, otherwise, we are trying to fix it
initialise_with_state model mayState =
    let modelGraph = getActiveGraph model in
    let failedRet = setMode DefaultMode model in
    
    case GraphDefs.selectedEdge modelGraph of
        Nothing -> failedRet
        Just e ->
            case e.label.details of
                GraphDefs.PullshoutEdge _ -> failedRet
                GraphDefs.NormalEdge l ->
                    let dir = GraphDefs.getEdgeDirection modelGraph e 
                                |> Maybe.withDefault (1, 0)
                    in
                    
                    let iniState = initialiseCapture dir <| 
                            case mayState of
                                Nothing -> l.style.bend
                                Just s -> s.captureState.value
                    in                   
                    -- if from == to then failedRet else
                    setMode (BendMode { edge = e,
                                captureState = iniState
                                -- initialiseCapture dir iniState                                    
                                }) model
                



{-

A component to handle bending edges.

It requires the Model.CmdFlag pointerLock to be true while active

-}




help : String
help = "Bend mode: move mouse to bend, [Click] or [SPC] to confirm, [ESC] to cancel. " ++ HtmlDefs.overlayHelpMsg


captureValue : Modes.CaptureState -> Float
captureValue state =
    let newBend = state.value in
    -- let newBend = state.origBend + delta in
    let finalBend = toFloat (round (newBend * 10)) / 10 in
    finalBend

initialiseCapture : Point -> Float -> Modes.CaptureState
initialiseCapture dir0 iniBend =
      { value = iniBend, 
    --   , origMouse = ini.origMouse
       direction = Point.orthoVectPx (0, 0) dir0 0.01,
         bounds = Nothing
      }

-- initialiseComponent : Point -> { bend : Float, origBend : Float } -> 
--    BendComponentState
-- initialiseComponent dir0 ini =
--     let dir = if dir0 == (0,0) then (1,0) else dir0 in
--      { -- bend = ini.bend
--        origBend = ini.origBend
--       , captureState = 
--          { value = ini.bend, 
--     --   , origMouse = ini.origMouse
--        direction = Point.orthoVectPx (0, 0) dir 0.01,
--          bounds = Nothing
--       } }
