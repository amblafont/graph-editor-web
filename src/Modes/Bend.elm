module Modes.Bend exposing (fixModel, computeFlags, update, graphDrawing, help, initialise, updateComponent, initialiseComponent, componentGetBend)

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
            let (result, newCompState) = updateComponent state.componentState msg in
            case result of
                NewState ->
                    let newState = { state | componentState = newCompState } in
                    noCmd <| setMode (BendMode newState) m
                NoChange ->
                    noCmd m
                Finalise ->
                    api.finalise m state
                -- ToggleHelp -> noCmd <| toggleHelpOverlay m
                Cancel ->
                    noCmd <| setMode DefaultMode m


initialise : Model -> Model
initialise model = initialise_with_state model Nothing

graphDrawing : Model -> BendState -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawing = api.graphDrawing



createModif : Graph NodeLabel EdgeLabel -> BendState -> Graph.ModifHelper NodeLabel EdgeLabel 
createModif modelGraph state =

            GraphDefs.updateStyleEdges
            (\ style -> Just <| { style | bend = componentGetBend state.componentState }) [state.edge] modelGraph


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
                    
                    let iniState = 
                            case mayState of
                                Nothing -> { bend = l.style.bend, origBend = l.style.bend }
                                Just s -> { bend = componentGetBend s.componentState, origBend = s.componentState.origBend}
                    in                   
                    -- if from == to then failedRet else
                    setMode (BendMode { edge = e,
                                componentState = 
                                initialiseComponent dir iniState                                    
                                }) model
                



{-

A component to handle bending edges.

It requires the Model.CmdFlag pointerLock to be true while active

-}




help : String
help = "Bend mode: " ++ Modes.Capture.help

componentGetBend : BendComponentState -> Float
componentGetBend state =
    let delta = state.captureState.value in
    let newBend = state.origBend + delta in
    let finalBend = toFloat (round (newBend * 10)) / 10 in
    finalBend

updateComponent : BendComponentState -> Msg -> (Modes.Capture.UpdateResult, BendComponentState )
updateComponent state msg =
    let (result, newState) = Modes.Capture.update state.captureState msg in
    (result, {state | captureState = newState } )


initialiseComponent : Point -> { bend : Float, origBend : Float } -> 
   BendComponentState
initialiseComponent dir0 ini =
    let dir = if dir0 == (0,0) then (1,0) else dir0 in
     { -- bend = ini.bend
       origBend = ini.origBend
      , captureState = 
         { value = ini.bend, 
    --   , origMouse = ini.origMouse
       direction = Point.orthoVectPx (0, 0) dir 0.01
      } }
    -- , if isFix then Cmd.none else pointerLock ()
    
