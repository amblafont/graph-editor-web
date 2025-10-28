module Modes.Bend exposing (fixModel, computeFlags, update, graphDrawing, help, initialise, ComponentUpdateResult(..), updateComponent, initialiseComponent)

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
            let result = updateComponent state.componentState msg in
            case result of
                NewState newCompState ->
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
            (\ style -> Just <| { style | bend = state.componentState.bend }) [state.edge] modelGraph


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
                    let posGraph = GraphDefs.posGraph modelGraph in
                    let getPos id = Graph.get id .pos .pos posGraph in
                    let iniState = 
                            case mayState of
                                Nothing -> { bend = l.style.bend, origBend = l.style.bend }
                                Just s -> { bend = s.componentState.bend, origBend = s.componentState.origBend}
                    in
                    case (getPos e.from , getPos e.to) of
                        (Nothing, _) -> failedRet
                        (_, Nothing) -> failedRet
                        (Just from, Just to) ->
                            -- if from == to then failedRet else
                            setMode (BendMode { edge = e,
                                       componentState = 
                                       initialiseComponent from to iniState                                    
                                      }) model
                



{-

A component to handle bending edges.

It requires the Model.CmdFlag pointerLock to be true while active

-}




type ComponentUpdateResult = 
      NewState BendComponentState
    | Finalise
    | Cancel
    | NoChange
    -- | ToggleHelp


initialiseComponent : Point -> Point -> {a | bend : Float, origBend : Float } -> 
   BendComponentState
initialiseComponent from to ini =
    ( { bend = ini.bend
      , origBend = ini.origBend
    --   , origMouse = ini.origMouse
      , direction = Point.orthoVectPx from to 1
      }
    -- , if isFix then Cmd.none else pointerLock ()
    )

newBendComponent : BendComponentState -> Point -> Float
newBendComponent state mousePos =
            let delta = Point.scalarProduct state.direction mousePos in
            let newBend = state.origBend + delta / 100 in
            let finalBend = toFloat (round (newBend * 10)) / 10 in
            finalBend

updateComponent : BendComponentState -> Msg -> ComponentUpdateResult
updateComponent state msg =
    -- let final = (Finalise, HtmlDefs.pointerUnlock ()) in
    case msg of
        MouseLockedDelta pos -> NewState 
            { state | bend = newBendComponent state pos }
        KeyChanged False _ (Control "Escape") ->  Cancel
        MouseUnlock -> Cancel
        -- to be fixed but, currently can't capture any key when in pointer lock
        -- KeyChanged False _ (Character ' ') -> Finalise
        -- KeyChanged False _ (Control "Enter") -> Finalise
        MouseClick -> Finalise
        _ -> NoChange

help : String
help = "Bend mode: move the mouse to bend the edge. [Click] to confirm, [ESC] to cancel."
