module Modes.Customize exposing (computeFlags,initialise, fixModel, update, help, graphDrawing,
    initialiseComponent, updateComponent, componentGetShift, updateShift, helpShift)

import GraphDefs exposing (NodeLabel, EdgeLabel)
import GraphDrawing exposing (NodeDrawingLabel,EdgeDrawingLabel)
import Modes exposing (Mode(..), CustomizeModeState, ShiftComponentState, CustomizeModeShiftState)
import Model exposing (noCmd, Model, collageGraphFromGraph, getActiveGraph, toggleHelpOverlay, switch_Default, setMode)
import Polygraph as Graph exposing (Graph, Edge)
import Msg exposing (Msg(..))
import HtmlDefs exposing (Key(..))
import CommandCodec exposing (updateModifHelper)
import ArrowStyle exposing (EdgePart(..))
import Drawing.Color as Color exposing (Color)
import Modes.Lib
import Modes.Capture exposing (UpdateResult(..))
import Modes exposing (CustomizeMode(..))
import GraphProof exposing (statementToString)
import Modes exposing (CaptureState)
import Geometry.Point exposing (Point)



initialise : Model -> Model
initialise model =
     let modelGraph = getActiveGraph model in
     let edges = GraphDefs.selectedEdges modelGraph -- |> List.filter (.label >> GraphDefs.isNormal) 
     in
     if edges == [] then setMode DefaultMode model else 
    setMode (CustomizeMode { edges = edges, mode = CustomizeModePart MainEdgePart}) model 

fixModel : Model -> Model
fixModel = initialise

computeFlags : CustomizeModeState -> Model.CmdFlags
computeFlags s = 
    case s.mode of
        CustomizeModePart _ -> { pointerLock = False }
        CustomizeModeShift _ -> { pointerLock = True }

updateState : Model -> CustomizeModeState  -> Model
updateState m state = setMode (CustomizeMode state) m

-- finaliseModif : Model -> List (Edge EdgeLabel) -> Graph.ModifHelper NodeLabel EdgeLabel
-- finaliseModif model edges = 
--     let modelGraph = getActiveGraph model in
--     Graph.md_updateEdges edges modelGraph

createModif : Graph NodeLabel EdgeLabel -> CustomizeModeState  -> Graph.ModifHelper NodeLabel EdgeLabel
createModif modelGraph state =
    let edges = case state.mode of
            CustomizeModePart _ -> state.edges
            CustomizeModeShift shiftState ->
                updateEdgeShift state.edges shiftState
    in
    Graph.md_updateEdges edges modelGraph

api = Modes.Lib.makeApi createModif



updateEdgeColor : List (Edge EdgeLabel) -> EdgePart -> Color -> List (Edge EdgeLabel)
updateEdgeColor edges part color =
    -- case Color.fromChar c of 
        -- Nothing -> edges
    List.map (Graph.edgeMap (GraphDefs.setColor color part )) edges


updateEdgeShift : List (Edge EdgeLabel) -> CustomizeModeShiftState -> List (Edge EdgeLabel)
updateEdgeShift edges state =
    let applyStyle label =
            let newStyle = 
                    ArrowStyle.updateShift { isSource = state.isSource,
                     shift = componentGetShift state.componentState }
                      label.style
            in
            { label | style = newStyle}
    in
    List.map (Graph.edgeMap (GraphDefs.mapNormalEdge applyStyle)) edges


updateShift : CustomizeModeState -> CustomizeModeShiftState -> Msg -> Model -> (Model, Cmd Msg)
updateShift state shiftState msg model =
    let (result, newCompState) = updateComponent shiftState.componentState msg in
    case result of
        NewState ->
            let newState = 
                    { state | mode = 
                    CustomizeModeShift { shiftState | componentState = newCompState } } 
            in
            noCmd <| updateState model newState
        NoChange ->
            noCmd model
        Finalise ->
            api.finalise model state
        Cancel ->
            switch_Default model
            -- noCmd <| updateState model { state | mode = CustomizeModePart MainEdgePart }

initialiseShiftMode : Bool -> CustomizeModeState -> Model -> Model
initialiseShiftMode isSource state model =
    case state.edges of
        [] -> model
        edge :: _ ->
            let id = if isSource then edge.from else edge.to in
            let odir = GraphDefs.getEdgeDirectionFromId (getActiveGraph model) id in
            case odir of
                Nothing -> model
                Just dir ->
                    updateState model { state | mode = 
                    CustomizeModeShift { isSource = isSource,
                        componentState = initialiseComponent isSource dir {shift = 0.5} } }
    

-- applyShiftToEdges : List (Edge EdgeLabel) -> CustomizeModeShiftState -> List (Edge EdgeLabel)
-- applyShiftToEdges edges shiftState =

update : CustomizeModeState -> Msg -> Model -> (Model, Cmd Msg)
update state msg model = 
    case state.mode of
        CustomizeModePart part -> mainUpdate state part msg model
        CustomizeModeShift s -> updateShift state s msg model

mainUpdate : CustomizeModeState -> EdgePart -> Msg -> Model -> (Model, Cmd Msg)
mainUpdate state edgepart msg model =
    let checkColorable part =
           noCmd <| 
            -- if  state.edges |> List.filterMap GraphDefs.filterEdgeNormal 
            --     |> List.any (\e -> ArrowStyle.isPartColorable part e.label.details.style)
            -- then
                updateState model { state | mode = CustomizeModePart part }
            -- else
            --     model
                
    in
    case msg of
        KeyChanged False _ (Character '?') -> noCmd <| toggleHelpOverlay model
        KeyChanged False _ (Control "Escape") ->
            switch_Default model
        KeyChanged False _ (Character 'H') ->
             checkColorable HeadPart
        KeyChanged False _ (Character 'T') ->
             checkColorable TailPart
        KeyChanged False _ (Character 's') ->
            initialiseShiftMode True state model |> 
            noCmd
        KeyChanged False _ (Character 'e') ->
            initialiseShiftMode False state model |> 
            noCmd
        KeyChanged False _ (Character ' ') -> api.finalise model state
        KeyChanged False _ (Character c) ->
            case Color.fromChar c of
                Just color -> api.finalise model 
                            { state | edges = updateEdgeColor state.edges edgepart color }
                Nothing ->
                        noCmd model
              
              --  switch_Default <|
               
        
                --  Nothing -> noCmd model
                --  Just g -> switch_Default <| setSaveGraph model g
        _ -> noCmd model

help : CustomizeModeState -> String
help state = 
      "Mode customise. "
      ++ 
      case state.mode of
        CustomizeModeShift {isSource} ->
                helpShift isSource
        CustomizeModePart part ->      
            (case part of
                MainEdgePart -> "." 
                HeadPart -> " head."
                TailPart -> " tail."
            )
            ++ " Color [H]ead/[T]ail. "
            ++ shiftHelpMsg ++ ". "
                ++ HtmlDefs.overlayHelpMsg
                ++ "\n[ESC] or [SPC] or colorise selected edges: " ++ Color.helpMsg

shiftHelpMsg : String
shiftHelpMsg = "shift [s]ource/targ[e]t"


graphDrawing : Model -> CustomizeModeState -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawing m s = api.graphDrawing m s




{-

A component to shift source/target of edges between edges.

It requires the Model.CmdFlag pointerLock to be true while active

-}




helpShift : Bool -> String
helpShift source = "Shift " ++ (if source then "source" else "target") ++ 
        " mode: " ++ Modes.Capture.help

componentGetShift : ShiftComponentState -> Float
componentGetShift state =
    -- let delta = state.captureState.value in
    -- let newBend = state.origBend + delta in
    -- let finalBend = toFloat (round (newBend * 10)) / 10 in
    max state.value 0 |> min 1

updateComponent : ShiftComponentState -> Msg -> (Modes.Capture.UpdateResult, ShiftComponentState )
updateComponent state msg = Modes.Capture.update state msg
    -- let (result, newState) = Modes.Capture.update state.captureState msg in
    -- (result, {state | captureState = newState } )


initialiseComponent : Bool -> Point -> { shift : Float } -> 
   ShiftComponentState
initialiseComponent isSource dir0 ini =
    let dir = if dir0 == (0,0) then (10,0) else Geometry.Point.resize 0.0001 dir0 in
     { value = ini.shift, 
       direction = -- (if isSource then Geometry.Point.flip dir else dir)
                   dir, -- |> Geometry.Point.orthogonal,
         bounds = Just (0, 1)
      } 