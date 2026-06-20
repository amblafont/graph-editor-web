module Modes.Customize exposing (computeFlags,initialise, fixModel, update, help, graphDrawing,
    initialiseShiftComponent, initialiseShortenComponent)

import GraphDefs exposing (NodeLabel, EdgeLabel)
import GraphDrawing exposing (NodeDrawingLabel,EdgeDrawingLabel)
import Modes exposing (Mode(..), CustomizeModeState, CustomizeModeShiftState, CustomizeModeShortenState)
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
        CustomizeModeShorten _ -> { pointerLock = True }

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
            CustomizeModeShorten shortenState ->
                updateEdgeShorten state.edges shortenState
    in
    Graph.md_updateEdges edges modelGraph

api = Modes.Lib.makeApi createModif



updateEdgeColor : Bool -> List (Edge EdgeLabel) -> EdgePart -> Color -> List (Edge EdgeLabel)
updateEdgeColor updateLabels edges part color =
    List.map (Graph.edgeMap (GraphDefs.setColor updateLabels color part )) edges

updateEdgeStyle : List (Edge EdgeLabel) -> (ArrowStyle.ArrowStyle -> ArrowStyle.ArrowStyle) -> List (Edge EdgeLabel)
updateEdgeStyle edges f =
  let applyStyle label =
            { label | style = f label.style}
  in
  List.map (Graph.edgeMap (GraphDefs.mapNormalEdge applyStyle)) edges


updateEdgeShift : List (Edge EdgeLabel) -> CustomizeModeShiftState -> List (Edge EdgeLabel)
updateEdgeShift edges state =
    updateEdgeStyle edges (ArrowStyle.updateShift { part = state.part,
                     shift = state.componentState.value })


updateEdgeShorten : List (Edge EdgeLabel) -> CustomizeModeShortenState -> List (Edge EdgeLabel)
updateEdgeShorten edges state =
    updateEdgeStyle edges (ArrowStyle.updateShorten { part = state.part,
                     shorten = state.componentState.value })


updateShift : CustomizeModeState -> CustomizeModeShiftState -> Msg -> Model -> (Model, Cmd Msg)
updateShift state shiftState msg model =
    case Modes.Capture.update shiftState.componentState msg of
        Cancel -> switch_Default model
        NewState newCompState ->
            let newState = 
                    { state | mode = 
                    CustomizeModeShift { shiftState | componentState = newCompState } } 
            in
            noCmd <| updateState model newState
        NoChange ->
            case msg of 
                KeyChanged False _ (Control "Escape") -> switch_Default model
                KeyChanged False _ (Character ' ') -> api.finalise model state
                MouseClick -> api.finalise model state
                KeyChanged False _ (Character '?') -> noCmd <| toggleHelpOverlay model
                _ -> noCmd model


updateShorten : CustomizeModeState -> CustomizeModeShiftState -> Msg -> Model -> (Model, Cmd Msg)
updateShorten state shortenState msg model =
    case Modes.Capture.update shortenState.componentState msg of
        Cancel -> switch_Default model
        NewState newCompState ->
            let newState =
                    { state | mode =
                    CustomizeModeShorten { shortenState | componentState = newCompState } }
            in
            noCmd <| updateState model newState
        NoChange ->
            case msg of
                KeyChanged False _ (Control "Escape") -> switch_Default model
                KeyChanged False _ (Character ' ') -> api.finalise model state
                MouseClick -> api.finalise model state
                KeyChanged False _ (Character '?') -> noCmd <| toggleHelpOverlay model
                _ -> noCmd model

-- getTopNormalEdge : CustomizeModeState ->  Maybe (Edge (GraphDefs.GenericEdge GraphDefs.NormalEdgeLabel)) 
-- getTopNormalEdge state = 
--      case state.edges of
--         [] -> Nothing
--         edge :: _ -> GraphDefs.filterEdgeNormal edge of
--                 Nothing -> model

initialiseShiftMode : ArrowStyle.ExtremePart -> CustomizeModeState -> Model -> Model
initialiseShiftMode part state model =
    case state.edges of
        [] -> model
        edge :: _ ->
            let id = if part == ArrowStyle.Tail then edge.from else edge.to in
            let odir = GraphDefs.getEdgeDirectionFromId (getActiveGraph model) id in
            case odir of
                Nothing -> model
                Just dir ->
                    updateState model { state | mode = 
                    CustomizeModeShift { part = part,
                        componentState = initialiseShiftComponent part dir {shift = 0.5} } }
    

initialiseShortenMode : ArrowStyle.ExtremePart -> CustomizeModeState -> Model -> Model
initialiseShortenMode part state model =
    case state.edges of
        [] -> model
        edge :: _ ->
            case GraphDefs.filterEdgeNormal edge of
                Nothing -> model
                Just normalEdge ->
                    let initVal = if part == ArrowStyle.Tail then normalEdge.label.details.style.shortenTail else normalEdge.label.details.style.shortenHead in
                    let odir = GraphDefs.getEdgeDirection (getActiveGraph model) edge in
                    case odir of
                        Nothing -> model
                        Just dir ->
                            let comp = initialiseShortenComponent part dir {shorten = initVal }  in
                            updateState model { state | mode =
                                CustomizeModeShorten { part = part,
                                componentState = comp } }
    

-- applyShiftToEdges : List (Edge EdgeLabel) -> CustomizeModeShiftState -> List (Edge EdgeLabel)
-- applyShiftToEdges edges shiftState =

update : CustomizeModeState -> Msg -> Model -> (Model, Cmd Msg)
update state msg model = 
    case state.mode of
        CustomizeModePart part -> mainUpdate state part msg model
        CustomizeModeShift s -> updateShift state s msg model
        CustomizeModeShorten s -> updateShorten state s msg model

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
            initialiseShiftMode ArrowStyle.Tail state model |> 
            noCmd
        KeyChanged False _ (Character 'e') ->
            initialiseShiftMode ArrowStyle.Head state model |> 
            noCmd
        KeyChanged False _ (Character 'S') ->
            initialiseShortenMode ArrowStyle.Tail state model |>
            noCmd
        KeyChanged False _ (Character 'E') ->
            initialiseShortenMode ArrowStyle.Head state model |>
            noCmd
        KeyChanged False _ (Character ' ') -> api.finalise model state
        KeyChanged False _ (Character c) ->
            case Color.fromChar c of
                Just color -> api.finalise model 
                            { state | edges = updateEdgeColor model.labelColorUpdateEnabled state.edges edgepart color }
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
        CustomizeModeShift {part} ->
                helpShift part
        CustomizeModeShorten {part} ->
                helpShorten part
        CustomizeModePart part ->      
            (case part of
                MainEdgePart -> "." 
                HeadPart -> " head."
                TailPart -> " tail."
            )
            ++ " Color [H]ead/[T]ail. "
            ++ shiftHelpMsg ++ ". "
            ++ shortenHelpMsg ++ ". "
                ++ HtmlDefs.overlayHelpMsg
                ++ "\n[ESC] or [SPC] or colorise selected edges: " ++ Color.helpMsg

shiftHelpMsg : String
shiftHelpMsg = "shift [s]ource/targ[e]t"

shortenHelpMsg : String
shortenHelpMsg = "shorten [S]ource/targ[E]t"


graphDrawing : Model -> CustomizeModeState -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawing m s = api.graphDrawing m s




{-

A component to shift source/target of edges between edges.

It requires the Model.CmdFlag pointerLock to be true while active

-}


helpCapture = " use mouse, [ESC] to cancel, [SPC] or [click] to confirm. "

helpShift : ArrowStyle.ExtremePart -> String
helpShift part = "Shift " ++ (if part == ArrowStyle.Tail then "source" else "target") ++ 
        helpCapture ++ HtmlDefs.overlayHelpMsg


helpShorten : ArrowStyle.ExtremePart -> String
helpShorten part = "Shorten " ++ (if part == ArrowStyle.Tail then "source" else "target") ++
        helpCapture ++ HtmlDefs.overlayHelpMsg


initialiseShiftComponent : ArrowStyle.ExtremePart -> Point -> { shift : Float } -> 
   CaptureState
initialiseShiftComponent part dir0 ini =
    let dir = if dir0 == (0,0) then (10,0) else Geometry.Point.resize 0.0001 dir0 in
     { value = ini.shift, 
       direction = -- (if isSource then Geometry.Point.flip dir else dir)
                   dir, -- |> Geometry.Point.orthogonal,
         bounds = Just (0, 1)
      } 

initialiseShortenComponent : ArrowStyle.ExtremePart -> Point -> { shorten : Float } -> 
   CaptureState
initialiseShortenComponent part dir initVal =
    let dir2 = if dir == (0,0) then (10,0) else Geometry.Point.resize 0.001 dir in
    let dir3 = if part == ArrowStyle.Head then Geometry.Point.flip dir2 else dir2 in
     { value = initVal.shorten, direction = dir3, bounds = Nothing } 