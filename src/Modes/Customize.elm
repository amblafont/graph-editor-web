module Modes.Customize exposing (initialise, fixModel, update, help, graphDrawing)

import GraphDefs exposing (NodeLabel, EdgeLabel)
import GraphDrawing exposing (NodeDrawingLabel,EdgeDrawingLabel)
import Modes exposing (Mode(..), CustomizeModeState)
import Model exposing (noCmd, Model, collageGraphFromGraph, getActiveGraph, toggleHelpOverlay, switch_Default, setMode)
import Polygraph as Graph exposing (Graph, Edge)
import Msg exposing (Msg(..))
import HtmlDefs exposing (Key(..))
import CommandCodec exposing (updateModifHelper)
import ArrowStyle exposing (EdgePart(..))
import Drawing.Color as Color exposing (Color)
import Modes.Lib

initialise : Model -> Model
initialise model =
     let modelGraph = getActiveGraph model in
     let edges = GraphDefs.selectedEdges modelGraph -- |> List.filter (.label >> GraphDefs.isNormal) 
     in
     if edges == [] then setMode DefaultMode model else 
    setMode (CustomizeMode { edges = edges, mode = MainEdgePart}) model 

fixModel : Model -> Model
fixModel = initialise

updateState : Model -> CustomizeModeState  -> Model
updateState m state = setMode (CustomizeMode state) m

-- finaliseModif : Model -> List (Edge EdgeLabel) -> Graph.ModifHelper NodeLabel EdgeLabel
-- finaliseModif model edges = 
--     let modelGraph = getActiveGraph model in
--     Graph.md_updateEdges edges modelGraph

api = Modes.Lib.makeApi (\modelGraph _ edges -> Graph.md_updateEdges edges modelGraph)


updateEdgeShiftBend : List (Edge EdgeLabel) -> Char -> List (Edge EdgeLabel)
updateEdgeShiftBend edges c =
    let applyStyle label =
                    { label | 
                    style = 
                    ArrowStyle.keyUpdateShiftBend (Character c) label.style
                    |> Maybe.withDefault label.style
                    }
    in
    List.map (Graph.edgeMap (GraphDefs.mapNormalEdge applyStyle)) edges

updateEdgeColor : List (Edge EdgeLabel) -> EdgePart -> Color -> List (Edge EdgeLabel)
updateEdgeColor edges part color =
    -- case Color.fromChar c of 
        -- Nothing -> edges
    List.map (Graph.edgeMap (GraphDefs.setColor color part )) edges

update : CustomizeModeState -> Msg -> Model -> (Model, Cmd Msg)
update state msg model =
    let checkColorable part =
           noCmd <| 
            -- if  state.edges |> List.filterMap GraphDefs.filterEdgeNormal 
            --     |> List.any (\e -> ArrowStyle.isPartColorable part e.label.details.style)
            -- then
                updateState model { state | mode = part }
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
        KeyChanged False _ (Character ' ') -> api.finalise model state.edges
        KeyChanged False _ (Character c) ->
            case Color.fromChar c of
                Just color -> api.finalise model 
                            <| updateEdgeColor state.edges state.mode color
                Nothing ->
                        let newEdges = updateEdgeShiftBend state.edges c in
                        noCmd <| updateState model
                             { state | 
                                edges = newEdges
                             }
              
              --  switch_Default <|
               
        
                --  Nothing -> noCmd model
                --  Just g -> switch_Default <| setSaveGraph model g
        _ -> noCmd model

help : CustomizeModeState -> String
help state = 
      "Mode customise"
      ++ 
        (case state.mode of
            MainEdgePart -> "." 
            HeadPart -> " head."
            TailPart -> " tail."
        )
        ++ " Color [H]ead/[T]ail. "
        ++ ArrowStyle.shiftHelpMsg ++ ". "
            ++ HtmlDefs.overlayHelpMsg
            ++ "\n[ESC] or [SPC] or colorise selected edges: " ++ Color.helpMsg


graphDrawing : Model -> CustomizeModeState -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawing m s = api.graphDrawing m s.edges