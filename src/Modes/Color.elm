module Modes.Color exposing (initialise, fixModel, update, help)

import GraphDefs
import Modes exposing (Mode(..), ColorModeState)
import Model exposing (noCmd, Model, getActiveGraph, toggleHelpOverlay, switch_Default)
import Polygraph as Graph exposing (Graph)
import Msg exposing (Msg(..))
import HtmlDefs exposing (Key(..))
import CommandCodec exposing (updateModifHelper)
import ArrowStyle exposing (EdgePart(..))
import Drawing.Color as Color

initialise : Model -> Model
initialise model =
     let modelGraph = getActiveGraph model in
     let edges = GraphDefs.selectedEdges modelGraph -- |> List.filter (.label >> GraphDefs.isNormal) 
     in
     if edges == [] then { model | mode = DefaultMode } else 
    { model | mode = ColorMode { edges = edges, mode = MainEdgePart} } 

fixModel : Model -> Model
fixModel = initialise

updateState : Model -> ColorModeState  -> Model
updateState m state = {m | mode = ColorMode state}

update : ColorModeState -> Msg -> Model -> (Model, Cmd Msg)
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
        KeyChanged False _ (Character c) ->
               let modelGraph = getActiveGraph model in
               let modifHelper = 
                        Model.returnSetColor 
                        (Color.fromChar c)
                            model 
                            state.mode
                            state.edges
                            -- (Graph.getEdges ids modelGraph) 
               in
               updateModifHelper { model | mode = DefaultMode } modifHelper
                 
              --  switch_Default <|
               
        
                --  Nothing -> noCmd model
                --  Just g -> switch_Default <| setSaveGraph model g
        _ -> noCmd model

help : ColorModeState -> String
help state = 
      "Mode color"
      ++ 
        (case state.mode of
            MainEdgePart -> "."
            HeadPart -> " head."
            TailPart -> " tail."
        )
        ++ " Color [H]ead/[T]ail. "
            ++ HtmlDefs.overlayHelpMsg
            ++ "\n[ESC] or colorise selected edges: " ++ Color.helpMsg

