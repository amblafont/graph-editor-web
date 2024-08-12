module Modes.Color exposing (initialise, fixModel, update)

import GraphDefs
import Modes exposing (Mode(..))
import Model exposing (noCmd, Model, getActiveGraph, toggleHelpOverlay, switch_Default)
import Polygraph as Graph exposing (Graph)
import Msg exposing (Msg(..))
import HtmlDefs exposing (Key(..))
import CommandCodec exposing (updateModifHelper)
import ArrowStyle

initialise : Model -> Model
initialise model =
     let modelGraph = getActiveGraph model in
     let ids = GraphDefs.selectedEdges modelGraph |> List.filter (.label >> GraphDefs.isNormal) 
                 |> List.map .id
     in
     if ids == [] then { model | mode = DefaultMode } else 
    { model | mode = ColorMode ids} 

fixModel : Model -> Model
fixModel = initialise


update : List Graph.EdgeId -> Msg -> Model -> (Model, Cmd Msg)
update ids msg model =
    case msg of
        KeyChanged False _ (Character '?') -> noCmd <| toggleHelpOverlay model
        KeyChanged False _ (Control "Escape") ->
            switch_Default model
        KeyChanged False _ k ->
               let modelGraph = getActiveGraph model in
               let modifHelper = 
                        Model.returnUpdateStyle 
                        (ArrowStyle.keyMaybeUpdateColor k)
                            model 
                            (Graph.getEdges ids modelGraph) 
               in
               updateModifHelper { model | mode = DefaultMode } modifHelper
                 
              --  switch_Default <|
               
        
                --  Nothing -> noCmd model
                --  Just g -> switch_Default <| setSaveGraph model g
        _ -> noCmd model

