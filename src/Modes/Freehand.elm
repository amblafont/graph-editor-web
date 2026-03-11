module Modes.Freehand exposing (update, graphDrawing, help, initialise)

import Modes exposing (Mode(..), FreeHandState)
import Model exposing (..)
import Msg exposing (Msg(..))
import HtmlDefs exposing (Key(..))
import CommandCodec exposing (updateModifHelper)
import Polygraph as Graph exposing (Graph)
import GraphDefs exposing (NodeLabel, EdgeLabel)
import Format.GraphInfo as GraphInfo exposing (Modif(..))
import GraphDrawing exposing (NodeDrawingLabel, EdgeDrawingLabel)
import Geometry.Point exposing (Point)

initialise : Model -> Point -> Model
initialise model p =  setMode (FreeHandMode { points = [ p ] }) model

update : Modes.FreeHandState -> Msg -> Model -> (Model, Cmd Msg)
update state msg model =
    let finalise () = 
         let newModel = setMode DefaultMode model in
            case state.points of
                [] -> noCmd newModel
                [_] -> noCmd newModel
                _ -> CommandCodec.updateModif newModel <| 
                    FreehandAdd newModel.graphInfo.activeTabId state.points
    in
    case msg of
        KeyChanged False _ (Control "Escape") -> switch_Default model
        MouseMove _ ->
            noCmd <| setMode (FreeHandMode { state | points = model.mousePos :: state.points }) model
        KeyChanged False _ (Character 'd') -> finalise ()
        PenUp -> finalise ()
        _ -> noCmd model

graphDrawing : Model -> FreeHandState -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawing m s =
   
    collageGraphFromGraph m <| getActiveGraph m

help : String
help = "FreeHand mode. Move mouse to draw. Release [d] to finalise, [ESC] to cancel."