module Modes.Delfreehand exposing (update, graphDrawing, help, initialise)

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
import Modes exposing (DeleteFreeHandState)

initialise : Model -> Point -> Model
initialise model p =  setMode (DeleteFreeHandMode { ids = [ ] }) model

update : Modes.DeleteFreeHandState -> Msg -> Model -> (Model, Cmd Msg)
update state msg model =
    let finalise () = 
         let newModel = setMode DefaultMode model in
            case state.ids of
                [] -> noCmd newModel
                _ -> CommandCodec.updateModif newModel <| 
                    FreehandRemove newModel.graphInfo.activeTabId state.ids
    in
    case msg of
        MouseOnHandFree id -> 
                noCmd <| setMode (DeleteFreeHandMode { state | ids = id :: state.ids }) model
        KeyChanged False _ (Control "Escape") -> switch_Default model
        KeyChanged False _ (Character ',') -> finalise ()
        -- PenUp -> finalise ()
        _ -> noCmd model

graphDrawing : Model -> DeleteFreeHandState -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawing m s =
   
    collageGraphFromGraph m <| getActiveGraph m

help : String
help = "Delete freehand drawing mode. Move mouse upon drawing to remove. Release [,] to finalise, [ESC] to cancel."