module Modes.Freehand exposing (update, graphDrawing, freehandDrawings, help, initialise, initialiseTemporary)

import Modes exposing (Mode(..), FreeHandState(..))
import Model exposing (..)
import Msg exposing (Msg(..))
import HtmlDefs exposing (Key(..))
import Html.Attributes
import CommandCodec exposing (updateModifHelper)
import Polygraph as Graph exposing (Graph)
import GraphDefs exposing (NodeLabel, EdgeLabel)
import Format.GraphInfo as GraphInfo exposing (Modif(..))
import GraphDrawing exposing (NodeDrawingLabel, EdgeDrawingLabel)
import Geometry.Point exposing (Point)
import Drawing exposing (Drawing)
import FreeHandDrawings

setState : Model -> FreeHandState -> Model
setState model state = setMode (FreeHandMode state) model

initialise : Model -> Model
initialise model =  setDefault model  

initialiseTemporary : Model -> Model
initialiseTemporary model = switchDown model True

switchDown : Model -> Bool -> Model
switchDown model temporary = 
    setState model  (DownFreeHandState {temporary = temporary, points = [ model.mousePos ] }) 
    

isDelete : Model -> Bool
isDelete model = model.specialKeys.alt

setDefault : Model -> Model
setDefault model = setState model DefaultFreeHandState

update_defaultState : Msg -> Model -> (Model, Cmd Msg)
update_defaultState msg model =
    case msg of
        MouseDown _ -> noCmd <| switchDown model False
        PenDown   _ -> noCmd <| switchDown model False
        MouseOnHandFree id -> 
            if not (isDelete model) then noCmd model else
            CommandCodec.updateModif model <|
                FreehandRemove model.graphInfo.activeTabId id
        KeyChanged False _ (Control "Escape") -> switch_Default model
        -- KeyChanged False _ (Character 'd') -> switch_Default model
        KeyChanged False _ (Character ' ') -> switch_Default model
        _ -> noCmd model


update_downFreeHandState : { temporary : Bool, points : List Point } -> Msg -> Model -> (Model, Cmd Msg)
update_downFreeHandState state msg model =
    let nextModel () =   if state.temporary then 
                            setMode DefaultMode model 
                         else
                            setDefault model
    in
    let finalise () = 
            let newModel = nextModel () in
                       
                case state.points of
                    [] -> noCmd newModel
                    [_] -> noCmd newModel
                    _ -> CommandCodec.updateModif newModel <| 
                        FreehandAdd newModel.graphInfo.activeTabId state.points
    in
    case msg of
        -- Can we merge them?
        MouseUp -> finalise ()
        PenUp -> finalise ()
        KeyChanged False _ (Control "Escape") -> 
             noCmd <| nextModel ()
        MouseMove _ ->
            -- if isDelete model then noCmd model else
            noCmd <| setState model (DownFreeHandState { state | points = model.mousePos :: state.points })
        _ -> noCmd model

update : Modes.FreeHandState -> Msg -> Model -> (Model, Cmd Msg)
update state msg model =
    case state of 
        DefaultFreeHandState -> update_defaultState msg model
        DownFreeHandState st -> update_downFreeHandState st msg model




graphDrawing : Model -> FreeHandState -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawing m s =
   
    collageGraphFromGraph m <| getActiveGraph m

freehandDrawings : Model -> FreeHandState -> Drawing Msg
freehandDrawings m state =
    case state of 
        DefaultFreeHandState ->
            let attrs = 
                  if isDelete m then 
                    \ id ->
                    [HtmlDefs.simpleOn "mousemove" (MouseOnHandFree id),
                     Html.Attributes.style "stroke-width" "10px"]
                  else \ _ -> []
            in
            let drawings = getActiveTab m |> .freehandDrawings in
            FreeHandDrawings.draw attrs drawings
        DownFreeHandState st ->
            FreeHandDrawings.draw (always [])
             <| FreeHandDrawings.add (getActiveTab m |> .freehandDrawings) st.points


help : FreeHandState -> String
help state = "FreeHand mode. Drag the mouse to draw, hold [ALT] to delete freehand drawing by hovering them, [ESC], or [SPC] to return to the default mode."