module Modes.Bend exposing (fixModel, update, graphDrawing, help, initialise)

import GraphDefs exposing (NodeLabel, EdgeLabel)
import Modes exposing (Mode(..), BendState)
import Model exposing (Model, noCmd, getActiveGraph, setMode, collageGraphFromGraph)
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


fixModel : Model -> BendState -> Model
fixModel m _ = initialise m

help : String
help = "Bend mode: move the mouse to bend the selected edge. [SPC] or mouse click to confirm, [ESC] to cancel."



initialise : Model -> Model
initialise model =
    let modelGraph = getActiveGraph model in
    let failedRet = setMode DefaultMode model in
    
    case GraphDefs.selectedEdge modelGraph of
        Nothing -> failedRet
        Just e ->
            case e.label.details of
                GraphDefs.NormalEdge l ->
                    let posGraph = GraphDefs.posGraph modelGraph in
                    let getPos id = Graph.get id .pos .pos posGraph in
                    case (getPos e.from , getPos e.to) of
                        (Nothing, _) -> failedRet
                        (_, Nothing) -> failedRet
                        (Just from, Just to) ->
                            if from == to then failedRet else
                            setMode (BendMode { edge = e
                                      , origMouse = model.mousePos
                                      , origBend = l.style.bend
                                      , direction = 
                                       Point.subtract to from |> Point.normalise 1
                                       |> Point.orthogonal 
                                      }) model
                _ -> failedRet

update : BendState -> Msg -> Model -> (Model, Cmd Msg)
update state msg m =
    -- let modelGraph = getActiveGraph m in
    -- let edgeId = state.edgeId in
    -- let origMouse = state.origMouse in
    -- let origBend = state.origBend in
    
    case msg of
        -- MouseMove (x, y) ->
        --     let newGraph = updateBend (x, y) in
        --     (Model.setActiveGraph m newGraph, Cmd.none)
        KeyChanged False _ (Control "Escape") -> (setMode DefaultMode m, Cmd.none)
        KeyChanged False _ (Character ' ') -> api.finalise m state
        MouseClick -> api.finalise m state
        _ -> noCmd m

graphDrawing : Model -> BendState -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawing = api.graphDrawing

updateBend : BendState -> Point -> ArrowStyle -> ArrowStyle
updateBend state mousePos style =
            let delta = Point.scalarProduct state.direction
                       <| Point.subtract mousePos state.origMouse
            in
            let newBend = style.bend + delta / 100 in
            let finalBend = toFloat (round (newBend * 10)) / 10 in
            { style | bend = finalBend}

createModif : Graph NodeLabel EdgeLabel -> Model -> BendState -> Graph.ModifHelper NodeLabel EdgeLabel 
createModif modelGraph model state =

            GraphDefs.updateStyleEdges
            (\ style -> Just <| updateBend state model.mousePos style) [state.edge] modelGraph


api = Modes.Lib.makeApi createModif 