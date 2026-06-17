module Modes.NodeColor exposing (initialise, update, help)

import GraphDefs exposing (NodeLabel, EdgeLabel)
import Polygraph as Graph exposing (Graph)
import Model exposing (noCmd, Model, getActiveGraph, setMode)
import Msg exposing (Msg(..))
import HtmlDefs exposing (Key(..))
import Modes exposing (Mode(..))
import Drawing.Color as Color
import CommandCodec exposing (updateModifHelper)


initialise : Model -> Model
initialise model =
    let modelGraph = getActiveGraph model in
    if GraphDefs.selectedNodes modelGraph == [] then
        setMode DefaultMode model
    else
        setMode NodeColorMode model


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        KeyChanged False _ (Character c) ->
            case Color.fromChar c of
                Just color ->
                    let modelGraph = getActiveGraph model in
                    let nodes = GraphDefs.selectedNodes modelGraph in
                    let setColor n = let l = n.label in { n | label = { l | color = color } } in
                    let newNodes = List.map setColor nodes in
                    updateModifHelper (setMode DefaultMode model)
                        <| Graph.md_updateNodes newNodes <| Graph.newModif modelGraph
                Nothing ->
                    noCmd model
        KeyChanged False _ (Control "Escape") ->
            noCmd <| setMode DefaultMode model
        _ -> noCmd model


help : String
help =
    "Node color mode. Press a color key to color selected nodes and return to default mode. "
        ++ Color.helpMsg
        ++ " [ESC] to cancel."
