module Modes.NewArrow exposing (graphDrawing, initialise, update)


import Color exposing (..)
import Graph exposing (..)
import GraphDrawing exposing (..)
import GraphExtra as Graph
import Maybe exposing (withDefault)
import Model exposing (..)
import Msg exposing (..)

import ArrowStyle

updateStep : Model -> NewArrowState -> NewArrowStep -> Model
updateStep m state step = {m | mode = NewArrow { state | step = step }}


initialise : Model -> ( Model, Cmd Msg )
initialise m =
    m.activeObj
        |> objToNode
        |> Maybe.map
            (\chosenNode ->
                { m
                    | mode = NewArrow

                            { step = NewArrowMoveNode ArrowStyle.empty, chosenNode = chosenNode }
                             -- prevent bugs (if the mouse is thought
                             -- to be kept on a point)
                      , mousePointOver = ONothing
                }



                 
            )
        |> Maybe.withDefault m
        |> noCmd




nextStep : Model -> Bool -> NewArrowState -> ( Model, Cmd Msg )
nextStep model validate state =
    let
        renamableNextMode m =
            let
                graph =
                    if validate then
                        m.graph

                    else
                        graphRenameObj m.graph (renamableFromState state) ""
            in
            noCmd { m | graph = graph }
    in
    let
        renamableNextStep step = updateStep model state step
            |> renamableNextMode            
    in
    case state.step of
        NewArrowMoveNode style ->
            if not validate then
                switch_Default model

            else
                let
                    info = moveNodeInfo model state style
                    step = if info.created then
                             NewArrowEditNode info.movedNode
                           else
                              NewArrowEditEdge info.movedNode
                in
                updateStep { model | graph = info.graph } state step
                |> noCmd

                
        NewArrowEditNode movedNode ->
            renamableNextStep <| NewArrowEditEdge movedNode

        NewArrowEditEdge movedNode ->
            renamableNextMode
                { model
                    | activeObj = ONode movedNode
                    , mode = DefaultMode
                }


update : NewArrowState -> Msg -> Model -> ( Model, Cmd Msg )
update state msg model =
    case msg of
        KeyChanged False (Control "Escape") ->
            nextStep model False state

        KeyChanged False (Control "Enter") ->
            nextStep model True state

        MouseClick ->
            nextStep model True state

        EdgeLabelEdit e s ->
            noCmd { model | graph = graphRenameObj model.graph (OEdge e) s }

        NodeLabelEdit n s ->
            noCmd { model | graph = graphRenameObj model.graph (ONode n) s }

        
            
        _ ->
            case state.step of
                NewArrowMoveNode style ->
                    style 
                     |> msgUpdateArrowStyle msg           
                     |> NewArrowMoveNode
                     |> updateStep model state 
                     |> noCmd
                  
                _ -> noCmd model
            


moveNodeInfo :
    Model
    -> NewArrowState
    -> ArrowStyle.Style
    ->
        { graph : Graph NodeLabel EdgeLabel
        , movedNode : NodeId
        , created : Bool
        }
moveNodeInfo m state style =
    let
        ( ( graph, movedNode ), created ) =
            mayCreateTargetNode m ""
    in
    { graph = Graph.addEdge graph ( state.chosenNode, movedNode ) 
       {label = "", style = style }
    , movedNode = movedNode
    , created = created
    }


makeGraph : Model -> NewArrowState -> Graph NodeLabel EdgeLabel
makeGraph m s =
    -- let defaultView movedNode = m.graph{ graph = m.graph, movedNode = movedNode}  in
    case s.step of
        NewArrowMoveNode style ->
            (moveNodeInfo m s style).graph

        NewArrowEditNode _ ->
            m.graph

        NewArrowEditEdge _ ->
            m.graph


renamableFromState : NewArrowState -> Obj
renamableFromState state =
    case state.step of
        NewArrowEditNode m ->
            ONode m

        NewArrowEditEdge m ->
            OEdge ( state.chosenNode, m )

        NewArrowMoveNode _ ->
            ONothing


graphDrawing : Model -> NewArrowState -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawing m astate =
    let
        g =
            makeGraph m astate
    in
    collageGraphFromGraph m g
        |> graphMakeEditable (renamableFromState astate)
