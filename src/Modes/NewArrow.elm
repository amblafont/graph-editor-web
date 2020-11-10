module Modes.NewArrow exposing (graphDrawing, initialise, update)


import Color exposing (..)
import Graph exposing (..)
import GraphDrawing exposing (..)
import GraphExtra as Graph
import Maybe exposing (withDefault)
import Model exposing (..)
import Msg exposing (..)


initialise : Model -> ( Model, Cmd Msg )
initialise m =
    m.activeObj
        |> objToNode
        |> Maybe.map
            (\chosenNode ->
                { m
                    | mode = NewArrow

                            { step = NewArrowMoveNode, chosenNode = chosenNode }
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
        renamableNextStep step =
            renamableNextMode { model | mode = NewArrow { state | step = step } }
    in
    case state.step of
        NewArrowMoveNode ->
            if not validate then
                switch_Default model

            else
                let
                    info =
                        moveNodeInfo model state
                in
                noCmd
                    { model
                        | graph = info.graph
                        , mode =
                            NewArrow <|
                                { state
                                    | step =
                                        if info.created then
                                            NewArrowEditNode info.movedNode

                                        else
                                            NewArrowEditEdge info.movedNode
                                }
                    }

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
            noCmd model


moveNodeInfo :
    Model
    -> NewArrowState
    ->
        { graph : Graph NodeLabel EdgeLabel
        , movedNode : NodeId
        , created : Bool
        }
moveNodeInfo m state =
    let
        ( ( graph, movedNode ), created ) =
            mayCreateTargetNode m ""
    in
    { graph = Graph.addEdge graph ( state.chosenNode, movedNode ) ""
    , movedNode = movedNode
    , created = created
    }


makeGraph : Model -> NewArrowState -> Graph NodeLabel EdgeLabel
makeGraph m s =
    -- let defaultView movedNode = m.graph{ graph = m.graph, movedNode = movedNode}  in
    case s.step of
        NewArrowMoveNode ->
            (moveNodeInfo m s).graph

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

        NewArrowMoveNode ->
            ONothing


graphDrawing : Model -> NewArrowState -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawing m astate =
    let
        g =
            makeGraph m astate
    in
    collageGraphFromGraph m g
        |> graphMakeEditable (renamableFromState astate)
