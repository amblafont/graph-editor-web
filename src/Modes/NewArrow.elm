module Modes.NewArrow exposing (graphDrawing, initialise, update)


import Color exposing (..)
import Graph exposing (..)
import GraphDrawing exposing (..)
import GraphExtra as Graph
import Maybe exposing (withDefault)
import Model exposing (..)
import Msg exposing (Msg(..))
import ArrowStyle exposing (ArrowStyle)
import HtmlDefs exposing (Key(..))
import GraphDefs exposing (NodeLabel, EdgeLabel)



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

                            { step = NewArrowMoveNode ArrowStyle.empty,
                             chosenNode = chosenNode }
                             -- prevent bugs (if the mouse is thought
                             -- to be kept on a point)
                      -- , mousePointOver = ONothing
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
        NewArrowMoveNode style  ->
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
                     |> Msg.updateArrowStyle msg           
                     |> NewArrowMoveNode
                     |> updateStep model state 
                     |> noCmd
                  
                _ -> noCmd model
            


moveNodeInfo :
    Model
    -> NewArrowState
    -> ArrowStyle
    ->
        { graph : Graph NodeLabel EdgeLabel
        , movedNode : NodeId
        , created : Bool
        }
moveNodeInfo m state style =
    let
        ( ( graph, movedNode ), created ) =
           -- Model.getTargetNodes |> List.filter (\i ->)
            mayCreateTargetNode m ""
    in
    { graph = Graph.addEdge graph ( state.chosenNode, movedNode ) 
       {label = "", style = style }
    , movedNode = movedNode
    , created = created
    }


graphDrawing : Model -> NewArrowState -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawing m s =
    -- let defaultView movedNode = m.graph{ graph = m.graph, movedNode = movedNode}  in
    graphMakeEditable (renamableFromState s) <|
    collageGraphFromGraph m <|
    case s.step of
        NewArrowMoveNode style ->
            let info = moveNodeInfo m s style in
             info.graph 
            --  |> collageGraphFromGraph m
            {-  |> if info.created then 
                Graph.updateNode info.movedNode
                 (\n -> {n | watchEnterLeave = False })
                else identity -}
        _ ->
            m.graph -- |> collageGraphFromGraph m



renamableFromState : NewArrowState -> Obj
renamableFromState state =
    case state.step of
        NewArrowEditNode m ->
            ONode m

        NewArrowEditEdge m ->
            OEdge ( state.chosenNode, m )

        NewArrowMoveNode _ ->
            ONothing


