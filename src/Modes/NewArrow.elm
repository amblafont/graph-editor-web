module Modes.NewArrow exposing (graphDrawing, initialise, update, help)


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
    activeObj m
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


type Action =
    ValidateNext
  | ValidateFinish
  | Cancel



nextStep : Model -> Action -> NewArrowState -> ( Model, Cmd Msg )
nextStep model action state =
    
    let
        renamableNextMode m =
            case action of
               Cancel -> switch_Default { m | graph = graphRenameObj m.graph (renamableFromState state) ""}
               ValidateNext -> noCmd m
               ValidateFinish -> switch_Default m            
    in
    let
        renamableNextStep step = updateStep model state step
            |> renamableNextMode            
    in
    case state.step of
        NewArrowMoveNode style  ->
                     
                let info = moveNodeInfo model state style in
          
                let step = if info.created then
                       NewArrowEditNode info.movedNode
                     else
                        NewArrowEditEdge info.movedNode
                in
                renamableNextMode <| 
                updateStep 
                (addOrSetSel True (ONode info.movedNode)
                  { model | graph = info.graph  }
                  ) 
                        state step
                

                

                
        NewArrowEditNode movedNode ->
            renamableNextStep <| NewArrowEditEdge movedNode

        NewArrowEditEdge movedNode ->
            renamableNextMode
                { model
                    | -- activeObj = ONode movedNode
                    -- , 
                    mode = DefaultMode
                }

keyToAction : Msg -> NewArrowStep -> Maybe Action
keyToAction k step =
   case k of 
       KeyChanged False (Control "Escape") -> Just Cancel
       MouseClick ->
           case step of
              NewArrowMoveNode _ -> Just <| ValidateNext
              _ -> Nothing
       KeyChanged False (Control "Enter") -> Just <| ValidateFinish     
       TabInput -> Just <| ValidateNext
       _ -> Nothing
            

update : NewArrowState -> Msg -> Model -> ( Model, Cmd Msg )
update state msg model =
    case msg of
      

        EdgeLabelEdit e s ->
            noCmd { model | graph = graphRenameObj model.graph (OEdge e) s }

        NodeLabelEdit n s ->
            noCmd { model | graph = graphRenameObj model.graph (ONode n) s }

        
            
        _ ->
            case keyToAction msg state.step of
              Just action -> nextStep model action state
              Nothing ->              
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
       (GraphDefs.newEdgeLabel "" style)
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

help : NewArrowStep -> String
help s =
 case s of
        NewArrowMoveNode _ ->
            "[ESC] cancel, [click] name the point (if new), "
             ++ "[RET] terminate the arrow creation, "
             ++ "[(,=,b,B,-,>] alternate between different arrow styles."
        NewArrowEditNode m ->
            "[ESC] empty label, [RET] confirm the label, "
            ++ "[TAB] edit the edge label."

        NewArrowEditEdge m ->
             "[ESC] empty label, [RET] confirm the label."
            


  
