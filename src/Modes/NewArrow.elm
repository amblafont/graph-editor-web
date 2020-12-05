module Modes.NewArrow exposing (graphDrawing, initialise, update, help)


import Color exposing (..)
import GraphDrawing exposing (..)
import Polygraph as Graph exposing (Graph, NodeId, EdgeId)
import Maybe exposing (withDefault)
import Model exposing (..)
import Msg exposing (Msg(..))
import ArrowStyle exposing (ArrowStyle)
import HtmlDefs exposing (Key(..))
import GraphDefs exposing (NodeLabel, EdgeLabel)
import Model exposing (InputPosition(..))
import Geometry.Point as Point exposing (Point)



updateStep : Model -> NewArrowState -> NewArrowStep -> Model
updateStep m state step = {m | mode = NewArrow { state | step = step }}


initialise : Model -> ( Model, Cmd Msg )
initialise m =
    activeObj m
        |> objId
        |> Maybe.map
            (\chosenNode ->               
                { m
                    | mode = NewArrow

                            { step = NewArrowMoveNode 
                                 { style = ArrowStyle.empty, 
                                   pos = InputPosMouse  }
                                 ,
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
               Cancel -> 
                  case state.step of
                     NewArrowMoveNode _ -> switch_Default model
                     _ ->  switch_Default { m | graph = graphRenameObj m.graph (renamableFromState state) ""}
               ValidateNext -> noCmd m
               ValidateFinish -> switch_Default m            
    in
    let
        renamableNextStep step = updateStep model state step
            |> renamableNextMode            
    in
    case state.step of
        NewArrowMoveNode {style, pos}  ->
                     
                let info = moveNodeInfo model state style pos in
          
                let step = if info.created then
                        NewArrowEditNode info.movedNode info.edgeId
                     else
                        NewArrowEditEdge info.movedNode info.edgeId
                in
                renamableNextMode <| 
                updateStep 
                (addOrSetSel False (ONode info.movedNode)
                  { model | graph = info.graph }) 
                        state step
                

                

                
        NewArrowEditNode movedNode e1 ->
            renamableNextStep <| NewArrowEditEdge movedNode e1

        NewArrowEditEdge movedNode _ ->
            renamableNextMode <|
            addOrSetSel False (ONode movedNode)
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
    --     TabInput -> Just <| ValidateNext
       KeyChanged False (Control "Tab") -> Just <| ValidateNext
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
                NewArrowMoveNode st ->
                   let st2 = { st | style = Msg.updateArrowStyle msg st.style } in
                   let offsetPos x y =
                             let (curx, cury) = getKeyboardPos st2.pos in
                             { st2 | pos = InputPosKeyboard (x + curx, y + cury)}
                   in
                   (case msg of
                     MouseMove _ -> { st2 | pos = InputPosMouse }
                     KeyChanged False (Character 'h') -> offsetPos -1 0
                     KeyChanged False (Character 'j') -> offsetPos 0 1
                     KeyChanged False (Character 'k') -> offsetPos 0 -1
                     KeyChanged False (Character 'l') -> offsetPos 1 0
                     _ -> st2
                   )
                     |> NewArrowMoveNode
                     |> updateStep model state 
                     |> noCmd
                  
                _ -> noCmd model
            



 

moveNodeInfo :
    Model
    -> NewArrowState
    -> ArrowStyle
    -> InputPosition
    ->
        { graph : Graph NodeLabel EdgeLabel
        , movedNode : NodeId
        , edgeId : EdgeId
        , created : Bool
        }
moveNodeInfo m state style posInfo =
    let pos = 
           case posInfo of
              InputPosMouse -> m.mousePos                
              InputPosKeyboard p ->     
                --  Debug.log "ici"             
                    (keyboardPosToPoint m state.chosenNode p)
    in
    let
        ( ( graph, movedNode ), created ) =
            -- Debug.log "movedNode? "
             (mayCreateTargetNodeAt False m pos "")
           
    in
    let (g, edgeId) =  Graph.newEdge graph state.chosenNode movedNode 
           (GraphDefs.newEdgeLabel "" style)
    in
     { graph = g
    , movedNode = movedNode
    , created = created
    , edgeId = edgeId
    }


graphDrawing : Model -> NewArrowState -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawing m s =
    -- let defaultView movedNode = m.graph{ graph = m.graph, movedNode = movedNode}  in
    graphMakeEditable (renamableFromState s) <|
    collageGraphFromGraph m <|
    case s.step of
        NewArrowMoveNode {style, pos} ->
            let info = moveNodeInfo m s style pos in
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
        NewArrowEditNode m _ ->
            ONode m

        NewArrowEditEdge _ m ->
            OEdge m

        NewArrowMoveNode _ ->
            ONothing

help : NewArrowStep -> String
help s =
 case s of
        NewArrowMoveNode _ ->
            -- Debug.toString st ++
            "[ESC] cancel, [click, TAB] name the point (if new), "
            ++ "[hjkl] position the new point with the keyboard, "
             ++ "[RET] terminate the arrow creation, "
             ++ "[(,=,b,B,-,>] alternate between different arrow styles."
        NewArrowEditNode _ _ ->
            "[ESC] empty label, [RET] confirm the label, "
            ++ "[TAB] edit the edge label."

        NewArrowEditEdge _ _ ->
             "[ESC] empty label, [RET] confirm the label."
            


  
