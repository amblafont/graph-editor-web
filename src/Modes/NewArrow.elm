module Modes.NewArrow exposing (graphDrawing, initialise, update, help)


import Color exposing (..)
import GraphDrawing exposing (..)
import Polygraph as Graph exposing (Graph, NodeId, EdgeId)
import Msg exposing (Msg(..))
import ArrowStyle 
import HtmlDefs exposing (Key(..))
import GraphDefs exposing (NodeLabel, EdgeLabel)
import Modes exposing ( NewArrowState, Mode(..))
import InputPosition exposing (InputPosition(..))
import Model exposing (..)




updateState : Model -> NewArrowState  -> Model
updateState m state = {m | mode = NewArrow state}


initialise : Model -> ( Model, Cmd Msg )
initialise m =
    GraphDefs.selectedId m.graph        
        |> Maybe.map
            (\chosenNode ->               
                { m
                    | mode = NewArrow

                            { style = ArrowStyle.empty, 
                              pos = InputPosMouse,                                 
                             chosenNode = chosenNode, 
                             inverted = False }
                             -- prevent bugs (if the mouse is thought
                             -- to be kept on a point)
                      -- , mousePointOver = ONothing
                }                 
            )
        |> Maybe.withDefault m
        |> noCmd





nextStep : Model -> Bool -> NewArrowState -> ( Model, Cmd Msg )
nextStep model finish state =
     let info = moveNodeInfo model state in
     
     -- let m2 = addOrSetSel False info.movedNode { model | graph = info.graph } in
     let m2 = { model | graph = GraphDefs.clearSelection info.graph
                             |> GraphDefs.weaklySelect info.movedNode }
     in
     
     if finish then switch_Default m2 else
        let ids = if info.created then 
                     [ info.movedNode , info.edgeId ] 
                  else [ info.edgeId ]
        in
        let label = GraphDefs.getLabelLabel state.chosenNode info.graph in
        let ids_labels = List.map (\ id -> (id, label)) ids in
        noCmd <| 
        initialise_RenameModeWithDefault ids_labels m2
       

{- eyToAction : Msg -> NewArrowStep -> Maybe Action
keyToAction k step =
   case k of 
       KeyChanged False _ (Control "Escape") -> Just Cancel
       MouseClick ->
           case step of
              NewArrowMoveNode _ -> Just <| ValidateNext
              _ -> Nothing
       KeyChanged False _ (Control "Enter") -> Just <| ValidateFinish     
    --     TabInput -> Just <| ValidateNext
       KeyChanged False _ (Control "Tab") -> Just <| ValidateNext
       _ -> Nothing -}
            

update : NewArrowState -> Msg -> Model -> ( Model, Cmd Msg )
update state msg model =
    let next finish = nextStep model finish state in
    case msg of
      

  
        KeyChanged False _ (Control "Escape") -> switch_Default model
        MouseClick -> next False          
        KeyChanged False _ (Control "Enter") -> next True
    --     TabInput -> Just <| ValidateNext
        KeyChanged False _ (Control "Tab") -> next False
        KeyChanged False _ (Character 'i') -> noCmd <| updateState model { state | inverted =  not state.inverted}         
        _ ->          
                   let st2 = { state | style = Msg.updateArrowStyle msg state.style } in
                   let st3 = { st2 | pos = InputPosition.update st2.pos msg} in
                   st3            
                     |> updateState model 
                     |> noCmd
                  
                
            



 

moveNodeInfo :
    Model
    -> NewArrowState
    ->
        { graph : Graph NodeLabel EdgeLabel
        , movedNode : NodeId
        , edgeId : EdgeId
        , created : Bool
        }
moveNodeInfo m state =
    
    let makeInfo pos = mayCreateTargetNodeAt m pos "" in
    let
        ( ( graph, movedNode ), created ) =
           case state.pos of
              InputPosMouse -> makeInfo m.mousePos                
              InputPosKeyboard p ->     
                --  Debug.log "ici"             
                    makeInfo (keyboardPosToPoint m state.chosenNode p)
              InputPosGraph id ->
                 ((m.graph, id), False)
            -- Debug.log "movedNode? "
             
           
    in
    let (source, target) =
              if state.inverted then
                (movedNode, state.chosenNode)
              else
                (state.chosenNode, movedNode)
    in
    let (g, edgeId) =  Graph.newEdge graph  source target  
           (GraphDefs.newEdgeLabel "" state.style)
    in
     { graph = g
    , movedNode = movedNode
    , created = created
    , edgeId = edgeId
    }


graphDrawing : Model -> NewArrowState -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawing m s =
    -- let defaultView movedNode = m.graph{ graph = m.graph, movedNode = movedNode}  in
    -- graphMakeEditable (renamableFromState s) <|
    collageGraphFromGraph m <|
            let info = moveNodeInfo m s in
             info.graph 
            --  |> collageGraphFromGraph m
            {-  |> if info.created then 
                Graph.updateNode info.movedNode
                 (\n -> {n | watchEnterLeave = False })
                else identity -}
        -- _ ->
        --     m.graph -- |> collageGraphFromGraph m



{- renamableFromState : NewArrowState -> Obj
renamableFromState state =
    case state.step of
        NewArrowEditNode m _ ->
            ONode m

        NewArrowEditEdge _ m ->
            OEdge m

        NewArrowMoveNode _ ->
            ONothing -}

help : String
help  =
--  case s of
--         NewArrowMoveNode _ ->
            -- Debug.toString st ++
            "[ESC] cancel, [click, TAB] name the point (if new), "
            ++ "[hjkl] position the new point with the keyboard, "
             ++ "[RET] terminate the arrow creation, "
             ++ "[\""
             ++ ArrowStyle.controlChars
             ++ "\"] alternate between different arrow styles, "
             ++ "[i]nvert arrow."
        -- NewArrowEditNode _ _ ->
        --     "[ESC] empty label, [RET] confirm the label, "
        --     ++ "[TAB] edit the edge label."

        -- NewArrowEditEdge _ _ ->
        --      "[ESC] empty label, [RET] confirm the label."
            


  
