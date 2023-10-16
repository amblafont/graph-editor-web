module Modes.NewArrow exposing (graphDrawing, initialise, update, help)


import GraphDrawing exposing (..)
import Polygraph as Graph exposing (Graph, NodeId, EdgeId)
import Msg exposing (Msg(..))
import ArrowStyle 
import HtmlDefs exposing (Key(..))
import GraphDefs exposing (NodeLabel, EdgeLabel)
import Modes exposing ( NewArrowState, Mode(..))
import InputPosition exposing (InputPosition(..))
import Model exposing (..)
import Modes exposing (PullshoutKind(..))
import Modes.Pullshout
import Maybe.Extra
import Drawing.Color as Color




updateState : Model -> NewArrowState  -> Model
updateState m state = {m | mode = NewArrow state}


initialise : Model -> ( Model, Cmd Msg )
initialise m =
    let modelGraph = getActiveGraph m in
    GraphDefs.selectedId modelGraph           
        |> Maybe.Extra.filter (GraphDefs.isNormalId modelGraph)
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
     let m2 = setSaveGraph model <| GraphDefs.weaklySelect info.movedNode
                                 <| GraphDefs.clearSelection info.graph
     in
     
     if finish then switch_Default m2 else
        let ids = if info.created then 
                     [ info.movedNode , info.edgeId ] 
                  else [ info.edgeId ]
        in
        let label = GraphDefs.getLabelLabel state.chosenNode info.graph
                    |> Maybe.withDefault ""
        in
        let ids_labels = List.map (\ id -> (id, label)) ids in
        noCmd <| 
        initialise_RenameModeWithDefault False ids_labels m2
       

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
    let modelGraph = getActiveGraph model in
    let next finish = nextStep model finish state in
    let pullshoutMode k = 
           noCmd <| { model | mode =
                          Modes.Pullshout.initialise modelGraph state.chosenNode k
                          |> Maybe.map PullshoutMode
                          |> Maybe.withDefault (NewArrow state)
                       }
    in
    case msg of
      

        KeyChanged False _ (Character '?') -> noCmd <| toggleHelpOverlay model
        KeyChanged False _ (Control "Escape") -> switch_Default model
        MouseClick -> next False          
        KeyChanged False _ (Control "Enter") -> next True
    --     TabInput -> Just <| ValidateNext
        KeyChanged False _ (Control "Tab") -> next False
        KeyChanged False _ (Character 'i') -> noCmd <| updateState model { state | inverted =  not state.inverted}         
        KeyChanged False _ (Character 'p') -> pullshoutMode Pullback
        KeyChanged False _ (Character 'P') -> pullshoutMode Pushout
        _ ->
            let newStyle =  case msg of
                       KeyChanged False _ k -> 
                            ArrowStyle.keyMaybeUpdateStyle k state.style
                           |> Maybe.withDefault 
                             ((ArrowStyle.keyMaybeUpdateColor k state.style)
                               |> Maybe.withDefault state.style)
                       _ -> state.style 
            in
            let st2 = { state | style = newStyle } in
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
    let modelGraph = getActiveGraph m in
    
    let makeInfo pos = mayCreateTargetNodeAt m pos "" in
    let
        ( ( graph, movedNode ), created ) =
           case state.pos of
              InputPosMouse -> makeInfo m.mousePos                
              InputPosKeyboard p ->     
                --  Debug.log "ici"             
                    makeInfo (keyboardPosToPoint m state.chosenNode p)
              InputPosGraph id ->
                 ((modelGraph, id), False)
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
    -- let defaultView movedNode = modelGraph{ graph = modelGraph, movedNode = movedNode}  in
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
        --     modelGraph -- |> collageGraphFromGraph m



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
            "[?] toggle help overlay, [ESC] cancel, [click, TAB] name the point (if new), "
            ++ "[hjkl] position the new point with the keyboard, "
             ++ "[RET] terminate the arrow creation, "
             ++ "[\""
             ++ ArrowStyle.controlChars
             ++ "\"] alternate between different arrow styles, "
             ++ "[i]nvert arrow, "
             ++ "[p]ullback/[P]ushout mode.\n"
             ++ "Colors: " ++ Color.helpMsg
        -- NewArrowEditNode _ _ ->
        --     "[ESC] empty label, [RET] confirm the label, "
        --     ++ "[TAB] edit the edge label."

        -- NewArrowEditEdge _ _ ->
        --      "[ESC] empty label, [RET] confirm the label."
            


  
