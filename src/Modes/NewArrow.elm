module Modes.NewArrow exposing (graphDrawing, initialise, update, help)


import GraphDrawing exposing (..)
import Polygraph as Graph exposing (Graph, NodeId, EdgeId)
import Msg exposing (Msg(..))
import ArrowStyle 
import HtmlDefs exposing (Key(..))
import GraphDefs exposing (NodeLabel, EdgeLabel)
import Modes exposing ( NewArrowState, Mode(..),  ArrowMode(..))
import InputPosition exposing (InputPosition(..))
import Model exposing (..)
import Modes exposing (PullshoutKind(..))
import Modes exposing (MoveDirection(..))
import Modes.Move
import Modes.Pullshout
import Maybe.Extra
import Drawing.Color as Color
import Zindex




updateState : Model -> NewArrowState  -> Model
updateState m state = {m | mode = NewArrow state}

initialise : Model -> ( Model, Cmd Msg )
initialise m =
    let modelGraph = getActiveGraph m in
    noCmd <|
    if GraphDefs.isEmptySelection modelGraph then m else
    let mode = 
            case GraphDefs.selectedId modelGraph 
                |> Maybe.Extra.filter (GraphDefs.isNormalId modelGraph)
                of 
            Just id -> CreateArrow id
            _ -> CreateCylinder
    in
     { m  | mode = NewArrow
        { style = ArrowStyle.empty, 
            pos = InputPosMouse,                                 
            chosen = GraphDefs.selectedGraph modelGraph,
            mode = mode,
            inverted = False,
            isAdjunction = False
            -- merge = False 
            }
        }  
            
nextStep : Model -> {finish:Bool, merge:Bool} -> NewArrowState -> ( Model, Cmd Msg )
nextStep model {finish, merge} state =
     let info = moveNodeInfo merge True model state in
     
     -- let m2 = addOrSetSel False info.movedNode { model | graph = info.graph } in
     let m2 = setSaveGraph model <| GraphDefs.weaklySelectMany info.selectable
                                 <| GraphDefs.clearSelection info.graph
     in
     
     if finish then switch_Default m2 else
        let ids = info.renamable
        in
        let label = Graph.topmostObject state.chosen |> 
                    Maybe.andThen (\ id -> GraphDefs.getLabelLabel id info.graph)
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
    let next finishMerge = nextStep model finishMerge state in
    let pullshoutMode k = 
           noCmd <|
           
           case state.mode  of
             CreateArrow id -> 
                { model | mode =
                          Modes.Pullshout.initialise modelGraph id k
                          |> Maybe.map PullshoutMode
                          |> Maybe.withDefault (NewArrow state)
                       }
             _ -> model
    in
    case msg of
      
        KeyChanged True _ (Control "Control") -> next {finish = False, merge = True}
        KeyChanged False _ (Character '?') -> noCmd <| toggleHelpOverlay model
        KeyChanged False _ (Control "Escape") -> switch_Default model
        MouseClick -> next {finish = False, merge = False}
        KeyChanged False _ (Control "Enter") -> next {finish = True, merge = state.isAdjunction}
    --     TabInput -> Just <| ValidateNext
        KeyChanged False _ (Control "Tab") -> next {finish = False, merge = state.isAdjunction}
        KeyChanged False _ (Character 'a') -> next {finish = True, merge = True}
        KeyChanged False _ (Character 'd') -> noCmd <| updateState model { state | isAdjunction = not state.isAdjunction}         
        KeyChanged False _ (Character 'i') -> noCmd <| updateState model { state | inverted = not state.inverted}                 
        KeyChanged False _ (Character 'p') -> pullshoutMode Pullback 
        KeyChanged False _ (Character 'P') -> pullshoutMode Pushout
        KeyChanged False _ (Character 'C') -> 
              let mode = nextPossibleMode state
                      |> Maybe.withDefault state.mode
              in
              noCmd <| updateState model { state | mode = mode}             

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
                  
                
            
nextPossibleMode : NewArrowState -> Maybe ArrowMode
nextPossibleMode s =
   case s.mode of
     CreateCone -> Nothing
     CreateCylinder -> Just CreateCone
     CreateArrow _ -> 
        if List.isEmpty <| Graph.edges s.chosen then
            Nothing
        else
            Just CreateCylinder


moveNodeInfo :
    Bool
    -> Bool
    -> Model
    -> NewArrowState
    ->
        { graph : Graph NodeLabel EdgeLabel
        , selectable : List Graph.Id
        , renamable : List Graph.Id
        }
moveNodeInfo merge emptyLabel model state =
                let modelGraph = getActiveGraph model in
                let style = ArrowStyle.getStyle state in                       
                let edgeLabel = GraphDefs.newEdgeLabelAdj 
                              (if state.isAdjunction then "\\vdash" else 
                                if emptyLabel then "" else "-") 
                              style state.isAdjunction
                in
                let nodePos = GraphDefs.centerOfNodes (Graph.nodes state.chosen) in
                let nodeLabel = GraphDefs.newNodeLabel nodePos "" True Zindex.defaultZ  in
                let extendedGraph = 
                     case state.mode of
                        CreateCylinder ->                        
                            Graph.makeCylinder modelGraph state.chosen edgeLabel state.inverted
                        CreateCone ->                            
                            Graph.makeCone modelGraph (Graph.nodeIds state.chosen) nodeLabel edgeLabel state.inverted
                        CreateArrow id ->
                            Graph.makeCone modelGraph [id] nodeLabel edgeLabel state.inverted        
                in            
                let moveInfo =
                        Modes.Move.mkGraph model state.pos
                        Free merge
                         extendedGraph.extendedGraph extendedGraph.newSubGraph 
                in
                let selectable = Graph.allIds extendedGraph.newSubGraph in
                { graph = moveInfo.graph,
                selectable = selectable,
                renamable = (if moveInfo.merged then [] else selectable) ++ 
                            (if state.isAdjunction then [] else extendedGraph.edgeIds)
                }



graphDrawing : Model -> NewArrowState -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawing m s =
    -- let defaultView movedNode = modelGraph{ graph = modelGraph, movedNode = movedNode}  in
    -- graphMakeEditable (renamableFromState s) <|
    collageGraphFromGraph m <|
            let info = moveNodeInfo s.isAdjunction False m s in
             info.graph 
         
help : String
help =
--  case s of
--         NewArrowMoveNode _ ->
            -- Debug.toString st ++
            HtmlDefs.overlayHelpMsg ++
            ", [ESC] cancel, [click, TAB] name the point (if new) and arrow, "
            ++ "[hjkl] position the new point with the keyboard, "
            ++ "[ctrl] merge, [a] merge without renaming, "
             ++ "[RET] terminate the arrow creation, "
             ++ "[\""
             ++ ArrowStyle.controlChars
             ++ "\"] alternate between different arrow styles, "
             ++ "[i]nvert arrow, "
             ++ "create a[d]junction arrow, "
             ++ "[p]ullback/[P]ushout mode, "
             ++ "[C] switch to cone/cylinder creation (if relevant).\n"
             ++ "[p]ullback/[P]ushout mode.\n"
             ++ "Colors: " ++ Color.helpMsg