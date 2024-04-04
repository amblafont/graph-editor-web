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
import Zindex exposing (defaultZ, backgroundZ)




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
            Just _ -> CreateArrow
            _ -> CreateCylinder
    in
     { m  | mode = NewArrow
        { style = ArrowStyle.empty, 
            pos = InputPosMouse,                                 
            chosen = GraphDefs.selectedGraph modelGraph,
            mode = mode,
            inverted = False }
        }  
            

getSingleIdFromGraph : Graph  n e -> Maybe Graph.Id
getSingleIdFromGraph g =
      case (Graph.edges g) of 
      [] -> Graph.nodes g |>
            List.head |> Maybe.map .id
      [ e ] -> Just e.id
      _ -> Nothing


getSingleId : NewArrowState -> Maybe Graph.Id
getSingleId s = 
   if s.mode /= CreateArrow then Nothing else
   getSingleIdFromGraph s.chosen
nextStep : Model -> Bool -> NewArrowState -> ( Model, Cmd Msg )
nextStep model finish state =
     let info = moveNodeInfo finish model state in
     
     -- let m2 = addOrSetSel False info.movedNode { model | graph = info.graph } in
     let m2 = setSaveGraph model <| GraphDefs.weaklySelectMany info.selectable
                                 <| GraphDefs.clearSelection info.graph
     in
     
     if finish then switch_Default m2 else
        let ids = info.renamable
        in
        let label = getSingleId state |> 
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
    let next finish = nextStep model finish state in
    let pullshoutMode k = 
           noCmd <|
           
           case getSingleId state  of
             Just id -> 
                { model | mode =
                          Modes.Pullshout.initialise modelGraph id k
                          |> Maybe.map PullshoutMode
                          |> Maybe.withDefault (NewArrow state)
                       }
             _ -> model
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
     CreateArrow -> 
        if List.isEmpty <| Graph.edges s.chosen then
            Nothing
        else
            Just CreateCylinder


moveNodeInfo :
    Bool
    -> Model
    -> NewArrowState
    ->
        { graph : Graph NodeLabel EdgeLabel
        , selectable : List Graph.Id
        , renamable : List Graph.Id
        }
moveNodeInfo finish model state = 
    case getSingleId state of
      Just id -> 
          -- TODO factorise
          let info = moveNodeInfo_createArrow finish id model state in
          { graph = info.graph,
            selectable = [info.movedNode],
            renamable = if info.created then 
                            [ info.movedNode , info.edgeId ] 
                        else [ info.edgeId ] }
      _ ->
                let modelGraph = getActiveGraph model in
                let label = GraphDefs.newEdgeLabel "" state.style in
                let extendedGraph = 
                        if state.mode == CreateCylinder then 
                            Graph.makeCylinder modelGraph state.chosen label state.inverted
                        else
                            let nodeLabel = GraphDefs.newNodeLabel model.mousePos "" True Zindex.defaultZ  in
                            Graph.makeCone modelGraph state.chosen nodeLabel label state.inverted
                in            
                let merge = model.specialKeys.ctrl in 
                let mergeId = getSingleIdFromGraph extendedGraph.newSubGraph in
                let direction = Free in
                let moveInfo =
                        Modes.Move.mkGraph model state.pos merge mergeId
                        direction 
                         extendedGraph.extendedGraph extendedGraph.newSubGraph 
                in
                let selectable = Graph.allIds extendedGraph.newSubGraph in
                { graph = moveInfo.graph,
                selectable = selectable,
                renamable = (if merge then [] else selectable) ++ extendedGraph.edgeIds}

-- TODO: factor with moveNodeInfo_createArrow
moveNodeInfo_createArrow :
    Bool
    -> Graph.Id
    -> Model
    -> NewArrowState
    ->
        { graph : Graph NodeLabel EdgeLabel
        , movedNode : NodeId
        , edgeId : EdgeId
        , created : Bool
        }
moveNodeInfo_createArrow finish chosenId m state =
    let modelGraph = getActiveGraph m in
    
    let makeInfo pos = mayCreateTargetNodeAt m pos "" finish in
    let
        ( ( graph, movedNode ), created ) =
           case state.pos of
              InputPosMouse -> makeInfo m.mousePos                
              InputPosKeyboard p ->     
                --  Debug.log "ici"             
                    makeInfo (keyboardPosToPoint m chosenId p)
              InputPosGraph id ->
                 ((modelGraph, id), False)
            -- Debug.log "movedNode? "
             
           
    in
    let (source, target) =
              if state.inverted then
                (movedNode, chosenId)
              else
                (chosenId, movedNode)
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
            let info = moveNodeInfo False m s in
             info.graph 
         
help : String
help =
--  case s of
--         NewArrowMoveNode _ ->
            -- Debug.toString st ++
            HtmlDefs.overlayHelpMsg ++
            ", [ESC] cancel, [click, TAB] name the point (if new), "
            ++ "[hjkl] position the new point with the keyboard, "
             ++ "[RET] terminate the arrow creation, "
             ++ "[\""
             ++ ArrowStyle.controlChars
             ++ "\"] alternate between different arrow styles, "
             ++ "[i]nvert arrow, "
             ++ "[p]ullback/[P]ushout mode, "
             ++ "[C] switch to cone/cylinder creation (if relevant).\n"
             ++ "[p]ullback/[P]ushout mode.\n"
             ++ "Colors: " ++ Color.helpMsg