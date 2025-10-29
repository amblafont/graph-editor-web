port module Modes.NewArrow exposing (graphDrawing, fixModel, initialise, update, help, 
    requestMarkerDefault, returnMarker, computeFlags)

import Modes.Capture exposing (UpdateResult(..))
import GraphDrawing exposing (..)
import Geometry.Point
import Polygraph as Graph exposing (Graph)
import Msg exposing (Msg(..))
import ArrowStyle exposing (EdgePart(..))
import HtmlDefs exposing (Key(..))
import GraphDefs exposing (NodeLabel, EdgeLabel)
import Modes exposing ( NewArrowState, Mode(..),  ArrowStateKind(..))
import InputPosition exposing (InputPosition(..))
import Model exposing (..)
import Modes exposing (PullshoutKind(..), NewArrowMode(..))
import Modes exposing (MoveDirection(..))
import Modes.Move
import Modes.Pullshout
import Modes.Bend
import Maybe.Extra
import Drawing.Color as Color
import Zindex
import Format.GraphInfo exposing (activeGraphModifHelper)
import Msg exposing (defaultModifId)
import IntDict
import CommandCodec exposing (protocolSend)
import Modes exposing (BendComponentState)

-- ask js a marker
port requestMarker : String -> Cmd a
-- js returns the marker
port returnMarker : (String -> a) -> Sub a

requestMarkerDefault : String -> Cmd a
requestMarkerDefault s = requestMarker <| if s == "" then "\\bullet" else s


computeFlags : NewArrowState -> Model.CmdFlags
computeFlags state = 
    case state.mode of
        NewArrowBend _ -> { pointerLock = True }
        _ -> { pointerLock = False }

updateState : Model -> NewArrowState  -> Model
updateState m state = setMode (NewArrow state) m
{-
fixModel : Model -> NewArrowState -> Model
fixModel model state = 
   let modelGraph = getActiveGraph model in
   let intersectGraph = 
        Graph.intersect state.chosen modelGraph 
   in 
   if Graph.isEmpty intersectGraph then 
      { model| mode = DefaultMode }
   else 
   -}
-- not <| GraphDefs.isEmptySelection <| getActiveGraph model 
{-
    { chosen : Graph.Graph NodeLabel EdgeLabel,
      mode : ArrowMode, 
      style : ArrowStyle, 
      pos : InputPosition, inverted : Bool,
      isAdjunction : Bool
      -- merge : Bool
       }
       -}

fixModel = reinitialise

reinitialise : Model -> NewArrowState -> Model
reinitialise m state =
    let modelGraph = getActiveGraph m in
    -- noCmd <|
    if GraphDefs.isEmptySelection modelGraph then setMode DefaultMode m else
     setMode (NewArrow
        { state | 
            chosen = GraphDefs.selectedGraph modelGraph
            -- mode = mode
            -- merge = False 
            }) m  

initialise : Model -> Model
initialise m =
    let modelGraph = getActiveGraph m in
    -- noCmd <|
    let kind = 
            case GraphDefs.selectedId modelGraph 
                |> Maybe.Extra.filter (GraphDefs.isNormalId modelGraph)
                of 
            Just id -> CreateArrow id
            _ -> CreateCylinder
    in
    reinitialise m 
        { style = ArrowStyle.empty, 
            pos = InputPosMouse,                                 
            chosen = Graph.empty, -- GraphDefs.selectedGraph modelGraph,
            kind = kind,
            mode = NewArrowPart MainEdgePart,
            inverted = False,
            isAdjunction = False,
            merge = False
            -- merge = False 
            }
        
            
nextStep : Model -> {finish:Bool, merge:Bool} -> NewArrowState -> ( Model, Cmd Msg )
nextStep model {finish, merge} state =
     let info = moveNodeInfo merge True model state in
     
     -- let m2 = addOrSetSel False info.movedNode { model | graph = info.graph } in
    --  let m2 = setSaveGraphWithGraph model info.graph
    --              (\ g -> GraphDefs.weaklySelectMany info.selectable
    --                              <| GraphDefs.clearSelection g)
    --  in
     let modif = activeGraphModifHelper model.graphInfo info.graph in
     let selIds = IntDict.insert model.graphInfo.activeTabId 
                    info.selectable IntDict.empty 
     in
    --  let m2 = updateActiveTab m (\ tab -> { tab | graph =
    --                 GraphDefs.weaklySelectMany info.selectable
    --                              <| GraphDefs.clearSelection tab.graph })
     
     
     if finish then 
        (setMode DefaultMode model, protocolSend { id = defaultModifId ,
          modif = modif,
          selIds = selIds,
          command = Msg.Noop
        })
    --  switch_Default m2 
     else
        let ids = info.renamable
        in
        let label = 
               if state.kind == CreateCylinder then Nothing else
                Graph.topmostObject state.chosen |> 
                    Maybe.andThen (\ id -> GraphDefs.getLabelLabel id state.chosen)
                    -- info.graph)
                    |> Maybe.withDefault "" 
                    |> Just                   
        in
        let ids_labels = List.map (\ id -> { id = id, label = label
                        , tabId = model.graphInfo.activeTabId}) ids 
        in
        let (nextModel, idModif) = popIdModif model in
        let finalModel = setMode DefaultMode nextModel in
        (finalModel, 
        protocolSend 
        { id = idModif ,
          modif = modif,
          selIds = selIds,
          command = Msg.RenameCommand ids_labels
        }
        )
        -- initialise_RenameModeWithDefault False ids_labels m2
       

update : NewArrowState -> Msg -> Model -> ( Model, Cmd Msg )
update state msg model =
   let updateHeadOrTail part = 
        case msg of
                KeyChanged False _ (Character '?') -> noCmd <| toggleHelpOverlay model
                KeyChanged False _ (Control "Escape")
                    -> noCmd <| updateState model { state | mode = NewArrowPart MainEdgePart }
                KeyChanged False _ k ->                              
                    case ArrowStyle.keyMaybeUpdateColor k part state.style of
                        Just newStyle -> 
                           let st2 = { state | style = newStyle, mode = NewArrowPart MainEdgePart } in
                             updateState model st2
                              |> noCmd
                        _ -> noCmd model                               
                _ -> noCmd model
    in

   case state.mode of
      NewArrowPart MainEdgePart -> updateNormal state msg model
      NewArrowPart HeadPart -> updateHeadOrTail HeadPart
      NewArrowPart TailPart -> updateHeadOrTail TailPart
      NewArrowBend b -> updateBend b state msg model

updateBend : BendComponentState -> NewArrowState -> Msg -> Model -> ( Model, Cmd Msg )
updateBend bendState state msg model =
    let (result, newCompState) = Modes.Bend.updateComponent bendState msg in
    case result of
        NewState ->
            let newState = { state | mode = NewArrowBend newCompState } in
            noCmd <| updateState model newState
        -- ToggleHelp -> noCmd <| toggleHelpOverlay model
        NoChange ->
            noCmd model
        Finalise ->
            let newState = { state | mode = NewArrowPart MainEdgePart,
                         style = getStyle state } 
            in
            noCmd <| updateState model newState
        Cancel ->
            let newState = { state | mode = NewArrowPart MainEdgePart } in
            noCmd <| updateState model newState

initialiseBendMode : NewArrowState -> Model -> Model
initialiseBendMode state model =
    -- case state.kind of
    --   CreateArrow id -> 
         let fromTo = 
                case state.kind of 
                    CreateArrow id ->
                        Maybe.map 
                        (\ from -> (from, model.mousePos))
                        (Graph.get id .pos .pos <| GraphDefs.posGraph state.chosen)
                    _ -> Just ((0, 0), (1, 0))
         in
         case fromTo of
            Nothing -> model
            Just (from, to) ->
                    let bend = ArrowStyle.getStyle state |> .bend in
                    updateState model { state | mode = NewArrowBend
                      <|  Modes.Bend.initialiseComponent 
                      (Geometry.Point.subtract to from)
                      {
                           bend = bend,
                           origBend = bend
                      } } 
    --   _ -> 
    --   noCmd model

getStyle : NewArrowState -> ArrowStyle.ArrowStyle
getStyle state = 
    let style = ArrowStyle.getStyle state in
    case state.mode of 
        NewArrowBend b -> { style | bend = Modes.Bend.componentGetBend b }
        _ -> style 

updateNormal : NewArrowState -> Msg -> Model -> ( Model, Cmd Msg )
updateNormal state msg model =
    let modelGraph = getActiveGraph model in
    let next finishMerge = nextStep model finishMerge state in
    let pullshoutMode k = 
           noCmd <|
           
           case state.kind  of
             CreateArrow id -> 
                setMode 
                          (Modes.Pullshout.initialise modelGraph id k
                          |> Maybe.map PullshoutMode
                          |> Maybe.withDefault (NewArrow state))
                          model
             _ -> model
    in
    let changePart m = 
                if ArrowStyle.isPartColorable m state.style then
                    noCmd <| updateState model { state | mode = NewArrowPart m } 
                else noCmd model
    in
    case msg of
      
        KeyChanged True _ (Control "Control") -> next {finish = False, merge = True}
        KeyChanged False _ (Character '?') -> noCmd <| toggleHelpOverlay model
        KeyChanged False _ (Control "Escape") -> switch_Default model
        MouseClick -> next {finish = False, merge = state.merge}
        KeyChanged False _ (Character ' ') -> next {finish = True, merge = state.merge || state.isAdjunction}
        KeyChanged False _ (Control "Enter") -> next {finish = True, merge = state.merge || state.isAdjunction}
        Marker s -> 
                    let style = state.style in
                    noCmd <| updateState model { state | style = 
                                   {style | marker = s}
                                }
        KeyChanged False _ (Character '.') -> (model, requestMarkerDefault state.style.marker)
    --     TabInput -> Just <| ValidateNext
        KeyChanged False _ (Control "Tab") -> next {finish = False, merge = state.merge || state.isAdjunction}
        KeyChanged False _ (Character 'a') -> next {finish = True, merge = True}
        KeyChanged False _ (Character 'b') -> noCmd <| initialiseBendMode state model
        KeyChanged False _ (Character 'd') -> noCmd <| updateState model { state | isAdjunction = not state.isAdjunction}         
        KeyChanged False _ (Character 'f') -> 
              noCmd <| updateState model { state | pos = truncateInputPosition model state.chosen }
        KeyChanged False _ (Character 'i') -> noCmd <| updateState model { state | inverted = not state.inverted}                 
        KeyChanged False _ (Character 'p') -> pullshoutMode Pullback 
        KeyChanged False _ (Character 'P') -> pullshoutMode Pushout
        -- KeyChanged False _ (Character 's') -> noCmd <| initialiseShifts model state
        KeyChanged False _ (Character 'm') -> noCmd <| updateState model { state | merge = not state.merge} 
        KeyChanged False _ (Character 'H') -> changePart <| HeadPart
        KeyChanged False _ (Character 'T') -> changePart <| TailPart
        KeyChanged False _ (Character 'C') -> 
              let kind = nextPossibleKind state
                      |> Maybe.withDefault state.kind
              in
              noCmd <| updateState model { state | kind = kind}             

        _ ->
            let newStyle =  case msg of
                       KeyChanged False _ k -> 
                            ArrowStyle.keyMaybeUpdateStyle k state.style
                           |> Maybe.withDefault 
                             ((ArrowStyle.keyMaybeUpdateColor k MainEdgePart state.style)
                               
                           |> Maybe.withDefault
                           (ArrowStyle.keyUpdateShiftBend k state.style
                             |> Maybe.withDefault state.style
                           ))
                       _ -> state.style 
            in
            let st2 = { state | style = newStyle } in
            let st3 = { st2 | pos = InputPosition.update st2.pos msg} in
               st3            
               |> updateState model 
               |> noCmd
                  
                
            
nextPossibleKind : NewArrowState -> Maybe ArrowStateKind
nextPossibleKind s =
   case s.kind of
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
        { graph : Graph.ModifHelper NodeLabel EdgeLabel
        , selectable : List Graph.Id
        , renamable : List Graph.Id
        , weaklySelection : Maybe Graph.Id
        }
moveNodeInfo merge emptyLabel model state =
                let modelGraph = getActiveGraph model in
                let style = getStyle state in                       
                let edgeLabel = GraphDefs.newEdgeLabelAdj 
                              (if state.isAdjunction then "\\vdash" else 
                                if emptyLabel then "" else "-") 
                              style state.isAdjunction
                in
                let nodePos = GraphDefs.centerOfNodes (Graph.nodes state.chosen) in
                let nodeLabel = GraphDefs.newNodeLabel nodePos "" True Zindex.defaultZ  in
                let modifGraph = Graph.newModif modelGraph in
                let extendedGraph = 
                     case state.kind of
                        CreateCylinder ->                        
                            Graph.md_makeCylinder modifGraph state.chosen edgeLabel state.inverted
                        CreateCone ->                            
                            Graph.md_makeCone modifGraph (Graph.nodeIds state.chosen) nodeLabel edgeLabel state.inverted
                        CreateArrow id ->
                            Graph.md_makeCone modifGraph [id] nodeLabel edgeLabel state.inverted        
                in            
                let moveInfo =
                        Modes.Move.mkGraph model
                        --  (getTargetPos state model)
                         state.pos
                        Free merge
                         extendedGraph.extendedGraph
                         modelGraph
                         extendedGraph.newSubGraph 
                in
                let selectable = Graph.allIds extendedGraph.newSubGraph in
                { graph = moveInfo.graph,
                weaklySelection = moveInfo.weaklySelection,
                selectable = selectable,
                renamable = (if moveInfo.merged then [] else selectable) ++ 
                            (if state.isAdjunction then [] else extendedGraph.edgeIds)
                }

-- getTargetPos : NewArrowState -> Model -> Point
-- getTargetPos state model = model.mousePos
    -- case state.mode of
    --     NewArrowBend b -> model.mousPos --b.origMouse
    --     _ -> model.mousePos

graphDrawing : Model -> NewArrowState -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawing m s =
     let info = moveNodeInfo (s.merge || s.isAdjunction) False m s in
     
    -- let defaultView movedNode = modelGraph{ graph = modelGraph, movedNode = movedNode}  in
    -- graphMakeEditable (renamableFromState s) <|
    collageGraphFromGraph m <| 
    Modes.Move.computeGraph info
   
            --  Graph.applyModifHelper info.graph 
         
help : NewArrowState -> String
help s =
    case s.mode of        
        NewArrowPart MainEdgePart ->
--  case s of
--         NewArrowMoveNode _ ->
            -- Debug.toString st ++
            HtmlDefs.overlayHelpMsg ++
            ", [ESC] cancel, [click, TAB] name the point (if new) and arrow, "
            ++ "[hjkl] position the new point with the keyboard "
            ++ "([f] to move by a multiple of the grid size), "
            ++ "[ctrl] merge, [a] merge without renaming, toggle [m]erge preview, "
             ++ "[RET] or space to terminate the arrow creation, "
             ++ "[\""
             ++ ArrowStyle.controlChars
             ++ "\"] alternate between different arrow styles, "
             ++ ArrowStyle.shiftHelpMsg
             ++ ", "
             ++ "[.] customise the marker"
             ++ "[i]nvert arrow, "
             ++ "create a[d]junction arrow, "
             ++ "[p]ullback/[P]ushout mode, "
             ++ "[C] switch to cone/cylinder creation (if relevant).\n"
             ++ "[p]ullback/[P]ushout mode.\n"
             ++ "Colors: " ++ Color.helpMsg
             ++ ", color [H]ead/[T]ail"
        NewArrowPart p -> "Submode color "
            ++ (if p == HeadPart then "head" else "tail")
            ++ " of the arrow: [ESC] to cancel, "
            ++ Color.helpMsg ++ ", "
            ++ HtmlDefs.overlayHelpMsg
        NewArrowBend _ -> Modes.Bend.help 