port module Main exposing (main)
-- TODO: avoir mode dans Model, et separer le fichier state de Square
-- move: with keyboard
-- copier ?


-- https://package.elm-lang.org/packages/mpizenberg/elm-pointer-events/4.0.2/Html-Events-Extra-Mouse
-- mouseclick prevent default?
-- split arrow, copy arrow
-- when saving, clean the graph
-- hover for bent lines
-- tab: prevent default
-- bug avec square
-- ctrl pour merge,
-- et close commutation
-- touche pour guess labels (s'il  a plusieurs possiblites')


import Html.Events.Extra.Mouse as MouseEvents

import Model exposing (..)
import Browser
import Browser.Events as E

import Task
import Browser.Dom as Dom


import Json.Decode as D
import Json.Encode as JE
import Polygraph as Graph exposing (Graph, NodeId, Node, Edge)

import Drawing exposing (Drawing)

import Geometry.Point as Point exposing (Point)

import Color exposing (..)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Maybe.Extra as Maybe


import Parser exposing ((|.), (|=), Parser)
import Set
import QuickInput exposing (equalityParser)

import GraphDrawing exposing (..)
import Msg exposing (Msg(..))

import Tuple
import Maybe exposing (withDefault)

import Modes.Square
import Modes.NewArrow 
import Modes.SplitArrow
import Modes exposing (Mode(..), isResizeMode, ResizeState, CutHeadState)

import ArrowStyle

import HtmlDefs exposing (Key(..), quickInputId, computeLayout)
import GraphDefs exposing (NodeLabel, EdgeLabel)
import GraphDefs exposing (newNodeLabel)
import GraphDefs exposing (getNodeLabelOrCreate)
import GraphDefs exposing (newEdgeLabel)
import Html
import Geometry exposing (Rect)
import Geometry exposing (rectEnveloppe)
import Html.Events
import Modes exposing (SplitArrowState)
import InputPosition exposing (InputPosition(..))
import Format.Version0
import Format.Version1
import Format.Version2
import Format.Version3
import Format.LastVersion as LastFormat

import List.Extra
import GraphDefs exposing (exportQuiver)
import GraphProof
import Format.GraphInfo

-- we tell js about some mouse move event
port onMouseMove : JE.Value -> Cmd a
-- we then get back the relative position of the mouse
-- (this cannot be done in pure elm because it requires
-- to access to the currentTarget field of the event,
-- which is a js object)
port onMouseMoveFromJS : (Point -> a) -> Sub a

port preventDefault : JE.Value -> Cmd a
port onKeyDownActive : (JE.Value -> a) -> Sub a

-- tell js to save the graph, version  is the format version
port saveGraph : { graph : LastFormat.Graph, fileName : String, version : Int } -> Cmd a
-- filename
port savedGraph : (String -> a) -> Sub a
port exportQuiver : JE.Value -> Cmd a
port alert : String -> Cmd a
port jumpToId : String -> Cmd a

-- js tells us to load the graph
port loadedGraph0 : ({ graph : Format.Version0.Graph, fileName : String } -> a) -> Sub a
port loadedGraph1 : ({ graph : Format.Version1.Graph, fileName : String } -> a) -> Sub a
port loadedGraph2 : ({ graph : Format.Version2.Graph, fileName : String } -> a) -> Sub a
port loadedGraph3 : ({ graph : Format.Version3.Graph, fileName : String } -> a) -> Sub a

port clipboardWriteGraph : LastFormat.Graph -> Cmd a
-- tells JS we got a paste event with such data
port pasteGraph : JE.Value -> Cmd a
-- JS would then calls us back with the decoded graph
port clipboardGraph : (LastFormat.Graph -> a) -> Sub a
port findReplace : ({ search: String, replace:String} -> a) -> Sub a














subscriptions : Model -> Sub Msg
subscriptions m = 
    Sub.batch 
     <|
    [
      findReplace FindReplace,
      -- upload a graph (triggered by js)
      
      loadedGraph0 (\ r -> Loaded (Format.Version0.fromJSGraph r.graph) r.fileName),
      loadedGraph1 (\ r -> Loaded (Format.Version1.fromJSGraph r.graph) r.fileName),
      loadedGraph2 (\ r -> Loaded (Format.Version2.fromJSGraph r.graph) r.fileName),
      loadedGraph3 (\ r -> Loaded (Format.Version3.fromJSGraph r.graph) r.fileName),
      clipboardGraph (LastFormat.fromJSGraph >> PasteGraph),
      savedGraph FileName,
      E.onClick (D.succeed MouseClick)
      {- Html.Events.preventDefaultOn "keydown"
        (D.map (\tab -> if tab then 
                            -- it is necessary to prevent defaults
                            -- otherwise the next immediate appearing input 
                            -- may not shows up
                                      (TabInput, True) 
                        else (Msg.noOp, False))
                         HtmlDefs.tabDecoder), -} ]
    ++
    if case m.mode of
        ResizeMode _ -> False
        QuickInputMode _ -> False
        _ -> not m.mouseOnCanvas
    then [] 
    else
    [  E.onKeyUp (D.map2 (KeyChanged False) HtmlDefs.keysDecoder HtmlDefs.keyDecoder),
      onMouseMoveFromJS MouseMove,
      onKeyDownActive
           (\e -> e |> D.decodeValue (D.map2 ( \ks k -> 
               let checkCtrl =
                    if ks.ctrl && m.mode == DefaultMode then
                      Do <| preventDefault e
                    else Msg.noOp
               in
               case k of
                 Character '/' ->
                    case m.mode of
                      DefaultMode  -> Do <| preventDefault e
                      SplitArrow _ -> Do <| preventDefault e
                      _ -> Msg.noOp 
                 Character 'a' -> checkCtrl
                 Control "Tab" ->  
                    case m.mode of
                      SquareMode _  -> Do <| preventDefault e
                      SplitArrow _ -> Do <| preventDefault e
                      NewArrow _ -> Do <| preventDefault e
                      _ -> Msg.noOp                
                 _ -> Msg.noOp
                
                )                
                HtmlDefs.keysDecoder
                HtmlDefs.keyDecoder)
                |> Result.withDefault Msg.noOp
                )
    ]












-- Model -----------------------------------------------------------------------





-- The graphs are updated according to the current mode





graph_RenameMode : List (Graph.Id, String) -> Model -> Graph NodeLabel EdgeLabel
graph_RenameMode l m = 
   case l of
      [] -> m.graph
      (id, s) :: _ ->   Graph.update id 
                        (\ n -> {n | label = s })  
                               (\ e -> {e | label = s })                               
                               m.graph 

info_MoveNode : Model -> Modes.MoveState -> 
   { graph : Graph NodeLabel EdgeLabel,
   -- The graph is not valid if we are in merge mode
   -- and no object is pointed at
     valid : Bool }
info_MoveNode model { orig, pos } =
    
    let merge = model.specialKeys.ctrl in
    let nodes = allSelectedNodes model in
    let updNode delta {id, label} = 
          {id = id, label = { label | pos = Point.add label.pos delta }}
    in
    let moveNodes delta = nodes |> List.map (updNode delta) in
   --  let moveGraph delta =  Graph.updateNodes (moveNodes delta) model.graph in
    let mkRet movedNodes = 
            let _ = Debug.log "selected" model.graph in
            let g = Graph.updateNodes movedNodes model.graph in
            let _ = Debug.log "mkRet" g in 
            { graph =  g, valid = not merge } in
    let retMerge movedNodes =                  
           case movedNodes of
              [ n ] ->        
                let (g, valid) = GraphDefs.mergeWithSameLoc n model.graph in
                if valid then
                  {graph = g, valid = True }
                else
                  mkRet movedNodes
              _ -> mkRet movedNodes      
    in       
    let retDelta delta =
            let movedNodes = moveNodes delta in
            if merge then
                retMerge movedNodes
            else
                mkRet movedNodes      
          
    in
   
    let mouseDelta = Point.subtract model.mousePos <| GraphDefs.centerOfNodes nodes in
    case pos of
      InputPosKeyboard p -> retDelta <| InputPosition.deltaKeyboardPos model.sizeGrid p
      InputPosGraph id ->         
         if not merge then 
            retDelta mouseDelta
         else            
            case nodes of 
               [n] -> {graph = Graph.merge id n.id model.graph, valid = True}
               _ -> retDelta mouseDelta
      InputPosMouse -> retDelta mouseDelta







{- switch_RenameMode : Model -> (Model, Cmd Msg)
switch_RenameMode model =
    let label : Maybe String
        label = case activeObj model of
              ONothing -> Nothing
              ONode id -> Graph.getNode id model.graph |> Maybe.map .label
              OEdge id -> Graph.getEdge id model.graph |> Maybe.map (\ (_, _, e) -> e.label)
    in
    case label of
        Nothing -> noCmd model
        Just l -> (
             {model | mode = RenameMode l}, Cmd.none {- focusLabelInput -})
 -}
-- Now, deal with incoming messages
update : Msg -> Model -> (Model, Cmd Msg)
update msg modeli =
    let model = case msg of
                FileName s -> { modeli | fileName = s }
                KeyChanged _ r _ -> { modeli | specialKeys = r }
                MouseMoveRaw _ keys -> { modeli | specialKeys = keys, mouseOnCanvas = True} 
                MouseMove p -> { modeli | mousePos = p} -- , mouseOnCanvas = True}
                MouseDown e -> { modeli | specialKeys = e.keys }
                MouseLeaveCanvas -> 
                   let _ = Debug.log "mouseleave" () in
                    { modeli | mouseOnCanvas = False }
                {- FindInitial -> selectInitial model -}
                QuickInput s -> let _ = Debug.log "coucou1!" () in
                     { modeli | quickInput = s, mode = QuickInputMode Nothing} -- , mouseOnCanvas = False}
                -- EditBottomText s -> 
                --     let _ = Debug.log "bottomText" model.bottomText in
                --     { model | bottomText = {- Debug.log "bottomText" -} s}
                       -- {model | mousePos = (x, y), statusMsg = "mouse " ++ Debug.toString (x, y)}
               -- KeyChanged False s -> {model | statusMsg = keyToString s}
               -- NodeClick n -> {model | statusMsg = "point " }
               -- NodeEnter n -> 
               --    let _ = Debug.log "ici" n in
               --   { model | mousePointOver = ONode n}
               -- NodeLeave n -> { model | mousePointOver = ONothing}
 
               -- MouseClick -> let _ = Debug.log "Mouse Click !" () in model
                _ -> modeli            
    in
    case msg of
     Save ->               
          (model, saveGraph { graph = LastFormat.toJSGraph 
                                    <| Model.toGraphInfo model,
                              fileName = model.fileName,
                              version = LastFormat.version})
     Clear -> noCmd iniModel --  (iniModel, Task.attempt (always Msg.noOp) (Dom.focus HtmlDefs.canvasId))
     ToggleHideGrid -> noCmd {model | hideGrid = not model.hideGrid}     
     ExportQuiver -> (model,  
                    exportQuiver <| 
                     GraphDefs.exportQuiver model.sizeGrid (GraphDefs.selectedGraph model.graph))
     MouseMoveRaw v _ -> (model, onMouseMove v)
     NodeRendered n dims ->
                -- let _ = Debug.log "nouvelle dims !" (n, dims) in
                noCmd { model | -- statusMsg = "newsize " ++ Debug.toString (n, dims),
                      graph = 
                      Graph.updateNode n (\l -> {l | dims = Just dims }) model.graph                      
                }
     EdgeRendered e dims ->
                -- let _ = Debug.log "nouvelle dims !" (e, dims) in
                noCmd { model | -- statusMsg = "newsize " ++ Debug.toString (e, dims),
                      graph = 
                      Graph.updateEdge e (\l -> {l | dims = Just dims }) model.graph                      
                }
     Do cmd -> (model, cmd)
     Loaded g fileName -> noCmd <| { model | graph = g.graph,
                                             sizeGrid = g.sizeGrid, 
                                             fileName = fileName,
                                             mode = DefaultMode }
     FindReplace req -> noCmd <| setSaveGraph model 
                          <| GraphDefs.findReplaceInSelected model.graph req
     _ ->
      case model.mode of
        QuickInputMode c -> update_QuickInput c msg model 
        DefaultMode -> update_DefaultMode msg model
        RectSelect orig -> update_RectSelect msg orig model.specialKeys.shift model
        EnlargeMode orig -> update_Enlarge msg orig model
        NewArrow astate -> Modes.NewArrow.update astate msg model
            -- update_Modes.NewArrow astate msg m
        RenameMode l -> update_RenameMode l msg model
        Move s -> update_MoveNode msg s model
        DebugMode -> update_DebugMode msg model
       -- NewNode -> update_NewNode msg model
        SquareMode state -> Modes.Square.update state msg model
        SplitArrow state -> Modes.SplitArrow.update state msg model
        CutHead state -> update_CutHead state msg model
        CloneMode -> update_Clone msg model
        ResizeMode s -> update_Resize s msg model


update_QuickInput : Maybe QuickInput.Equation -> Msg -> Model -> (Model, Cmd Msg)
update_QuickInput ch msg model =
    case msg of
        KeyChanged False _ (Control "Escape") ->
            ({model | mode = DefaultMode}, 
                 Task.attempt (\_ -> Msg.noOp) (Dom.blur quickInputId))
        KeyChanged False _ (Control "Enter") ->
            ({model | graph = graphQuickInput model ch, 
                     quickInput = "",
                     mode = DefaultMode }, 
                     -- new nodes may have sent their dimensions
                     -- but the model graph did not contain
                     -- them at this time
                     -- so we need to compute them again
                     -- TODO: take these dimensions into account
                     -- even before
                     computeLayout ())
        QuickInput s ->
                let (statusMsg, chain) =
                        case Parser.run equalityParser s of
                            Ok l -> (Debug.toString l, Just l)
                            Err e -> (Parser.deadEndsToString e, Nothing)
                in
                noCmd {model | statusMsg = statusMsg, mode = QuickInputMode chain} -- , mouseOnCanvas = False}
        _ -> noCmd model


update_MoveNode : Msg -> Modes.MoveState -> Model -> (Model, Cmd Msg)
update_MoveNode msg state model =
    let movedRet = 
           let info = info_MoveNode model state in
           if info.valid then
              switch_Default {model | graph = info.graph}
           else
              noCmd model
    in
    let updateState st = { model | mode = Move st } in
    case msg of
        KeyChanged False _ (Control "Escape") -> switch_Default model
        MouseClick -> movedRet
        KeyChanged False _ (Control "Enter") -> movedRet
        _ ->  noCmd <| updateState { state | pos = InputPosition.update state.pos msg }




        
update_RenameMode : List (Graph.Id, String) -> Msg -> Model -> (Model, Cmd Msg)
update_RenameMode labels msg model =
   let edit_label s = 
         noCmd {model | mode = RenameMode <| 
         case labels of
           (id, _) :: q -> (id, s) :: q
           _ -> labels -- should not happen
         }
   in 
    case msg of
      KeyChanged False _ (Control "Escape") -> switch_Default model
      KeyChanged False _ (Control "Enter") -> noCmd <| next_RenameMode True labels model
      KeyChanged False _ (Control "Tab") -> noCmd <| next_RenameMode False labels  model
    --   MouseClick -> finalise_RenameMode label model
      NodeLabelEdit _ s -> edit_label s
      EdgeLabelEdit _ s -> edit_label s
      _ -> noCmd model



next_RenameMode : Bool -> List (Graph.Id, String) -> Model -> Model
next_RenameMode finish labels model =
    let g = graph_RenameMode labels model in
    let m2 =  {model | graph = g} in
    if finish then
      { m2 | mode = DefaultMode }
    else
      case labels of
        [] ->   { m2 | mode = DefaultMode }
        _ :: q -> { m2 | mode = RenameMode q }
        

update_RectSelect : Msg -> Point -> Bool -> Model -> (Model, Cmd Msg)
update_RectSelect msg orig keep model =
   case msg of
      KeyChanged False _ (Control "Escape") -> switch_Default model
      {- MouseUp -> switch_Default 
                  { model | graph = selectGraph model orig keep } -}
      MouseClick ->
          if model.mousePos == orig then
           switch_Default <| selectByClick model
          else
           switch_Default 
                  { model | graph = selectGraph model orig keep }
      -- au cas ou le click n'a pas eu le temps de s'enregistrer
      --   NodeClick n -> switch_Default { model | selectedObjs = [ONode n]} 
      --   EdgeClick n -> switch_Default { model | selectedObjs = [OEdge n]}
      _ -> noCmd model

update_Enlarge : Msg -> Point -> Model -> (Model, Cmd Msg)
update_Enlarge msg orig model =
   case msg of
      KeyChanged False _ (Control "Escape") -> switch_Default model
      MouseUp -> switch_Default 
                  { model | graph = enlargeGraph model orig }
      -- au cas ou le click n'a pas eu le temps de s'enregistrer
      --   NodeClick n -> switch_Default { model | selectedObjs = [ONode n]} 
      --   EdgeClick n -> switch_Default { model | selectedObjs = [OEdge n]}
      _ -> noCmd model

selectLoop : Bool -> Model -> Model
selectLoop direction model =
     let g = GraphDefs.toProofGraph model.graph in
     let edges =  GraphDefs.selectedEdgeId model.graph 
              |> Maybe.andThen (\id -> 
                   Graph.getEdge id g) -- |> Maybe.map (\ e -> { id = id, label = e.label}))
              |> Maybe.map (GraphProof.loopFrom direction g)
              |> Maybe.withDefault []
     in
     let diag = GraphProof.loopToDiagram edges in
     let _ = Debug.log "sel lhs" (diag.lhs |> List.map (.label >> .label)) in
     let _ = Debug.log "sel rhs" (diag.rhs |> List.map (.label >> .label)) in
     let _ = Debug.log "isBorder?" (GraphProof.isBorder diag) in     
            { model | graph = edges |> List.map (Tuple.first >> .id)            
              |> List.foldl (\ e -> Graph.updateEdge e (\n -> {n | selected = True})) 
                  (GraphDefs.clearSelection model.graph) }

rename : Model -> (Model, Cmd Msg)
rename model =
    let ids = GraphDefs.selectedId model.graph 
            |> Maybe.map List.singleton 
            |> Maybe.withDefault []
    in
        noCmd <| initialise_RenameMode ids <| pushHistory model
            
update_DefaultMode : Msg -> Model -> (Model, Cmd Msg)
update_DefaultMode msg model =
    let delta_angle = pi / 5 in    
    let move angle = 
               GraphDefs.selectedNode model.graph                  
                 -- |> Maybe.andThen (\id -> Graph.getNode id model.graph) 
                 |> Maybe.map (.label >> .pos)
                 |> Maybe.andThen (\p ->
                    Graph.filterNodes model.graph 
                      (\ n -> n.pos /= p && (Point.subtract n.pos p |> Point.pointToAngle |>
                      Point.angleWithInRange delta_angle angle  ))
                    |>
                    List.Extra.minimumBy (.label >> .pos >> Point.distance p )                  
                
                 )
                 |> Maybe.map (\n ->
                 Model.addOrSetSel False n.id model
                 )                 
                 |> Maybe.withDefault model
                 |> noCmd
    in
    let fillBottom s err =
          let c = if s == "" then alert err
                      else jumpToId HtmlDefs.bottomTextId
          in
          ({ model | bottomText = s }, c)
    in
    let generateProof stToString =
           let s = String.join "\n\n"
                 <| List.map stToString 
                 <| GraphProof.fullProofs
                 <| GraphDefs.toProofGraph model.graph
           in
           fillBottom s "No diagram found!"
           
    in
    let weaklySelect id =
             noCmd <|              
                if model.specialKeys.shift then
                { model | graph = GraphDefs.addOrSetSel True id model.graph }
                else 
               -- if model.hoverId == Nothing then model else                    
               { model | graph = GraphDefs.weaklySelect  
                                        id
                                         model.graph 
                                         }
    in
    
    {- let updateStr =
       GraphProof.proofStatementToString  -}
    -- Tuples.mapFirst (changeModel model) <|
    case msg of
        MouseOn id ->
              weaklySelect id
             
        MouseClick -> noCmd <| selectByClick model
           
        MouseMove _ -> 
             weaklySelect <| GraphDefs.closest model.mousePos model.graph             
        MouseDown _ -> noCmd <| { model | mode = RectSelect model.mousePos }
        KeyChanged False _ (Control "Escape") ->
            noCmd <| { model | graph = GraphDefs.clearSelection model.graph
                             } -- , hoverId = Nothing }
        KeyChanged False _ (Character 'e') -> noCmd <| pushHistory { model | mode = EnlargeMode model.mousePos }
        KeyChanged False k (Character 'a') -> 
            if not k.ctrl then
             Modes.NewArrow.initialise <| pushHistory model 
            else
              noCmd <| { model | graph = GraphDefs.selectAll model.graph}
        CopyGraph ->
              (model,
               clipboardWriteGraph <| 
                 LastFormat.toJSGraph 
                   <| Format.GraphInfo.makeGraphInfo
                      (GraphDefs.selectedGraph model.graph)
                       model.sizeGrid)
        KeyChanged False _ (Character 'd') ->
            noCmd <| { model | mode = DebugMode }
        KeyChanged False _ (Character 'g') -> 
            noCmd <| initialiseMoveMode <| pushHistory model
        KeyChanged False _ (Character 'i') -> 
           noCmd <| case GraphDefs.selectedEdgeId model.graph of
                      Just id -> setSaveGraph model <| Graph.invertEdge id model.graph
                      _ -> model
        {- KeyChanged False _ (Character 'I') -> noCmd <| selectInitial model -}
        {- KeyChanged False _ (Character 'E') -> 
           noCmd <| { model | graph = 
              activeObj model |> objToNode 
              |> Maybe.andThen (\id -> 
                   Graph.getNode id model.graph |> Maybe.map (\ n -> { id = id, label = n}))
              |> Maybe.map (GraphProof.selectExtremePath model.graph 0)
              |> Maybe.withDefault [] 
              |> List.foldl (\ e -> Graph.updateEdge e (\n -> {n | selected = True})) 
s                  (GraphDefs.clearSelection model.graph) } -}
        KeyChanged False _ (Character 'L') -> 
           noCmd <| selectLoop True model
        KeyChanged False _ (Character 'K') -> 
           noCmd <| selectLoop False model
        KeyChanged False _ (Character 'G') -> 
           generateProof GraphProof.proofStatementToString            
        KeyChanged False _ (Character 'T') -> 
           generateProof GraphProof.proofStatementToDebugString   
        KeyChanged False _ (Character 'S') ->         
           noCmd <| { model | graph = GraphDefs.selectSurroundingDiagram model.mousePos model.graph }
        KeyChanged False k (Character 'c') -> 
            if k.ctrl then noCmd model -- we don't want to interfer with the copy event C-c
            else if k.alt then noCmd { model | mode = CloneMode } else
            case GraphDefs.selectedEdgeId model.graph of
              Nothing -> noCmd model
              Just id -> noCmd {  model | mode = CutHead { id = id, head = True, duplicate = False } }              
        KeyChanged False _ (Character 'C') -> 
               let gc = GraphDefs.toProofGraph model.graph in
               let s = 
                       GraphDefs.selectedIncompleteDiagram model.graph
                       |> Maybe.andThen
                         (GraphProof.generateIncompleteProofStepFromDiagram gc >> 
                          Maybe.map GraphProof.incompleteProofStepToString 
                         )
                       |> Maybe.withDefault ""
                      
                        
                                                   
               in
                     fillBottom s "No selected subdiagram found!"
        KeyChanged False _ (Character 'R') -> 
            noCmd <| initialise_Resize model
        KeyChanged False _ (Character 'r') -> rename model
        KeyChanged False _ (Character 's') -> 
            Modes.Square.initialise <| pushHistory model 
        
        KeyChanged False _ (Character 'p') -> 
            let (newGraph, newId) = Graph.newNode model.graph 
                    (newNodeLabel model.mousePos "")
                newModel = addOrSetSel False newId
                   <| setSaveGraph model newGraph                    
            in
            noCmd <| initialise_RenameMode [ newId ] newModel
           --noCmd <| { model | mode = NewNode }
        --   KeyChanged False _ (Character 'q') -> ({ model | mode = QuickInputMode Nothing },
        --                                            Msg.focusId quickInputId)

        
        KeyChanged False _ (Character '/') -> Modes.SplitArrow.initialise <| pushHistory model 
        KeyChanged False _ (Character 'x') ->
            noCmd <| setSaveGraph model <| GraphDefs.removeSelected model.graph
                     
        KeyChanged False _ (Control "Delete") ->
            noCmd <| setSaveGraph model <| GraphDefs.removeSelected model.graph            
        {- NodeClick n e ->
            
            let _ = Debug.log "nodeclick" () in
            noCmd <| addOrSetSel e.keys.shift n model -}
        EltDoubleClick n e ->
            noCmd <| initialise_RenameMode [n] model
        {- EdgeClick n e ->
            let _ = Debug.log "edgeclick" () in
             noCmd <| addOrSetSel e.keys.shift n model  -}
        KeyChanged False _ (Character 'f') -> noCmd
              <| setSaveGraph model 
              <| Graph.map 
                (\ _ n -> if n.selected then GraphDefs.snapNodeToGrid model.sizeGrid n else n )
                (always identity) model.graph 
        KeyChanged False _ (Character 'h') -> move pi
        KeyChanged False _ (Character 'j') -> move (pi/2)
        KeyChanged False _ (Character 'k') -> move (3 * pi / 2)
        KeyChanged False _ (Character 'l') -> move 0       
        PasteGraph g -> noCmd <| initialiseMoveMode
               <|  setSaveGraph model <|              
                Graph.union 
                  (GraphDefs.clearSelection model.graph)
                  (GraphDefs.selectAll g.graph)
        KeyChanged False _ (Character 'u') ->
            noCmd <| initialise_RenameMode 
              (GraphDefs.closestUnnamed model.mousePos model.graph)
               <| pushHistory model
             
               
        KeyChanged False k (Character 'z') -> 
             if k.ctrl then noCmd <| undo model else noCmd model
        -- KeyChanged False _ (Character 'n') -> noCmd <| createModel defaultGridSize <| Graph.normalise model.graph
   
        _ ->

            case GraphDefs.selectedEdgeId model.graph of
              Nothing -> noCmd model
              Just id -> 
                 Graph.getEdge id model.graph
                 |> Maybe.andThen (\e ->
                    Msg.mayUpdateArrowStyle msg e.label.style
                   )
                 |> Maybe.map ( \style ->
                  setSaveGraph model <| 
                   Graph.updateEdge id 
                    (\e -> {e | style = style})
                    model.graph
                 )
                 |> Maybe.withDefault model
                 |> noCmd

selectByClick : Model -> Model
selectByClick model =  
    if model.mouseOnCanvas then           
           { model | graph = GraphDefs.addWeaklySelected <|
                      if model.specialKeys.shift then 
                        model.graph 
                      else
                       GraphDefs.clearSelection model.graph }
    else
            model
               
initialiseMoveMode : Model -> Model
initialiseMoveMode model =
         { model | mode = 
                       if GraphDefs.isEmptySelection model.graph
                        then
                          -- Nothing is selected
                          DefaultMode
                        else Move { orig = model.mousePos, pos = InputPosMouse }
                     }    

initialise_Resize : Model -> Model
initialise_Resize model =
         { model | mode = 
                       ResizeMode { sizeGrid = model.sizeGrid,
                                    onlyGrid = False
                                  }
                     }                

update_DebugMode : Msg -> Model -> (Model, Cmd Msg)
update_DebugMode msg model =
    case msg of
        KeyChanged False _ (Control "Escape") -> switch_Default model
        _ -> noCmd model



update_CutHead : CutHeadState -> Msg -> Model -> (Model, Cmd Msg)
update_CutHead state msg m =
  let finalise () = 
         ({m | mode = DefaultMode, graph = graphCutHead state m}, Cmd.none)
         -- computeLayout())
  in
  let changeState s = { m | mode = CutHead s } in
  case msg of
        KeyChanged False _ (Control "Escape") -> ({ m | mode = DefaultMode}, Cmd.none)
        KeyChanged False _ (Control "Enter") -> finalise ()
        MouseClick -> finalise ()
        KeyChanged False _ (Character 'c') -> (changeState { state | head = (not state.head)} , Cmd.none)
        KeyChanged False _ (Character 'd') -> (changeState { state | duplicate = (not state.duplicate)} , Cmd.none)
        _ -> noCmd m

update_Clone : Msg -> Model -> (Model, Cmd Msg)
update_Clone msg m =
  let finalise () = 
        let m2 = pushHistory m in
         ({m2 | mode = DefaultMode, graph = graphClone m}, Cmd.none)
         -- computeLayout())
  in
  case msg of
        KeyChanged False _ (Control "Escape") -> ({ m | mode = DefaultMode}, Cmd.none)
        KeyChanged False _ (Control "Enter") -> finalise ()
        MouseClick -> finalise ()        
        _ -> noCmd m

update_Resize : ResizeState -> Msg -> Model -> (Model, Cmd Msg)
update_Resize st msg m =
  let finalise () = 
        let m2 = pushHistory m in
         ({m2 | mode = DefaultMode, graph = graphResize st m,
                sizeGrid = st.sizeGrid}, Cmd.none)
         -- computeLayout())
  in
  -- let _ = Debug.log "msg resize" msg in
  let increment = 10 in
  let newState st2 = 
       noCmd {m | mode = ResizeMode st2 }
  in
  let newSize si =
       let s = max minSizeGrid <| min maxSizeGrid si in
       newState { st | sizeGrid = s}       
  in
  case msg of
        KeyChanged False _ (Control "Escape") -> ({ m | mode = DefaultMode}, Cmd.none)
        KeyChanged False _ (Control "Enter") -> finalise ()
        -- MouseClick -> finalise ()        
        KeyChanged False _ (Character 'k') -> newSize (st.sizeGrid + increment)
        KeyChanged False _ (Character 'j') -> newSize (st.sizeGrid - increment)
        KeyChanged False _ (Character 'g') -> newState { st | onlyGrid = not st.onlyGrid}
        SizeGrid s -> newSize s
        _ -> noCmd m


-- functions that turns a model graph into one with more information
-- about the display, based on the mode


selectGraph : Model -> Point -> Bool -> Graph NodeLabel EdgeLabel
selectGraph m orig keep = 
   let selRect = (Geometry.makeRect orig m.mousePos) in
   let g = if keep then m.graph else GraphDefs.clearSelection m.graph in
   let isSel n = Geometry.isInRect selRect n.pos in
   GraphDefs.addNodesSelection g isSel
   
enlargeGraph : Model -> Point -> Graph NodeLabel EdgeLabel
enlargeGraph m orig =
   let (ox, oy) = Point.subtract m.mousePos orig in
   let (xi, yi) = orig in
   let mkp n i o = if n >= i then n + o else n in
    
   let mapNode n =
        let (nx, ny) = n.pos in        
          { n | pos = (mkp nx xi ox, mkp ny yi oy)}
         
   in
       
   let g = Graph.map 
            (always mapNode)
            (always identity)
            m.graph
   in
   g

{- enlargeGraph : Model -> Maybe Point -> Graph NodeLabel EdgeLabel
enlargeGraph m orig = 
  orig |> Maybe.map (enlargeGraphSel m) |> Maybe.withDefault m.graph
 -}
graphDrawingFromModel : Model -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawingFromModel m =
    case m.mode of
        DefaultMode -> collageGraphFromGraph m m.graph
        RectSelect p -> GraphDrawing.toDrawingGraph  <| selectGraph m p m.specialKeys.shift
        EnlargeMode p ->
             enlargeGraph m p
             |> collageGraphFromGraph m
--        NewNode -> collageGraphFromGraph m m.graph
        QuickInputMode ch -> collageGraphFromGraph m <| graphQuickInput m ch
        Move s -> info_MoveNode m s |> .graph |>
            collageGraphFromGraph m 
        RenameMode l ->
            let g = graph_RenameMode l m in
            let g2 = collageGraphFromGraph m g in
            case l of
                (id, _) :: _ ->
                    Graph.update id 
                    (\n -> {n | editable = True })
                    (\e -> {e | editable = True })
                    g2
                _ -> g2
              
        DebugMode ->
            m.graph |> collageGraphFromGraph m 
                |> Graph.map
                   (\id n ->  {n | label = String.fromInt id}) 
                   (\_ -> identity)
        NewArrow astate -> Modes.NewArrow.graphDrawing m astate
        SquareMode state ->
            Modes.Square.graphDrawing m state
        SplitArrow state -> Modes.SplitArrow.graphDrawing m state
        CutHead state -> graphCutHead state m |> GraphDrawing.toDrawingGraph
        CloneMode -> graphClone m |> GraphDrawing.toDrawingGraph
        ResizeMode sizeGrid -> graphResize sizeGrid m |> GraphDrawing.toDrawingGraph


graphCutHead : CutHeadState -> Model -> Graph NodeLabel EdgeLabel
graphCutHead {id, head, duplicate} m = 
   let pos = m.mousePos in
    Graph.getEdge id m.graph 
    |> Maybe.andThen (\e -> Graph.getNode (if head then e.to else e.from)
         m.graph 
    |> Maybe.map (\ nto -> 
    let g1 = if duplicate then GraphDefs.unselect id m.graph else Graph.removeEdge id m.graph in
    let label = {nto | pos = pos } in
    let (g2, newId) = Graph.newNode g1 label in
    let (n1, n2) = if head then (e.from, newId) else (newId, e.to) in
    let (g3, _) = Graph.newEdge g2 n1 n2  e.label in
    let g4 = if m.specialKeys.ctrl then 
                     Tuple.first <| 
                     GraphDefs.mergeWithSameLoc
                       { id = newId, label = label }
                       g3
             else g3
    in
    g4
    ))   
    |> Maybe.withDefault m.graph

graphClone : Model -> Graph NodeLabel EdgeLabel
graphClone m =       
   let nodes = allSelectedNodes m in
   let mouseDelta = Point.subtract m.mousePos <| GraphDefs.centerOfNodes nodes in
   GraphDefs.cloneSelected m.graph mouseDelta 

graphResize : ResizeState -> Model -> Graph NodeLabel EdgeLabel
graphResize st m =
   if st.onlyGrid then m.graph else
   let ratio = toFloat st.sizeGrid / toFloat m.sizeGrid in
   Graph.map (\_ n -> { n | pos = Point.resize ratio n.pos}) 
      (\_ -> identity)
    m.graph
   


graphQuickInput : Model -> Maybe QuickInput.Equation -> Graph NodeLabel EdgeLabel
graphQuickInput model ch = 
  case ch of
    Nothing -> model.graph
    Just (eq1, eq2) -> 
      -- if an incomplete subdiagram is selected, we use it
      let od = Debug.log "selected subdiag" <| GraphDefs.selectedIncompleteDiagram model.graph in
      let default = graphDrawingChain model.sizeGrid model.graph (eq1, eq2) in 
      let split l edges = GraphProof.isEmptyBranch l |> 
              Maybe.map (QuickInput.splitWithChain model.graph edges) 
      in
      case od of
        Nothing -> default
        Just d ->           
            Maybe.or (split d.lhs eq2)
                  (split d.rhs eq2)
                    |> Maybe.withDefault default
               

graphDrawingChain : Int -> Graph NodeLabel EdgeLabel -> QuickInput.Equation -> Graph NodeLabel EdgeLabel
graphDrawingChain offset g eq =         
            let mid = toFloat offset / 2 in
            let iniP = (mid, mid) in
            {- graphDrawingEquation -}
            QuickInput.graphEquation iniP (toFloat offset) 
               eq g  -- QuickInput.Right



type HelpStrType = Bold | Plain
helpMsgParser_aux : Parser (String, HelpStrType)
helpMsgParser_aux =
    
    let varParser cend = 
          let correctChar = \ c -> c /= cend in
          Parser.variable { start = correctChar, inner = correctChar, reserved = Set.empty }
    in
    Parser.oneOf [
         Parser.succeed (\ s -> (s , Bold))
             |. Parser.token "["
             |= Parser.oneOf [
               Parser.succeed identity 
               |. Parser.token "\""
               |= varParser '\"'
               |. Parser.token "\""
               ,
              varParser ']']
             |. Parser.symbol "]" ,
         Parser.succeed (\ s -> (s , Plain))
             |= varParser '['
        ]

helpMsgParser : Parser (List (String, HelpStrType))
helpMsgParser =
    Parser.succeed identity
        |= Parser.oneOf [
            Parser.succeed [] |. Parser.end,
            Parser.succeed (::) |= helpMsgParser_aux |= Parser.lazy (\_ -> helpMsgParser)
           ]

helpStr_collage : (String, HelpStrType) -> Html msg
helpStr_collage (s , h) =
    case h of
        Bold -> Html.span [] [Html.text "[", Html.b [] [Html.text s], Html.text"]"]
        Plain -> Html.text s
        

helpMsg : Model -> Html Msg
helpMsg model =
    let cl = Html.Attributes.class "help-div" in
    let makeHelpDiv l = Html.div [ cl ] l in
    let msg = \ s -> s |>
                  Parser.run helpMsgParser |>
                  Result.withDefault [("Parsing help msg error", Plain)] |>
                  List.map helpStr_collage |> makeHelpDiv
    in
    -- let b = \ s -> Html.span [] [Html.text "[", Html.b [] [Html.text s], Html.text"]"]
    -- in
    -- let info = \ s1 s2 -> Html.span [] (b s1) (Html.text s2)
    case model.mode of
        DefaultMode ->
            -- msg <| "Default mode. couc[c]" 
            msg <| "Default mode (the basic tutorial can be completed before reading this). Commands: [click] for point/edge selection (hold for selection rectangle"
                ++ ", rename closest [u]nnamed objects (then [TAB] to alternate)"
                ++ ", [shift] to keep previous selection)" 
                ++ ", [C-a] select all" 
                ++ ", [ESC] clear selection" 
                ++ ", [C-z] undo" 
                ++ ", [C-c] copy selection" 
                ++ ", [C-v] paste" 
                ++ ", [M-c] clone selection (same as C-c C-v)"
                ++ ", new [a]rrow from selected point"
                ++ ", new [p]oint"
                ++ ", new (commutative) [s]quare on selected point (with two already connected edges)"
                ++ ", [del]ete selected object (also [x])"
               --  ++ ", [q]ickInput mode" 
                ++ ", [d]ebug mode" 
                -- ++ ", [u]named flag (no labelling on point creation)" 
                ++ ", [r]ename selected object (or double click)" 
                ++ ", [R]esize canvas and grid size" 
                ++ ", [g] move selected objects (also merge, if wanted)"
                ++ ", [/] split arrow" 
                ++ ", [c]ut head of selected arrow" 
                ++ ", [f]ix (snap) selected objects on the grid" 
                ++ ", [e]nlarge diagram (create row/column spaces)" 
                ++ ", [hjkl] to move the selection from a point to another"                 
                ++ ", if an arrow is selected: [\""
                ++ ArrowStyle.controlChars
                ++ "\"] alternate between different arrow styles, [i]nvert arrow."               
                ++ ", [S]elect pointer surrounding subdiagram"
                ++ ", [G]enerate Coq script ([T]: generate test Coq script)"
                ++ ", [C] generate Coq script to address selected incomplete subdiagram "
                ++ "(i.e., a subdiagram with an empty branch)"
                ++ ", [L] and [K]: select subdiagram adjacent to selected edge"
                
                   
                      -- b "b",
                      -- Html.text "litz flag (no labelling on point creation)."
             -- "[r]ename selected object, move selected point [g], [d]ebug mode"
        DebugMode ->
            "Debug Mode. [ESC] to cancel and come back to the default mode. " ++
              Debug.toString model |> Html.text |> List.singleton |> makeHelpDiv
            {- QuickInputMode ch ->
            makeHelpDiv [
            "Mode: QuickInput" ++ Debug.toString model.mode ++ "." |> Html.text,
                Html.p [] [
                     msg <| " Syntax: v1 -> v2 - edgeLabel >@d v3 vr |down arrow v X | \"uparrow \" ^ end yo."
                    ++ " [RET] to accept the current chain"       
                    ++ ", [ESC] to cancel and comeback to the default mode."]
                ] -}
        NewArrow _ -> "Mode NewArrow. "
                          -- ++ Debug.toString model 
                           ++  Modes.NewArrow.help |> msg
        SquareMode _ -> "Mode Commutative square. "
                             ++ Modes.Square.help |> msg
        SplitArrow _ -> "Mode Split Arrow. "
                             ++ Modes.SplitArrow.help |> msg
        Move _ -> "Mode Move."                
                   
                ++ "Use mouse or h,j,k,l. [RET] or [click] to confirm."
                ++ " Hold [ctrl] to merge the selected point onto another node."                
                  |> msg
        CutHead _ -> "Mode cut arrow."
                ++ " [RET] or [click] to confirm, [ctrl] to merge the endpoint with existing node. [ESC] to cancel. "
                ++ "[c] to switch between head/tail"                
                ++ ", [d] to duplicate (or not) the arrow."
                  |> msg
        RenameMode _ -> msg "Rename mode: [RET] to confirm, [TAB] to next label, [ESC] to cancel"
        EnlargeMode _ -> msg "Enlarge mode: draw a rectangle to create space"
        QuickInputMode _ -> msg <| "Equation mode: enter equation in the textfield "
                          -- ++ "(e.g., a - f ⟩ b - g ⟩ c =  a - h ⟩ d - k ⟩ c)"
                          ++ "(e.g., a -- f -> b -- g -> c =  a -- h -> d -- k -> c)"
                          ++ ",  [RET] to confirm, [ESC] to cancel."
                          ++ " If an incomplete subdiagram (i.e. a subdiagram "
                          ++ "where one branch is a single arrow with empty label)"
                          ++ " is selected, it will replace the empty branch with"
                          ++ " the lhs or the rhs (depending on the orientation)."
        ResizeMode { onlyGrid } -> msg <| "Resize mode. [k]/[j] to increase/decrease, "
                         ++ "or use the slider above. "
                         ++ 
                         if onlyGrid then 
                         "[g] to resize the objects as well as the grid. "
                         else 
                         "[g] to resize the grid only. "
                         ++ "[ESC] to cancel, "
                         ++ "[RET] to confirm"

        _ -> let txt = "Mode: " ++ Debug.toString model.mode ++ ". [ESC] to cancel and come back to the default"
                   ++ " mode."
             in
                makeHelpDiv [ Html.text txt ]

quickInputView : Model -> Html Msg
quickInputView m = 
             Html.p []
                 [
         Html.text "Enter equation: ",
         Html.input  [Html.Attributes.type_ "text",
                       Html.Attributes.id quickInputId,
                     Html.Events.onInput QuickInput,
                     -- Html.Events.onFocus (QuickInput ""),
                     Html.Attributes.value m.quickInput
                     ]
                          -- ++
                          -- case m.mode of
                          --     QuickInputMode _ -> []
                          --     _ -> [Html.Attributes.value ""]
                     []]

additionnalDrawing : Model -> Drawing Msg
additionnalDrawing m = 
   let drawSel orig = Drawing.rect (Geometry.makeRect orig m.mousePos) in
   case m.mode of
      RectSelect orig -> drawSel orig
      EnlargeMode orig -> drawSel orig
      _ -> --GraphDrawing.make_input (100.0,100.0) "coucou" (always Msg.noOp)
          Drawing.empty

view : Model -> Html Msg
view model =
    let missings = Graph.invalidEdges model.graph in
    let drawings= graphDrawing (graphDrawingFromModel model) in
    let grid = if model.hideGrid then Drawing.empty else Drawing.grid (Model.modedSizeGrid model) in
    let nmissings = List.length missings in
    let svg =   Drawing.group [grid,
                 drawings,
                 additionnalDrawing model
              ]
              -- |> debug
              |> Drawing.svg [
                          --   Html.Attributes.style "width" "4000px",
                          --   Html.Attributes.height 4000,
                             Html.Attributes.id HtmlDefs.canvasId,
                             Html.Attributes.style "border-style" "solid",
                             Html.Events.on "mousemove"
                               (D.map2 MouseMoveRaw D.value HtmlDefs.keysDecoder),
                             Html.Events.onMouseLeave MouseLeaveCanvas                               
                            ,                 
                           MouseEvents.onWithOptions "mousedown" 
                            { stopPropagation = False, preventDefault = False }
                            MouseDown
                            -- MouseEvents.onDown MouseDown
                     , Html.Events.onMouseUp MouseUp
                     , Html.Events.on "copy" (D.succeed CopyGraph)
                          --    , Msg.onTabAttribute
                       ]
    in
    let contents = 
          [  Html.button [Html.Events.onClick Save] [Html.text "Save"]
           , Html.button [Html.Events.onClick Clear] [Html.text "Clear"]
            {- , Html.button [Html.Events.onClick (Do <| computeLayout ()),
                   Html.Attributes.title "Should not be necessary"
            ] [Html.text "Recompute labels"] -}
           --  , Html.button [Html.Events.onClick FindInitial] [Html.text "Initial"]
           , HtmlDefs.checkbox ToggleHideGrid "Show grid"  (not model.hideGrid)           
           , Html.button [Html.Events.onClick ExportQuiver] [Html.text "Export selection to quiver"] 
           ]
          ++ 
           (if isResizeMode model.mode then
               [ HtmlDefs.slider SizeGrid "Grid size" minSizeGrid maxSizeGrid (Model.modedSizeGrid model) ]
            else
               [])
          ++ [ Html.text model.statusMsg,
            -- if model.unnamedFlag then Html.p [] [Html.text "Unnamed flag On"] else Html.text "",
            -- if state.blitzFlag then Html.p [] [Html.text "Blitz flag"] else Html.text "",
            Html.p [] 
            [(helpMsg model),
             quickInputView model
            ],
            Html.p [] [ Html.text <| if nmissings > 0 then 
               String.fromInt nmissings ++ " nodes or edges could not be rendered."
               else "" ]
            , HtmlDefs.makePasteCapture (Do << pasteGraph) []
                [   svg  ]
           , Html.p []
           [ Html.textarea [Html.Attributes.cols 100, Html.Attributes.rows 100, 
              Html.Attributes.value model.bottomText, 
              Html.Attributes.id HtmlDefs.bottomTextId
             {- Html.Events.onInput EditBottomText -}]
            [{- Html.text model.bottomText -} ]
           ]
          ]
    in
    Html.div [] contents



main : Program () Model Msg
main = Browser.element { init = \ _ -> (iniModel, Cmd.none),
                         view = view, update = update,
                         subscriptions = subscriptions}
