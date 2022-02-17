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


import Parser exposing ((|.), (|=), Parser)
import Set
import QuickInput exposing (equalityParser, Orient, NonEmptyChain, 
   orientToPoint, orientEquation)

import GraphDrawing exposing (..)
import Msg exposing (Msg(..))

import Tuple
import Maybe exposing (withDefault)

import Modes.Square
import Modes.NewArrow 
import Modes.SplitArrow
import Modes exposing (Mode(..))

import ArrowStyle

import HtmlDefs exposing (Key(..), quickInputId)
import GraphDefs exposing (NodeLabel, EdgeLabel, EdgeLabelJs, NodeLabelJs)
import GraphDefs exposing (newNodeLabel)
import GraphDefs exposing (getNodeLabelOrCreate)
import GraphDefs exposing (newEdgeLabel)
import Html
import Geometry exposing (Rect)
import Geometry exposing (rectEnveloppe)
import Html.Events
import Modes exposing (SplitArrowState)
import InputPosition exposing (InputPosition(..))

import List.Extra
import GraphDefs exposing (exportQuiver, GraphJS)
import GraphProof

-- we tell js about some mouse move event
port onMouseMove : JE.Value -> Cmd a
-- we then get back the relative position of the mouse
-- (this cannot be done in pure elm because it requires
-- to access to the currentTarget field of the event,
-- which is a js object)
port onMouseMoveFromJS : (Point -> a) -> Sub a

port preventDefault : JE.Value -> Cmd a
port onKeyDownActive : (JE.Value -> a) -> Sub a

-- tell js to save the graph
port saveGraph : { graph : GraphJS, fileName : String } -> Cmd a
port savedGraph : (String -> a) -> Sub a
port exportQuiver : JE.Value -> Cmd a
port alert : String -> Cmd a
port jumpToId : String -> Cmd a

-- js tells us to load the graph
port loadedGraph : ({ graph : GraphJS, fileName : String } -> a) -> Sub a

port clipboardWriteGraph : GraphJS -> Cmd a
-- tells JS we got a paste event with such data
port pasteGraph : JE.Value -> Cmd a
-- JS would then calls us back with the decoded graph
port clipboardGraph : (GraphJS -> a) -> Sub a

port computeLayout : () -> Cmd a













subscriptions : Model -> Sub Msg
subscriptions m = Sub.batch 
    [
      -- upload a graph (triggered by js)
      
      loadedGraph (\ r -> Loaded (GraphDefs.jsToGraph r.graph) r.fileName),
      clipboardGraph (GraphDefs.jsToGraph >> PasteGraph),
      savedGraph FileName,
      E.onClick (D.succeed MouseClick),
      {- Html.Events.preventDefaultOn "keydown"
        (D.map (\tab -> if tab then 
                            -- it is necessary to prevent defaults
                            -- otherwise the next immediate appearing input 
                            -- may not shows up
                                      (TabInput, True) 
                        else (Msg.noOp, False))
                         HtmlDefs.tabDecoder), -}
      E.onKeyUp (D.map2 (KeyChanged False) HtmlDefs.keysDecoder HtmlDefs.keyDecoder),
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
                 -- Character 'a' -> checkCtrl
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
    let mkRet movedNodes = { graph = Graph.updateNodes movedNodes model.graph, valid = not merge } in
    let retMerge movedNodes =           
           case movedNodes of
              [ n ] ->         
                case GraphDefs.getNodesAt model.graph n.label.pos of
                  [ i ] -> { graph = Graph.removeLoops <| Graph.merge i n.id model.graph, valid = True }
                  _ -> mkRet movedNodes
              _ -> mkRet movedNodes      
    in       
    let retDelta delta =
            let movedNodes = moveNodes delta in
            if merge then
                retMerge movedNodes
            else
                mkRet movedNodes      
          
    in
   
    let mouseDelta = Point.subtract           
          model.mousePos
          ((Geometry.rectEnveloppe <| List.map (.pos << .label) nodes) |> Geometry.centerRect)
    -- orig 
    in
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
update msg model =
    let m = case msg of
                FileName s -> { model | fileName = s }
                KeyChanged _ r _ -> { model | specialKeys = r }
                MouseMoveRaw _ keys -> { model | specialKeys = keys }
                MouseMove p -> { model | mousePos = p} -- , mouseOnCanvas = True}
                MouseDown e -> { model | specialKeys = e.keys }
                {- FindInitial -> selectInitial model -}
                QuickInput s -> { model | quickInput = s, mode = QuickInputMode Nothing} -- , mouseOnCanvas = False}
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
                _ -> model            
    in
    case msg of
     Save ->               
          (model, saveGraph { graph = GraphDefs.graphToJs model.graph, 
                              fileName = model.fileName})
     Clear -> noCmd iniModel --  (iniModel, Task.attempt (always Msg.noOp) (Dom.focus HtmlDefs.canvasId))
     ToggleHideGrid -> noCmd {model | hideGrid = not model.hideGrid}
     SizeGrid s -> noCmd { model | sizeGrid = s }
     ExportQuiver -> (model,  
                    exportQuiver <| 
                     GraphDefs.exportQuiver model.sizeGrid (GraphDefs.selectedGraph model.graph))
     MouseMoveRaw v _ -> (m, onMouseMove v)
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
     Do cmd -> (m, cmd)
     Loaded g fileName -> noCmd <| { model | graph = g, 
                                             fileName = fileName,
                                             mode = DefaultMode }
     _ ->
      case model.mode of
        QuickInputMode c -> update_QuickInput c msg m 
        DefaultMode -> update_DefaultMode msg m
        RectSelect orig -> update_RectSelect msg orig m.specialKeys.shift m
        EnlargeMode orig -> update_Enlarge msg orig m
        NewArrow astate -> Modes.NewArrow.update astate msg m
            -- update_Modes.NewArrow astate msg m
        RenameMode l -> update_RenameMode l msg m
        Move s -> update_MoveNode msg s m
        DebugMode -> update_DebugMode msg m
        NewNode -> update_NewNode msg m
        SquareMode state -> Modes.Square.update state msg m
        SplitArrow state -> Modes.SplitArrow.update state msg m


update_QuickInput : Maybe QuickInput.Equation -> Msg -> Model -> (Model, Cmd Msg)
update_QuickInput ch msg model =
    case msg of
        KeyChanged False _ (Control "Escape") ->
            ({model | mode = DefaultMode}, 
                 Task.attempt (\_ -> Msg.noOp) (Dom.blur quickInputId))
        KeyChanged False _ (Control "Enter") ->
            ({model | graph = graphDrawingChain model.sizeGrid model.graph ch, 
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
                            Ok l -> (Debug.toString l, l)
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
      MouseUp -> switch_Default 
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
     let g = GraphProof.convertGraph model.graph in
     let edges =  GraphDefs.selectedEdgeId model.graph 
              |> Maybe.andThen (\id -> 
                   Graph.getEdge id g) -- |> Maybe.map (\ e -> { id = id, label = e.label}))
              |> Maybe.map (GraphProof.loopFrom direction g)
              |> Maybe.withDefault []
     in
     let diag = GraphProof.loopToDiagram edges in
    --  let _ = Debug.log "lhs" (diag.lhs |> List.map (.label >> .label)) in
    --  let _ = Debug.log "rhs" (diag.rhs |> List.map (.label >> .label)) in
     let _ = Debug.log "isBorder?" (GraphProof.isBorder diag) in     
            { model | graph = edges |> List.map (Tuple.first >> .id)            
              |> List.foldl (\ e -> Graph.updateEdge e (\n -> {n | selected = True})) 
                  (GraphDefs.clearSelection model.graph) }

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
    let generateProof stToString =
           let s = String.join "\n\n"
                 <| List.map stToString 
                 <| GraphProof.fullProofs
                 <| GraphProof.convertGraph model.graph
           in
           let c = if s == "" then alert "No diagram found!"
                   else jumpToId HtmlDefs.bottomTextId
           in
           ({ model | bottomText = s }, c)
    in
    {- let updateStr =
       GraphProof.proofStatementToString  -}
    -- Tuples.mapFirst (changeModel model) <|
    case msg of
       
        MouseDown _ -> noCmd <| { model | mode = RectSelect model.mousePos }
        KeyChanged False _ (Character 'e') -> noCmd <| { model | mode = EnlargeMode model.mousePos }
        KeyChanged False k (Character 'a') -> 
            -- if not k.ctrl then
             Modes.NewArrow.initialise model 
            -- else
            --  noCmd <| { model | graph = GraphDefs.selectAll model.graph}
        CopyGraph ->
              (model,
               clipboardWriteGraph <| 
                 GraphDefs.graphToJs <| GraphDefs.selectedGraph model.graph)
        KeyChanged False _ (Character 'd') ->
            noCmd <| { model | mode = DebugMode }
        KeyChanged False _ (Character 'g') -> 
            noCmd <| initialiseMoveMode model
        KeyChanged False _ (Character 'i') -> 
           noCmd <| case GraphDefs.selectedEdgeId model.graph of
                      Just id -> { model | graph = Graph.invertEdge id model.graph }                                         
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
                    
        
        KeyChanged False _ (Character 'r') -> 
            let ids = GraphDefs.selectedId model.graph 
                      |> Maybe.map List.singleton 
                      |> Maybe.withDefault []
            in
            noCmd <| initialise_RenameMode ids model
        KeyChanged False _ (Character 's') -> Modes.Square.initialise model 
        
        KeyChanged False _ (Character 'p') -> noCmd <| { model | mode = NewNode }
        --   KeyChanged False _ (Character 'q') -> ({ model | mode = QuickInputMode Nothing },
        --                                            Msg.focusId quickInputId)

        KeyChanged False _ (Character 'x') ->
            noCmd <| { model | graph = GraphDefs.removeSelected model.graph} 
        
        KeyChanged False _ (Character '/') -> Modes.SplitArrow.initialise model 
                     
        KeyChanged False _ (Control "Delete") ->
            noCmd <| { model | graph = GraphDefs.removeSelected model.graph }
        NodeClick n e ->
            noCmd <| addOrSetSel e.keys.shift n model
        EdgeClick n e ->
             noCmd <| addOrSetSel e.keys.shift n model 
        KeyChanged False _ (Character 'f') -> noCmd 
             { model | graph = Graph.map 
                (\ _ n -> if n.selected then GraphDefs.snapNodeToGrid model.sizeGrid n else n )
                (always identity) model.graph } 
        KeyChanged False _ (Character 'h') -> move pi
        KeyChanged False _ (Character 'j') -> move (pi/2)
        KeyChanged False _ (Character 'k') -> move (3 * pi / 2)
        KeyChanged False _ (Character 'l') -> move 0       
        PasteGraph g -> noCmd <| initialiseMoveMode
              {
              model | graph = 
                Graph.union 
                  (GraphDefs.clearSelection model.graph)
                  (GraphDefs.selectAll g)
               }        
        -- KeyChanged False _ (Character 'n') -> noCmd <| createModel defaultGridSize <| Graph.normalise model.graph
   
        _ ->

            case GraphDefs.selectedEdgeId model.graph of
              Nothing -> noCmd model
              Just id -> noCmd 
                { model | graph =
                  Graph.updateEdge id 
                    (\e -> {e | style = Msg.updateArrowStyle msg e.style})
                    model.graph
                }
               
initialiseMoveMode : Model -> Model
initialiseMoveMode model =
         { model | mode = 
                       if GraphDefs.isEmptySelection model.graph
                        then
                          -- Nothing is selected
                          DefaultMode
                        else Move { orig = model.mousePos, pos = InputPosMouse }
                     }                 

update_DebugMode : Msg -> Model -> (Model, Cmd Msg)
update_DebugMode msg model =
    case msg of
        KeyChanged False _ (Control "Escape") -> switch_Default model
        _ -> noCmd model

update_NewNode : Msg -> Model -> (Model, Cmd Msg)
update_NewNode msg m =
    case msg of
       MouseClick ->
         let (newGraph, newId) = Graph.newNode m.graph 
               (newNodeLabel m.mousePos "")
             newModel = addOrSetSel False newId
                    {m | graph = newGraph  }
         in
         -- if m.unnamedFlag then
         --   switch_Default newModel
         -- else
           noCmd <| initialise_RenameMode [ newId ] newModel
       KeyChanged False _ (Control "Escape") -> switch_Default m
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
        RectSelect p -> collageGraphFromGraph m <| selectGraph m p m.specialKeys.shift
        EnlargeMode p ->
             enlargeGraph m p
             |> collageGraphFromGraph m
        NewNode -> collageGraphFromGraph m m.graph
        QuickInputMode ch -> collageGraphFromGraph m <| graphDrawingChain m.sizeGrid m.graph ch
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


graphDrawingChain : Int -> Graph NodeLabel EdgeLabel -> Maybe QuickInput.Equation -> Graph NodeLabel EdgeLabel
graphDrawingChain offset g ch = 
    case ch of
        Nothing -> g
        Just nonEmptyCh ->
            let mid = toFloat offset / 2 in
            let iniP = (mid, mid ) in
            graphDrawingEquation (toFloat offset) g 
               (orientEquation nonEmptyCh)  iniP -- QuickInput.Right

graphDrawingEquation : Float -> Graph NodeLabel EdgeLabel -> QuickInput.Equation -> Point -- -> QuickInput.Orient
                    -> Graph NodeLabel EdgeLabel
graphDrawingEquation offset g (e1, e2) loc -- defOrient
    =
    let (g2, startId, finalId) = graphDrawingNonEmptyChain Nothing offset g e1 loc in
    case e2 of
       QuickInput.Singleton _ -> g2
       QuickInput.Cons _ (QuickInput.Edge olabel oorient) tail ->
            let (g3, _, _) = drawChainCons (Just finalId) offset g2 startId loc olabel oorient tail loc in
             g3



graphDrawingNonEmptyChain : Maybe NodeId -> Float -> Graph NodeLabel EdgeLabel -> NonEmptyChain -> Point -- -> QuickInput.Orient
                    -> (Graph NodeLabel EdgeLabel, NodeId, NodeId)
graphDrawingNonEmptyChain finalTarget offset g ch loc -- defOrient
    =
    case ch of
        QuickInput.Singleton v ->  
                case finalTarget of
                    Just id -> (g, id, id)
                    Nothing -> let (g2, source, _ ) = GraphDefs.createNodeLabel g v loc in -- getNodeLabelOrCreate g v loc in
                                  (g2, source, source)
                                   
        QuickInput.Cons v (QuickInput.Edge olabel oorient) tail ->
            let (g2, source, source_pos) = GraphDefs.createNodeLabel  g v loc in -- getNodeLabelOrCreate g v loc in
                drawChainCons finalTarget offset g2 source source_pos olabel oorient tail loc

drawChainCons : Maybe NodeId -> Float -> Graph NodeLabel EdgeLabel 
      -> NodeId -> Point
      ->  Maybe String -> Maybe QuickInput.Orient
      -> NonEmptyChain -> Point -- -> QuickInput.Orient
                    -> (Graph NodeLabel EdgeLabel, NodeId, NodeId)
drawChainCons finalTarget offset g2 source source_pos olabel oorient tail loc =
            -- let (g2, source, source_pos) = GraphDefs.createNodeLabel  g v loc in -- getNodeLabelOrCreate g v loc in
            -- let defOrient = QuickInput.Right in
            let orient = withDefault QuickInput.defOrient oorient in
            -- length of arrows
            
            let newPoint = Point.add source_pos <| Point.resize offset <| (orientToPoint orient) in
            let (g3, target, finalId) = graphDrawingNonEmptyChain finalTarget offset g2 tail newPoint -- orient
            in
            let label = withDefault "" olabel in

            (Tuple.first <| Graph.newEdge g3 source target
               <| GraphDefs.newEdgeLabel label ArrowStyle.empty            
            , source, finalId)          



type HelpStrType = Bold | Plain
helpMsgParser_aux : Parser (String, HelpStrType)
helpMsgParser_aux =
    let correctChar = \ c -> c /= '[' && c /= ']' in
    let varParser = Parser.variable { start = correctChar, inner = correctChar, reserved = Set.empty }
    in
    Parser.oneOf [
         Parser.succeed (\ s -> (s , Bold))
             |. Parser.symbol "["
             |= varParser
             |. Parser.symbol "]" ,
         Parser.succeed (\ s -> (s , Plain))
             |= varParser
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
            msg <| "Default mode (the basic tutorial can be completed before reading this). Commands: [click] for point/edge selection (hold for selection rectangle, "
                ++ "[shift] to keep previous selection)" 
                -- ++ ", [C-a] select all" 
                ++ ", [C-c] copy selection" 
                ++ ", [C-v] paste" 
                ++ ", new [a]rrow from selected point"
                ++ ", new [p]oint"
                ++ ", new (commutative) [s]quare on selected point (with two already connected edges)"
                ++ ", [del]ete selected object (also [x])"
               --  ++ ", [q]ickInput mode" 
                ++ ", [d]ebug mode" 
                -- ++ ", [u]named flag (no labelling on point creation)" 
                ++ ", [r]ename selected object" 
                ++ ", [g] move selected objects (also merge, if wanted)"
                ++ ", [/] split arrow" 
                ++ ", [f]ix (snap) selected objects on the grid" 
                ++ ", [e]nlarge diagram (create row/column spaces)" 
                ++ ", [hjkl] to move the selection from a point to another"                 
                ++ ", if an arrow is selected: [(,=,b,B,-,>] alternate between different arrow styles, [i]nvert arrow."
                ++ ", [L] and [K]: select subdiagram adjacent to selected edge"
                ++ ", [G]enerate Coq script ([T]: generate test Coq script)"
                   
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
        RenameMode _ -> msg "Rename mode: [RET] to confirm, [TAB] to next label, [ESC] to cancel"
        EnlargeMode _ -> msg "Enlarge mode: draw a rectangle to create space"
        QuickInputMode _ -> msg <| "Equation mode: enter equation in the textfield "
                          ++ "(e.g., a - f ⟩ b - g ⟩ c =  a - h ⟩ d - k ⟩ c)"
                          ++ ",  [RET] to confirm, [ESC] to cancel."

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
    let grid = if model.hideGrid then Drawing.empty else Drawing.grid model.sizeGrid in
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
                             (D.map2 MouseMoveRaw D.value HtmlDefs.keysDecoder)                   
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
    Html.div [] [
           Html.button [Html.Events.onClick Save] [Html.text "Save"]
         , Html.button [Html.Events.onClick Clear] [Html.text "Clear"]
         {- , Html.button [Html.Events.onClick (Do <| computeLayout ()),
                Html.Attributes.title "Should not be necessary"
         ] [Html.text "Recompute labels"] -}
        --  , Html.button [Html.Events.onClick FindInitial] [Html.text "Initial"]
         , HtmlDefs.checkbox ToggleHideGrid "Show grid"  (not model.hideGrid)           
        , Html.button [Html.Events.onClick ExportQuiver] [Html.text "Export selection to quiver"]
         , HtmlDefs.slider SizeGrid "Grid size" 2 500 model.sizeGrid
          ,   Html.text model.statusMsg,
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



main : Program () Model Msg
main = Browser.element { init = \ _ -> (iniModel, Cmd.none),
                         view = view, update = update,
                         subscriptions = subscriptions}
