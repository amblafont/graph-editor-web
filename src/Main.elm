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
-- import QuickInput exposing (chainParser, NonEmptyChain, orientToPoint)

import GraphDrawing exposing (..)
import Msg exposing (Msg(..))

import Tuple
import Maybe exposing (withDefault)

import Modes.Square
import Modes.NewArrow 
import Modes.SplitArrow
import Modes exposing (Mode(..))

import ArrowStyle

import HtmlDefs exposing (Key(..))
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
import GraphDefs exposing (exportQuiver)

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
port saveGraph : (List (Node NodeLabelJs) , List (Edge EdgeLabelJs)) -> Cmd a
port exportQuiver : JE.Value -> Cmd a

-- js tells us to load the graph
port loadedGraph : ((List (Node NodeLabelJs) , List (Edge EdgeLabelJs)) -> a) -> Sub a














subscriptions : Model -> Sub Msg
subscriptions m = Sub.batch 
    [
      -- upload a graph (triggered by js)
      
      loadedGraph (\ (ns,es) -> Graph.fromNodesAndEdges ns es
                       |> Graph.map 
                          (\_ -> GraphDefs.nodeLabelFromJs)
                          (\_ -> GraphDefs.edgeLabelFromJs)
                       |> Loaded),
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
               case k of
                 Character '/' ->
                    case m.mode of
                      DefaultMode  -> Do <| preventDefault e
                      SplitArrow _ -> Do <| preventDefault e
                      _ -> Msg.noOp 
                 Character 'a' ->
                    if ks.ctrl && m.mode == DefaultMode then
                      Do <| preventDefault e
                    else Msg.noOp
                 _ -> Msg.noOp
                
                )                
                HtmlDefs.keysDecoder
                HtmlDefs.keyDecoder)
                |> Result.withDefault Msg.noOp
                )
    ]












-- Model -----------------------------------------------------------------------





-- The graphs are updated according to the current mode





graph_RenameMode : String -> List Graph.Id -> Model -> Graph NodeLabel EdgeLabel
graph_RenameMode s l m = 
   case l of
      [] -> m.graph
      id :: _ ->   Graph.update id 
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
                  [ i ] -> { graph = Graph.merge i n.id model.graph, valid = True }
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
    let
       m = case msg of            
             KeyChanged _ r _ -> { model | specialKeys = r }
             MouseMoveRaw _ keys -> { model | specialKeys = keys }
             MouseMove p -> { model | mousePos = p} -- , mouseOnCanvas = True}
             MouseDown e -> { model | specialKeys = e.keys }
           --  QuickInput s -> { model | quickInput = s, mode = QuickInputMode Nothing} -- , mouseOnCanvas = False}
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
          let g =  model.graph 
                   |> Graph.map 
                    (\_ -> GraphDefs.nodeLabelToJs)
                    (\_ -> GraphDefs.edgeLabelToJs) 
                   |> Graph.normalise           
          in
          let nodes = Graph.nodes g
              edges = Graph.edges g
          in
          (model, saveGraph (nodes, edges))
     Clear -> noCmd iniModel --  (iniModel, Task.attempt (always Msg.noOp) (Dom.focus HtmlDefs.canvasId))
     ToggleHideGrid -> noCmd {model | hideGrid = not model.hideGrid}
     SizeGrid s -> noCmd { model | sizeGrid = s }
     ExportQuiver -> (model, exportQuiver <| GraphDefs.exportQuiver model.sizeGrid model.graph)
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
     Loaded g -> noCmd <| createModel defaultGridSize g
     _ ->
      case model.mode of
        -- QuickInputMode c -> update_QuickInput c msg m 
        DefaultMode -> update_DefaultMode msg m
        RectSelect orig -> update_RectSelect msg orig m.specialKeys.shift m
        NewArrow astate -> Modes.NewArrow.update astate msg m
            -- update_Modes.NewArrow astate msg m
        RenameMode s l -> update_RenameMode s l msg m
        Move s -> update_MoveNode msg s m
        DebugMode -> update_DebugMode msg m
        NewNode -> update_NewNode msg m
        SquareMode state -> Modes.Square.update state msg m
        SplitArrow state -> Modes.SplitArrow.update state msg m

{- 
update_QuickInput : Maybe NonEmptyChain -> Msg -> Model -> (Model, Cmd Msg)
update_QuickInput ch msg model =
    case msg of
        KeyChanged False _ (Control "Escape") ->
            ({model | mode = DefaultMode}, 
                 Task.attempt (\_ -> Msg.noOp) (Dom.blur quickInputId))
        KeyChanged False _ (Control "Enter") ->
            switch_Default {model | graph = graphDrawingChain model.graph ch, quickInput = ""}
        QuickInput s ->
                let (statusMsg, chain) =
                        case Parser.run chainParser s of
                            Ok l -> (Debug.toString l, l)
                            Err e -> (Parser.deadEndsToString e, Nothing)
                in
                noCmd {model | statusMsg = statusMsg, mode = QuickInputMode chain} -- , mouseOnCanvas = False}
        _ -> noCmd model
-}

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




        
update_RenameMode : String -> List Graph.Id -> Msg -> Model -> (Model, Cmd Msg)
update_RenameMode label ids msg model =
    case msg of
      KeyChanged False _ (Control "Escape") -> switch_Default model
      KeyChanged False _ (Control "Enter") -> noCmd <| next_RenameMode True label ids model
      KeyChanged False _ (Control "Tab") -> noCmd <| next_RenameMode False label ids model
    --   MouseClick -> finalise_RenameMode label model
      NodeLabelEdit _ s -> noCmd {model | mode = RenameMode s ids}
      EdgeLabelEdit _ s -> noCmd {model | mode = RenameMode s ids}
      _ -> noCmd model



next_RenameMode : Bool -> String -> List Graph.Id -> Model -> Model
next_RenameMode finish label ids model =
    let g = graph_RenameMode label ids model in
    let m2 =  {model | graph = g} in
    if finish then
      { m2 | mode = DefaultMode }
    else
      case ids of
        [] ->   { m2 | mode = DefaultMode }
        _ :: q -> initialise_RenameMode q m2
        

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

update_DefaultMode : Msg -> Model -> (Model, Cmd Msg)
update_DefaultMode msg model =
    let delta_angle = pi / 5 in    
    let move angle = 
               activeObj model
                 |> objToNode
                 |> Maybe.andThen (\id -> Graph.getNode id model.graph) 
                 |> Maybe.map .pos
                 |> Maybe.andThen (\p ->
                    Graph.filterNodes model.graph 
                      (\ n -> n.pos /= p && (Point.subtract n.pos p |> Point.pointToAngle |>
                      Point.angleWithInRange delta_angle angle  ))
                    |>
                    List.Extra.minimumBy (.label >> .pos >> Point.distance p )                  
                
                 )
                 |> Maybe.map (\n ->
                 Model.addOrSetSel False (ONode n.id) model
                 )                 
                 |> Maybe.withDefault model
                 |> noCmd
    in
    -- Tuples.mapFirst (changeModel model) <|
    case msg of
       
        MouseDown _ -> noCmd <| { model | mode = RectSelect model.mousePos }
        KeyChanged False k (Character 'a') -> 
            if not k.ctrl then
             Modes.NewArrow.initialise model 
            else
             noCmd <| { model | graph = GraphDefs.selectAll model.graph}
        KeyChanged False _ (Character 'c') ->  noCmd <|
           initialiseMoveMode {model | graph = GraphDefs.cloneSelected model.graph (30, 30)}
        KeyChanged False _ (Character 'd') ->
            noCmd <| { model | mode = DebugMode }
        KeyChanged False _ (Character 'g') -> 
            noCmd <| initialiseMoveMode model
        KeyChanged False _ (Character 'i') -> 
           noCmd <| case activeObj model of
                      OEdge id -> { model | graph = Graph.invertEdge id model.graph }                                         
                      _ -> model
        
        KeyChanged False _ (Character 'r') -> 
            let ids = activeObj model |> objId |> Maybe.map List.singleton 
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
            noCmd <| addOrSetSel e.keys.shift (ONode n) model
        EdgeClick n e ->
             noCmd <| addOrSetSel e.keys.shift (OEdge n) model 
        KeyChanged False _ (Character 'f') -> noCmd 
             { model | graph = Graph.map 
                (\ _ n -> if n.selected then GraphDefs.snapNodeToGrid model.sizeGrid n else n )
                (always identity) model.graph } 
        KeyChanged False _ (Character 'h') -> move pi
        KeyChanged False _ (Character 'j') -> move (pi/2)
        KeyChanged False _ (Character 'k') -> move (3 * pi / 2)
        KeyChanged False _ (Character 'l') -> move 0
        -- KeyChanged False _ (Character 'n') -> noCmd <| createModel defaultGridSize <| Graph.normalise model.graph
   
        _ ->

            case objToEdge <| activeObj model of
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
                       if selectedObjs model |> List.isEmpty
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
             newModel = addOrSetSel False (ONode newId)
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
   


graphDrawingFromModel : Model -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawingFromModel m =
    case m.mode of
        DefaultMode -> collageGraphFromGraph m m.graph
        RectSelect p -> collageGraphFromGraph m <| selectGraph m p m.specialKeys.shift
        NewNode -> collageGraphFromGraph m m.graph
     --   QuickInputMode ch -> collageGraphFromGraph m <| graphDrawingChain m.graph ch
        Move s -> info_MoveNode m s |> .graph |>
            collageGraphFromGraph m 
        RenameMode s l ->
            let g = graph_RenameMode s l m in
            let g2 = collageGraphFromGraph m g in
            case l of
                id :: _ ->
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

{-
graphDrawingChain : Graph NodeLabel EdgeLabel -> Maybe NonEmptyChain -> Graph NodeLabel EdgeLabel
graphDrawingChain g ch = 
    case ch of
        Nothing -> g
        Just nonEmptyCh ->
            let iniP = (400, 200) in
            Tuple.first <| graphDrawingNonEmptyChain g nonEmptyCh iniP -- QuickInput.Right


graphDrawingNonEmptyChain : Graph NodeLabel EdgeLabel -> NonEmptyChain -> Point -- -> QuickInput.Orient
                    -> (Graph NodeLabel EdgeLabel, NodeId)
graphDrawingNonEmptyChain g ch loc -- defOrient
    =
    case ch of
        QuickInput.Singleton v -> let (g2, source, _ ) = getNodeLabelOrCreate g v loc in
                                  (g2, source)
        QuickInput.Cons v (QuickInput.Edge olabel oorient) tail ->
            let defOrient = QuickInput.Right in
            let (g2, source, source_pos) = getNodeLabelOrCreate g v loc in
            let orient = withDefault defOrient oorient in
            -- length of arrows
            let offset = 200 in
            let newPoint = Point.add source_pos <| Point.resize offset <| (orientToPoint orient) in
            let (g3, target) = graphDrawingNonEmptyChain g2 tail newPoint -- orient
            in
            let label = withDefault "" olabel in

            (Tuple.first <| Graph.newEdge g3 source target
               <| GraphDefs.newEdgeLabel label ArrowStyle.empty            
            , source)

-}

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
                ++ ", [C-a] select all" 
                ++ ", new [a]rrow from selected point"
                ++ ", new [p]oint"
                ++ ", new (commutative) [s]quare on selected point (with two already connected edges)"
                ++ ", [del]ete selected object (also [x])"
               --  ++ ", [q]ickInput mode" 
                ++ ", [d]ebug mode" 
                -- ++ ", [u]named flag (no labelling on point creation)" 
                ++ ", [r]ename selected object" 
                ++ ", [g] move selected objects (also merge, if wanted)" 
                ++ ", [c]lone selected objects" 
                ++ ", [/] split arrow" 
                ++ ", [f]ix (snap) selected objects on the grid" 
                ++ ", [hjkl] to move the selection from a point to another"                 
                ++ ", if an arrow is selected: [(,=,b,B,-,>] alternate between different arrow styles, [i]nvert arrow."
                   
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
        RenameMode _ _ -> msg "Rename mode: [RET] to confirm, [TAB] to next label, [ESC] to cancel"


        _ -> let txt = "Mode: " ++ Debug.toString model.mode ++ ". [ESC] to cancel and come back to the default"
                   ++ " mode."
             in
                makeHelpDiv [ Html.text txt ]

{- quickInputView : Model -> Html Msg
quickInputView m = 
             Html.p []
                 [
         Html.text "QuickInput",
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
 -}
additionnalDrawing : Model -> Drawing Msg
additionnalDrawing m = 
   case m.mode of
      RectSelect orig -> Drawing.rect (Geometry.makeRect orig m.mousePos)
      _ -> --GraphDrawing.make_input (100.0,100.0) "coucou" (always Msg.noOp)
          Drawing.empty

view : Model -> Html Msg
view model =
    let missings = Graph.invalidEdges model.graph in
    let drawings= graphDrawing (graphDrawingFromModel model) in
    let grid = if model.hideGrid then Drawing.empty else Drawing.grid model.sizeGrid in
    let nmissings = List.length missings in
    Html.div [] [
         Html.button [Html.Events.onClick Save] [Html.text "Save"]
         , Html.button [Html.Events.onClick Clear] [Html.text "Clear"]
         , HtmlDefs.checkbox ToggleHideGrid "Show grid"  (not model.hideGrid)           
        , Html.button [Html.Events.onClick ExportQuiver] [Html.text "Export to quiver"]
         , HtmlDefs.slider SizeGrid "Grid size" 2 500 model.sizeGrid
          ,   Html.text model.statusMsg,
         -- if model.unnamedFlag then Html.p [] [Html.text "Unnamed flag On"] else Html.text "",
         -- if state.blitzFlag then Html.p [] [Html.text "Blitz flag"] else Html.text "",
         Html.p [] 
         [(helpMsg model){- ,
          quickInputView model -}
         ],
         Html.p [] [ Html.text <| if nmissings > 0 then 
            String.fromInt nmissings ++ " nodes or edges could not be rendered."
            else "" ]
         ,
    Drawing.group [grid,
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
                --    , Msg.onTabAttribute
             ]
             ]



main : Program () Model Msg
main = Browser.element { init = \ _ -> (iniModel, Cmd.none),
                         view = view, update = update,
                         subscriptions = subscriptions}
