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
import QuickInput exposing (chainParser, NonEmptyChain, orientToPoint)

import GraphDrawing exposing (..)
import Msg exposing (Msg(..))

import Tuple
import Maybe exposing (withDefault)

import Modes.Square
import Modes.NewArrow 
import Modes.SplitArrow
import Modes exposing (Mode(..))

import ArrowStyle

import HtmlDefs exposing (quickInputId, Key(..))
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

-- we tell js about some mouse move event
port onMouseMove : JE.Value -> Cmd a
-- we then get back the relative position of the mouse
-- (this cannot be done in pure elm because it requires
-- to access to the currentTarget field of the event,
-- which is a js object)
port onMouseMoveFromJS : (Point -> a) -> Sub a

port preventDefault : JE.Value -> Cmd a
port onSlashKey : (JE.Value -> a) -> Sub a

-- tell js to save the graph
port saveGraph : (List (Node NodeLabelJs) , List (Edge EdgeLabelJs)) -> Cmd a
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
      E.onKeyUp (D.map (KeyChanged False) HtmlDefs.keyDecoder),
      onMouseMoveFromJS MouseMove,
      onSlashKey 
           ( \e -> 
              case m.mode of
                DefaultMode -> Do <| preventDefault e
                _ -> Msg.noOp )
    ]










save : Model -> Msg
save model = 
   let g =  model.graph 
            |> Graph.map 
             (\_ -> GraphDefs.nodeLabelToJs)
             (\_ -> GraphDefs.edgeLabelToJs)            
   in
   let nodes = Graph.nodes g
       edges = Graph.edges g
    in
   Do (saveGraph (nodes, edges))

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

graph_MoveNode : Model -> Modes.MoveState -> Graph NodeLabel EdgeLabel
graph_MoveNode model { orig, pos } =
    let nodes = allSelectedNodes model in
    let delta = 
          case pos of
            InputPosKeyboard p -> InputPosition.deltaKeyboardPos p
            _ -> 
              Point.subtract model.mousePos orig
    in
    (nodes |> List.map .id
           |> Graph.updateNodes) 
           ( \n -> { n | pos = Point.add n.pos delta })
     model.graph






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
            MouseMove p -> { model | mousePos = p} -- , mouseOnCanvas = True}
            QuickInput s -> { model | quickInput = s, mode = QuickInputMode Nothing} -- , mouseOnCanvas = False}
                    -- {model | mousePos = (x, y), statusMsg = "mouse " ++ Debug.toString (x, y)}
            -- KeyChanged False s -> {model | statusMsg = keyToString s}
            -- NodeClick n -> {model | statusMsg = "point " }
            -- NodeEnter n -> 
            --    let _ = Debug.log "ici" n in
            --   { model | mousePointOver = ONode n}
            -- NodeLeave n -> { model | mousePointOver = ONothing}
            NodeRendered n dims ->
                -- let _ = Debug.log "nouvelle dims !" (n, dims) in
                { model | -- statusMsg = "newsize " ++ Debug.toString (n, dims),
                      graph = 
                      Graph.updateNode n (\l -> {l | dims = Just dims }) model.graph                      
                }
            EdgeRendered e dims ->
                -- let _ = Debug.log "nouvelle dims !" (e, dims) in
                { model | -- statusMsg = "newsize " ++ Debug.toString (e, dims),
                      graph = 
                      Graph.updateEdge e (\l -> {l | dims = Just dims }) model.graph                      
                }
            -- MouseClick -> let _ = Debug.log "Mouse Click !" () in model
            _ -> model            
    in
    case msg of
     Do cmd -> (m, cmd)
     Loaded g -> noCmd <| createModel g
     _ ->
      case model.mode of
        QuickInputMode c -> update_QuickInput c msg m 
        DefaultMode -> update_DefaultMode msg m
        RectSelect orig keep -> update_RectSelect msg orig keep m
        NewArrow astate -> Modes.NewArrow.update astate msg m
            -- update_Modes.NewArrow astate msg m
        RenameMode s l -> update_RenameMode s l msg m
        Move s -> update_MoveNode msg s m
        DebugMode -> update_DebugMode msg m
        NewNode -> update_NewNode msg m
        SquareMode state -> Modes.Square.update state msg m
        SplitArrow state -> Modes.SplitArrow.update state msg m


update_QuickInput : Maybe NonEmptyChain -> Msg -> Model -> (Model, Cmd Msg)
update_QuickInput ch msg model =
    case msg of
        KeyChanged False (Control "Escape") ->
            ({model | mode = DefaultMode}, 
                 Task.attempt (\_ -> Msg.noOp) (Dom.blur quickInputId))
        KeyChanged False (Control "Enter") ->
            switch_Default {model | graph = graphDrawingChain model.graph ch, quickInput = ""}
        QuickInput s ->
                let (statusMsg, chain) =
                        case Parser.run chainParser s of
                            Ok l -> (Debug.toString l, l)
                            Err e -> (Parser.deadEndsToString e, Nothing)
                in
                noCmd {model | statusMsg = statusMsg, mode = QuickInputMode chain} -- , mouseOnCanvas = False}
        _ -> noCmd model

update_MoveNode : Msg -> Modes.MoveState -> Model -> (Model, Cmd Msg)
update_MoveNode msg state model =
    let movedRet = switch_Default {model | graph = graph_MoveNode model state} in
    case msg of
        -- MouseMove pageX pageY -> { model | graph = g}
        KeyChanged False (Control "Escape") -> switch_Default model
        MouseClick -> movedRet
        KeyChanged False (Control "Enter") -> movedRet
        _ ->       
            noCmd { model | mode = Move { state | 
                             pos = InputPosition.update state.pos msg }}




        
update_RenameMode : String -> List Graph.Id -> Msg -> Model -> (Model, Cmd Msg)
update_RenameMode label ids msg model =
    case msg of
      KeyChanged False (Control "Escape") -> switch_Default model
      KeyChanged False (Control "Enter") -> noCmd <| next_RenameMode True label ids model
      KeyChanged False (Control "Tab") -> noCmd <| next_RenameMode False label ids model
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
      KeyChanged False (Control "Escape") -> switch_Default model
      MouseUp -> switch_Default 
                  { model | graph = selectGraph model orig keep }
      -- au cas ou le click n'a pas eu le temps de s'enregistrer
    --   NodeClick n -> switch_Default { model | selectedObjs = [ONode n]} 
    --   EdgeClick n -> switch_Default { model | selectedObjs = [OEdge n]}
      _ -> noCmd model

update_DefaultMode : Msg -> Model -> (Model, Cmd Msg)
update_DefaultMode msg model =
    -- Tuples.mapFirst (changeModel model) <|
    case msg of
        MouseDown e -> noCmd <| { model | mode = RectSelect model.mousePos e.keys.shift }
        KeyChanged False (Character 'a') -> Modes.NewArrow.initialise model
        KeyChanged False (Character 'c') ->  noCmd <|
           initialiseMoveMode {model | graph = GraphDefs.cloneSelected model.graph (30, 30)}
        KeyChanged False (Character 'd') ->
            noCmd <| { model | mode = DebugMode }
        KeyChanged False (Character 'g') -> 
            noCmd <| initialiseMoveMode model
        KeyChanged False (Character 'i') -> 
           noCmd <| case activeObj model of
                      OEdge id -> { model | graph = Graph.invertEdge id model.graph }                                         
                      _ -> model
        
        KeyChanged False (Character 'r') -> 
            let ids = activeObj model |> objId |> Maybe.map List.singleton 
                     |> Maybe.withDefault []
            in
            noCmd <| initialise_RenameMode ids model
        KeyChanged False (Character 's') -> Modes.Square.initialise model 
        
        KeyChanged False (Character 'p') -> noCmd <| { model | mode = NewNode }
        KeyChanged False (Character 'q') -> ({ model | mode = QuickInputMode Nothing },
                                                 Msg.focusId quickInputId)

        KeyChanged False (Character 'x') ->
            noCmd <| { model | graph = GraphDefs.removeSelected model.graph} 
        
        KeyChanged False (Character '/') -> Modes.SplitArrow.initialise model 
                     
        KeyChanged False (Control "Delete") ->
            noCmd <| { model | graph = GraphDefs.removeSelected model.graph }
        NodeClick n e ->
            noCmd <| addOrSetSel e.keys.shift (ONode n) model
        EdgeClick n e ->
             noCmd <| addOrSetSel e.keys.shift (OEdge n) model            
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
        KeyChanged False (Control "Escape") -> switch_Default model
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
       KeyChanged False (Control "Escape") -> switch_Default m
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
        RectSelect p r -> collageGraphFromGraph m <| selectGraph m p r
        NewNode -> collageGraphFromGraph m m.graph
        QuickInputMode ch -> collageGraphFromGraph m <| graphDrawingChain m.graph ch
        Move s -> graph_MoveNode m s |> 
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
            msg <| "Default mode. Commands: [click] for point/edge selection (hold for selection rectangle, "
                ++ "[shift] to keep previous selection)" 
                ++ ", new [a]rrow from selected point"
                ++ ", new [p]oint"
                ++ ", new (commutative) [s]quare on selected point (with two already connected edges)"
                ++ ", [del]ete selected object (also [x])"
                ++ ", [q]ickInput mode" 
                ++ ", [d]ebug mode" 
                -- ++ ", [u]named flag (no labelling on point creation)" 
                ++ ", [r]ename selected object" 
                ++ ", [g] move selected objects" 
                ++ ", [c]lone selected objects" 
                ++ ", [/] split arrow" 
                ++ "."
                ++ case activeObj model of
                     OEdge _ ->
                       " [(,=,b,B,-,>]: alternate between different arrow styles, [i]nverse arrow."
                     _ -> ""
                      -- b "b",
                      -- Html.text "litz flag (no labelling on point creation)."
             -- "[r]ename selected object, move selected point [g], [d]ebug mode"
        DebugMode ->
            "Debug Mode. [ESC] to cancel and come back to the default mode. " ++
              Debug.toString model |> Html.text |> List.singleton |> makeHelpDiv
        QuickInputMode ch ->
            makeHelpDiv [
            "Mode: QuickInput" ++ Debug.toString model.mode ++ "." |> Html.text,
                Html.p [] [
                     msg <| " Syntax: v1 -> v2 - edgeLabel >@d v3 vr |down arrow v X | \"uparrow \" ^ end yo."
                    ++ " [RET] to accept the current chain"       
                    ++ ", [ESC] to cancel and comeback to the default mode."]
                ]
        NewArrow _ -> "Mode NewArrow. "
                          -- ++ Debug.toString model 
                           ++  Modes.NewArrow.help |> msg
        SquareMode _ -> "Mode Commutative square. "
                             ++ Modes.Square.help |> msg
        SplitArrow _ -> "Mode Split Arrow. "
                             ++ Modes.SplitArrow.help |> msg
        Move _ -> "Mode move. Use mouse or h,j,k,l. [RET] or [click] to confirm" |> msg
        RenameMode _ _ -> msg "Rename mode: [RET] to confirm, [TAB] to next label, [ESC] to cancel"


        _ -> let txt = "Mode: " ++ Debug.toString model.mode ++ ". [ESC] to cancel and come back to the default"
                   ++ " mode."
             in
                makeHelpDiv [ Html.text txt ]

quickInputView : Model -> Html Msg
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

additionnalDrawing : Model -> Drawing Msg
additionnalDrawing m = 
   case m.mode of
      RectSelect orig _ -> Drawing.rect (Geometry.makeRect orig m.mousePos)
      _ -> --GraphDrawing.make_input (100.0,100.0) "coucou" (always Msg.noOp)
          Drawing.empty

view : Model -> Html Msg
view model =
    let missings = Graph.invalidEdges model.graph in
    let drawings= graphDrawing (graphDrawingFromModel model) in
    let nmissings = List.length missings in
    Html.div [] [
         Html.button [Html.Events.onClick (save model)
              ] [Html.text "Save"],             
             Html.text model.statusMsg,
         -- if model.unnamedFlag then Html.p [] [Html.text "Unnamed flag On"] else Html.text "",
         -- if state.blitzFlag then Html.p [] [Html.text "Blitz flag"] else Html.text "",
         Html.p [] 
         [(helpMsg model),
          quickInputView model
         ],
         Html.p [] [ Html.text <| if nmissings > 0 then 
            String.fromInt nmissings ++ " nodes or edges could not be rendered."
            else "" ]
         ,
    Drawing.group [drawings,
       additionnalDrawing model
    ]
    -- |> debug
    |> Drawing.svg [
                   Html.Attributes.style "width" "100%",
                   Html.Attributes.height 2000,
                   Html.Attributes.style "border-style" "solid",
                   Html.Events.on "mousemove"
                   (D.map (Do << onMouseMove) D.value)                   
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
