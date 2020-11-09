port module GraphEditor exposing (main)
-- TODO: avoir mode dans Model, et separer le fichier state de Square



import Model exposing (..)
import Browser
import Browser.Events as E


import Json.Decode as D
import Json.Encode as JE
import Graph exposing (..)
import GraphExtra as Graph

import Drawing 

import Point exposing (Point)

import Color exposing (..)
import Html exposing (Html)
import Html.Attributes
import Html.Events


import Parser exposing ((|.), (|=), Parser)
import Set
import QuickInput exposing (chainParser, NonEmptyChain, orientToPoint)

import GraphDrawing exposing (..)
import Msg exposing (..)

import Tuple
import Maybe exposing (withDefault)

import Modes.Square
import Modes.NewArrow 

import Dict
import DictExtra as Dict


-- we tell js about some mouse move event
port onMouseMove : JE.Value -> Cmd a
-- we then get back the relative position of the mouse
-- (this cannot be done in pure elm because it requires
-- to access to the currentTarget field of the event,
-- which is a js object)
port onMouseMoveFromJS : (Point -> a) -> Sub a

-- tell js to save the graph
port saveGraph : (List (Node NodeLabel) , List (Edge EdgeLabel)) -> Cmd a
-- js tells us to load the graph
port loadedGraph : ((List (Node NodeLabel) , List (Edge EdgeLabel)) -> a) -> Sub a





quickInputId : String
quickInputId = "quickinput"









subscriptions : Model -> Sub Msg
subscriptions m = Sub.batch 
    [
      -- upload a graph (triggered by js)
      loadedGraph (\ (n,e) -> Graph.fromNodesAndEdges n e |> Loaded),
      E.onClick (D.succeed MouseClick),
      E.onKeyUp (D.map (KeyChanged False) keyDecoder),
      onMouseMoveFromJS MouseMove
    ]
  -- ++
  --   if True -- captureKeyboard m
  --   then
  --       [E.onKeyUp (D.map (KeyChanged False) keyDecoder)]
  --   else
        -- []







iniModel : Model
iniModel = createModel <| fromNodesAndEdges [] []



save : Model -> Msg
save model = Do (saveGraph (Graph.nodes model.graph, Graph.edges model.graph))

-- Model -----------------------------------------------------------------------






-- captureKeyboard : Model -> Bool
-- captureKeyboard m =
--     case m.mode of
--         QuickInputMode _ -> False
--         _ -> True



  -- | Modes.Square SquareState


-- The graphs are updated according to the current mode





graph_RenameMode : String -> Model -> Graph NodeLabel EdgeLabel
graph_RenameMode s m = graphRenameObj m.graph m.activeObj s

graph_MoveNode : Model -> Graph NodeLabel EdgeLabel
graph_MoveNode model =
    Graph.updateNode (activeNode model) (setPos model.mousePos) model.graph





-- rightInOut : InOut -> InOut -> (InOut, InOut)
-- rightInOut i1 i2 = 
--     case (i1, i2) of
--         (In, In) -> (Out, Out)
--         (Out, Out) -> (In, In)
--         p -> p


switch_RenameMode : Model -> (Model, Cmd Msg)
switch_RenameMode model =
    let label : Maybe String
        label = case model.activeObj of
              ONothing -> Nothing
              ONode id -> Graph.getNode id model.graph |> Maybe.map .label
              OEdge id -> Graph.getEdge id model.graph
    in
    case label of
        Nothing -> noCmd model
        Just l -> ( {model | mode = RenameMode l}, focusLabelInput)

-- Now, deal with incoming messages
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
       m = case msg of
            MouseMove p -> { model | mousePos = Point.flipY p} -- , mouseOnCanvas = True}
            QuickInput s -> { model | quickInput = s, mode = QuickInputMode Nothing} -- , mouseOnCanvas = False}
                    -- {model | mousePos = (x, y), statusMsg = "mouse " ++ Debug.toString (x, y)}
            -- KeyChanged False s -> {model | statusMsg = keyToString s}
            -- NodeClick n -> {model | statusMsg = "point " }
            NodeEnter n -> { model | mousePointOver = ONode n}
            NodeLeave n -> { model | mousePointOver = ONothing}
            SizeChanged n dims ->
                { model | statusMsg = "newsize " ++ Debug.toString (n, dims)
                      , dimNodes = Dict.insertOrRemove n dims model.dimNodes
                }
            _ -> model
    in
    case msg of
     Do cmd -> (m, cmd)
     Loaded g -> noCmd <| createModel g
     _ ->
      case model.mode of
        QuickInputMode c -> update_QuickInput c msg m 
        DefaultMode -> update_DefaultMode msg m
        NewArrow astate -> Modes.NewArrow.update astate msg m
            -- update_Modes.NewArrow astate msg m
        RenameMode l -> update_RenameMode l msg m
        MoveNode -> update_MoveNode msg m
        DebugMode -> update_DebugMode msg m
        NewNode -> update_NewNode msg m
        SquareMode state -> Modes.Square.update state msg m


update_QuickInput : Maybe NonEmptyChain -> Msg -> Model -> (Model, Cmd Msg)
update_QuickInput ch msg model =
    case msg of
        KeyChanged False (Control "Escape") -> switch_Default model
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

update_MoveNode : Msg -> Model -> (Model, Cmd Msg)
update_MoveNode msg model =
    case msg of
        -- MouseMove pageX pageY -> { model | graph = g}
        KeyChanged False (Control "Escape") -> switch_Default model
        MouseClick ->
             switch_Default {model | graph = graph_MoveNode model}
        _ -> noCmd model




        
update_RenameMode : String -> Msg -> Model -> (Model, Cmd Msg)
update_RenameMode label msg model =
    case msg of
      KeyChanged False (Control "Escape") -> switch_Default model
      KeyChanged False (Control "Enter") -> finalise_RenameMode label model
      MouseClick -> finalise_RenameMode label model
      NodeLabelEdit _ s -> noCmd {model | mode = RenameMode s}
      EdgeLabelEdit _ s -> noCmd {model | mode = RenameMode s}
      _ -> noCmd model

finalise_RenameMode : String -> Model -> (Model, Cmd Msg)
finalise_RenameMode label model =
    let g = graph_RenameMode label model in
    switch_Default {model | graph = g}

-- type ChangeModel =
--     ChangeState State
--   | ChangeModel Model
--   | { model | mode = Mode }
--   | NoChange

-- changeModel : Model -> ChangeModel -> Model
-- changeModel ({state} as m) c =
--     case c of
--         ChangeState st -> {m | state = st }
--         ChangeModel m2 -> m2
--         { model | mode = mode } -> { m | mode = mode}
--         NoChange -> m

-- changeModelStuff : Model -> (ChangeModel, a)  -> (Model, a)
-- changeModelStuff m (c, x) = (changeModel m c, x)

update_DefaultMode : Msg -> Model -> (Model, Cmd Msg)
update_DefaultMode msg model =
    -- Tuples.mapFirst (changeModel model) <|
    case msg of
        KeyChanged False (Character 'a') -> Modes.NewArrow.initialise model
        KeyChanged False (Character 's') -> Modes.Square.initialise model 
        KeyChanged False (Character 'r') -> switch_RenameMode model
        KeyChanged False (Character 'p') -> noCmd <| { model | mode = NewNode }
        KeyChanged False (Character 'q') -> ({ model | mode = QuickInputMode Nothing },
                                                 focusId quickInputId)
        -- KeyChanged False (Character 'u') ->
        --     noCmd {model | unnamedFlag = not model.unnamedFlag} 
        -- KeyChanged False (Character 'b') -> ({model | blitzFlag = not model.blitzFlag}, Cmd.none)
        KeyChanged False (Character 'd') ->
            noCmd <| { model | mode = DebugMode }
        KeyChanged False (Character 'g') -> noCmd <| { model | mode = MoveNode }
        KeyChanged False (Control "Delete") ->
            noCmd <| { model | graph = graphRemoveObj model.activeObj model.graph}
        KeyChanged False (Character 'x') ->
            noCmd <| { model | graph = graphRemoveObj model.activeObj model.graph} 
        NodeClick n ->
            noCmd <| { model | activeObj = ONode n} 
        EdgeClick n ->
            noCmd <| { model | activeObj = OEdge n}
        _ -> noCmd model

update_DebugMode : Msg -> Model -> (Model, Cmd Msg)
update_DebugMode msg model =
    case msg of
        KeyChanged False (Control "Escape") -> switch_Default model
        _ -> noCmd model

update_NewNode : Msg -> Model -> (Model, Cmd Msg)
update_NewNode msg m =
    case msg of
       MouseClick ->
         let (newGraph, newId) = Graph.newNode m.graph {pos = m.mousePos, label = ""}
             newModel = {m | graph = newGraph,
                             activeObj = ONode newId
                        }
         in
         -- if m.unnamedFlag then
         --   switch_Default newModel
         -- else
           switch_RenameMode newModel
       _ -> noCmd m






-- functions that turns a model graph into one with more information
-- about the display, based on the mode




graphDrawingFromModel : Model -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawingFromModel m =
    case m.mode of
        DefaultMode -> collageGraphFromGraph m m.graph
        NewNode -> collageGraphFromGraph m m.graph
        QuickInputMode ch -> collageGraphFromGraph m <| graphDrawingChain m.graph ch
        MoveNode -> graph_MoveNode m |> 
            collageGraphFromGraph m 
        RenameMode l ->
            let g = graph_RenameMode l m in
            collageGraphFromGraph m g
                |> graphMakeEditable m.activeObj
        DebugMode ->
            m.graph |> collageGraphFromGraph m 
                |> Graph.mapNodeEdges
                   (\n -> let l = n.label in {l | label = String.fromInt n.id}) .label
        NewArrow astate -> Modes.NewArrow.graphDrawing m astate
        SquareMode state ->
            Modes.Square.graphDrawing m state


graphDrawingChain : Graph NodeLabel EdgeLabel -> Maybe NonEmptyChain -> Graph NodeLabel EdgeLabel
graphDrawingChain g ch = 
    case ch of
        Nothing -> g
        Just nonEmptyCh ->
            let iniP = (100, -100) in
            Tuple.first <| graphDrawingNonEmptyChain g nonEmptyCh iniP -- QuickInput.Right

createNodeLabel : Graph NodeLabel EdgeLabel -> String -> Point -> (Graph NodeLabel EdgeLabel,
                                                                       NodeId, Point)
createNodeLabel g s p =
    let label = { pos = p, label = s} in
    let (g2, id) = Graph.newNode g label in
     (g2, id, p)

getNodeLabelOrCreate : Graph NodeLabel EdgeLabel -> String -> Point -> (Graph NodeLabel EdgeLabel,
                                                                       NodeId, Point)
getNodeLabelOrCreate g s p =
    if s == "" then
       createNodeLabel g s p
    else
        case Graph.filterNodes g (\ l -> l.label == s) of
            [] -> createNodeLabel g s p
            t :: _ -> (g , t.id, t.label.pos)

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

            (Graph.addEdge g3 (source, target) label, source)


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
    let msg = \ s -> s |>
                  Parser.run helpMsgParser |>
                  Result.withDefault [("Parsing help msg error", Plain)] |>
                  List.map helpStr_collage |> Html.div []
    in
    -- let b = \ s -> Html.span [] [Html.text "[", Html.b [] [Html.text s], Html.text"]"]
    -- in
    -- let info = \ s1 s2 -> Html.span [] (b s1) (Html.text s2)
    case model.mode of
        DefaultMode ->
            -- msg <| "Default mode. couc[c]" 
            msg <| "Default mode. Commands: [click] for point/edge selection" 
                ++ ", new [a]rrow from selected point"
                ++ ", new [p]oint"
                ++ ", new (commutative) [s]quare on selected point"
                ++ ", [del]ete selected object (also [x])"
                ++ ", [q]ickInput mode" 
                ++ ", [d]ebug mode" 
                -- ++ ", [u]named flag (no labelling on point creation)" 
                ++ ", [r]ename selected object" 
                ++ ", [g] move selected object" 
                ++ "."
                      -- b "b",
                      -- Html.text "litz flag (no labelling on point creation)."
             -- "[r]ename selected object, move selected point [g], [d]ebug mode"
        DebugMode ->
            "Debug Mode. [ESC] to cancel and come back to the default mode. " ++
              Debug.toString model |> Html.text
        QuickInputMode ch ->
            Html.div [] [
            "Mode: QuickInput" ++ Debug.toString model.mode ++ "." |> Html.text,
                Html.p [] [
                     msg <| " Syntax: v1 -> v2 - edgeLabel >@d v3 vr |down arrow v X | \"uparrow \" ^ end yo."
                    ++ " [RET] to accept the current chain"       
                    ++ ", alternative possible [s]quare"
                    ++ ", [ESC] to cancel and comeback to the default mode."]
                ]
        _ -> "Mode: " ++ Debug.toString model.mode ++ ". [ESC] to cancel and come back to the default"
             ++ " mode."
                 |> Html.text

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


view : Model -> Html Msg
view model =
    Html.div [] [
         Html.button [Html.Events.onClick (save model)
              ] [Html.text "Save"],
             quickInputView model,
             Html.text model.statusMsg,
         -- if model.unnamedFlag then Html.p [] [Html.text "Unnamed flag On"] else Html.text "",
         -- if state.blitzFlag then Html.p [] [Html.text "Blitz flag"] else Html.text "",
         Html.p [] 
         [(helpMsg model)
         ],
    graphDrawing (graphDrawingFromModel model)
    -- |> debug
    |> Drawing.svg [
                   Html.Attributes.style "width" "100%",
                   Html.Attributes.height 2000,
                   Html.Attributes.style "border-style" "solid",
                   Html.Events.on "mousemove"
                   (D.map (Do << onMouseMove) D.value)
             ]
             ]



main : Program () Model Msg
main = Browser.element { init = \ _ -> (iniModel, Cmd.none),
                         view = view, update = update,
                         subscriptions = subscriptions}
