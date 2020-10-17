port module GraphEditor exposing (main)


import Browser
import Browser.Events as E
import Browser.Dom as Dom
import Task
import Json.Decode as D
import Json.Encode as JE
import Graph exposing (..)
import GraphExtra as Graph exposing (EdgeId)
import Collage exposing (..)
import IntDict
import Collage.Events exposing (onClick)
import CollageExtra as Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg, svgExplicit)
import Collage.Text exposing (fromString)
import Color exposing (..)
import Html exposing (Html)
import Html.Attributes
import Html.Events

import Parser exposing ((|.), (|=), Parser)
import Set
import QuickInput exposing (chainParser, NonEmptyChain, orientToPoint)

import GraphCollage exposing (..)
import Msg exposing (..)

import Tuple
import Maybe exposing (withDefault)


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

focusId : String -> Cmd Msg
focusId s = Task.attempt (\_ -> noOp) (Dom.focus s) 

-- Focus on the input
focusLabelInput : Cmd Msg
focusLabelInput = focusId curIdInput




subscriptions : Model -> Sub Msg
subscriptions m = Sub.batch 
    ([
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
    )





save : Model -> Msg
save model = (Do (saveGraph (Graph.nodes model.graph, Graph.edges model.graph)))

-- Model -----------------------------------------------------------------------


-- core data that will be saved


type alias Model =
  { graph : Graph NodeLabel EdgeLabel, mode : Mode, activeObj : Obj,
    mousePos : Point,
    -- if the mouse is over some node or edge
    mousePointOver : Obj,
    statusMsg : String,
    unnamedFlag : Bool,
    quickInput : String
    -- quickInput : Maybe NonEmptyChain
    -- mouseOnCanvas : Bool
    -- blitzFlag : Bool
  }

createModel : Graph NodeLabel EdgeLabel -> Model
createModel g =  { graph = g,
              mode = DefaultMode,
              statusMsg = "",
                       -- Debug.toString ([pointToAngle (0,1), pointToAngle (0.001,1),
                       --                    pointToAngle (1,0), pointToAngle(0,-1),
                       --                        pointToAngle (-1, 0.01)]),
              quickInput = "",
              mousePos = (0,0),
              mousePointOver = ONothing,
              activeObj = ONothing,
              unnamedFlag = False
              -- mouseOnCanvas = False,
              -- quickInput = Nothing
                 -- blitzFlag = False
                 }

-- captureKeyboard : Model -> Bool
-- captureKeyboard m =
--     case m.mode of
--         QuickInputMode _ -> False
--         _ -> True


-- initial state
init : Model
init = createModel <| fromNodesAndEdges [] []

type Obj = ONothing | ONode NodeId | OEdge EdgeId

graphRemoveObj : Obj -> Graph a b -> Graph a b
graphRemoveObj o g =
    case o of
        ONothing -> g
        ONode id -> Graph.remove id g
        OEdge id -> Graph.removeEdge id g


graphRenameObj : Graph NodeLabel EdgeLabel -> Obj -> String -> Graph NodeLabel EdgeLabel
graphRenameObj g o s = 
    case o of
        ONode id -> Graph.updateNode id (\ nl -> {nl | label = s}) g
        OEdge id -> Graph.updateEdge id (\ nl -> s) g
        ONothing -> g

graphMakeEditable : Obj -> Graph NodeCollageLabel EdgeCollageLabel -> Graph NodeCollageLabel EdgeCollageLabel
graphMakeEditable o g =
    case o of
        ONode id -> Graph.updateNode id (\ e -> {e | editable = True}) g
        OEdge id -> Graph.updateEdge id (\ e -> {e | editable = True}) g
        ONothing -> g
                                            

type Mode =
    DefaultMode
  | NewArrow NewArrowState
  | MoveNode
  | RenameMode String
  | DebugMode
  | NewNode
  | QuickInputMode (Maybe NonEmptyChain)
  | SquareMode SquareState

type alias NewArrowState = { edgeLabel : String , nodeLabel : String , step : NewArrowStep }
type NewArrowStep = EditEdge 
                  | EditNode
                  | NoEdit

-- type alias SquareState =
    -- { from : NodeId, to : NodeId,
    --     nodeLabel : String, edge1Label : String, edge2Label : String, step : SquareStep }

type InOut = In | Out
type SquareState =
         -- the first argument is the next possibility of square to be tested
          SquareMoveNode Int NodeId InOut NodeId InOut
        | SquareEditNode NodeId EdgeId EdgeId
        | SquareEditEdge1 EdgeId EdgeId
        | SquareEditEdge2 EdgeId


activeNode : Model -> NodeId
activeNode m = obj_NodeId m.activeObj

obj_NodeId : Obj -> NodeId
obj_NodeId x = case x of
                   ONode id -> id
                   _ -> 0
obj_EdgeId : Obj -> EdgeId
obj_EdgeId x = case x of
                   OEdge id -> id
                   _ -> (0, 0)


-- The graphs are updated according to the current mode

-- returns also the endpoint id of the arrow
-- graph_Square : SquareState -> Model -> (Graph NodeLabel EdgeLabel, NodeId)
-- graph_Square state m =
--     let (graph, target) = mayCreateTargetNode m "" in
--          (Graph.addEdge graph (activeNode m, target) state.edgeLabel, target)

-- returns also the endpoint id of the arrow
graph_NewArrow : NewArrowState -> Model -> (Graph NodeLabel EdgeLabel, NodeId)
graph_NewArrow state m =
    let ((graph, target), _) = mayCreateTargetNode m state.nodeLabel in
         (Graph.addEdge graph (activeNode m, target) state.edgeLabel, target)

-- returns the target node of the new arrow, if it already exists.
getTargetNode : Model -> Maybe NodeId
getTargetNode m =
    case m.mousePointOver of
        -- if the mouse is over a node, that is the target node
        ONode i -> if Graph.member i m.graph then Just i else Nothing
        _ -> Nothing

-- True if created
mayCreateTargetNode : Model -> String -> ((Graph NodeLabel EdgeLabel, NodeId), Bool)
mayCreateTargetNode m s =
    case getTargetNode m of
        Just n -> ((m.graph, n), False)
        Nothing -> (Graph.newNode m.graph { label = s, pos = m.mousePos }, True)
-- will it be necessary to create a new node?
newArrowIsNewNode : Model -> Bool
newArrowIsNewNode m = case getTargetNode m of
                          Just _ -> False
                          Nothing -> True


graph_RenameMode : String -> Model -> Graph NodeLabel EdgeLabel
graph_RenameMode s m = graphRenameObj m.graph m.activeObj s

graph_MoveNode : Model -> Graph NodeLabel EdgeLabel
graph_MoveNode model =
    Graph.updateNode (activeNode model) (setPos model.mousePos) model.graph

-- switch between modes

noCmd : a -> (a, Cmd Msg)
noCmd m = (m , Cmd.none)

switch_Default : Model -> (Model, Cmd Msg)
switch_Default m = noCmd { m | mode = DefaultMode }

squareMode_mayFocus : SquareState -> Cmd Msg
squareMode_mayFocus state =
 case state of
   SquareMoveNode _ _ _ _ _ -> Cmd.none
   _ -> focusLabelInput

switch_NewArrow : NewArrowState -> Model -> (Model, Cmd Msg)
switch_NewArrow state model =
    -- if model.blitzFlag then
    --      finalise_NewArrow state model
    -- else
      ({model | mode = NewArrow state}, newArrow_mayFocus state.step)

newArrow_mayFocus : NewArrowStep -> Cmd Msg
newArrow_mayFocus step =
 case step of
   NoEdit -> Cmd.none
   _ -> focusLabelInput

initialise_NewArrow : Model -> (Model, Cmd Msg)
initialise_NewArrow m =
    case m.activeObj of
        ONode _ -> switch_NewArrow { edgeLabel = "", nodeLabel = "", step = EditEdge } m
        _ -> switch_Default m

-- from ListExtra package
uniquePairs : List a -> List ( a, a )
uniquePairs xs =
    case xs of
        [] ->
            []

        x :: xs_ ->
            List.map (\y -> ( x, y )) xs_ ++ uniquePairs xs_

getAt : Int -> List a -> Maybe a
getAt idx xs = List.head <| List.drop idx xs

rightInOut : InOut -> InOut -> (InOut, InOut)
rightInOut i1 i2 = 
    case (i1, i2) of
        (In, In) -> (Out, Out)
        (Out, Out) -> (In, In)
        p -> p

-- second argument: the n-th possibility
initialise_Square : Model -> Int -> (Model, Cmd Msg)
initialise_Square m idx =
    case m.activeObj of
        ONode n ->
            case Graph.get n m.graph of
                Nothing -> noCmd m
                Just nc ->
                    let ins  = IntDict.keys nc.incoming |> List.map (\x -> (x, Out))
                        outs = IntDict.keys nc.outgoing |> List.map (\x -> (x, In))
                    in
                    let possibilities = ins ++ outs |> uniquePairs in
                    case getAt idx possibilities of
                        Just ((n1, i1), (n2, i2)) ->
                            let (j1, j2) = rightInOut i1 i2 in
                            noCmd {m | mode = SquareMode
                                       <| SquareMoveNode
                                       (idx + 1 |> modBy
                                            (List.length possibilities))
                                       n1 j1 n2 j2}
                        _ -> noCmd m
            -- switch_NewArrow { edgeLabel = "", nodeLabel = "", step = EditEdge } m
        _ -> noCmd m

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
            MouseMove p -> {model | mousePos = flipY p} -- , mouseOnCanvas = True}
            QuickInput s -> {model | quickInput = s, mode = QuickInputMode Nothing} -- , mouseOnCanvas = False}
                    -- {model | mousePos = (x, y), statusMsg = "mouse " ++ Debug.toString (x, y)}
            -- KeyChanged False s -> {model | statusMsg = keyToString s}
            -- NodeClick n -> {model | statusMsg = "point " }
            NodeEnter n -> {model | mousePointOver = ONode n}
            NodeLeave n -> {model | mousePointOver = ONothing}
            _ -> model
    in
    case msg of
     Do cmd -> (m, cmd)
     Loaded g -> noCmd <| createModel g
     _ ->
      case model.mode of
        QuickInputMode c -> update_QuickInput c msg m 
        DefaultMode -> update_DefaultMode msg m
        NewArrow state -> update_NewArrow state msg m
        RenameMode l -> update_RenameMode l msg m
        MoveNode -> update_MoveNode msg m
        DebugMode -> update_DebugMode msg m
        NewNode -> update_NewNode msg m
        SquareMode state -> update_SquareMode state msg m


update_QuickInput : Maybe NonEmptyChain -> Msg -> Model -> (Model, Cmd Msg)
update_QuickInput ch msg model =
    case msg of
        KeyChanged False (Control "Escape") -> switch_Default model
        KeyChanged False (Control "Enter") ->
            switch_Default {model | graph = graphCollageChain model.graph ch, quickInput = ""}
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

make_EdgeId : NodeId -> InOut -> NodeId -> EdgeId
make_EdgeId n1 i1 n2 =
    case i1 of
        In -> (n2, n1)
        Out -> (n1, n2)


squareMoveMode_graph : NodeId -> InOut -> NodeId -> InOut -> NodeId ->
                   Graph NodeLabel EdgeLabel ->
                   (Graph NodeLabel EdgeLabel, EdgeId, EdgeId)
squareMoveMode_graph n1 i1 n2 i2 n g =
    let e1 = make_EdgeId n1 i1 n
        e2 = make_EdgeId n2 i2 n
    in
      (Graph.addEdge (Graph.addEdge g e1 "") e2 "", e1, e2)

-- returns the id of the created node, if there was a created node
squareMode_graph : Model ->
                   (Graph NodeLabel EdgeLabel, (EdgeId, EdgeId), Maybe NodeId)
squareMode_graph m =
    case m.mode of
        SquareMode (SquareMoveNode _ n1 i1 n2 i2) ->
            let ((g, n), created) = mayCreateTargetNode m "" in
            let (g2, e1, e2) = squareMoveMode_graph n1 i1 n2 i2 n g in
            (g2, (e1, e2), if created then Just n else Nothing)
        _ -> (m.graph, ((0, 0), (0, 0)), Nothing)

squareMode_next : SquareState -> Mode
squareMode_next state =
    case state of
        SquareEditNode n e1 e2 -> SquareMode <| SquareEditEdge1 e1 e2 
        SquareEditEdge1 e1 e2 ->  SquareMode <| SquareEditEdge2 e2 
        SquareEditEdge2 e2 -> DefaultMode
        _ -> DefaultMode

squareMode_renObj : SquareState -> Obj
squareMode_renObj state =
    case state of
           SquareEditNode n e1 e2 -> ONode n
           SquareEditEdge1 e1 e2 -> OEdge e1
           SquareEditEdge2 e2 -> OEdge e2
           _ -> ONothing

update_SquareMode_next : SquareState -> Model -> (Model, Cmd Msg)
update_SquareMode_next state model =
          case state of
              SquareMoveNode _ _ _ _ _ ->
                  let (g, (e1, e2), created) = squareMode_graph model in
                  noCmd { model | graph = g, mode = SquareMode <|
                              case created of
                                  Just n  -> SquareEditNode n e1 e2
                                  Nothing -> SquareEditEdge1 e1 e2
                        }

              st2 -> noCmd { model | mode = squareMode_next st2 }

update_SquareMode : SquareState -> Msg -> Model -> (Model, Cmd Msg)
update_SquareMode state msg model =
    case msg of
      KeyChanged False (Control "Escape") ->
        case state of
           SquareMoveNode _ _ _ _ _ -> switch_Default model
           _ ->
             noCmd { model | graph = graphRenameObj model.graph (squareMode_renObj state) "",
                             mode = squareMode_next state }
      KeyChanged False (Control "Enter") -> update_SquareMode_next state model
      MouseClick -> update_SquareMode_next state model

      EdgeLabelEdit e s ->
             noCmd { model | graph = graphRenameObj model.graph (OEdge e) s }
      NodeLabelEdit n s ->
             noCmd { model | graph = graphRenameObj model.graph (ONode n) s }

      KeyChanged False (Character 's') ->
          case state of
            SquareMoveNode idx _ _ _ _ ->
                initialise_Square model idx
            _ -> noCmd model

      -- I don't know why this is necessary, but I may need to refocus
      NodeLeave _ -> (model, squareMode_mayFocus state)
      NodeEnter _ -> (model, squareMode_mayFocus state)

      _ -> noCmd model



update_NewArrow : NewArrowState -> Msg -> Model -> (Model, Cmd Msg)
update_NewArrow state msg model =
    case msg of
      KeyChanged False (Control "Escape") -> switch_Default model
      MouseClick ->
          if model.unnamedFlag then
              finalise_NewArrow state model
          else
           case state.step of
               EditEdge ->
                   if newArrowIsNewNode model then
                       switch_NewArrow { state | step = EditNode } model
                   else
                       finalise_NewArrow state model
               EditNode -> finalise_NewArrow state model
               NoEdit -> finalise_NewArrow state model
      KeyChanged False (Control "Enter") -> 
          if model.unnamedFlag then
              finalise_NewArrow state model
          else
            case state.step of
             EditEdge ->  switch_NewArrow { state | step = EditNode } model
                          -- nextStep_NewArrow state model
             EditNode -> -- nextStep_NewArrow state model
                 if newArrowIsNewNode model then
                     switch_NewArrow { state | step = NoEdit} model
                 else
                     finalise_NewArrow state model
             NoEdit -> noCmd model
      EdgeLabelEdit _ s -> noCmd {model | mode = NewArrow { state | edgeLabel = s}}
      NodeLabelEdit _ s -> noCmd {model | mode = NewArrow { state | nodeLabel = s}}
      -- I don't know why this is necessary, but I may need to refocus
      NodeLeave _ -> (model, newArrow_mayFocus state.step)
      NodeEnter _ -> (model, newArrow_mayFocus state.step)
      _ -> noCmd model


-- creates the new arrow for real
finalise_NewArrow : NewArrowState -> Model -> (Model, Cmd Msg)
finalise_NewArrow state model =
    let (g, targetNode) = graph_NewArrow state model in
    -- let newmodel =
            initialise_NewArrow
                { model | graph = g, activeObj = ONode targetNode } -- in
    -- (newmodel, Cmd.none)



        
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
    switch_Default {model | mode = DefaultMode, graph = g}


update_DefaultMode : Msg -> Model -> (Model, Cmd Msg)
update_DefaultMode msg model =
    case msg of
        KeyChanged False (Character 'a') -> initialise_NewArrow model
        KeyChanged False (Character 's') -> initialise_Square model 0
        KeyChanged False (Character 'r') -> switch_RenameMode model
        KeyChanged False (Character 'p') -> noCmd {model | mode = NewNode}
        KeyChanged False (Character 'q') -> ({model | mode = QuickInputMode Nothing}, focusId quickInputId)
        KeyChanged False (Character 'u') ->
            noCmd {model | unnamedFlag = not model.unnamedFlag}
        -- KeyChanged False (Character 'b') -> ({model | blitzFlag = not model.blitzFlag}, Cmd.none)
        KeyChanged False (Character 'd') ->
            noCmd {model | mode = DebugMode}
        KeyChanged False (Character 'g') -> noCmd {model | mode = MoveNode}
        KeyChanged False (Control "Delete") ->
            noCmd {model | graph = graphRemoveObj model.activeObj model.graph}
        KeyChanged False (Character 'x') ->
            noCmd {model | graph = graphRemoveObj model.activeObj model.graph}
        NodeClick n -> noCmd {model | activeObj = ONode n}
        EdgeClick n -> noCmd {model | activeObj = OEdge n}
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
             newModel = {m | graph = newGraph, activeObj = ONode newId}
         in
         if m.unnamedFlag then
           switch_Default newModel
         else
           switch_RenameMode newModel
       _ -> noCmd m






-- functions that turns a model graph into one with more information
-- about the display, based on the mode


make_defaultNodeCollageLabel : Model -> Node NodeLabel -> NodeCollageLabel
make_defaultNodeCollageLabel m n = make_nodeCollageLabel { editable = False,
                                                               isActive = n.id == (activeNode m)}
                                   n.label

collageGraphFromGraph : Model -> Graph NodeLabel EdgeLabel -> Graph NodeCollageLabel EdgeCollageLabel
collageGraphFromGraph m =
            Graph.mapNodeEdges
               (make_defaultNodeCollageLabel m)
               (\ e -> e.label |> make_edgeCollageLabel
                    {editable = False, isActive = (e.from, e.to) == (m.activeObj |> obj_EdgeId)})


graphCollageFromModel : Model -> Graph NodeCollageLabel EdgeCollageLabel
graphCollageFromModel m =
    case m.mode of
        DefaultMode -> collageGraphFromGraph m m.graph
        NewNode -> collageGraphFromGraph m m.graph
        QuickInputMode ch -> collageGraphFromGraph m <| graphCollageChain m.graph ch
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
        NewArrow state ->
            let
                (g, newId) = graph_NewArrow state m
                cg = collageGraphFromGraph m g
            in
            case state.step of
                EditEdge -> graphMakeEditable (OEdge ((activeNode m), newId)) cg
                EditNode ->
                    if newArrowIsNewNode m then
                        graphMakeEditable (ONode newId) cg
                    else
                        cg
                NoEdit -> cg
        SquareMode state ->
            let (g, _, _) = squareMode_graph m in
            graphMakeEditable (squareMode_renObj state) (collageGraphFromGraph m g)


graphCollageChain : Graph NodeLabel EdgeLabel -> Maybe NonEmptyChain -> Graph NodeLabel EdgeLabel
graphCollageChain g ch = 
    case ch of
        Nothing -> g
        Just nonEmptyCh ->
            let iniP = (100, -100) in
            Tuple.first <| graphCollageNonEmptyChain g nonEmptyCh iniP -- QuickInput.Right

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

graphCollageNonEmptyChain : Graph NodeLabel EdgeLabel -> NonEmptyChain -> Point -- -> QuickInput.Orient
                    -> (Graph NodeLabel EdgeLabel, NodeId)
graphCollageNonEmptyChain g ch loc -- defOrient
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
            let newPoint = addP source_pos <| resizeP offset <| (orientToPoint orient) in
            let (g3, target) = graphCollageNonEmptyChain g2 tail newPoint -- orient
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
                ++ ", [u]named flag (no labelling on point creation)" 
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
         Html.input  ([Html.Attributes.type_ "text",
                       Html.Attributes.id quickInputId,
                     Html.Events.onInput QuickInput,
                     -- Html.Events.onFocus (QuickInput ""),
                     Html.Attributes.value m.quickInput
                     ]
                          -- ++
                          -- case m.mode of
                          --     QuickInputMode _ -> []
                          --     _ -> [Html.Attributes.value ""]
                     )[]]


view : Model -> Html Msg
view model =
    Html.div [] [
         Html.button [Html.Events.onClick (save model)
              ] [Html.text "Save"],
             quickInputView model,
             Html.text model.statusMsg,
         if model.unnamedFlag then Html.p [] [Html.text "Unnamed flag On"] else Html.text "",
         -- if model.blitzFlag then Html.p [] [Html.text "Blitz flag"] else Html.text "",
         Html.p [] 
         [(helpMsg model)
         ],
    graphCollage (graphCollageFromModel model)
    -- |> debug
    |> svgExplicit [
                   Html.Attributes.style "width" "100%",
                   Html.Attributes.height 2000,
                   Html.Attributes.style "border-style" "solid",
                   Html.Events.on "mousemove"
                   (D.map (Do << onMouseMove) D.value)
             ]
             ]



main : Program () Model Msg
main = Browser.element { init = \ _ -> (init, Cmd.none),
                         view = view, update = update,
                         subscriptions = subscriptions}
