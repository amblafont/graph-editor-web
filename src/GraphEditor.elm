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
import Collage.Layout exposing (..)
import Collage.Render exposing (svg, svgExplicit)
import Collage.Text exposing (fromString)
import Color exposing (..)
import Html exposing (Html)
import Html.Attributes
import Html.Events


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


-- id of the text input when the user labels an edge or a node
curIdInput : String
curIdInput = "edited_label"

-- Focus on the input
focusLabelInput : Cmd Msg
focusLabelInput = Task.attempt (\_ -> noOp) (Dom.focus curIdInput)

-- From https://github.com/elm/browser/blob/1.0.2/notes/keyboard.md
-- useful for keyboard events
type Key
  = Character Char
  | Control String

keyToString : Key -> String
keyToString k = case k of
                    Character c -> String.fromChar c
                    Control s -> s

toKey : String -> Key
toKey string = 
  case String.uncons string of
    Just (char, "") -> Character char
    _ -> Control string

keyDecoder : D.Decoder Key
keyDecoder = (D.field "key" D.string)
             |> D.map toKey

subscriptions : Model -> Sub Msg
subscriptions m = Sub.batch 
    [
      -- upload a graph (triggered by js)
      loadedGraph (\ (n,e) -> Graph.fromNodesAndEdges n e |> Loaded),
      E.onKeyUp (D.map (KeyChanged False) keyDecoder),
      E.onClick (D.succeed MouseClick),
      onMouseMoveFromJS MouseMove
    ]




type Msg
  = -- call some js function
    Do (Cmd Msg)
  | KeyChanged Bool Key
  | MouseMove Point
  | MouseClick 
  | NodeEnter NodeId
  | NodeLeave NodeId
  | NodeClick NodeId
  | EdgeClick EdgeId
  | EdgeLabelEdit EdgeId String
  | NodeLabelEdit NodeId String
  | Loaded (Graph NodeLabel EdgeLabel)

noOp : Msg
noOp = Do (Cmd.none)

save : Model -> Msg
save model = (Do (saveGraph (Graph.nodes model.graph, Graph.edges model.graph)))

-- Model -----------------------------------------------------------------------

-- core data that will be saved
type alias EdgeLabel = String
type alias NodeLabel = { pos : Point , label : String}

setPos : Point -> NodeLabel -> NodeLabel
setPos p l = { l | pos = p}


-- these are extended node and edge labels used for drawing (discarded for saving)
type alias EdgeCollageLabel = { label : String, editable : Bool, isActive : Bool }
type alias NodeCollageLabel = { pos : Point, label : String, editable : Bool, isActive : Bool }

make_edgeCollageLabel : {editable : Bool, isActive : Bool} 
                      -> EdgeLabel-> EdgeCollageLabel
make_edgeCollageLabel {editable, isActive} label =
    { label = label, editable = editable, isActive = isActive}

make_nodeCollageLabel : {editable : Bool, isActive : Bool} -> NodeLabel ->  NodeCollageLabel
make_nodeCollageLabel {editable, isActive} {label, pos} =
    { label = label, pos = pos, editable = editable, isActive = isActive }


type alias Model =
  { graph : Graph NodeLabel EdgeLabel, mode : Mode, activeObj : Obj,
    mousePos : Point,
    -- if the mouse is over some node or edge
    mousePointOver : Obj,
    -- statusMsg : String,
    unnamedFlag : Bool
    -- blitzFlag : Bool
  }

createModel : Graph NodeLabel EdgeLabel -> Model
createModel g =  { graph = g,
              mode = DefaultMode,
              -- statusMsg = "",
              mousePos = (0,0),
              mousePointOver = ONothing,
              activeObj = ONothing,
                 unnamedFlag = False
                 -- blitzFlag = False
                 }

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

type Mode =
    DefaultMode
  | NewArrow NewArrowState
  | MoveNode
  | RenameMode String
  | DebugMode
  | NewNode

type alias NewArrowState = { edgeLabel : String , nodeLabel : String , step : NewArrowStep }
type NewArrowStep = EditEdge 
                  | EditNode
                  | NoEdit


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
graph_NewArrow : NewArrowState -> Model -> (Graph NodeLabel EdgeLabel, NodeId)
graph_NewArrow state m =
    let (graph, target) = case newArrowGetTargetNode m of
                              Just t -> (m.graph, t)
                              Nothing -> Graph.newNode m.graph
                                         { label = state.nodeLabel, pos = m.mousePos }
    in
         (Graph.addEdge graph (activeNode m, target) state.edgeLabel, target)

-- returns the target node of the new arrow, if it already exists.
newArrowGetTargetNode : Model -> Maybe NodeId
newArrowGetTargetNode m =
    case m.mousePointOver of
        -- if the mouse is over a node, that is the target node
        ONode i -> if Graph.member i m.graph then Just i else Nothing
        _ -> Nothing

-- will it be necessary to create a new node?
newArrowIsNewNode : Model -> Bool
newArrowIsNewNode m = case newArrowGetTargetNode m of
                          Just _ -> False
                          Nothing -> True


graph_RenameMode : String -> Model -> Graph NodeLabel EdgeLabel
graph_RenameMode s m = 
    case m.activeObj of
        ONode id -> Graph.updateNode id (\ nl -> {nl | label = s}) m.graph
        OEdge id -> Graph.updateEdge id (\ nl -> s) m.graph
        ONothing -> m.graph

graph_MoveNode : Model -> Graph NodeLabel EdgeLabel
graph_MoveNode model =
    Graph.updateNode (activeNode model) (setPos model.mousePos) model.graph

-- switch between modes

noCmd : a -> (a, Cmd Msg)
noCmd m = (m , Cmd.none)

switch_Default : Model -> (Model, Cmd Msg)
switch_Default m = noCmd { m | mode = DefaultMode }

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

update_MoveNode : Msg -> Model -> (Model, Cmd Msg)
update_MoveNode msg model =
    case msg of
        -- MouseMove pageX pageY -> { model | graph = g}
        KeyChanged False (Control "Escape") -> switch_Default model
        MouseClick ->
             switch_Default {model | graph = graph_MoveNode model}
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
        KeyChanged False (Character 'r') -> switch_RenameMode model
        KeyChanged False (Character 'p') -> noCmd {model | mode = NewNode}
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


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
       m = case msg of
            MouseMove p -> {model | mousePos = flipY p}
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
        DefaultMode -> update_DefaultMode msg m
        NewArrow state -> update_NewArrow state msg m
        RenameMode l -> update_RenameMode l msg m
        MoveNode -> update_MoveNode msg m
        DebugMode -> update_DebugMode msg m
        NewNode -> update_NewNode msg m




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
        MoveNode -> graph_MoveNode m |> 
            collageGraphFromGraph m 
        RenameMode l ->
            let g = graph_RenameMode l m in
            collageGraphFromGraph m g
                |>
                  case m.activeObj of
                      ONode id -> Graph.updateNode id (\ e -> {e | editable = True})
                      OEdge id -> Graph.updateEdge id (\ e -> {e | editable = True})
                      ONothing -> \ x -> x
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
                EditEdge -> Graph.updateEdge
                         ((activeNode m), newId)
                         (\ e -> {e | editable = True}) cg
                EditNode ->
                    if newArrowIsNewNode m then
                       Graph.updateNode newId (\ l -> { l | editable = True}) cg
                    else
                        cg
                NoEdit -> cg

-- get the radius of a pont
radius : Point -> Float
radius (x, y) = sqrt (x * x + y * y) 

normaliseP : Float -> Point -> Point
normaliseP len (x, y) =
    let r = radius (x, y) in
    (len * x / r, len * y / r)

orthogonalP : Point -> Point
orthogonalP (x, y) = (0 - y , x)

flip : Point -> Point
flip (x, y) = (0 - x, 0 - y)

flipY : Point -> Point
flipY (x, y) = (x, 0 - y)

minusP : Point -> Point -> Point
minusP (x1, y1)(x2, y2) = (x1 - x2, y1 - y2)

addP : Point -> Point -> Point
addP (x1, y1)(x2, y2) = (x1 + x2, y1 + y2)

middleP : Point -> Point -> Point
middleP (x1, y1) (x2, y2) = ((x1 + x2) / 2, (y1 + y2) / 2)
-- convert a point to an angle 
pointToAngle : Point -> Float
pointToAngle (x, y) = 2 * atan (y / (x + radius (x, y)))

-- draws an arrow
arrow : Color.Color -> Point -> Point -> Collage a
arrow c from to =
    let
        delta =  minusP to from
        -- pos = to
        offset = 15
        offsetP = normaliseP offset delta
        pos = minusP to offsetP
        fromOffset = addP from offsetP
    in
      Collage.group
         [ triangle 10
        |> filled (uniform c)
           |> rotate (pointToAngle <| flip <| orthogonalP delta)
              |> shift pos, segment fromOffset pos
               |> traced (solid thin (uniform c))
         ]


-- create an input with id curIdInput
make_input : String -> (String -> a) -> Collage a
make_input label onChange =
         Html.input ([ Html.Attributes.value label ,
                       Html.Events.onInput onChange,
                       Html.Attributes.id curIdInput
                    ] ) []
             |> Collage.html (100,50)

nodeLabelCollage : Node NodeCollageLabel -> Collage Msg
nodeLabelCollage node =
    let n = node.label in
    let id = node.id in
    let color = if n.isActive then red else black in
    (
     if n.editable then
         make_input n.label (NodeLabelEdit id)
     else
         if  n.label == "" then
             (circle 5 |> filled (uniform color))
         else
             (Collage.Text.fromString n.label
             |> Collage.Text.color color
             |> rendered)
        ) |> shift n.pos

nodeCollage : Node NodeCollageLabel -> Collage Msg
nodeCollage n =
    nodeLabelCollage n
        |> Collage.Events.onClick (NodeClick n.id)
        |> Collage.Events.onMouseEnter (\ _ -> NodeEnter n.id)
        |> Collage.Events.onMouseLeave (\ _ -> NodeLeave n.id)



segmentLabel : Graph.EdgeNodes NodeCollageLabel EdgeCollageLabel -> Collage Msg
segmentLabel {from, to, label} =
    let
        edgeId = (from.id, to.id)
        fromP = from.label.pos
        toP = to.label.pos
        delta = minusP toP fromP
        middle = middleP fromP toP
        coef  = 10
        orth = normaliseP coef <| orthogonalP delta
        labelpos = addP middle orth
    in
        (if label.editable then
             make_input label.label
             (EdgeLabelEdit edgeId)
        else
            (Collage.Text.fromString label.label |> rendered)
        ) |> shift labelpos


edgeCollage : Graph.EdgeNodes NodeCollageLabel EdgeCollageLabel -> Collage Msg
edgeCollage ({from, to , label} as e) =
    let c = if label.isActive then red else black in
    let edgeId = (from.id, to.id) in
    Collage.group [
         arrow c from.label.pos to.label.pos,
                       segmentLabel e]
        |>  Collage.Events.onClick (EdgeClick edgeId)


graphCollage : Graph NodeCollageLabel EdgeCollageLabel -> Collage Msg
graphCollage g =
      let nodes = Graph.nodes g
          edges = Graph.edgesWithNodes g
      in
          List.map nodeCollage nodes ++
          List.map edgeCollage edges |>
          Collage.group

helpMsg : Model -> Html Msg
helpMsg model =
    case model.mode of
        DefaultMode ->
            let b = \ s -> Html.span []
                    [Html.text "[", Html.b [] [Html.text s], Html.text"]"]
            in
             Html.div []
                 [Html.text "Default mode. Commands: ",
                      b "click",
                      Html.text " for point/edge selection, new ",
                      b "a",
                      Html.text"rrow from selected point, new ",
                      b "p",
                      Html.text "oint, ",
                      b "del",
                      Html.text "ete selected object (also ",
                      b "x",
                      Html.text "), ",
                      b "r",
                      Html.text "ename selected object, ",
                      b "g",
                      Html.text " move selected point, ",
                      b "d",
                      Html.text "ebug mode, ",
                      b "u",
                      Html.text "nnamed flag (no labelling on point creation)."
                      -- b "b",
                      -- Html.text "litz flag (no labelling on point creation)."
            --  "[r]ename selected object, move selected point [g], [d]ebug mode"
                 ]
        DebugMode ->
           "Debug Mode. [ESC] to cancel and come back to the default mode. "
             ++ Debug.toString model
                 |> Html.text
        _ -> "Mode: " ++ Debug.toString model.mode ++ ". [ESC] to cancel and come back to the default"
             ++ " mode."
                 |> Html.text



view : Model -> Html Msg
view model =
    Html.div [] [
         Html.button [Html.Events.onClick (save model)
              ] [Html.text "Save"],
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
