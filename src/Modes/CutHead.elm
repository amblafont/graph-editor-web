module Modes.CutHead exposing (update, makeGraph, help, initialise)
import Modes exposing (CutHeadState, Mode(..), MoveDirection(..))
import Modes.Move
import Model exposing (Model, setActiveGraph, noCmd, toggleHelpOverlay, getActiveGraph)
import Msg exposing (Msg(..))
import Polygraph as Graph exposing (Graph)
import HtmlDefs exposing (Key(..))
import GraphDefs exposing (NodeLabel, EdgeLabel, edgeToNodeLabel)
import InputPosition exposing (InputPosition(..))
import Zindex

initialise : Model -> Model
initialise model =
   let modelGraph = getActiveGraph model in
   case GraphDefs.selectedEdge modelGraph of
      Nothing -> model
      Just e -> if GraphDefs.isPullshout e.label then model else 
                 {  model | mode = CutHead { edge = e, merge = False, head = True, duplicate = False } }   

help : String 
help =          HtmlDefs.overlayHelpMsg
                ++ ", [RET] or [click] to confirm, [ctrl] to toggle merging. [ESC] to cancel. "
                ++ "[c] to switch between head/tail"                
                ++ ", [d] to duplicate (or not) the arrow."

update : CutHeadState -> Msg -> Model -> (Model, Cmd Msg)
update state msg m =
  let finalise () = 
         (setActiveGraph {m | mode = DefaultMode} (makeGraph state m), Cmd.none)
         -- computeLayout())
  in
  let changeState s = { m | mode = CutHead s } in
  case msg of
        KeyChanged False _ (Character '?') -> noCmd <| toggleHelpOverlay m
        KeyChanged False _ (Control "Escape") -> ({ m | mode = DefaultMode}, Cmd.none)
        KeyChanged False _ (Control "Enter") -> finalise ()
        MouseClick -> finalise ()
        KeyChanged False _ (Character 'c') -> (changeState { state | head = (not state.head)} , Cmd.none)
        KeyChanged False _ (Character 'd') -> (changeState { state | duplicate = (not state.duplicate)} , Cmd.none)
        _ -> noCmd m

-- TODO: factor with newArrow.moveNodeInfo
makeGraph  : CutHeadState -> Model -> Graph NodeLabel EdgeLabel
makeGraph  {edge, head, duplicate, merge} model =
   let modelGraph = getActiveGraph model in
   let pos = model.mousePos in
   let (id1, id2) = if head then (edge.from, edge.to) else (edge.to, edge.from) in
   let nodeLabel = Graph.get id2
         (\label -> {label | pos = pos})(edgeToNodeLabel pos)
         modelGraph |> 
         Maybe.withDefault (GraphDefs.newNodeLabel pos "" True Zindex.defaultZ )
   in
   let extGraph =  Graph.makeCone modelGraph [id1] nodeLabel edge.label (not head) in
    let g4 = 
         if duplicate then 
            GraphDefs.unselect edge.id extGraph.extendedGraph 
         else
            List.foldl (\ id -> Graph.merge id edge.id) extGraph.extendedGraph
            extGraph.edgeIds
    in
   let moveInfo =
         Modes.Move.mkGraph model InputPosMouse Free merge g4 extGraph.newSubGraph 
   in
    moveInfo.graph
