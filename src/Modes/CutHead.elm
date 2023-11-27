module Modes.CutHead exposing (update, makeGraph, help)
import Modes exposing (CutHeadState, Mode(..))
import Model exposing (Model, setActiveGraph, noCmd, toggleHelpOverlay, getActiveGraph)
import Msg exposing (Msg(..))
import Polygraph as Graph exposing (Graph)
import HtmlDefs exposing (Key(..))
import GraphDefs exposing (NodeLabel, EdgeLabel)

help : String 
help =             "[?] to toggle help overlay,"
                ++ " [RET] or [click] to confirm, [ctrl] to merge the endpoint with existing node. [ESC] to cancel. "
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


makeGraph  : CutHeadState -> Model -> Graph NodeLabel EdgeLabel
makeGraph  {id, head, duplicate} m =
   let modelGraph = getActiveGraph m in
   let pos = m.mousePos in
    Graph.getEdge id modelGraph 
    |> Maybe.andThen (\e -> Graph.getNode (if head then e.to else e.from)
         modelGraph 
    |> Maybe.map (\ nto -> 
    let g1 = modelGraph in
    let label = {nto | pos = pos } in
    let (g2, newId) = Graph.newNode g1 label in
    let (n1, n2) = if head then (e.from, newId) else (newId, e.to) in
    let (g3, edgeId) = Graph.newEdge g2 n1 n2  e.label in
    let g4 = 
         if duplicate then 
            GraphDefs.unselect id g3 
         else 
            Graph.merge edgeId id g3 
    in
    let g5 = if m.specialKeys.ctrl then 
                     Tuple.first <| 
                     GraphDefs.mergeWithSameLoc
                       { id = newId, label = label }
                       g4
             else g4
    in
    
    g5
    ))   
    |> Maybe.withDefault modelGraph