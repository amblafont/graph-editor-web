module Modes.Pullback exposing (initialise, update, graphDrawing, help)
import Polygraph as Graph exposing (EdgeId, Graph)
import Modes exposing (Mode(..), PullbackState, PullbackKind(..))
import Model exposing (switch_Default)
import Model exposing (Model, switch_Default, noCmd, collageGraphFromGraph)
import Msg exposing (Msg(..))
import HtmlDefs exposing (Key(..))
import GraphDrawing exposing (NodeDrawingLabel, EdgeDrawingLabel)
import GraphDefs exposing (NodeLabel, EdgeLabel)
import List.Extra
import Model exposing (setSaveGraph)

initialise : Graph NodeLabel EdgeLabel -> EdgeId -> PullbackKind -> Maybe PullbackState
initialise g id k =
   case possibleDests g id k of
     t :: q ->
          Just { chosenEdge = id,
            source = 0,
            target = 0,
            kind = k,
            currentDest = t, 
            possibilities = q}
     [] -> Nothing

possibleDests : Graph NodeLabel EdgeLabel -> EdgeId -> PullbackKind -> List EdgeId
possibleDests g id k =
   let l = 
         Graph.getEdge id g
        |> Maybe.map (if k == Pullback then .from else .to)
        |> Maybe.map (\ n -> (if k == Pullback then Graph.outgoings 
               else Graph.incomings) n g)
         |> Maybe.withDefault []
         |> List.map .id
         |> List.Extra.remove id
   in
   let pbks = Graph.outgoings id g
          |> List.filter (.label >> GraphDefs.isPullback)
          |> List.map .to
   in   
      List.filter (\ i -> List.Extra.notMember i pbks) l

    
graph : Model -> PullbackState -> Graph NodeLabel EdgeLabel
graph m s =
   Graph.newEdge m.graph s.chosenEdge s.currentDest
   GraphDefs.newPullback
   |> Tuple.first

graphDrawing : Model -> PullbackState -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawing m s =
    -- let defaultView movedNode = m.graph{ graph = m.graph, movedNode = movedNode}  in
    -- graphMakeEditable (renamableFromState s) <|
    collageGraphFromGraph m <|
        graph m s
        {-
            let info = moveNodeInfo m s in
             info.graph  -}

nextPullback : Model -> PullbackState -> PullbackState
nextPullback m st = 
   case st.possibilities of
       t :: q -> { st | currentDest = t, possibilities = q}
       [] -> nextPullback m 
           { st | possibilities = possibleDests m.graph st.chosenEdge st.kind }

update : PullbackState -> Msg -> Model -> ( Model, Cmd Msg )
update state msg model =
    let updateState st = { model | mode = PullbackMode st } in
    case msg of  
        KeyChanged False _ (Control "Escape") -> switch_Default model  
        KeyChanged False _ (Character 'p') -> noCmd <| updateState <| nextPullback model state 
        KeyChanged False _ (Control "Enter") -> 
           switch_Default <| setSaveGraph model <| graph model state
        _ -> noCmd model


help : String
help =
            "[ESC] cancel, "
            ++ "[p] cycle between pullback possibilities, "
            ++ "[RET] confirm"             
             