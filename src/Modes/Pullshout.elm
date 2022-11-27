module Modes.Pullshout exposing (initialise, update, graphDrawing, help)
import Polygraph as Graph exposing (EdgeId, Graph)
import Modes exposing (Mode(..), PullshoutState, PullshoutKind(..))
import Model exposing (switch_Default)
import Model exposing (Model, switch_Default, noCmd, collageGraphFromGraph)
import Msg exposing (Msg(..))
import HtmlDefs exposing (Key(..))
import GraphDrawing exposing (NodeDrawingLabel, EdgeDrawingLabel)
import GraphDefs exposing (NodeLabel, EdgeLabel)
import List.Extra
import Model exposing (setSaveGraph)

initialise : Graph NodeLabel EdgeLabel -> EdgeId -> PullshoutKind -> Maybe PullshoutState
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

possibleDests : Graph NodeLabel EdgeLabel -> EdgeId -> PullshoutKind -> List EdgeId
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
          |> List.filter (.label >> GraphDefs.isPullshout)
          |> List.map .to
   in   
      List.filter (\ i -> List.Extra.notMember i pbks) l

    
graph : Model -> PullshoutState -> Graph NodeLabel EdgeLabel
graph m s =
   Graph.newEdge m.graph s.chosenEdge s.currentDest
   GraphDefs.newPullshout
   |> Tuple.first

graphDrawing : Model -> PullshoutState -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawing m s =
    -- let defaultView movedNode = m.graph{ graph = m.graph, movedNode = movedNode}  in
    -- graphMakeEditable (renamableFromState s) <|
    collageGraphFromGraph m <|
        graph m s
        {-
            let info = moveNodeInfo m s in
             info.graph  -}

nextPullshout : Model -> PullshoutKind -> PullshoutState -> PullshoutState
nextPullshout m k st =
   let recompute () = 
         case possibleDests m.graph st.chosenEdge k of
           [] -> st
           t :: q -> { st | currentDest = t, possibilities = q, kind = k}
   in
   if k /= st.kind then recompute () else
   case st.possibilities of
       t :: q -> { st | currentDest = t, possibilities = q}
       [] -> recompute ()

update : PullshoutState -> Msg -> Model -> ( Model, Cmd Msg )
update state msg model =
    let updateState st = { model | mode = PullshoutMode st } in
    case msg of  
        KeyChanged False _ (Control "Escape") -> switch_Default model  
        KeyChanged False _ (Character 'p') -> noCmd <| updateState <| nextPullshout model Pullback state 
        KeyChanged False _ (Character 'P') -> noCmd <| updateState <| nextPullshout model Pushout state 
        KeyChanged False _ (Control "Enter") -> 
           switch_Default <| setSaveGraph model <| graph model state
        _ -> noCmd model


help : String
help =
            "[ESC] cancel, "
            ++ "cycle between [p]ullback/[P]ushout possibilities, "
            ++ "[RET] confirm"             
             