module Modes.Pullshout exposing (initialise, fixModel, update, graphDrawing, help)
import Polygraph as Graph exposing (EdgeId, Graph)
import Modes exposing (Mode(..), PullshoutState, PullshoutKind(..))
import Model exposing (switch_Default)
import Model exposing (Model, switch_Default, noCmd, collageGraphFromGraph, getActiveGraph)
import Msg exposing (Msg(..))
import HtmlDefs exposing (Key(..))
import GraphDrawing exposing (NodeDrawingLabel, EdgeDrawingLabel)
import GraphDefs exposing (NodeLabel, EdgeLabel)
import List.Extra
import Model exposing ({- setSaveGraph, -} toggleHelpOverlay)
import CommandCodec exposing (updateModifHelper)
import Drawing.Color as Color

initialise : Graph NodeLabel EdgeLabel -> EdgeId -> PullshoutKind -> Maybe PullshoutState
initialise g id k =
   case possibleDests g id k of
     t :: q ->
          Just { chosenEdge = id,
            kind = k,
            color = Color.black,
            currentDest = t, 
            possibilities = q}
     [] -> Nothing

fixModel : Model -> PullshoutState -> Model
fixModel model state =
   let modelGraph = getActiveGraph model in
   if Graph.getEdge state.chosenEdge modelGraph == Nothing 
   then {model | mode = DefaultMode} else
   case List.filter (Graph.exists modelGraph) 
     (state.currentDest::state.possibilities) of
        [] -> {model | mode = DefaultMode}
        t :: q -> {model | mode = PullshoutMode 
                        {state | currentDest = t, possibilities = q}}
   

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

    
graph : Model -> PullshoutState -> Graph.ModifHelper NodeLabel EdgeLabel
graph m s =
   Graph.md_newEdge (Graph.newModif <| getActiveGraph m) 
      s.chosenEdge s.currentDest
   (GraphDefs.newPullshout s.color)
   |> Tuple.first

graphDrawing : Model -> PullshoutState -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawing m s =
    -- let defaultView movedNode = m.graph{ graph = m.graph, movedNode = movedNode}  in
    -- graphMakeEditable (renamableFromState s) <|
    collageGraphFromGraph m <| Graph.applyModifHelper <|
        graph m s
        {-
            let info = moveNodeInfo m s in
             info.graph  -}

nextPullshout : Model -> PullshoutKind -> PullshoutState -> PullshoutState
nextPullshout m k st =
   let recompute () = 
         case possibleDests (getActiveGraph m) st.chosenEdge k of
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
        KeyChanged False _ (Character '?') -> noCmd <| toggleHelpOverlay model
        KeyChanged False _ (Control "Escape") -> switch_Default model  
        KeyChanged False _ (Character 'p') -> noCmd <| updateState <| nextPullshout model Pullback state 
        KeyChanged False _ (Character 'P') -> noCmd <| updateState <| nextPullshout model Pushout state 
        KeyChanged False _ (Control "Enter") -> 
            updateModifHelper { model | mode = DefaultMode } <| graph model state
        KeyChanged False _ (Character c) ->
           case Color.fromChar c of
            Nothing -> noCmd model
            Just col -> noCmd <| updateState { state | color = col }
        _ -> noCmd model


help : String
help =
            "[ESC] cancel, " ++
            HtmlDefs.overlayHelpMsg 
            ++ ", cycle between [p]ullback/[P]ushout possibilities, "
            ++ Color.helpMsg
            ++ ", [RET] confirm"             
             