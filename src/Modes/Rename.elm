module Modes.Rename exposing (initialise, fixModel, help, graphDrawing, update, newState, renameSelectedId, renameId)

import Polygraph as Graph exposing (Graph)
import GraphDefs exposing (EdgeLabel, NodeLabel)
import Format.GraphInfo as GraphInfo exposing (GraphInfo, getActiveGraph)
import Msg exposing (Msg(..), Command(..))
import Format.GraphInfo exposing (Modif, TabId)
import Format.GraphInfoCodec
import GraphDrawing exposing (NodeDrawingLabel, EdgeDrawingLabel)
import Model exposing (..)
import Modif
import Maybe.Extra
import Modes exposing (Mode(..),RenameState)
import HtmlDefs exposing (Key(..))
import Html.Attributes exposing (action)
import CommandCodec exposing (protocolSend, protocolSendGraphModif, protocolSendModif)
import Msg exposing (ModifId)


newStateWithDefault : ModifId -> List { id : Graph.Id, tabId : TabId, label : String} -> RenameState
newStateWithDefault modifId ids = 
   { -- modifs = [GraphInfo.activeGraphModifHelper graphInfo modif], 
        next = ids
     
      , idModif = modifId,
      alreadySelected = False}

newState : GraphInfo -> ModifId -> List { id : Graph.Id, tabId : TabId, label : Maybe String} -> RenameState
newState graphInfo modifId modifs =
  newStateWithDefault modifId <|
   List.filterMap (\ {id, tabId, label} -> 
      case label of 
        Just l -> Just {id = id, tabId = tabId, label = l}
        Nothing ->
            GraphInfo.getGraph graphInfo tabId 
              |> Maybe.andThen (\graph -> GraphDefs.getLabelLabel id graph)
              |> Maybe.map (\s -> {id = id, tabId = tabId, label = s })
    ) modifs 
   
fixModel : Model -> RenameState -> Model
fixModel = initialise

initialise : Model -> RenameState -> Model
initialise m state =
    -- let m = applyTopModif state mi in

         
    let cancelModel = { m | mode = DefaultMode } in
    case (getHead m.graphInfo state, nextState state) of 
      (Nothing,Nothing) -> cancelModel
      (Just head,_) -> 
         activateTab { m | mode = RenameMode state } head.tabId
         |> Maybe.withDefault cancelModel
      (Nothing, Just state2) -> initialise m state2
    

getHead : GraphInfo -> RenameState -> Maybe { id : Graph.Id, label : String, tabId : TabId }
getHead gi {next} = 
    case next of
        [] -> Nothing
        head :: _ -> 
           let currentLabel = (GraphInfo.getGraph gi head.tabId |>
                     Maybe.andThen (GraphDefs.getLabelLabel head.id))
           in
           case currentLabel of 
             Nothing -> Nothing
             _ -> Just head
            --  (Just s, Just _) -> Just {id = id, tabId = tabId, label = s}


nextState : RenameState -> Maybe RenameState
nextState state =
  case state.next of
        [] -> Nothing
        _ :: q -> Just {state | next = q}

graphDrawing : Model -> RenameState -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawing model state =
  let graphInfo = model.graphInfo in
  case getHead graphInfo state of
    Nothing -> collageGraphFromGraph model <| Model.getActiveGraph model
    Just head ->
        let g = rename head graphInfo 
              |> Graph.applyModifHelper
              |> collageGraphFromGraph model
        in
            Graph.update head.id 
            (\n -> {n | editable = True })
            (GraphDrawing.mapNormalEdge
              <| \e ->  {e | editable = True })
            g

rename : { id : Graph.Id, label : String, tabId : TabId } 
      ->  GraphInfo -> Graph.ModifHelper NodeLabel EdgeLabel
  --  { modif : Modif,
      --  nextCommand : Command }
rename head m =
 
    
  --  case getHead m info of
  --     Nothing -> GraphInfo.Noop
  --     Just head -> 
       case GraphInfo.getGraph m head.tabId of
        Nothing -> Graph.emptyModifHelper
        Just modelGraph -> 
          let g = Graph.newModif modelGraph in
          let modif = Graph.md_update head.id 
                        (\ n -> {n | label = head.label, isCoqValidated = False })  
                         (GraphDefs.mapNormalEdge (\e -> {e | label = head.label }))
                               <| g
          in
          let finalModif = 
                --  GraphInfo.makeGraphChange  head.tabId <| Graph.finaliseModif
                  modif 
             
          in
          finalModif 


editLabel : RenameState -> String -> RenameState
editLabel state s = 
    {state | next = 
        case state.next of
          head :: q -> {head | label = s} :: q
          _ -> state.next -- should not happen
    }

type Action = 
     Finish
   | Tab
   | Cancel

update : RenameState -> Msg -> Model -> (Model, Cmd Msg)
update state msg model =
   let edit_label s = 
         noCmd {model | mode = RenameMode 
         <| editLabel state s
         }
   in 
    case msg of
      RenderedTextInput ->
            ({model | mode = RenameMode { state | alreadySelected = True }},
              Cmd.batch <| Msg.focusId HtmlDefs.idInput ::
              if state.alreadySelected then [] else
              [ HtmlDefs.select HtmlDefs.idInput ] )
      KeyChanged False _ (Control "Escape") -> nextStage Cancel state model
      KeyChanged False _ (Control "Enter") -> nextStage Finish state model
      KeyChanged False _ (Control "Tab") -> nextStage Tab state  model
    --   MouseClick -> finalise_RenameMode label model
      NodeLabelEdit _ s -> edit_label s
      EdgeLabelEdit _ s -> edit_label s
      _ -> noCmd model



nextStage : Action -> RenameState -> Model -> (Model, Cmd Msg)
nextStage action state model =
    let command =
         if action == Cancel then Cmd.none else
         getHead model.graphInfo state
         |> Maybe.map (\head ->
         rename head model.graphInfo |> -- .modif |>
          Graph.finaliseModif |>
         GraphInfo.makeGraphChange  head.tabId 
         |>
          protocolSendModif (state.idModif))
          |> Maybe.withDefault Cmd.none
    in
    let aux list =
            case (action, list) of
              (Tab, head :: t :: q) ->
                case 
                   activateTab {model|mode = RenameMode {state | next = t :: q}}
                      head.tabId
                      of 
                  Nothing -> aux (t :: q)
                  Just newModel -> newModel
              _ -> {model | mode = DefaultMode}
    in
    (aux state.next, command)
    

renameId : Model -> Graph.Id -> (Model, Cmd Msg)
renameId model id =
    let ids = [{id = id, label = Nothing, tabId = model.graphInfo.activeTabId}]
    in
        noCmd <| initialise model
        <| newState model.graphInfo Msg.defaultModifId
        <| ids
renameSelectedId : Model -> (Model, Cmd Msg)
renameSelectedId model =
    let modelGraph = Model.getActiveGraph model in
    GraphDefs.selectedId modelGraph 
    |> Maybe.map (renameId model)
    |> Maybe.withDefault (noCmd model)

help : String
help = "Rename mode: [RET] to confirm, [TAB] to next label, [ESC] to cancel"