module Command exposing (applyCommands,undo)

import Format.GraphInfo as GraphInfo exposing (GraphInfo, Modif)
import Geometry.Point exposing (Point)
import Polygraph as Graph exposing (Graph)
import Codec exposing (Codec)
import Format.GraphInfoCodec as GraphInfoCodec exposing (codecModif, defaultModifJS)
import Format.GraphInfoCodec exposing (defaultModifJS)
import Polygraph exposing (modifCodec)
import Modif
import Msg exposing (Command(..), ProtocolMsg(..), ProtocolModif, Scenario(..), Msg)
import Model exposing (..)
import Modes exposing (Mode(..))
import GraphDefs
import IntDict
import Modes.Rename
import Modes.Move
import Modes exposing (EnlargeState)
import Modes.NewArrow
import Modes.Customize
import Modes.CutHead
import Modes.SplitArrow
import Modes.Pullshout
import Modes.Square
import Modes.Bend
import CommandCodec exposing (protocolSendMsg)
import HtmlDefs -- exposing (computeLayout)
import Maybe.Extra



{-
applyModif : GraphInfo -> ProtocolMsg -> 
     Modif.Result {command : Command, graphInfo : GraphInfo} Modif
applyModif gi msg =
   GraphInfo.applyModif gi msg.modif 
   |> Modif.map 
     (\ {graphInfo, idTranslator} ->
        {graphInfo = graphInfo, command = recomputeCommandIds idTranslator msg.command})
    identity -}


-- maybe the mode is not valid anymore, because of what happened?
fixModel : Model -> Model
fixModel modeli =
   let (model, changedTab) = 
        if GraphInfo.getActiveTabOption modeli.graphInfo == Nothing then
          (activateFirstTab modeli, True)
        else 
          (modeli, False)
   in
   let defaultModel = setMode DefaultMode model in
   let ifTabChanged m = if changedTab then defaultModel else m () in
   let defaultIfTabChanged = ifTabChanged (always model) in
   case currentMode model of
      MakeSaveMode -> model
      DefaultMode -> model
      DebugMode -> model
      RectSelect _ -> defaultIfTabChanged
      ResizeMode _ -> defaultIfTabChanged 
      EnlargeMode _ -> defaultIfTabChanged
      NewLine state -> defaultIfTabChanged
      BendMode state -> Modes.Bend.fixModel model state
      NewArrow state -> 
         ifTabChanged <| \ _ -> Modes.NewArrow.fixModel model state
      Move _ -> if changedTab || not (Modes.Move.isValid model) then 
                    defaultModel
                else 
                    model
      RenameMode state ->
         ifTabChanged <| \ _ -> Modes.Rename.fixModel model state
      CustomizeMode ids -> ifTabChanged <| \ _ -> Modes.Customize.fixModel model
      CutHead state -> ifTabChanged <| \ _ -> Modes.CutHead.fixModel model state
      SplitArrow state -> ifTabChanged <| \ _ -> Modes.SplitArrow.fixModel model state
      PullshoutMode state -> ifTabChanged <| \ _ -> Modes.Pullshout.fixModel model state
      SquareMode state ->  ifTabChanged <| \ _ -> Modes.Square.fixModel model state
      LatexPreamble _ -> model
      

undo : Model -> (Model, Cmd Msg.Msg)
undo m =
  --  case Debug.log "undo" m.history of
   case m.history of
      [] -> noCmd m
      t :: q ->
        let m2 = { m | history= q, 
                     topModifId = Msg.defaultModifId
                     }
        in
        -- case  Modif.fold GraphInfo.applyModifSimple m2.graphInfo t of 
        --   Nothing -> 
        --     --  let _ = Debug.log "" "failed to undo" in
        --      noCmd m2
        --   Just _ ->
            (m2, protocolSendMsg (Undo t))
      
{-

Dealing with Undo is a bit tricky: we want to undo the previous action if
reversing the top action is a noop. But the top action is a sequence of modifs,
so in principle we would need to check if a sequence of modifs is a noop, which
is tricky. So only detect the case where it is a sequence of noops.
-}
applyCommands :  List {isSender : Bool, msg : ProtocolMsg} -> Model -> (Model, Cmd Msg)
applyCommands arg model = 
   let aux msg status =
            let ret = applyCommand msg status.model in
            {
               model = ret.model,
               -- computeLayout = ret.computeLayout || status.computeLayout,
               undo = mergeUndoStatus ret.undo status.undo,
               focus = Maybe.Extra.or ret.focus status.focus
            }
   in
   let ret = List.foldl aux 
                   {model = model, -- computeLayout = False,
                   focus = Nothing, undo = NoUndo }
                      arg
   in
   let (finalModel, cmd) = if ret.undo == FailedUndo then 
                               undo ret.model 
                           else noCmd ret.model 
   in
   let (finalModel2, cmd2) =
         (
         if currentMode finalModel /= DefaultMode then noCmd finalModel else
         case ret.focus of
           Nothing -> noCmd finalModel
           Just {tabId, pos, selIds} -> 
            case activateTab finalModel tabId of 
              Nothing -> noCmd finalModel
              Just newModel -> 
                let model2 = 
                        updateActiveGraph  (setMode DefaultMode newModel)
                      (GraphDefs.selectIds selIds)
                in
                (model2, HtmlDefs.focusPosition pos)
         )
      
   in 
  (fixModel finalModel2, Cmd.batch <| 
     [cmd,  cmd2] -- :: (if ret.computeLayout then [computeLayout ()] else [])
    
     
     )

type UndoStatus =
    FailedUndo
  | AtLeastOneUndo
  | NoUndo

mergeUndoStatus : UndoStatus -> UndoStatus -> UndoStatus
mergeUndoStatus s1 s2 =
   case (s1,s2) of
      (FailedUndo, _) -> FailedUndo
      (_, FailedUndo) -> FailedUndo
      (AtLeastOneUndo, _) -> AtLeastOneUndo
      (_, AtLeastOneUndo) -> AtLeastOneUndo
      (NoUndo, NoUndo) -> NoUndo

applyCommand : {isSender : Bool, msg : ProtocolMsg} -> Model ->
               { model : Model, -- computeLayout : Bool, 
               undo : UndoStatus,
                 focus : Maybe {tabId : GraphInfo.TabId, pos : Point, selIds : List Graph.Id} }
applyCommand {isSender, msg} model =
   let returnComputeLayout m = { model = m, -- computeLayout = True, 
                       focus = Nothing, undo = NoUndo } 
   in
   case msg of
     Snapshot g -> returnComputeLayout <| setGraphInfo model g
     ModifProtocol m -> returnComputeLayout <| applyModifProtocol isSender m model
     Undo modifs ->
          case  Modif.fold GraphInfo.applyModifSimple model.graphInfo modifs of
           Just r -> { model = { model | graphInfo = r.next},
                           focus = Nothing,
                          -- computeLayout = True, 
                           undo = if isSender then AtLeastOneUndo else NoUndo }
           Nothing -> { model = model, -- computeLayout = False, 
                       focus = Nothing,
                      undo = if isSender then FailedUndo else NoUndo }
     ClearProtocol {scenario, preamble} -> 
        let modelf = clearModel model in
        let or s1 s2 = if s1 == "" then s2 else s1 in 
        let gi = modelf.graphInfo in
        returnComputeLayout 
        { modelf | scenario = scenario,
            graphInfo = { gi | 
              latexPreamble = or preamble gi.latexPreamble }
         } 
     LoadProtocol {graph, scenario} ->
        let m =  clearHistory <| setGraphInfo (setMode DefaultMode { model | 
                                              scenario = scenario
                                            }) graph 
        in
        let m2 =
                if scenario /= Exercise1 then m else
                updateTabs m ( List.map (\t -> 
                  { t | graph = 
                      Graph.map 
                        (\ _ n -> { n | selected = String.contains "\\bullet" n.label})
                        (always identity) t.graph}
                    ) )
        in 
        returnComputeLayout m2
     FocusPosition arg -> 
         { model = model, -- computeLayout = False, 
           focus =  Just arg, -- if not isSender then Just arg else Nothing,
           undo = NoUndo }
        

applyModifProtocol : Bool -> ProtocolModif -> Model -> Model
applyModifProtocol isSender msg model =
   case GraphInfo.applyModif model.graphInfo msg.modif of
     Nothing -> model
     Just result ->
       let nextModel = 
             (if isSender then pushHistory msg.id [result.undo] else identity) 
             { model | graphInfo = result.next.graphInfo}
       in
       -- applyModifResult isSender model msg.id (Just result) in 
       if currentMode model /= DefaultMode || not isSender then nextModel else
       let idTranslator = result.next.idTranslator in
       let selModel = 
            updateTabs nextModel
                  (List.map  (\ tab -> 
                    case IntDict.get tab.id msg.selIds of
                      Nothing -> tab
                      Just ids -> 
                         {tab | graph = GraphDefs.selectIds 
                                     (List.map (Graph.translateId idTranslator) ids) 
                                      tab.graph}
                    )
                  ) 
      in
       case recomputeCommandIds idTranslator msg.command of
           RenameCommand ls ->
              Modes.Rename.initialise selModel
                 (Modes.Rename.newState selModel.graphInfo msg.id ls)
           MoveCommand mode -> Modes.Move.initialise msg.id mode selModel
           Noop -> selModel
          

recomputeCommandIds : Graph.TranslationId -> Command -> Command
recomputeCommandIds translator cmd =
   case cmd of
         RenameCommand l -> RenameCommand <| List.map (\ r -> {r | id = Graph.translateId translator r.id} ) l
         MoveCommand _ -> cmd -- <| List.map (Graph.translateId translator) l
         Noop -> cmd
      --    LoadCommand _ -> cmd


      



