module Command exposing (applyCommand)

import Format.GraphInfo as GraphInfo exposing (GraphInfo, Modif)
import Geometry.Point exposing (Point)
import Polygraph as Graph exposing (Graph)
import Codec exposing (Codec)
import Format.GraphInfoCodec as GraphInfoCodec exposing (codecModif, defaultModifJS)
import Format.GraphInfoCodec exposing (defaultModifJS)
import Polygraph exposing (modifCodec)
import Modif
import Msg exposing (Command(..), ProtocolMsg(..), ProtocolModif, Scenario(..))
import Model exposing (..)
import Modes exposing (Mode(..))
import GraphDefs
import IntDict
import Modes.Rename
import Modes.Move
import Modes exposing (EnlargeState)
import Modes.NewArrow
import Modes.Color
import Modes.CutHead
import Modes.SplitArrow
import Modes.Pullshout
import Modes.Square



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
   let defaultModel = { model | mode = DefaultMode } in
   let ifTabChanged m = if changedTab then defaultModel else m () in
   let defaultIfTabChanged = ifTabChanged (always model) in
   case model.mode of
      DefaultMode -> model
      DebugMode -> model
      RectSelect _ -> defaultIfTabChanged
      ResizeMode _ -> defaultIfTabChanged 
      EnlargeMode _ -> defaultIfTabChanged
      NewLine state -> defaultIfTabChanged
      NewArrow state -> 
         ifTabChanged <| \ _ -> Modes.NewArrow.fixModel model state
      Move _ -> if changedTab || not (Modes.Move.isValid model) then 
                    defaultModel
                else 
                    model
      RenameMode state ->
         ifTabChanged <| \ _ -> Modes.Rename.fixModel model state
      ColorMode ids -> ifTabChanged <| \ _ -> Modes.Color.fixModel model
      CutHead state -> ifTabChanged <| \ _ -> Modes.CutHead.fixModel model state
      SplitArrow state -> ifTabChanged <| \ _ -> Modes.SplitArrow.fixModel model state
      PullshoutMode state -> ifTabChanged <| \ _ -> Modes.Pullshout.fixModel model state
      SquareMode state ->  ifTabChanged <| \ _ -> Modes.Square.fixModel model state
      
      
      


applyCommand : {isSender : Bool, msg : ProtocolMsg} -> Model -> Model
applyCommand {isSender, msg} model =
   case msg of
     Snapshot g -> fixModel <| setGraphInfo model g
     ModifProtocol m -> fixModel <| applyModifProtocol isSender m model
     ClearProtocol {scenario, preamble} -> 
        let modelf = clearModel model in
        let or s1 s2 = if s1 == "" then s2 else s1 in 
        let gi = modelf.graphInfo in
        { modelf | scenario = scenario,
            graphInfo = { gi | 
              latexPreamble = or preamble gi.latexPreamble }
         } 
     LoadProtocol {graph, scenario} ->
        let m =  clearHistory <| setGraphInfo { model | 
                                              mode = DefaultMode, 
                                              scenario = scenario
                                            } graph 
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
        m2
       

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
       if model.mode /= DefaultMode || not isSender then nextModel else
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


      



