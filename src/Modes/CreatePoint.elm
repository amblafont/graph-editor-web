module Modes.CreatePoint exposing (initialise)
import GraphDefs exposing (NodeLabel, EdgeLabel, newNodeLabel)
import Polygraph as Graph exposing (Graph)
import Geometry.Point as Point exposing (Point)
import Zindex exposing (defaultZ)
import Msg exposing (Command(..))
import Format.GraphInfo as GraphInfo exposing (GraphInfo, Modif, TabId)
import Model exposing (..)
import Msg exposing (Msg)
import Format.GraphInfo exposing (activeGraphModif)
import Modes.Rename
import GraphDefs exposing (addOrSetSel)
import CommandCodec exposing (protocolSendModif, protocolSend)
import IntDict
import Format.GraphInfo exposing (activeGraphModifHelper)

initialise : Bool -> Model -> (Model, Cmd Msg)
initialise isMath model =
  let modelGraph = getActiveGraph model in
  let tabId = model.graphInfo.activeTabId in
  
  let pos = model.mousePos in
  let (modif, newId) =
                    Graph.md_newNode 
                    (Graph.newModif modelGraph )
                    (newNodeLabel pos "" isMath defaultZ)
  in
  -- let finalModif = GraphInfo.GraphChange in
  let selIds = IntDict.insert tabId [newId] IntDict.empty in
  let renIds = [{id = newId, label = Just "", tabId = tabId}] 
  in
  let (nextModel, idModif) = popIdModif model in
  (nextModel, protocolSend {
     id = idModif,
     modif = activeGraphModifHelper model.graphInfo modif,
     command = RenameCommand renIds,
     selIds = selIds
    })
  

   
     