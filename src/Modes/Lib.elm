module Modes.Lib exposing (..)
-- utilities

import Model exposing (..)
import GraphDefs exposing (NodeLabel, EdgeLabel)
import Polygraph as Graph exposing (Graph, Edge, EdgeId)
import GraphDrawing exposing (NodeDrawingLabel, EdgeDrawingLabel)
import CommandCodec exposing (updateModifHelper)
import Modes exposing (Mode(..))

type alias Api state a = 
 { -- finaliseModif : Model -> state -> Graph.ModifHelper NodeLabel EdgeLabel,
    finalise : Model -> state -> (Model, Cmd a),
    graphDrawing : Model -> state -> Graph NodeDrawingLabel EdgeDrawingLabel
  }

makeApi : (Graph NodeLabel EdgeLabel -> state -> Graph.ModifHelper NodeLabel EdgeLabel) -> Api state a 
makeApi f = 
  let finaliseModif m state = 
         f (getActiveGraph m) state
  in
  {
    graphDrawing = \ m state -> 
       let modif = finaliseModif m state in
        collageGraphFromGraph m <| Graph.applyModifHelper modif,
    finalise =  \model state ->   updateModifHelper (setMode DefaultMode model)
    <| finaliseModif model state
  }
