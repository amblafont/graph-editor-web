module Modes.NewLine exposing (graphDrawing, initialise, update, help)


import GraphDrawing exposing (..)
import Polygraph as Graph exposing (Graph)
import Msg exposing (Msg(..))
import ArrowStyle 
import HtmlDefs exposing (Key(..))
import GraphDefs exposing (NodeLabel, EdgeLabel)
import Modes exposing ( NewLineState, Mode(..),  ArrowMode(..))
import InputPosition exposing (InputPosition(..))
import Model exposing (..)
import Modes exposing (PullshoutKind(..))
import Modes exposing (MoveDirection(..))
import Zindex





initialise : Model -> ( Model, Cmd Msg )
initialise m =
     noCmd { m  | mode = NewLine
        { initialPos = m.mousePos }  }
            

update : NewLineState -> Msg -> Model -> ( Model, Cmd Msg )
update state msg model =
    let finalise () = let newModel = setSaveGraph model <| makeGraph model state in
                switch_Default newModel
    in
    case msg of
        KeyChanged False _ (Character '?') -> noCmd <| toggleHelpOverlay model
        KeyChanged False _ (Control "Escape") -> switch_Default model
        MouseClick -> finalise ()
        KeyChanged False _ (Character 'n') -> finalise ()
        _ -> noCmd model
           
    
                  
                
makeGraph : Model -> NewLineState -> Graph NodeLabel EdgeLabel
makeGraph m s =       
    let graph = getActiveGraph m in
    let style = ArrowStyle.simpleLineStyle in
    let edgeLabel = GraphDefs.newEdgeLabel 
                              "" 
                              style 
    in
    let nodeLabel = (GraphDefs.newNodeLabel s.initialPos " " True Zindex.defaultZ) in
    let newNodeLabel = {nodeLabel | pos = m.mousePos } in
    let (ngraph,id) = Graph.newNode graph nodeLabel in
    let finalGraph = Graph.makeCone ngraph [id] newNodeLabel edgeLabel False in
    finalGraph.extendedGraph

graphDrawing : Model -> NewLineState -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawing m s =
   
    collageGraphFromGraph m <| makeGraph m s
         
help : String
help =
--  case s of
--         NewLineMoveNode _ ->
            -- Debug.toString st ++
            HtmlDefs.overlayHelpMsg ++
            ", [ESC] cancel, [click, n] to finalise, "
            