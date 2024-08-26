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
import CommandCodec exposing (updateModifHelper)




initialise : Model -> ( Model, Cmd Msg )
initialise m =
     noCmd { m  | mode = NewLine
        { initialPos = m.mousePos }  }
            

update : NewLineState -> Msg -> Model -> ( Model, Cmd Msg )
update state msg model =
    let finalise () = let newModel = { model | mode = DefaultMode } in 
              updateModifHelper newModel <| makeGraph newModel state

    in
    case msg of
        KeyChanged False _ (Character '?') -> noCmd <| toggleHelpOverlay model
        KeyChanged False _ (Control "Escape") -> switch_Default model
        MouseClick -> finalise ()
        KeyChanged False _ (Character 'n') -> finalise ()
        _ -> noCmd model
           
    
                  
                
makeGraph : Model -> NewLineState -> Graph.ModifHelper NodeLabel EdgeLabel
makeGraph m s =       
    let graph = getActiveGraph m |> Graph.newModif in
    let style = ArrowStyle.simpleLineStyle in
    let edgeLabel = GraphDefs.newEdgeLabel 
                              "" 
                              style 
    in
    let nodeLabel = (GraphDefs.newNodeLabel s.initialPos " " True Zindex.defaultZ) in
    let newNodeLabel = {nodeLabel | pos = m.mousePos } in
    let (ngraph,id) = Graph.md_newNode graph nodeLabel in
    let finalGraph = Graph.md_makeCone ngraph [id] newNodeLabel edgeLabel False in
    finalGraph.extendedGraph

graphDrawing : Model -> NewLineState -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawing m s =
   
    collageGraphFromGraph m <| Graph.applyModifHelper <| makeGraph m s
         
help : String
help =
--  case s of
--         NewLineMoveNode _ ->
            -- Debug.toString st ++
            HtmlDefs.overlayHelpMsg ++
            ", [ESC] cancel, [click, n] to finalise, "
            