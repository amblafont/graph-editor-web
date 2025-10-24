module Modes.NewLine exposing (graphDrawing, initialise, update, help)


import GraphDrawing exposing (..)
import Polygraph as Graph exposing (Graph)
import Msg exposing (Msg(..))
import ArrowStyle 
import HtmlDefs exposing (Key(..))
import GraphDefs exposing (NodeLabel, EdgeLabel)
import Modes exposing ( NewLineState, Mode(..))
import InputPosition exposing (InputPosition(..))
import Model exposing (..)
import Modes exposing (PullshoutKind(..))
import Modes exposing (MoveDirection(..))
import Zindex
import CommandCodec exposing (updateModifHelper)




initialise : Model -> ( Model, Cmd Msg )
initialise m =
     noCmd (setMode (NewLine
        { initialPos = m.mousePos, bend = 0 }) m)
            

update : NewLineState -> Msg -> Model -> ( Model, Cmd Msg )
update state msg model =
    let finalise () = let newModel = setMode DefaultMode model in 
              updateModifHelper newModel <| makeGraph newModel state

    in
    let updState s = noCmd (setMode (NewLine s) model) in
    case msg of
        KeyChanged False _ (Character '?') -> noCmd <| toggleHelpOverlay model
        KeyChanged False _ (Control "Escape") -> switch_Default model
        MouseClick -> finalise ()
        KeyChanged False _ (Character 'n') -> finalise ()
        KeyChanged False _ (Character 'b') -> updState {state | bend = ArrowStyle.decreaseBend state.bend}
        KeyChanged False _ (Character 'B') -> updState {state | bend = ArrowStyle.increaseBend state.bend}
        _ -> noCmd model
           
    
                  
                
makeGraph : Model -> NewLineState -> Graph.ModifHelper NodeLabel EdgeLabel
makeGraph m s =       
    let graph = getActiveGraph m |> Graph.newModif in
    let style = ArrowStyle.simpleLineStyle s.bend in
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
            ", [ESC] cancel, [click, n] to finalise, [bB] change bend"
            