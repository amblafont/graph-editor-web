module Modes.SplitArrow exposing (graphDrawing, initialise, update, help)



-- import Graph exposing (..)

import Polygraph as Graph exposing (Graph, NodeId, EdgeId)
import Maybe
import Msg exposing (Msg(..))
import HtmlDefs exposing (Key(..), computeLayout)
import GraphDefs exposing (NodeLabel, EdgeLabel)

import Modes exposing ( Mode(..), SplitArrowState)
import Model exposing (..)
import InputPosition exposing (InputPosition(..))

import GraphDrawing exposing (NodeDrawingLabel, EdgeDrawingLabel)
import Geometry.Point exposing (Point)





initialise : Model -> ( Model, Cmd Msg )
initialise m =
    case GraphDefs.selectedEdgeId m.graph of
      Nothing -> switch_Default m
      Just id ->        
        Graph.getEdge id m.graph
        |> Maybe.map 
        (\ e -> noCmd {m | mode = SplitArrow 
        { chosenEdge = id, source = e.from, target = e.to, pos = InputPosMouse,
          label = e.label,
          labelOnSource = True,
          guessPos = True}
        })
        
        -- |> Maybe.map
        -- -- prevent bugs (if the mouse is thought
        -- -- to be kept on a point)
        --    (Tuple.mapFirst (\model -> { model | mousePointOver = ONothing}))
        |> Maybe.withDefault (switch_Default m)






nextStep : Model -> Bool -> SplitArrowState -> ( Model, Cmd Msg )
nextStep model finish state =
         
    let
        info =
            stateInfo model state
    in
    let m2 = addOrSetSel False info.movedNode { model | graph = info.graph } in
     if finish then ({ m2 | mode = DefaultMode }, computeLayout())  else
        let ne1 = (info.ne1, info.le1)
            ne2 = (info.ne2, info.le2)
        in
        let ids = 
              if info.created then 
                [ (info.movedNode, GraphDefs.getLabelLabel info.movedNode info.graph),
                  ne1, ne2 ]
              else
                 [ ne1, ne2 ]
        in
        (initialise_RenameModeWithDefault ids m2, computeLayout ())
                          



-- for view







-- movedNode

type alias Info = { graph : Graph NodeLabel EdgeLabel,
                    movedNode : NodeId ,                    
                    created : Bool,
                    ne1 : EdgeId,
                    ne2 : EdgeId,
                    -- default label for renaming
                    le1 : String,
                    -- default label for renaming
                    le2 : String }

guessPosition : Model -> SplitArrowState -> Point
guessPosition m s = 
     case Graph.getNodes [s.source, s.target] m.graph
                        |> List.map (.label >> .pos)  of
       [p1, p2] -> Geometry.Point.middle p1 p2
       _ -> m.mousePos

stateInfo : Model -> SplitArrowState -> Info
stateInfo m state =
    let otherLabel = 
              m.graph |> GraphDefs.getLabelLabel 
              (if state.labelOnSource then 
                 state.target 
               else 
                 state.source)
    in
    let
        ( ( g, n ), created ) =
          let makeInfo pos = mayCreateTargetNodeAt m pos otherLabel in
           if state.guessPos then
             makeInfo (guessPosition m state)
           else
            case state.pos of
              InputPosGraph id -> ((m.graph, id), False)
              _ -> makeInfo m.mousePos                              
           
    in
    let ((l1, d1), (l2, d2)) = 
           let existingLabels = (state.label, state.label.label) in
           let newLabel = (GraphDefs.emptyEdge, otherLabel) in                  
           if state.labelOnSource then 
             (existingLabels, newLabel)
           else
             (newLabel, existingLabels)
    in
    let (g1, ne1) = (Graph.newEdge g state.source n l1) in
    let (g2, ne2) = (Graph.newEdge g1 n state.target l2) in
    { graph = Graph.removeEdge state.chosenEdge g2,
      created = created,
      movedNode = n,
      ne1 = ne1,
      le1 = d1,
      ne2 = ne2,
      le2 = d2 }
   





graphDrawing : Model -> SplitArrowState -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawing m state =
    let
        info =
            stateInfo m state
    in
    collageGraphFromGraph m info.graph        


update : SplitArrowState -> Msg -> Model -> ( Model, Cmd Msg )
update state msg model =
    let next finish = nextStep model finish state in
    let updateState st = { model | mode = SplitArrow st } in
    let updatePos st = InputPosition.updateNoKeyboard st.pos msg in
    case msg of  
        KeyChanged False _ (Control "Escape") -> switch_Default model
        KeyChanged False _ (Character '/') -> noCmd <| updateState
           { state | labelOnSource = not state.labelOnSource } 
        MouseClick -> next False          
        KeyChanged False _ (Control "Enter") -> next True
    --     TabInput -> Just <| ValidateNext
        KeyChanged False _ (Control "Tab") -> next False
        _ ->
           let newPos = InputPosition.updateNoKeyboard state.pos msg in
           let guessPos = case (msg, newPos) of 
                      (MouseMove _, _) -> False
                      (_, InputPosMouse) -> state.guessPos
                      _ -> False                      
           in          
           
            noCmd 
             <| updateState 
             { state | pos = newPos, guessPos = guessPos } 
           
help : String
help =
            "[ESC] cancel, [click] name the point (if new), "
            ++ "[/] to move the existing label on the other edge, "
            ++ "[RET] terminate the square creation"             
             
      