module Modes.SplitArrow exposing (graphDrawing, initialise, update, help)



-- import Graph exposing (..)

import Polygraph as Graph exposing (Graph, NodeId, EdgeId)
import Maybe exposing (withDefault)
import Msg exposing (Msg(..))
import HtmlDefs exposing (Key(..))
import GraphDefs exposing (NodeLabel, EdgeLabel)

import Modes exposing ( Mode(..), SplitArrowState)
import Model exposing (..)
import InputPosition exposing (InputPosition(..))

import GraphDrawing exposing (NodeDrawingLabel, EdgeDrawingLabel)






-- second argument: the n-th possibility


initialise : Model -> ( Model, Cmd Msg )
initialise m =
    case 
       activeObj m
        |> objToEdge
        of
      Nothing -> switch_Default m
      Just id ->        
        Graph.getEdge id m.graph
        |> Maybe.map 
        (\(i1, i2, _) -> noCmd {m | mode = SplitArrow 
        { chosenEdge = id, source = i1, target = i2, pos = InputPosMouse}
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
    let m2 = addOrSetSel False (ONode info.movedNode) { model | graph = info.graph } in
     if finish then switch_Default m2 else
        let ids = 
                         if info.created then [ info.movedNode , info.ne1, info.ne2 ]
                                    else [ info.ne1, info.ne2 ]
        in
        noCmd <|         
        initialise_RenameMode ids m2
                          



-- for view







-- movedNode

type alias Info = { graph : Graph NodeLabel EdgeLabel,
                    movedNode : NodeId ,
                    created : Bool,
                    ne1 : EdgeId,
                    ne2 : EdgeId }


stateInfo : Model -> SplitArrowState -> Info
stateInfo m state =
    let
        ( ( g, n ), created ) =
          let makeInfo pos = mayCreateTargetNodeAt m pos "" in
           case state.pos of
              InputPosGraph id -> ((m.graph, id), False)
              _ -> makeInfo m.mousePos                              
           
    in
    let (g1, ne1) = (Graph.newEdge g state.source n GraphDefs.emptyEdge) in
    let (g2, ne2) = (Graph.newEdge g1 n state.target GraphDefs.emptyEdge) in
    { graph = Graph.removeEdge state.chosenEdge g2,
      created = created,
      movedNode = n,
      ne1 = ne1,
      ne2 = ne2 }
    
   




     



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
    case msg of   

   
       
        KeyChanged False (Control "Escape") -> switch_Default model
        MouseClick -> next False          
        KeyChanged False (Control "Enter") -> next True
    --     TabInput -> Just <| ValidateNext
        KeyChanged False (Control "Tab") -> next False
        
        _ -> noCmd 
            { model | mode = SplitArrow 
            { state | pos = InputPosition.updateNoKeyboard state.pos msg } }

help : String
help =
            "[ESC] cancel, [click] name the point (if new), "
             ++ "[RET] terminate the square creation"             
             
      