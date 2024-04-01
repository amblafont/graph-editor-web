module Modes.Move exposing (update, initialise, graphDrawing, help)

import GraphDrawing exposing (NodeDrawingLabel, EdgeDrawingLabel)
import Msg exposing (Msg(..))
import Modes exposing (MoveDirection(..), MoveMode(..), Mode(..), MoveState)
import Model exposing (Model, getActiveGraph, setActiveGraph, 
   getActiveSizeGrid, 
   switch_Default,setSaveGraph, noCmd, toggleHelpOverlay,
   collageGraphFromGraph)
import Polygraph as Graph exposing (Graph)
import GraphDefs exposing (NodeLabel, EdgeLabel)
import Geometry.Point as Point
import InputPosition exposing (InputPosition(..))
import HtmlDefs exposing (Key(..))

               
initialise : Bool -> MoveMode -> Model -> Model
initialise save mode model =
   let modelGraph = getActiveGraph model in
   { model | mode = 
        if GraphDefs.isEmptySelection modelGraph
         then
           -- Nothing is selected
           DefaultMode
         else 
           Move 
              { save = save,               
              pos = InputPosMouse,
              direction = Free,
              mode = mode }
      }    

update : Msg -> Modes.MoveState -> Model -> (Model, Cmd Msg)
update msg state model =
    let movedRet = 
           let info = mkInfo model state in
           if info.valid then
              switch_Default 
              <| if state.save then
                   setSaveGraph model info.graph
                 else
                   setActiveGraph model info.graph
           else
              noCmd model
    in
    let terminable = state.mode /= PressMove in
    let terminedRet = 
         if terminable then movedRet else noCmd model
    in
    let updateState st = { model | mode = Move st } in
    let updateDirection direction = noCmd <| updateState  { state | direction = direction} in
    case msg of
        KeyChanged False _ (Character '?') -> noCmd <| toggleHelpOverlay model
        KeyChanged False _ (Control "Escape") -> switch_Default model
        PressTimeout ->
          noCmd <| 
            if state.mode == UndefinedMove then
              updateState { state | mode = PressMove }
            else
              model
        KeyChanged False _ (Character 'g') -> 
           case state.mode of              
             UndefinedMove -> 
                noCmd <| updateState { state | mode = FreeMove }
             PressMove -> movedRet
             FreeMove -> noCmd model
        KeyChanged False _ (Character 'f') -> updateDirection Free
        KeyChanged False _ (Character 'x') -> updateDirection Horizontal
        KeyChanged False _ (Character 'y') -> updateDirection Vertical
       
        MouseClick -> terminedRet
        KeyChanged False _ (Control "Enter") -> terminedRet
        _ ->  noCmd <| updateState { state | pos = InputPosition.update state.pos msg }



mkInfo : Model -> Modes.MoveState -> 
   { graph : Graph NodeLabel EdgeLabel,
   -- The graph is not valid if we are in merge mode
   -- and no object is pointed at
     valid : Bool }

mkInfo model { pos, direction } =    
    let merge = model.specialKeys.ctrl in
    let modelGraph = getActiveGraph model in
    let selectedGraph = GraphDefs.selectedGraph modelGraph in
    let nodes = Graph.nodes selectedGraph in
    let updNode delta {id, label} = 
          {id = id, label = { label | pos = Point.add label.pos delta }}
    in
    let moveNodes delta = nodes |> List.map (updNode delta) in
   --  let moveGraph delta =  Graph.updateNodes (moveNodes delta) modelGraph in
    let mkRet movedNodes = 
            let g = Graph.updateNodes movedNodes modelGraph in
            { graph =  g, valid = not merge } in
    let retMerge movedNodes =                  
           case movedNodes of
              [ n ] ->        
                let (g, valid) = GraphDefs.mergeWithSameLoc n modelGraph in
                if valid then
                  {graph = g, valid = True }
                else
                  mkRet movedNodes
              _ -> mkRet movedNodes      
    in       
    let retDelta delta =
            let movedNodes = moveNodes delta in
            if merge then
                retMerge movedNodes
            else
                mkRet movedNodes      
          
    in
   
    let mouseDelta = 
            let (dx, dy) = Point.subtract model.mousePos <| GraphDefs.centerOfNodes nodes in
              case direction of
                      Free -> (dx, dy)
                      Vertical -> (0, dy)
                      Horizontal -> (dx, 0)
    in
    let sizeGrid = getActiveSizeGrid model in
    case pos of
      InputPosKeyboard p -> retDelta <| InputPosition.deltaKeyboardPos sizeGrid p
      InputPosGraph id ->         
         if not merge then 
            retDelta mouseDelta
         else        
            case GraphDefs.selectedId modelGraph of
               Just selId -> { graph = Graph.recursiveMerge id selId modelGraph, valid = True }  
               Nothing -> retDelta mouseDelta
      InputPosMouse -> retDelta mouseDelta

graphDrawing : Model -> MoveState -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawing m s = mkInfo m s |> .graph |>
            collageGraphFromGraph m 

help : MoveState -> String
help s =
         "Mode Move. " ++
                HtmlDefs.overlayHelpMsg        
                ++ "Use mouse or h,j,k,l."
                ++ " Hold [ctrl] to merge the selected point onto another node,"
                ++ " Press [x] or [y] to restrict to horizontal / vertical directions, or let it [f]ree " 
                ++ "(currently, "
                ++ (case s.direction of
                      Vertical -> "vertical"
                      Horizontal -> "horizontal"
                      Free -> "free")
                ++ ")."
                ++
                (case s.mode of 
                    FreeMove ->  " [RET] or [click] to confirm."
                    PressMove -> " Release [g] to confirm."
                    UndefinedMove -> ""
                ) 
                  