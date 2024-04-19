module Modes.Move exposing (update, initialise, graphDrawing, help, mkGraph)

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
              merge = False,
              mode = mode }
      }    

update : Msg -> Modes.MoveState -> Model -> (Model, Cmd Msg)
update msg state model =
    let movedRet () = 
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
    let terminedRet () = 
         if terminable then movedRet () else noCmd model
    in
    let updateState st = { model | mode = Move st } in
    let updateDirection direction = noCmd <| updateState  { state | direction = direction} in
    case msg of
        KeyChanged False _ (Control "Control") -> noCmd <| updateState { state | merge =  not state.merge}         
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
             PressMove -> movedRet ()
             FreeMove -> noCmd model
        KeyChanged False _ (Character 'f') -> updateDirection Free
        KeyChanged False _ (Character 'x') -> updateDirection Horizontal
        KeyChanged False _ (Character 'y') -> updateDirection Vertical
       
        MouseClick -> terminedRet ()
        KeyChanged False _ (Control "Enter") -> terminedRet ()
        _ ->  noCmd <| updateState { state | pos = InputPosition.update state.pos msg }


mkGraph : Model -> InputPosition -> MoveDirection -> Bool -> Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel -> 
 -- what is marked as weakly selected are the potential merged target
   { graph : Graph NodeLabel EdgeLabel,
     merged : Bool }

mkGraph model pos direction shouldMerge modelGraph selectedGraph = 
-- even if shouldMerge is false, it could attempt a merge if
-- there is a node precisely at the location, and the move is
-- directed by the keyboard
    -- let shouldMerge = model.specialKeys.ctrl in
    let mergeId = Graph.topmostObject selectedGraph in
    let complementGraph = Graph.complement modelGraph selectedGraph in
    let nodes = Graph.nodes selectedGraph in
    let updNode delta {id, label} = 
          {id = id, label = { label | pos = Point.add label.pos delta }}
    in
    let moveNodes delta = nodes |> List.map (updNode delta) in
   
    let retDelta allowOverlap delta =
          let movedNodes = moveNodes delta in
          let newPos = GraphDefs.centerOfNodes movedNodes in
          let overlapId = 
                 if allowOverlap then (GraphDefs.getNodesAt complementGraph newPos |> List.head)
                 else Nothing
          in
          let closestId = GraphDefs.closest newPos complementGraph in 
          let retMerge id1 id2 =
                    { graph = Graph.recursiveMerge id1 id2 modelGraph, 
                        merged = True } 
          in
          case (overlapId, mergeId, shouldMerge) of
             (Just targetId, Just sourceId, _) -> retMerge targetId sourceId
             (_, Just sourceId, True) -> retMerge closestId sourceId
             _ -> let g = Graph.updateNodes movedNodes modelGraph 
                         |>  GraphDefs.weaklySelect closestId 
                  in
                    { graph = g, merged = False }           
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
      InputPosKeyboard p -> retDelta 
                           True
                           (InputPosition.deltaKeyboardPos sizeGrid p)
      -- not reliable, as it could be the moving stuff
      InputPosGraph _ ->  
         retDelta False mouseDelta
      InputPosMouse -> 
        -- let _ = Debug.log "input pos mouse" "" in
        retDelta False mouseDelta 

mkInfo : Model -> Modes.MoveState -> 
   { graph : Graph NodeLabel EdgeLabel,
   -- The graph is not valid if we are in merge mode
   -- and no object is pointed at
     valid : Bool }

mkInfo model { pos, direction, merge } =    
    let modelGraph = getActiveGraph model in
    let selectedGraph = GraphDefs.selectedGraph modelGraph in
    let {merged, graph} = mkGraph model pos direction merge modelGraph selectedGraph in
    { graph = graph, valid = True } -- merge ==> merged
  

graphDrawing : Model -> MoveState -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawing m s = mkInfo m s |> .graph |>
            collageGraphFromGraph m 

help : MoveState -> String
help s =
         "Mode Move. " ++
                HtmlDefs.overlayHelpMsg        
                ++ ". Use mouse or h,j,k,l."
                ++ " [ctrl] to toggle merging,"
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
                  