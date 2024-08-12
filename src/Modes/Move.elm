module Modes.Move exposing (update, isValid, initialise, graphDrawing, help, mkGraph, computeGraph)

import GraphDrawing exposing (NodeDrawingLabel, EdgeDrawingLabel)
import Msg exposing (Msg(..), ModifId, MoveMode(..))
import Modes exposing (MoveDirection(..), Mode(..), MoveState)
import Model exposing (Model, getActiveGraph, setActiveGraph, 
   getActiveSizeGrid, 
   switch_Default, noCmd, toggleHelpOverlay,
   collageGraphFromGraph)
import Polygraph as Graph exposing (Graph)
import GraphDefs exposing (NodeLabel, EdgeLabel)
import Geometry.Point as Point
import InputPosition exposing (InputPosition(..))
import HtmlDefs exposing (Key(..))
import GraphDefs exposing (weaklySelect)
import CommandCodec exposing (updateModifHelper, updateModifHelperWithId)

isValid : Model -> Bool
isValid model = 
  let modelGraph = getActiveGraph model in
  GraphDefs.isEmptySelection modelGraph |> not

initialise : ModifId -> MoveMode -> Model -> Model
initialise id mode model =
  --  let modelGraph = getActiveGraph model in
   { model | mode = 
        if not <| isValid model 
         then
           -- Nothing is selected
           DefaultMode
         else 
           Move 
              { -- save = save,  
              idModif = id,             
              pos = InputPosMouse,
              direction = Free,
              -- merge = False,
              mode = mode }
      }

update : Msg -> Modes.MoveState -> Model -> (Model, Cmd Msg)
update msg state model =
    let movedRet merge = 
           let info = mkInfo model merge state in
           if info.valid then
              updateModifHelperWithId { model | mode = DefaultMode }
                 state.idModif info.graph
           else
              
              noCmd model
    in
    let terminable = state.mode /= PressMove in
    let terminedRet merge = 
         if terminable then movedRet merge else noCmd model
    in
    let updateState st = { model | mode = Move st } in
    let updateDirection direction = noCmd <| updateState  { state | direction = direction} in
    case msg of
        KeyChanged True _ (Control "Control") -> terminedRet True
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
             PressMove -> movedRet False
             FreeMove -> noCmd model
        KeyChanged False _ (Character 'f') -> updateDirection Free
        KeyChanged False _ (Character 'x') -> updateDirection Horizontal
        KeyChanged False _ (Character 'y') -> updateDirection Vertical
       
        MouseClick -> terminedRet False
        KeyChanged False _ (Control "Enter") -> terminedRet False
        _ ->  noCmd <| updateState { state | pos = InputPosition.update state.pos msg }


mkGraph : Model -> InputPosition -> MoveDirection -> Bool 
   -> Graph.ModifHelper NodeLabel EdgeLabel 
   -> Graph NodeLabel EdgeLabel 
   -> Graph NodeLabel EdgeLabel 
   -> 
 -- what is marked as weakly selected are the potential merged target
   { graph : Graph.ModifHelper NodeLabel EdgeLabel,
   -- the graph is redundant with the modif: it includes selection information
   -- Other idea: have a DSL for selection information
    --  graph : Graph NodeLabel EdgeLabel,
     weaklySelection : Maybe Graph.Id,
     merged : Bool }

mkGraph model pos direction shouldMerge modelGraph complementGraph selectedGraph = 
-- even if shouldMerge is false, it could attempt a merge if
-- there is a node precisely at the location, and the move is
-- directed by the keyboard
    -- let shouldMerge = model.specialKeys.ctrl in
    let mergeId = Graph.topmostObject selectedGraph in
    -- let complementGraph = Graph.complement modelGraph <| Graph.allIds selectedGraph in
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
                    { graph = Graph.md_recursiveMerge id1 id2 modelGraph, 
                        merged = True, weaklySelection = Nothing } 
          in
          case (overlapId, mergeId, shouldMerge) of
             (Just targetId, Just sourceId, _) -> retMerge targetId sourceId
             (_, Just sourceId, True) -> retMerge closestId sourceId
             _ -> let modif = {- Graph.newModif -} modelGraph 
                        |> Graph.md_updateNodes movedNodes
                  in
                  -- let g = Graph.applyModifHelper modif 
                        --  |>  GraphDefs.weaklySelect closestId 
                  -- in
                    { graph = modif, merged = False, weaklySelection = Just closestId }           
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

mkInfo : Model -> Bool -> Modes.MoveState -> 
   { graph : Graph.ModifHelper NodeLabel EdgeLabel,
    -- modif : Graph.ModifHelper NodeLabel EdgeLabel,
   -- The graph is not valid if we are in merge mode
   -- and no object is pointed at
     weaklySelection : Maybe Graph.Id,
     valid : Bool }

mkInfo model merge { pos, direction } =    
    let modelGraph = getActiveGraph model in
    let selectedGraph = GraphDefs.selectedGraph modelGraph in
    let complementGraph = Graph.complement modelGraph <| Graph.allIds selectedGraph in
    let {merged, graph, weaklySelection} = mkGraph model pos direction merge (Graph.newModif modelGraph) complementGraph selectedGraph in
    -- let modifiedGraph = Graph.applyModifHelper graph in
    --

    { graph = graph, weaklySelection = weaklySelection, valid = True } -- merge ==> merged
  

graphDrawing : Model -> MoveState -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawing m s = 

     collageGraphFromGraph m <| computeGraph <| mkInfo m False s 

  
computeGraph : {a | weaklySelection : Maybe Graph.Id,
                  graph : Graph.ModifHelper NodeLabel EdgeLabel}
              -> Graph NodeLabel EdgeLabel
computeGraph {weaklySelection, graph} =
     let modifiedGraph = Graph.applyModifHelper graph in
     let finalGraph = case weaklySelection of
            Just id -> GraphDefs.weaklySelect id modifiedGraph
            Nothing -> modifiedGraph
     in
     finalGraph


help : MoveState -> String
help s =
         "Mode Move. " ++
                HtmlDefs.overlayHelpMsg        
                ++ ". Use mouse or h,j,k,l."
                ++ " [ctrl] to merge,"
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
                  