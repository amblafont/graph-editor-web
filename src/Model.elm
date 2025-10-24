module Model exposing (..)

import GraphDrawing exposing (..)
import Polygraph as Graph exposing (EdgeId, NodeId, Graph, Node)
import Msg exposing (..)
import Geometry.Point as Point exposing (Point)
import InputPosition
import HtmlDefs exposing (Key)
import Format.GraphInfoCodec
import GraphDefs exposing (NodeLabel, EdgeLabel, newNodeLabel, coqProofTexCommand)
import HtmlDefs
import List.Extra
import ArrowStyle exposing (EdgePart)
import Drawing.Color as Color

import Modes exposing (Mode(..))

import Format.GraphInfo exposing (GraphInfo, Tab, TabId, Modif)
-- import ParseLatex
import Polygraph exposing (empty)
import Html exposing (q)
import Zindex exposing (backgroundZ, defaultZ)
import Html exposing (a)
import Format.GraphInfo as GraphInfo
import Modif




-- State -----------------------------------------------------------------------
-- core data that will be saved

depthHistory = 20
minSizeGrid = 2
maxSizeGrid = 500

minRulerMargin = 50
maxRulerMargin = 2000


-- type alias ModifId = Maybe Int
type alias Model = {
  -- only one tab should be active
      graphInfo : GraphInfo
    , saveLoadButtons : Bool
      -- tabs : List Tab
    -- , nextTabId : TabId
    -- , activeTabId : TabId
    , history : List (List Modif)
    {-
    when receiving a modif, it is added to the head 
    of the history if its id is the sme as nextModifId
    -}
    , topModifId : ModifId
    , nextModifId : Int
    -- , selectedObjs : List Obj
    , mousePos : Point
    , specialKeys : HtmlDefs.Keys
    -- , -- if the mouse is over some node or edge
    --  mousePointOver : Obj
    , statusMsg : String
    , mode : Mode
    , hideGrid : Bool
    , showOverlayHelp : Bool
    -- do we add a proof node when creating a square?
    , squareModeProof : Bool
    -- whether there is an id which is "hovered on"
   -- , hoverId : Maybe Graph.Id
    -- , selOnMove : Bool
    -- real dimensions of nodes
    -- , dimNodes : Dict NodeId Point
    -- quickInput : Maybe NonEmptyChain
    , mouseOnCanvas : Bool
    -- blitzFlag : Bool
    -- , bottomText : String
    , autoSave : Bool
    -- , latexPreamble : String
    , scenario : Scenario
    , defaultGridSize : Int
    -- margin for the rule
    , rulerMargin : Int
    , rulerShow : Bool
    
    }




toGraphInfo : Model -> GraphInfo
toGraphInfo m = m.graphInfo
  --  { tabs = m.tabs, latexPreamble = m.latexPreamble, 
    --  nextTabId = m.nextTabId, activeTabId = m.activeTabId }

setGraphInfo : Model -> GraphInfo -> Model
setGraphInfo m gi = { m | graphInfo = gi }
  --  { m | tabs = tabs, latexPreamble = latexPreamble}

updateGraphInfo : Model -> (GraphInfo -> GraphInfo) -> Model 
updateGraphInfo m f = setGraphInfo m <| f <| toGraphInfo m

clearHistory : Model -> Model
clearHistory m = { m | history = [], topModifId = defaultModifId, nextModifId = 0 }

-- pushHistoryMaybe : List Modif -> Model -> Model 

pushHistory : ModifId -> List Modif -> Model -> Model
pushHistory id modif m = if modif == [] then m else 
         case (modifIdsEq m.topModifId id, m.history) of
            (True, t  :: q) ->
               { m | history = (modif ++ t) :: q}
            _ ->
               { m | history = List.take depthHistory 
                               (modif :: m.history)
                    , topModifId = id}
pushHistorySingle : ModifId -> Modif -> Model -> Model
pushHistorySingle id modif m = pushHistory id [modif] m




-- getActiveTabInTabs : Int -> List Tab -> Tab
-- getActiveTabInTabs activeId tabs =
--    List.Extra.find (.id >> (==) activeId) tabs |> 
--    Maybe.withDefault (emptyTab 0)

getActiveTab : Model -> Tab
getActiveTab m = 
   GraphInfo.getActiveTab m.graphInfo -- |> Maybe.withDefault (emptyTab 0)

-- keep the id
updateFirstTab : Model -> (Tab -> Tab) -> Model
updateFirstTab m f =
   let gi = m.graphInfo in
   case gi.tabs of
     [] -> m
     t :: q ->
         let newTab = f t in         
         {m | graphInfo = { gi | tabs = { newTab | id = t.id} :: q}}
{-
clearActiveTabs : List Tab -> List Tab
clearActiveTabs tabs =
  List.map (\t -> {t | active = False}) tabs
  -}



activateTab : Model -> TabId -> Maybe Model
activateTab m id = 
  let gi = m.graphInfo in
  if GraphInfo.getTabById gi id == Nothing then Nothing else
  Just { m | graphInfo = { gi | activeTabId = id } }
  -- { m | activeTabId = id }
  -- let tabs = 
  --        clearActiveTabs m.tabs
  --        |> List.Extra.updateAt i (\t -> {t | active = True})
  -- in 
  --   { m | tabs = tabs }

activateFirstTab : Model -> Model
activateFirstTab m =
  List.head m.graphInfo.tabs |> Maybe.map .id
  |> Maybe.withDefault 0
  |> activateTab m
  |> Maybe.withDefault m


updateActiveTab : Model -> (Tab -> Tab) -> Model
updateActiveTab m f = 
    let gi = m.graphInfo in
    { m | graphInfo = { gi | tabs = List.Extra.updateIf 
                        (GraphInfo.isActiveTab m.graphInfo) f gi.tabs } }
  
-- { m | tabs = List.Extra.updateIf 
--                        (GraphInfo.isActiveTab m) f m.tabs }

renameActiveTab : Model -> String -> Model
renameActiveTab m s =
   updateActiveTab m <|
   \ t -> {t | title = s}

getActiveGraph : Model -> Graph NodeLabel EdgeLabel
getActiveGraph m =
  getActiveTab m |> .graph

getActiveTitle m =
  getActiveTab m |> .title

updateTabs m f =
   let gi = m.graphInfo in
   { m | graphInfo = { gi | tabs = f gi.tabs } }

getActiveSizeGrid m = m.graphInfo |> GraphInfo.getActiveSizeGrid

updateActiveGraph : Model -> (Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel)
   -> Model
updateActiveGraph m f =
  updateActiveTab m <| \t -> { t | graph = f t.graph}
  
setActiveGraph : Model -> Graph NodeLabel EdgeLabel -> Model
setActiveGraph m g = 
  updateActiveGraph m <| always g

getCurrentSizeGrid : Model -> Int
getCurrentSizeGrid m = 
  case currentMode m of
      ResizeMode s -> s.sizeGrid
      _ -> GraphInfo.getActiveSizeGrid m.graphInfo

-- Fonctions helper pour gérer la pile de modes

-- Obtenir le mode actuel (en tête de pile)
currentMode : Model -> Mode
currentMode m = m.mode




-- Remplacer complètement la pile de modes
setMode : Mode -> Model -> Model
setMode mode m =
    { m | mode = mode }

setActiveSizeGrid : Model -> Int -> Model
setActiveSizeGrid m s =
    updateActiveTab m <| \t -> { t | sizeGrid = s}

{-
setSaveGraphWithGraph : Model -> Graph.ModifHelper NodeLabel EdgeLabel 
          -> (Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel) -> Model 
setSaveGraphWithGraph model modif f = 
   updateActiveGraph (setSaveGraph model modif) f
-}
{-
applyModifsMaySave : Model -> Bool -> List GraphInfo.Modif -> Model
applyModifsMaySave m save g =
    case Modif.fold GraphInfo.applyModif m.graphInfo g of 
      Nothing -> m
      Just r -> let m2 = {m | graphInfo = r.next  } in
                if save then
                   pushHistory Nothing r.undo m2
                else
                   m2

applyModifs : Model -> List GraphInfo.Modif -> Model
applyModifs m g = applyModifsMaySave m True g

applyModif : Model -> GraphInfo.Modif -> Model
applyModif m g = applyModifs m [g]
  --  case GraphInfo.applyModif m g of 
  --     Nothing -> m
  --     Just r -> pushHistory [r.undo] r.next
  -}
applyModifResult : Bool -> Model -> ModifId
             -> GraphInfo.ModifResult
             -> Model
applyModifResult save m idModif result =
     case result of 
      Nothing -> m
      Just r -> let m2 = {m | graphInfo = r.next.graphInfo  } in
                if save then
                pushHistory idModif [r.undo] m2
                else m2
{-
setSaveGraph : Model -> Graph.ModifHelper NodeLabel EdgeLabel -> Model
setSaveGraph m g = 
    applyModif m <| GraphInfo.activeGraphModifHelper m.graphInfo g 
    -}
  --  let m2 = pushHistory m [] in
  --  setActiveGraph m2 g


-- inputPositionPoint : Point -> InputPosition -> Point
-- inputPositionPoint source pos =
--    case pos of
--       InputPosMouse p -> p
--       InputPosKeyboard p -> Point.add source <| deltaKeyboardPos p)

clearModel : Model -> Model
clearModel m =
   createModel <| modelToFlag m

modelToFlag : Model -> Flags
modelToFlag m = {defaultGridSize = m.defaultGridSize, rulerMargin = m.rulerMargin, saveLoadButtons = m.saveLoadButtons}

type alias Flags = {defaultGridSize : Int, rulerMargin : Int, saveLoadButtons:Bool}

createModel : Flags -> Model
createModel {defaultGridSize, rulerMargin, saveLoadButtons} =
    let g = Graph.empty in
    { 
      graphInfo = {tabs = [ { graph = g, sizeGrid = defaultGridSize, title = "1", id = 0 } ]
    , nextTabId = 1
    , activeTabId = 0
    , latexPreamble = "\\newcommand{\\" ++ coqProofTexCommand ++ "}[1]{\\checkmark}"
      }
    , saveLoadButtons = saveLoadButtons
    , defaultGridSize = defaultGridSize
    , history = []
    , nextModifId = 0
    , topModifId = defaultModifId
    , mode = DefaultMode  -- Pile initialisée avec DefaultMode
    , statusMsg = ""
    , mouseOnCanvas = False
     -- Debug.toString ([pointToAngle (0,1), pointToAngle (0.001,1),
      --                    pointToAngle (1,0), pointToAngle(0,-1),
      --                        pointToAngle (-1, 0.01)]),
    , mousePos = ( 0, 0 )
    , specialKeys = { ctrl = False, alt = False, shift = False }
    , hideGrid = False
    -- , bottomText = ""
    , autoSave = True
    
    , scenario = Standard
    , showOverlayHelp = False
    , squareModeProof = False
    , rulerMargin = rulerMargin
    , rulerShow = False
    --, hoverId = Nothing
    -- whether we should select the closest object 
    -- when moving the mouse
   -- , selOnMove = True
   -- , mousePointOver = ONothing
  --  , selectedObjs = []
    -- , dimNodes = Dict.empty

    -- unnamedFlag = False
    -- mouseOnCanvas = False,
    -- quickInput = Nothing
    -- blitzFlag = False
    }

-- iniModel : Model
-- iniModel =    
  {-  let dbg = Debug.log "test" 
        <| ParseLatex.convertString """
    \\Diag{%
    \\justify{m-2-2}{naturality of $\\widetilde{h^∘}$ in $X$}{m-1-3} %
    \\justify[.4]{m-2-1}{extranaturality of $\\widetilde{h^∘}$ in $Y$}{m-4-2} %
    \\justify{m-2-2}{interchange}{m-3-3} %
    \\justify{m-5-1}{interchange}{m-4-2} %
    \\justify{m-6-1}{interchange}{m-5-3} %
    \\justify[.6]{m-4-2}{\\cite[Theorem~IV.7.2]{MacLane:cwm}}{m-3-3} %
    }{%
    Σ R_Z K_Z L_Z X \\& \\& Σ R_Y K_Z L_Z X \\\\
    {\\ } \\& R_Y K_Y L_Y R_Z K_Z L_Z X \\& R_Y K_Y L_Y R_Y K_Z L_Z X \\\\
    \\& R_Y K_Z L_Y R_Z K_Z L_Z X \\& R_Y K_Z L_Y R_Y K_Z L_Z X \\\\
    R_Z K_Z L_Z R_Z K_Z L_Z X \\& R_Y K_Z L_Z R_Z K_Z L_Z X  \\\\
    R_Z K_Z K_Z L_Z X \\&          \\& R_Y K_Z K_Z L_Z X \\\\
        R_Z K_Z L_Z X \\&              \\& R_Y K_Z L_Z X %
      }{%
        (m-1-1) edge[labela={Σ R_f K_Z L_Z X}] (m-1-3) %
        edge[labell={\\widetilde{h^∘}_{R_Z K_Z L_Z X,Z}}] (m-4-1) %
        edge[labelon={\\widetilde{h^∘}_{R_Z K_Z L_Z X,Y}}] (m-2-2) %
        (m-2-2) edge[label={[above=.7ex]{$\\scriptstyle
            R_Y K_Y L_Y R_f K_Z L_Z X$}}] (m-2-3) %
        (m-3-2) edge[label={[above=.7ex]{$\\scriptstyle
            R_Y K_Z L_Y R_f K_Z L_Z X$}}] (m-3-3) %
        (m-1-3) edge[labelon={\\widetilde{h^∘}_{R_Y K_Z L_Z X,Y}}]
                      (m-2-3) %
        (m-2-2) edge[labelon={R_Y K_f L_Y R_Z K_Z L_Z X}] %
                      (m-3-2) %
        (m-3-2) edge[labell={R_Y K_Z L_f R_Z K_Z L_Z X}] %
                      (m-4-2) %
        (m-3-3) edge[labelon={R_Y K_Z ε_Y K_Z L_Z X}] (m-5-3) %
        (m-2-3) edge[labelon={R_Y K_f L_Y R_Y K_Z L_Z X}] (m-3-3) %
        (m-4-1)  edge[label={[above=.7ex]{$\\scriptstyle
            R_f K_Z L_Z R_Z K_Z L_Z X$}}] (m-4-2) %
        (m-4-1) edge[labelon={R_Z K_Z ε_Z K_Z L_Z X}] (m-5-1) %
        (m-5-1)  edge[label={[above=.7ex]{$\\scriptstyle
            R_f K_Z K_Z L_Z X$}}] (m-5-3) %
        (m-4-2) edge[labelon={R_Z K_Z ε_Z K_Z L_Z X}] (m-5-3) %
        (m-5-1) edge[labelon={R_Z μ^{K_Z} L_Z X }] (m-6-1) %
        (m-5-3) edge[labelon={R_Y μ^{K_Z} L_Z X }] (m-6-3) %
        (m-6-1) edge[labela={R_f K_Z L_Z X}] (m-6-3) %
  }

         """
   in -}
--    let graph = {- dbg -} Nothing |> Maybe.map (ParseLatex.buildGraph sizeGrid) |>
--                Maybe.withDefault Graph.empty
--    in
--    createModel sizeGrid <| graph
--       {- Tuple.first <|
        -- Graph.newNode Graph.empty
        --  { pos = (sizeGrid / 2, sizeGrid / 2), label = "", dims = Nothing,
        --    selected = True } -}

popIdModif : Model -> (Model, ModifId)
popIdModif m = ({m | nextModifId = m.nextModifId + 1}, trueModifId m.nextModifId)


addOrSetSel : Bool -> Graph.Id -> Model -> Model
addOrSetSel keep o m =
   updateActiveGraph m (GraphDefs.addOrSetSel keep o)



        {-
setSelModif : Graph.Id -> Graph.ModifHelper NodeLabel EdgeLabel -> Graph.ModifHelper NodeLabel EdgeLabel
setSelModif id g = 
    Graph.md_update id (\n -> {n | selected = True}) (\n -> {n | selected = True})
    g
      -}


  
    
--     let (l1, l2) = selectedEdges m
--            |> List.map (\e -> (e.from, e.to)) |> List.unzip
--     in
--    selectedNodes m ++ l1 ++ l2 |> List.Extra.uniqueBy .id
     
--   |> List.filterMap (\ id -> Graph.get id m.graph |> Maybe.map .node)

-- captureKeyboard : State -> Bool
-- captureKeyboard m =
--     case m.mode of
--         QuickInputMode _ -> False
--         _ -> True







-- True if created


-- TODO: check if it is still used
mayCreateTargetNodeAt : Model -> Point -> String -> Bool -> ( ( Graph.ModifHelper NodeLabel EdgeLabel, NodeId ), Bool )
mayCreateTargetNodeAt m pos s isDefaultZ =
   let g = getActiveGraph m in
   let modifGraph = Graph.newModif g in
   case GraphDefs.getNodesAt g pos of
      [ n ] -> ((modifGraph, n), False)
      _ ->
          let label = newNodeLabel pos s True (if isDefaultZ then defaultZ else backgroundZ) in
              ( Graph.md_newNode modifGraph 
              <| label
            , True )

-- only Nodes ?
mayCreateTargetNode : Model -> String -> Bool -> ( ( Graph.ModifHelper NodeLabel EdgeLabel, NodeId ), Bool )
mayCreateTargetNode m =
  mayCreateTargetNodeAt m m.mousePos
    


-- switch between modes


noCmd : a -> ( a, Cmd Msg )
noCmd m =
    ( m, Cmd.none )


switch_Default : Model -> ( Model, Cmd Msg )
switch_Default m = noCmd <| setMode DefaultMode m




collageGraphFromGraph : Model -> Graph NodeLabel EdgeLabel -> Graph NodeDrawingLabel EdgeDrawingLabel
collageGraphFromGraph _ g =
   GraphDrawing.toDrawingGraph <| GraphDefs.makeSelection g
    


keyboardPosToPoint : Model -> NodeId -> (Int, Int) -> Point
keyboardPosToPoint m chosenNode p =
   case Graph.getNode chosenNode <| getActiveGraph m of
      Nothing -> m.mousePos
      Just { pos } -> 
         let delta = InputPosition.deltaKeyboardPos 
                 (GraphInfo.getActiveSizeGrid m.graphInfo) p 
         in
         Point.add pos delta

toggleHelpOverlay : Model -> Model
toggleHelpOverlay model = {model | showOverlayHelp = not model.showOverlayHelp } 

restrictSelection : Model -> Model
restrictSelection model = 
    let modelGraph = getActiveGraph model in
    let sizeGrid = GraphInfo.getActiveSizeGrid model.graphInfo in
    let gi = model.graphInfo in
       { model | graphInfo = {gi |
                           tabs = [ {graph = GraphDefs.selectedGraph modelGraph
                               , sizeGrid = sizeGrid, title = getActiveTitle model, 
                               id = 0 }]
                               , activeTabId = 0
                               , nextTabId = 1 }
      }


returnUpdatePullshout : Key -> Model -> List (Graph.Edge EdgeLabel) 
                       -> Graph.ModifHelper NodeLabel EdgeLabel
returnUpdatePullshout k model edges = 
   let modifHelper = GraphDefs.updatePullshoutEdges 
                  (GraphDefs.keyMaybeUpdatePullshout k)
                  edges
                  (getActiveGraph model)
    in
    modifHelper
              
returnUpdateStyle : (ArrowStyle.ArrowStyle -> Maybe ArrowStyle.ArrowStyle) 
                  -> Model -> List (Graph.Edge EdgeLabel) 
                  -> Graph.ModifHelper NodeLabel EdgeLabel -- (Model, Cmd Msg)
returnUpdateStyle updateStyle model edges =
    let modifHelper = GraphDefs.updateStyleEdges 
                  updateStyle
                  edges
                  (getActiveGraph model)
    in
    modifHelper
    {-
    let _ = Debug.log "applied modif helper" 
             <| Codec.encoder Graph.codec
             <| Graph.debugModifHelperGraph
             <| modifHelper in
             -}
    -- let modif = {-  Debug.log "mdoif" <| -} Graph.finaliseModif modifHelper in
    -- updateModifHelper { model | mode = DefaultMode } modifHelper

  
truncateInputPosition : Model -> Graph NodeLabel EdgeLabel -> InputPosition.InputPosition
truncateInputPosition model graph = 
  InputPosition.computeKeyboardPos
                (GraphDefs.centerOfNodes
                   (Graph.nodes graph)) 
                (getActiveSizeGrid model) model.mousePos 