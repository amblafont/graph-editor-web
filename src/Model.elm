module Model exposing (..)


import GraphDrawing exposing (..)
import Polygraph as Graph exposing (EdgeId, NodeId, Graph, Node)
import Msg exposing (..)
import Geometry.Point as Point exposing (Point)
import InputPosition
import GraphDefs exposing (NodeLabel, EdgeLabel, newNodeLabel, coqProofTexCommand)
import HtmlDefs
import List.Extra

import Modes exposing (Mode(..))

import Format.GraphInfo exposing (GraphInfo, Tab)
import ParseLatex
import Polygraph exposing (empty)
import Html exposing (q)
import Zindex exposing (backgroundZ, defaultZ)




-- State -----------------------------------------------------------------------
-- core data that will be saved

depthHistory = 20
minSizeGrid = 2
maxSizeGrid = 500



type alias Model = {
  -- only one tab should be active
      tabs : List Tab
    , history : List GraphInfo
    -- , selectedObjs : List Obj
    , mousePos : Point
    , specialKeys : HtmlDefs.Keys
    -- , -- if the mouse is over some node or edge
    --  mousePointOver : Obj
    , statusMsg : String
    , -- unnamedFlag : Bool,
      quickInput : String
    , mode : Mode
    , hideGrid : Bool
    -- filename in the web version / full path in the electron version
    , fileName : String
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
    , latexPreamble : String
    , scenario : Scenario
    , defaultGridSize : Int
    }


undo : Model -> Model
undo m = popHistory <| updateWithGraphInfo m <| peekHistory m

toGraphInfo : Model -> GraphInfo
toGraphInfo m = { tabs = m.tabs, latexPreamble = m.latexPreamble }

updateWithGraphInfo : Model -> GraphInfo -> Model
updateWithGraphInfo m {tabs, latexPreamble} = 
   { m | tabs = tabs, latexPreamble = latexPreamble}

clearHistory : Model -> Model
clearHistory m = { m | history = [] }

peekHistory : Model -> GraphInfo
peekHistory m = List.head m.history |> Maybe.withDefault (toGraphInfo m)

pushHistory : Model -> Model
pushHistory m = { m | history = List.take depthHistory (toGraphInfo m :: m.history)}

popHistory : Model -> Model
popHistory m = { m | history = List.tail m.history |> Maybe.withDefault []}



emptyTab : Tab
emptyTab = { active = True, title = "1", sizeGrid = 200, graph = Graph.empty}

getActiveTabInTabs : List Tab -> Tab
getActiveTabInTabs tabs =
   List.Extra.find .active tabs |> Maybe.withDefault emptyTab

getActiveTab : Model -> Tab
getActiveTab m = 
  getActiveTabInTabs m.tabs


updateFirstTab : Model -> (Tab -> Tab) -> Model
updateFirstTab m f =
   case m.tabs of
     [] -> m
     t :: q -> {m | tabs = f t :: q}

clearActiveTabs : List Tab -> List Tab
clearActiveTabs tabs =
  List.map (\t -> {t | active = False}) tabs

activateNthTab : Model -> Int -> Model
activateNthTab m i =
  let tabs = 
         clearActiveTabs m.tabs
         |> List.Extra.updateAt i (\t -> {t | active = True})
  in 
    { m | tabs = tabs }

activateFirstTab : Model -> Model
activateFirstTab m =
  activateNthTab m 0

removeActiveTabs : Model -> Model
removeActiveTabs m =
   let tabs = List.filter (not << .active) m.tabs in 
   case tabs of 
     [] -> m
     _ -> 
       let m2 = pushHistory m in
       activateFirstTab { m2 | tabs = tabs }

nextTabName : Model -> String
nextTabName m =
   let n = Maybe.withDefault 0 <| List.maximum <| List.filterMap (.title >> String.toInt) m.tabs in
   String.fromInt (1 + n)


createNewTab : Model -> String -> Model
createNewTab m title =
  let sizeGrid = getActiveSizeGrid m in
  { m | tabs = clearActiveTabs m.tabs ++ 
    [ 
      { graph = Graph.empty ,
        sizeGrid = sizeGrid ,
        title = title ,
        active = True
      }
    ]
  }

duplicateTab : Model -> String -> Model
duplicateTab m title =
  let tab = getActiveTab m in 
  { m | tabs = clearActiveTabs m.tabs ++ 
    [ 
      { tab |
        title = title ,
        active = True
      }
    ]
  }

updateActiveTab : Model -> (Tab -> Tab) -> Model
updateActiveTab m f = { m | tabs = List.Extra.updateIf .active f m.tabs }

renameActiveTab : Model -> String -> Model
renameActiveTab m s =
   updateActiveTab m <|
   \ t -> {t | title = s}

getActiveGraph : Model -> Graph NodeLabel EdgeLabel
getActiveGraph m =
  getActiveTab m |> .graph

getActiveTitle m =
  getActiveTab m |> .title

updateActiveGraph : Model -> (Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel)
   -> Model
updateActiveGraph m f =
  updateActiveTab m <| \t -> { t | graph = f t.graph}
  
setActiveGraph : Model -> Graph NodeLabel EdgeLabel -> Model
setActiveGraph m g = 
  updateActiveGraph m <| always g

getActiveSizeGrid : Model -> Int
getActiveSizeGrid m = getActiveTab m |> .sizeGrid

getCurrentSizeGrid : Model -> Int
getCurrentSizeGrid m = 
  case m.mode of
      ResizeMode s -> s.sizeGrid
      _ -> getActiveSizeGrid m

setActiveSizeGrid : Model -> Int -> Model
setActiveSizeGrid m s =
    updateActiveTab m <| \t -> { t | sizeGrid = s}


setSaveGraph : Model -> Graph NodeLabel EdgeLabel -> Model
setSaveGraph m g = 
   let m2 = pushHistory m in
   setActiveGraph m2 g


-- inputPositionPoint : Point -> InputPosition -> Point
-- inputPositionPoint source pos =
--    case pos of
--       InputPosMouse p -> p
--       InputPosKeyboard p -> Point.add source <| deltaKeyboardPos p)


createModel : Int -> Model
createModel sizeGrid =
    let g = Graph.empty in
    { tabs = [ { active = True, graph = g, sizeGrid = sizeGrid, title = "1" } ]
    , defaultGridSize = sizeGrid
    , history = []
    , mode = DefaultMode
    , statusMsg = ""
    , mouseOnCanvas = False
    , -- Debug.toString ([pointToAngle (0,1), pointToAngle (0.001,1),
      --                    pointToAngle (1,0), pointToAngle(0,-1),
      --                        pointToAngle (-1, 0.01)]),
      quickInput = ""
    , mousePos = ( 0, 0 )
    , specialKeys = { ctrl = False, alt = False, shift = False }
    , hideGrid = False
    , fileName = "graph.json"
    -- , bottomText = ""
    , autoSave = False
    , latexPreamble = "\\newcommand{\\" ++ coqProofTexCommand ++ "}[1]{\\checkmark}"
    , scenario = Standard
    , showOverlayHelp = False
    , squareModeProof = False    
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

initialise_RenameModeWithDefault : Bool -> List (Graph.Id, String) -> Model -> Model
initialise_RenameModeWithDefault save l m =
  case l of
     [] -> { m | mode = DefaultMode }
     _ -> { m | mode = RenameMode save l }



initialise_RenameMode : Bool -> List Graph.Id -> Model -> Model
initialise_RenameMode save l m =
    let ls =  List.filterMap 
              (\id -> 
                getActiveGraph m |>
                GraphDefs.getLabelLabel id 
                |> Maybe.map (\s -> (id, s))
              
              ) l
    in
        initialise_RenameModeWithDefault save ls m
        


addOrSetSel : Bool -> Graph.Id -> Model -> Model
addOrSetSel keep o m =
   updateActiveGraph m (GraphDefs.addOrSetSel keep o)
        
      


  
    
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


mayCreateTargetNodeAt : Model -> Point -> String -> Bool -> ( ( Graph NodeLabel EdgeLabel, NodeId ), Bool )
mayCreateTargetNodeAt m pos s isDefaultZ =
   let g = getActiveGraph m in
   case GraphDefs.getNodesAt g pos of
      [ n ] -> ((g, n), False)
      _ ->
            ( Graph.newNode g 
              <| newNodeLabel pos s True (if isDefaultZ then defaultZ else backgroundZ)
            , True )

-- only Nodes ?
mayCreateTargetNode : Model -> String -> Bool -> ( ( Graph NodeLabel EdgeLabel, NodeId ), Bool )
mayCreateTargetNode m =
  mayCreateTargetNodeAt m m.mousePos
    


-- switch between modes


noCmd : a -> ( a, Cmd Msg )
noCmd m =
    ( m, Cmd.none )


switch_Default : Model -> ( Model, Cmd Msg )
switch_Default m =
    noCmd { m | mode = DefaultMode }




collageGraphFromGraph : Model -> Graph NodeLabel EdgeLabel -> Graph NodeDrawingLabel EdgeDrawingLabel
collageGraphFromGraph _ g =
   GraphDrawing.toDrawingGraph <| GraphDefs.makeSelection g
    


keyboardPosToPoint : Model -> NodeId -> (Int, Int) -> Point
keyboardPosToPoint m chosenNode p =
   case Graph.getNode chosenNode <| getActiveGraph m of
      Nothing -> m.mousePos
      Just { pos } -> 
         let delta = InputPosition.deltaKeyboardPos (getActiveSizeGrid m) p in
         Point.add pos delta

toggleHelpOverlay : Model -> Model
toggleHelpOverlay model = {model | showOverlayHelp = not model.showOverlayHelp } 