module Msg exposing (Msg(..), noOp, focusId, unfocusId,
  onTabPreventDefault, Scenario(..), scenarioOfString, LoadGraphInfo, mapLoadGraphInfo,
  isSimpleScenario, loadGraphInfoToMsg)

import Geometry.Point exposing (Point)
-- import Graph exposing (Graph, NodeId)
-- import GraphExtra exposing (EdgeId)
import Polygraph as Graph exposing (EdgeId, NodeId, Graph)
import Format.GraphInfo exposing (GraphInfo)

import HtmlDefs exposing (Key)
import Task

import Browser.Dom as Dom
import ArrowStyle exposing (ArrowStyle)
import GraphDefs exposing (NodeLabel, EdgeLabel)
import Html.Events.Extra.Mouse as MouseEvents
import Html
import Json.Encode as JE

-- SimpleScenario: just display the model status message
type Scenario = Standard | Exercise1 | SimpleScenario | Watch | CoqLsp

isSimpleScenario : Scenario -> Bool
isSimpleScenario s = s == SimpleScenario
scenarioOfString : String -> Scenario
scenarioOfString s =
  case s of
      "exercise1" -> Exercise1
      "watch" -> Watch
      "coqlsp" -> CoqLsp
      _ -> Standard

type alias LoadGraphInfo a = 
   { graph : a, fileName : String, 
   -- TODO: remove this
     scenario : String,
     clipboard : Bool, -- is it a paste event?
     setFirstTab : Bool -- set the active tab on the first tab
     -- (if true, the clipboard flag is ignored)
   }

mapLoadGraphInfo : (a -> b) -> LoadGraphInfo a -> LoadGraphInfo b 
mapLoadGraphInfo f { graph, fileName, scenario, clipboard, setFirstTab } =
   { graph = f graph, fileName = fileName, scenario = scenario,
     clipboard = clipboard, setFirstTab = setFirstTab }

-- the model automatically updates its record of HtmlDefs.Keys (shift,alt,ctrl status) in any case
-- when the message gives it, so there is a kind of redundancy on this matter
type Msg
  = -- call some js function
    Do (Cmd Msg)
  | Save
  | FileName String -- new file name
  | ExportQuiver
    -- on reception of this message, the js function onMouseMove is called
    -- which sends back a MouseMove message with the relative position to 
    -- the canvas
  | MouseMoveRaw JE.Value HtmlDefs.Keys
  | MouseLeaveCanvas
  | KeyChanged Bool HtmlDefs.Keys Key
  | MouseMove Point
  | MouseClick 
  | MouseDown MouseEvents.Event -- is Shift selected?
  | MouseUp
  -- | NodeEnter NodeId
  -- | NodeLeave NodeId
  | NodeClick NodeId MouseEvents.Event
  | EltDoubleClick Graph.Id MouseEvents.Event
  | EdgeClick EdgeId MouseEvents.Event
 -- | EltHover Graph.Id 
  | EdgeLabelEdit EdgeId String
  | NodeLabelEdit NodeId String
  | CopyGraph
  | Loaded (LoadGraphInfo GraphInfo)
  | SetFirstTab GraphInfo  
  -- a graph is pasted
  | PasteGraph GraphInfo
  | QuickInput Bool String -- flag: is it the final string?
  | SetFirstTabEquation String
  | NodeRendered NodeId Point
  | EdgeRendered EdgeId Point
  | MouseOn Graph.Id
  | Clear Scenario
  | SizeGrid Int
  | ToggleHideGrid
  | ToggleAutosave
  | SaveGridSize
  | OptimalGridSize
  | SwitchTab Int
  | NewTab
  | RemoveTab
  | RenameTab String
  | FindReplace { search: String, replace:String}
  | MinuteTick
  -- means that some key has been pressed
  -- for a long time
  | PressTimeout
  | LatexPreambleEdit String
  | SimpleMsg String
  | CompleteEquation { statement : String, script : String}
  | AppliedProof String
  -- | ComputeLayout
  -- | FindInitial
  -- | EditBottomText String
  -- pressing tab when editing the input text
  -- | TabInput

loadGraphInfoToMsg : LoadGraphInfo GraphInfo -> Msg
loadGraphInfoToMsg g =
   if g.setFirstTab then
      SetFirstTab g.graph
   else if g.clipboard then
  --  Debug.log "coucou" <|
      PasteGraph g.graph
   else
      Loaded g

noOp : Msg
noOp = Do Cmd.none


-- Focus on the input

focusId : String -> Cmd Msg
focusId s = Task.attempt (\_ -> noOp) (Dom.focus s)

unfocusId : String -> Cmd Msg
unfocusId s = Task.attempt (\_ -> noOp) (Dom.blur s)


onTabPreventDefault : Html.Attribute Msg
onTabPreventDefault = HtmlDefs.preventsDefaultOnKeyDown
   noOp (\ _ k -> k == HtmlDefs.Control "Tab")
