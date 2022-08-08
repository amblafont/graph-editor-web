module Msg exposing (Msg(..), noOp, updateArrowStyle, focusId, unfocusId,
  onTabPreventDefault, mayUpdateArrowStyle, Scenario(..), scenarioOfString, LoadGraphInfo, mapLoadGraphInfo)

import Collage exposing (Point)
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

type Scenario = Standard | Exercise1

scenarioOfString : String -> Scenario
scenarioOfString s =
  case s of
      "exercise1" -> Exercise1
      _ -> Standard

type alias LoadGraphInfo a = { graph : a, fileName : String, scenario : String }

mapLoadGraphInfo : (a -> b) -> LoadGraphInfo a -> LoadGraphInfo b 
mapLoadGraphInfo f { graph, fileName, scenario } =
   { graph = f graph, fileName = fileName, scenario = scenario }

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
  | Loaded (LoadGraphInfo GraphInfo)
  | CopyGraph
  -- a graph is pasted
  | PasteGraph GraphInfo
  | QuickInput Bool String -- flag: is it the final string?
  | NodeRendered NodeId Point
  | EdgeRendered EdgeId Point
  | MouseOn Graph.Id
  | Clear
  | SizeGrid Int
  | ToggleHideGrid
  | ToggleAutosave
  | FindReplace { search: String, replace:String}
  | MinuteTick
  | LatexPreambleEdit String
  -- | ComputeLayout
  -- | FindInitial
  -- | EditBottomText String
  -- pressing tab when editing the input text
  -- | TabInput

noOp : Msg
noOp = Do Cmd.none


-- Focus on the input

focusId : String -> Cmd Msg
focusId s = Task.attempt (\_ -> noOp) (Dom.focus s)

unfocusId : String -> Cmd Msg
unfocusId s = Task.attempt (\_ -> noOp) (Dom.blur s)


mayUpdateArrowStyle : Msg -> ArrowStyle -> Maybe ArrowStyle
mayUpdateArrowStyle m style =
   case m of 
      KeyChanged False _ k -> ArrowStyle.keyMaybeUpdateStyle k style  
      _ -> Nothing




updateArrowStyle : Msg -> ArrowStyle -> ArrowStyle
updateArrowStyle m style = mayUpdateArrowStyle m style |> Maybe.withDefault style


onTabPreventDefault : Html.Attribute Msg
onTabPreventDefault = HtmlDefs.preventsDefaultOnKeyDown
   noOp (\ _ k -> k == HtmlDefs.Control "Tab")
