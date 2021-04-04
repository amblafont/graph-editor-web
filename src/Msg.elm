module Msg exposing (Msg(..), noOp, updateArrowStyle, focusId, onTabPreventDefault)

import Collage exposing (Point)
-- import Graph exposing (Graph, NodeId)
-- import GraphExtra exposing (EdgeId)
import Polygraph as Graph exposing (EdgeId, NodeId, Graph)

import HtmlDefs exposing (Key)
import Task

import Browser.Dom as Dom
import ArrowStyle exposing (ArrowStyle)
import GraphDefs exposing (NodeLabel, EdgeLabel)
import Html.Events.Extra.Mouse as MouseEvents
import Html
import Json.Encode as JE



-- the model automatically updates its record of HtmlDefs.Keys (shift,alt,ctrl status) in any case
-- when the message gives it, so there is a kind of redundancy on this matter
type Msg
  = -- call some js function
    Do (Cmd Msg)
  | Save
  | ExportQuiver
    -- on reception of this message, the js function onMouseMove is called
    -- which sends back a MouseMove message with the relative position to 
    -- the canvas
  | MouseMoveRaw JE.Value HtmlDefs.Keys
  | KeyChanged Bool HtmlDefs.Keys Key
  | MouseMove Point
  | MouseClick 
  | MouseDown MouseEvents.Event -- is Shift selected?
  | MouseUp
  -- | NodeEnter NodeId
  -- | NodeLeave NodeId
  | NodeClick NodeId MouseEvents.Event
  | EdgeClick EdgeId MouseEvents.Event
  | EdgeLabelEdit EdgeId String
  | NodeLabelEdit NodeId String
  | Loaded (Graph NodeLabel EdgeLabel)
  | QuickInput String
  | NodeRendered NodeId Point
  | EdgeRendered EdgeId Point
  | MouseOn Graph.Id
  | Clear
  | SizeGrid Int
  | ToggleHideGrid
  | SnapToGrid
  -- pressing tab when editing the input text
  -- | TabInput

noOp : Msg
noOp = Do Cmd.none


-- Focus on the input

focusId : String -> Cmd Msg
focusId s = Task.attempt (\_ -> noOp) (Dom.focus s)


updateArrowStyle : Msg -> ArrowStyle -> ArrowStyle
updateArrowStyle m style =
   case m of 
      KeyChanged False _ k -> ArrowStyle.keyUpdateStyle k style  
      _ -> style

onTabPreventDefault : Html.Attribute Msg
onTabPreventDefault = HtmlDefs.onTab noOp noOp
        