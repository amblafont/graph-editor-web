module Msg exposing (Msg(..), noOp, updateArrowStyle, focusId)

import Collage exposing (Point)
import Graph exposing (Graph, NodeId)
import GraphExtra exposing (EdgeId)

import HtmlDefs exposing (Key)
import Task

import Browser.Dom as Dom
import ArrowStyle exposing (ArrowStyle)
import GraphDefs exposing (NodeLabel, EdgeLabel)





type Msg
  = -- call some js function
    Do (Cmd Msg)
  | KeyChanged Bool Key
  | MouseMove Point
  | MouseClick 
  | MouseDown
  | MouseUp
  -- | NodeEnter NodeId
  -- | NodeLeave NodeId
  | NodeClick NodeId
  | EdgeClick EdgeId
  | EdgeLabelEdit EdgeId String
  | NodeLabelEdit NodeId String
  | Loaded (Graph NodeLabel EdgeLabel)
  | QuickInput String
  | NodeRendered NodeId Point
  | EdgeRendered EdgeId Point
  -- pressing tab when editing the input text
  | TabInput

noOp : Msg
noOp = Do Cmd.none


-- Focus on the input

focusId : String -> Cmd Msg
focusId s = Task.attempt (\_ -> noOp) (Dom.focus s)


updateArrowStyle : Msg -> ArrowStyle -> ArrowStyle
updateArrowStyle m style =
   case m of 
      KeyChanged False k -> ArrowStyle.keyUpdateStyle k style  
      _ -> style
        