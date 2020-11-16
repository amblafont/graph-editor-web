module Msg exposing (..)

import Collage exposing (Point)
import Graph exposing (Graph, NodeId)
import GraphExtra exposing (EdgeId)
import Json.Decode as D
import Tuple
import Task

import Browser.Dom as Dom
import ArrowStyle  
-- From https://github.com/elm/browser/blob/1.0.2/notes/keyboard.md
-- useful for keyboard events
type Key
  = Character Char
  | Control String
  
keyToString : Key -> String
keyToString k = case k of
                    Character c -> String.fromChar c
                    Control s -> s

toKey : String -> Key
toKey string = 
  case String.uncons string of
    Just (char, "") -> Character char
    _ -> Control string

keyDecoder : D.Decoder Key
keyDecoder = D.field "key" D.string
             |> D.map toKey

type alias ArrowStyle = { s : ArrowStyle.Style, bend : Float }

emptyArrowStyle : ArrowStyle
emptyArrowStyle = ArrowStyle ArrowStyle.empty 0

type alias EdgeLabel = { label : String, style : ArrowStyle}
type alias NodeLabel = { pos : Point , label : String}

type alias EdgeLabelJs = { label : String, style : ArrowStyle.JsStyle, bend : Float}

edgeLabelToJs : EdgeLabel -> EdgeLabelJs
edgeLabelToJs {label, style} = 
  {label = label, style = ArrowStyle.toJsStyle style.s, bend = style.bend}

edgeLabelFromJs : EdgeLabelJs -> EdgeLabel
edgeLabelFromJs {label, style, bend } = 
  EdgeLabel label <| ArrowStyle (ArrowStyle.fromJsStyle style) bend



setPos : Point -> NodeLabel -> NodeLabel
setPos p l = { l | pos = p}


type Msg
  = -- call some js function
    Do (Cmd Msg)
  | KeyChanged Bool Key
  | MouseMove Point
  | MouseClick 
  -- | NodeEnter NodeId
  -- | NodeLeave NodeId
  | NodeClick NodeId
  | EdgeClick EdgeId
  | EdgeLabelEdit EdgeId String
  | NodeLabelEdit NodeId String
  | Loaded (Graph NodeLabel EdgeLabel)
  | QuickInput String
  | SizeChanged NodeId (Maybe Point)

noOp : Msg
noOp = Do (Cmd.none)


newsizeDecoder : NodeId -> D.Decoder Msg
newsizeDecoder n = 
    D.field "detail" <|
    D.map (SizeChanged n) <|
    D.map Just <|
    D.map2 Tuple.pair 
        (D.field "width" D.float)
        (D.field "height" D.float)

nosizeDecoder : NodeId -> D.Decoder Msg
nosizeDecoder n = D.succeed (SizeChanged n Nothing)


focusId : String -> Cmd Msg
focusId s =
    Task.attempt (\_ -> noOp) (Dom.focus s)



-- Focus on the input

-- id of the text input when the user labels an edge or a node
curIdInput : String
curIdInput = "edited_label"

focusLabelInput : Cmd Msg
focusLabelInput =
    focusId curIdInput

keyUpdateArrowStyle : Key -> ArrowStyle.Style -> ArrowStyle.Style
keyUpdateArrowStyle k style =
   case k of 
        Character '>' -> ArrowStyle.toggleHead style
        Character '(' -> ArrowStyle.toggleHook style
        Character '=' -> ArrowStyle.toggleDouble style
        Character '-' -> ArrowStyle.toggleDashed style
        _ -> style

keyUpdateBend : Key -> Float -> Float
keyUpdateBend k bend =
  case k of
     Character 'b' -> bend + 0.1
     Character 'B' -> bend - 0.1
     _ -> bend

msgUpdateArrowStyle : Msg -> ArrowStyle -> ArrowStyle
msgUpdateArrowStyle m style =
   case m of 
      KeyChanged False k ->
         {s = keyUpdateArrowStyle k style.s, 
          bend = keyUpdateBend k style.bend }
         
      _ -> style
        