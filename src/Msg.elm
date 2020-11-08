module Msg exposing (..)

import Collage exposing (Point)
import Graph exposing (Graph, NodeId)
import GraphExtra exposing (EdgeId)
import Json.Decode as D
import Tuple
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


type alias EdgeLabel = String
type alias NodeLabel = { pos : Point , label : String}

setPos : Point -> NodeLabel -> NodeLabel
setPos p l = { l | pos = p}


type Msg
  = -- call some js function
    Do (Cmd Msg)
  | KeyChanged Bool Key
  | MouseMove Point
  | MouseClick 
  | NodeEnter NodeId
  | NodeLeave NodeId
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
