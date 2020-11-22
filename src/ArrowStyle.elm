module ArrowStyle exposing (ArrowStyle, empty, keyUpdateStyle)

import ArrowStyle.Core as Core
import HtmlDefs exposing (Key(..))

type alias ArrowStyle = { s : Core.Style , bend : Float }
type alias Style = ArrowStyle

empty : Style
empty = ArrowStyle Core.empty 0

keyUpdateCore : Key -> Core.Style -> Core.Style
keyUpdateCore k style =
   case k of 
        Character '>' -> Core.toggleHead style
        Character '(' -> Core.toggleHook style
        Character '=' -> Core.toggleDouble style
        Character '-' -> Core.toggleDashed style
        _ -> style

keyUpdateBend : Key -> Float -> Float
keyUpdateBend k bend =
  case k of
     Character 'b' -> bend + 0.1
     Character 'B' -> bend - 0.1
     _ -> bend

keyUpdateStyle : Key -> Style -> Style
keyUpdateStyle k { s , bend } = 
  {s = keyUpdateCore k s, 
  bend = keyUpdateBend k bend}