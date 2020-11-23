module HtmlDefs exposing (onRendered, tabDecoder, quickInputId, idInput, Key(..), keyDecoder, makeLatex)
import Html
import Html.Attributes
import Html.Events
import Geometry.Point exposing (Point)
import Json.Decode as D


-- id of the text input when the user labels an edge or a node
idInput : String
idInput = "edited_label"

quickInputId : String
quickInputId = "quickinput"

renderedClass = "rendered-callback"
renderedEvent = "rendered"

renderedDecoder : D.Decoder Point
renderedDecoder = 
    D.field "detail" <|    
    D.map2 Tuple.pair 
        (D.field "width" D.float)
        (D.field "height" D.float)

onRendered : (Point -> msg) -> List (Html.Attribute msg)
onRendered onRender =
    [ Html.Events.on renderedEvent (D.map onRender renderedDecoder),
      Html.Attributes.class renderedClass ]

-- From https://github.com/elm/browser/blob/1.0.2/notes/keyboard.md
-- useful for keyboard events
type Key
  = Character Char
  | Control String
  
-- keyToString : Key -> String
-- keyToString k = case k of
--                     Character c -> String.fromChar c
--                     Control s -> s

toKey : String -> Key
toKey string = 
  case String.uncons string of
    Just (char, "") -> Character char
    _ -> Control string

keyDecoder : D.Decoder Key
keyDecoder = D.field "key" D.string
             |> D.map toKey

-- is it a tab?
tabDecoder : D.Decoder Bool
tabDecoder = keyDecoder |>
 D.map 
 (\ k -> case k of 
          Control "Tab" -> True 
          _ -> False )

makeLatex : List (Html.Attribute a) -> String -> Html.Html a
makeLatex attrs s = 
   Html.node "math-latex" attrs [Html.text s]