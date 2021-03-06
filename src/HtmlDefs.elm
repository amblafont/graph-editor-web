module HtmlDefs exposing (onRendered, tabDecoder, {- quickInputId, -} idInput, canvasId,
   Key(..), Keys, keyDecoder, keysDecoder, makeLatex, checkbox, slider,
   onTab)
import Html
import Html.Attributes
import Html.Events
import Geometry.Point exposing (Point)
import Json.Decode as D



-- id of the text input when the user labels an edge or a node
idInput : String
idInput = "edited_label"

canvasId : String
canvasId = "canvas"

-- quickInputId : String
-- quickInputId = "quickinput"

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

onTab : a -> a -> Html.Attribute a 
onTab msgOnTab msgNotOnTab =
 Html.Events.preventDefaultOn "keydown" 
                         (D.map (\tab -> if tab then 
                            -- it is necessary to prevent defaults
                            -- otherwise the next immediate appearing input 
                            -- may not shows up
                                      (msgOnTab, True) 
                                    else (msgNotOnTab, False))
                         tabDecoder
                         )
   

makeLatex : List (Html.Attribute a) -> String -> Html.Html a
makeLatex attrs s = 
   Html.node "math-latex" attrs [Html.text s]


type alias Keys =
    { alt : Bool, ctrl : Bool, shift : Bool }

keysDecoder : D.Decoder Keys
keysDecoder = D.map3 (\ alt ctrl shift -> { alt = alt, ctrl = ctrl, shift = shift})
     (D.field "altKey" D.bool) (D.field "ctrlKey" D.bool) (D.field "shiftKey" D.bool)

-- copied from https://github.com/dwyl/learn-elm/blob/master/examples/checkboxes.elm
checkbox : msg -> String -> Bool -> Html.Html msg
checkbox msg name checked =
    Html.label
        [ Html.Attributes.style "padding" "20px" ]
        [ Html.input [ Html.Attributes.type_ "checkbox", 
           Html.Events.onClick msg,
           Html.Attributes.checked checked ] []
        , Html.text name
        ]

slider : (Int -> msg) -> String -> Int -> Int -> Int -> Html.Html msg
slider msg name min max value =
     let f = String.fromInt in 
      Html.label
        [ Html.Attributes.style "padding" "20px" ]
        [ Html.input [ Html.Attributes.type_ "range", 
           Html.Attributes.value <| f value,
           Html.Attributes.min <| f min,
           Html.Attributes.max <| f max,
          Html.Events.onInput (String.toInt >> Maybe.withDefault value >> msg) ] []
        , Html.text name
        ]