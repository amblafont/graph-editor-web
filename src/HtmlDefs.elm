port module HtmlDefs exposing (onRendered, quickInputId, idInput, canvasId,
   Key(..), Keys, keyDecoder, keysDecoder, makeLatex, checkbox, slider
   , preventsDefaultOnKeyDown, makePasteCapture,
   bottomTextId, computeLayout)
import Html
import Html.Attributes
import Html.Events
import Geometry.Point exposing (Point)
import Json.Decode as D

port computeLayout : () -> Cmd a



-- id of the text input when the user labels an edge or a node
idInput : String
idInput = "edited_label"

canvasId : String
canvasId = "canvas"

bottomTextId : String
bottomTextId = "bottom-text"

pasteElement = "paste-capture"
latexElement = "math-latex"

quickInputId : String
quickInputId = "quickinput"

renderedClass = "rendered-callback"
renderedEvent = "rendered"

pasteEvent = "pasteData"

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

onPaste : (D.Value -> msg) -> Html.Attribute msg
onPaste handler = Html.Events.on pasteEvent 
                  <| D.map handler D.value


makePasteCapture : (D.Value -> a) -> List (Html.Attribute a) -> List (Html.Html a) -> Html.Html a
makePasteCapture handler attrs s =
  Html.node pasteElement (onPaste handler :: attrs) s
  
   
      


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






          


preventsDefaultOnKeyDown : a -> (Keys -> Key -> Bool) -> Html.Attribute a
preventsDefaultOnKeyDown noOp filter =
    Html.Events.preventDefaultOn "keydown" 
                         (D.map2 (\ks k -> if filter ks k then                             
                                      (noOp, True) 
                                    else (noOp, False))
                         keysDecoder
                         keyDecoder
                         )
makeLatex : List (Html.Attribute a) -> String -> Html.Html a
makeLatex attrs s = 
   Html.node latexElement attrs [Html.text s]


type alias Keys =
    { alt : Bool, ctrl : Bool, shift : Bool }

keysDecoder : D.Decoder Keys
keysDecoder = D.map3 (\ alt ctrl shift -> { alt = alt, ctrl = ctrl, shift = shift})
     (D.field "altKey" D.bool) (D.field "ctrlKey" D.bool) (D.field "shiftKey" D.bool)

-- copied from https://github.com/dwyl/learn-elm/blob/master/examples/checkboxes.elm
checkbox : msg -> String -> String -> Bool -> Html.Html msg
checkbox msg name tooltip checked =
    Html.label
        [ Html.Attributes.title tooltip {- Html.Attributes.style "padding" "20px" -} ]
        [ Html.input [ Html.Attributes.type_ "checkbox", 
           Html.Events.onClick msg,
           Html.Attributes.checked checked,
           Html.Attributes.title tooltip
            ] []
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