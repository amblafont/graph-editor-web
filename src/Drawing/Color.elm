module Drawing.Color exposing (Color, helpMsg, merge, toString,
  black, red, blue, green, yellow, purple, orange, fromChar, codec)

import Codec exposing (Codec)
type Color = Black | Red | Blue | White | Purple | Yellow | Green | Orange

merge : Color -> Color -> Color
merge c1 c2 =  case(c1, c2) of
   (Black, _) -> c2
   (_, Black) -> c1
   _ -> c1

helpMsg : String
helpMsg = "bla[c]k, bl[u]e, [g]reen, [o]range, [r]ed, [v]iolet, [y]ellow" 
-- [w]hite" --, [y]ellow"

fromChar : Char -> Maybe Color
fromChar s = case s of
   'r' -> Just Red
   'u' -> Just Blue
  -- 'w' -> Just White
   'v' -> Just Purple
   'g' -> Just Green
   'y' -> Just Yellow
   'o' -> Just Orange
   'c' -> Just Black
   _ -> Nothing

codec : Codec Color String
codec = 
   Codec.enum
   [(Red, "red")
   , (Blue, "blue")
   , (White, "white")
   , (Purple, "purple")
   , (Green, "green")
   , (Yellow, "yellow")
   , (Orange, "orange")]
    (Black, "black")

toString : Color -> String
toString = Codec.encoder codec

black : Color
black = Black

red : Color
red = Red

blue : Color
blue = Blue
purple : Color
purple = Purple
green : Color
green = Green
yellow : Color
yellow = Yellow
orange : Color
orange = Orange