module Drawing.Color exposing (Color, helpMsg, merge, toString,
  black, red, blue, green, yellow, purple, orange, white, fromChar, codec)

import Codec exposing (Codec)
type Color = Black | Red | Blue | White | Purple | Yellow | Gray | Green | Orange

merge : Color -> Color -> Color
merge c1 c2 =  case(c1, c2) of
   (Black, _) -> c2
   (_, Black) -> c1
   _ -> c1

helpMsg : String
helpMsg = "bla[c]k, bl[u]e, [g]reen, [o]range, [r]ed, [v]iolet, yello[w], gra[y]" 
-- [w]hite" --, [y]ellow"

fromChar : Char -> Maybe Color
fromChar s = case s of
   'r' -> Just Red
   'u' -> Just Blue
   'w' -> Just Yellow
   'v' -> Just Purple
   'g' -> Just Green
   'y' -> Just Gray
   'o' -> Just Orange
   'c' -> Just Black
   _ -> Nothing

codec : Codec Color String
codec = 
   let split cgray cred cblue cwhite cpurple cgreen cyellow corange cblack v =
            case v of 
               Gray -> cgray
               Red -> cred
               Blue -> cblue
               White -> cwhite
               Purple -> cpurple
               Green -> cgreen
               Yellow -> cyellow
               Orange -> corange
               Black -> cblack
   in
   Codec.customEnum split 
   |> Codec.variant0 "gray" Gray
   |> Codec.variant0 "red" Red
   |> Codec.variant0 "blue" Blue
   |> Codec.variant0 "white" White
   |> Codec.variant0 "purple" Purple
   |> Codec.variant0 "green" Green
   |> Codec.variant0 "yellow" Yellow
   |> Codec.variant0 "orange" Orange
   |> Codec.variant0 "black" Black
   |> Codec.buildVariant (always Black)


toString : Color -> String
toString = Codec.encoder codec

black : Color
black = Black

red : Color
red = Red

white : Color
white = White

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