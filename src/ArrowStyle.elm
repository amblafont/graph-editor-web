module ArrowStyle exposing (ArrowStyle, empty, keyUpdateStyle, quiverStyle,
   tailToString , tailFromString,
   headToString, headFromString,  
   makeHeadTailImgs, isDouble, doubleSize,
   toggleDashed, dashedStr )

import HtmlDefs exposing (Key(..))

import Geometry.Point as Point exposing (Point)
import Svg exposing (Svg)
import Svg.Attributes as Svg
import Geometry.QuadraticBezier exposing (QuadraticBezier)
import Json.Encode as JEncode

imgDir : String
imgDir = "img/arrow/"

-- following the tikzcd-editor
dashedStr : String
dashedStr = "7, 3"

-- from GridCellArrow in tikz-cd editor
imgWidth : Float
imgWidth = 9.764

imgHeight : Float
imgHeight = 13

doubleSize = 2.5


type alias Style = { tail : TailStyle, head : HeadStyle, double : Bool, dashed : Bool, bend : Float} 
type alias ArrowStyle = Style

tailToString : TailStyle -> String
tailToString tail =
   case tail of
         DefaultTail -> "none"
         Hook -> "hook"
         HookAlt -> "hookalt"
tailFromString : String -> TailStyle
tailFromString tail =
   case tail of         
         "hook" -> Hook
         "hookalt" -> HookAlt
         _ -> DefaultTail

headToString : HeadStyle -> String
headToString head =
  case head of
       DefaultHead -> "default" 
       TwoHeads    -> "twoheads" 
       NoHead      -> "none"

headFromString : String -> HeadStyle
headFromString head =
  case head of        
       "twoheads" -> TwoHeads    
       "none" -> NoHead      
       _ -> DefaultHead

empty : Style
empty = { tail = DefaultTail, head = DefaultHead, double = False, dashed = False,
          bend = 0 }

isDouble : Style -> Basics.Bool
isDouble { double } = double
  

type HeadStyle = DefaultHead | TwoHeads | NoHead
type TailStyle = DefaultTail | Hook | HookAlt

toggleHead : Style -> Style
toggleHead s =  { s | head =
                      case s.head of
                         DefaultHead -> NoHead
                         NoHead -> TwoHeads
                         TwoHeads -> DefaultHead
                }

toggleHook : Style -> Style
toggleHook s =  
        { s | tail =
              case s.tail of
                  DefaultTail -> Hook
                  Hook -> HookAlt
                  HookAlt -> DefaultTail
        }
    

toggleDouble : Style -> Style
toggleDouble s = { s | double = not s.double }
  
toggleDashed : Style -> Style
toggleDashed s = { s | dashed = not s.dashed }

prefixDouble : Style -> String
prefixDouble { double } = 
  if double then "double-" else ""

headFileName : Style -> String
headFileName s = 
  prefixDouble s
   ++ 
   headToString s.head ++ ".svg"
     
tailFileName : Style -> String
tailFileName s =
  prefixDouble s
   ++ tailToString s.tail ++ ".svg"

   
     

svgRotate : Point -> Float -> Svg.Attribute a
svgRotate (x2, y2) angle = 
     Svg.transform <|         
        " rotate(" ++ String.fromFloat angle 
          ++ " " ++ String.fromFloat x2
          ++ " " ++ String.fromFloat y2 ++ ")"

makeImg : Point -> Float -> String -> Svg a
makeImg (x,y) angle file =
     let (xh, yh) = (x - imgHeight / 2, y - imgHeight / 2) in
     let f = String.fromFloat in
     Svg.image
          ([Svg.xlinkHref <| imgDir ++ file,
          Svg.x <| f xh,
          Svg.y <| f yh,
          Svg.width <| f imgWidth,
          Svg.height <| f imgHeight,
          svgRotate (x,y) angle]          
          )
           []

makeHeadTailImgs : QuadraticBezier -> Style -> List (Svg a)
makeHeadTailImgs {from, to, controlPoint} style =
   
    let angle delta =  Point.pointToAngle delta * 180 / pi in
     
    -- let mkImg = makeImg from to angle in
    [ makeImg to (angle <| Point.subtract to controlPoint) 
       <| headFileName style,
      makeImg from (angle <| Point.subtract controlPoint from) 
       <| tailFileName style ]




keyUpdateStyle : Key -> Style -> Style
keyUpdateStyle k style = 
   case k of 
        Character '>' -> toggleHead style
        Character '(' -> toggleHook style
        Character '=' -> toggleDouble style
        Character '-' -> toggleDashed style
        Character 'b' -> {style | bend = style.bend + 0.1}
        Character 'B' -> {style | bend = style.bend - 0.1}
        _ -> style

quiverStyle : ArrowStyle -> List (String, JEncode.Value)
quiverStyle st =
   let { tail, head, double, dashed } = st in
   let makeIf b x = if b then [x] else [] in
   let headStyle = case head of 
          DefaultHead -> []       
          TwoHeads -> [("head", [("name", "epi")])]
          NoHead -> [("head", [("name", "none")])]
   in
   let tailStyle = case tail of 
          DefaultTail -> []
          Hook -> [("tail", [("name", "hook"),("side", "top")])]
          HookAlt -> [("tail", [("name", "hook"),("side", "bottom")])]
   in
   let style = List.map (\(x,y) -> (x, JEncode.object <| List.map (\(s, l) -> (s, JEncode.string l)) y)) <|
               headStyle
               ++
               tailStyle ++
               (makeIf dashed ("body", [("name", "dashed")]))
   in
   (makeIf double ("level", JEncode.int 2))  
   ++ [("style", JEncode.object style )]
   ++ (makeIf (st.bend /= 0) ("curve", JEncode.int <| floor (st.bend * 10)))
