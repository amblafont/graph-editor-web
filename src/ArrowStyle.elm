module ArrowStyle exposing (Style, JsStyle, toJsStyle, fromJsStyle, empty, toggleHead, toggleHook, toggleDouble,
                                 makeHeadTailImgs, isDouble, doubleSize,
                                 toggleDashed, dashedStr)

import Point exposing (Point)
import Svg exposing (Svg)
import Svg.Attributes as Svg

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


type alias Style = { tail : TailStyle, head : HeadStyle, double : Bool, dashed : Bool} 

-- For the purpose of saving to json
type alias JsStyle = { tail : String, head : String, double : Bool, dashed : Bool}
    
toJsStyle : Style -> JsStyle
toJsStyle { head, tail, double, dashed }=
  { tail = tailToString tail,
    head = headToString head,
    double = double,
    dashed = dashed
  }

fromJsStyle : JsStyle -> Style
fromJsStyle {tail, head, double, dashed} =
  {tail = tailFromString tail,
   head = headFromString head,
   double = double,
   dashed = dashed
  }
  
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
empty = { tail = DefaultTail, head = DefaultHead, double = False, dashed = False }

isDouble : Style -> Basics.Bool
isDouble { double } = double
  

type HeadStyle = DefaultHead | TwoHeads | NoHead
type TailStyle = DefaultTail | Hook | HookAlt

toggleHead : Style -> Style
toggleHead s =
        { s | head =
         case s.head of
            DefaultHead -> TwoHeads
            TwoHeads -> NoHead
            NoHead -> DefaultHead }
      

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

makeImg : Bool -> Point -> Float -> String -> Svg a
makeImg dashed (x,y) angle file =
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

makeHeadTailImgs : Point -> Point -> Style -> List (Svg a)
makeHeadTailImgs from to style =
    let delta = Point.subtract to from in
    let angle = (\ a -> a * 180 / pi) <| Point.pointToAngle <| delta in
    -- let mkImg = makeImg from to angle in
    [ makeImg style.dashed to   angle <| headFileName style,
      makeImg style.dashed from angle <| tailFileName style ]
