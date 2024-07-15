module ArrowStyle exposing (ArrowStyle, empty, {- keyUpdateStyle, -} quiverStyle,
   tikzStyle, tailToString , tailFromString,
   headToString, headFromString, 
   alignmentToString, alignmentFromString, 
   isDouble, doubleSize,
   controlChars, kindToString, kindFromString,
   toggleDashed, dashedStr, -- PosLabel(..),
   -- quiver
    keyMaybeUpdateStyle,
    keyMaybeUpdateColor, makeHeadShape, makeTailShape, getStyle, isNone, simpleLineStyle)

import HtmlDefs exposing (Key(..))


import Drawing.Color as Color exposing (Color)
import Geometry.Epsilon exposing (norm0, epsilon)
import Geometry exposing (LabelAlignment(..))
import Json.Encode as JEncode
import List.Extra as List
import ListExtraExtra exposing (nextInList)
import String.Svg as Svg exposing (Svg)


doubleSize = 2.5

{-}
type PosLabel =
     DefaultPosLabel
   | LeftPosLabel
   | RightPosLabel -}
type alias Style = { tail : TailStyle, 
                     head : HeadStyle, kind : ArrowKind, 
                     dashed : Bool, bend : Float,
                     labelAlignment : LabelAlignment,
                     -- betweeon 0 and 1, 0.5 by default
                     labelPosition : Float,
                     color : Color
                    } 

simpleLineStyle : Style
simpleLineStyle = { tail = DefaultTail, head = NoHead, kind = NormalArrow, dashed = False,
          bend = 0, labelAlignment = Left,
          labelPosition = 0.5, color = Color.black }
type alias ArrowStyle = Style
type ArrowKind = NormalArrow | NoneArrow | DoubleArrow

kindToString : ArrowKind -> String
kindToString kind =
   case kind of
         NormalArrow -> "normal"
         NoneArrow -> "none"
         DoubleArrow -> "double"
kindFromString : String -> ArrowKind
kindFromString kind =
   case kind of
         "none" -> NoneArrow
         "double" -> DoubleArrow
         _ -> NormalArrow
tailToString : TailStyle -> String
tailToString tail =
   case tail of
         DefaultTail -> "none"
         Hook -> "hook"
         HookAlt -> "hookalt"
         Mapsto -> "mapsto"
tailFromString : String -> TailStyle
tailFromString tail =
   case tail of         
         "hook" -> Hook
         "hookalt" -> HookAlt
         "mapsto" -> Mapsto
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

alignmentToString : LabelAlignment -> String
alignmentToString tail =
   case tail of
         Centre -> "centre"
         Over -> "over"
         Left -> "left"
         Right -> "right"
alignmentFromString : String -> LabelAlignment
alignmentFromString tail =
   case tail of         
         "centre" -> Centre
         "right" -> Right
         "over" -> Over
         _ -> Left

empty : Style
empty = { tail = DefaultTail, head = DefaultHead, dashed = False,
          bend = 0, labelAlignment = Left,
          labelPosition = 0.5, color = Color.black, kind = NormalArrow }
isDouble : Style -> Basics.Bool
isDouble { kind } = kind == DoubleArrow
  
isNone : Style -> Bool
isNone { kind } = kind == NoneArrow

getStyle : { a | style : Style, isAdjunction : Bool} -> Style
getStyle {style, isAdjunction} =
   if isAdjunction then
      { style | head = NoHead, tail = DefaultTail, kind = NoneArrow, labelAlignment = Over }
   else style

type HeadStyle = DefaultHead | TwoHeads | NoHead
type TailStyle = DefaultTail | Hook | HookAlt | Mapsto






toggleHead : Style -> Style
toggleHead s =  { s | head = nextInList [DefaultHead, NoHead, TwoHeads] s.head }

toggleHook : Style -> Style
toggleHook s =  
        { s | tail = nextInList [Hook, HookAlt, DefaultTail] s.tail }

toggleMapsto : Style -> Style
toggleMapsto s =  { s | tail = nextInList [Mapsto, DefaultTail] s.tail }


toggleLabelAlignement : Style -> Style
toggleLabelAlignement s =  
        { s | labelAlignment = nextInList [Left, Right]
        -- , Centre, Over] 
        -- the other ones do not seem to work properly
        s.labelAlignment }


toggleDouble : Style -> Style
toggleDouble s = { s | kind = nextInList [DoubleArrow, NormalArrow] s.kind }
  
toggleDashed : Style -> Style
toggleDashed s = { s | dashed = not s.dashed }



-- chars used to control in keyUpdateStyle
controlChars = "|>(=-bBA]["
maxLabelPosition = 0.9
minLabelPosition = 0.1

-- doesn't update the color
keyMaybeUpdateStyle : Key -> Style -> Maybe Style
keyMaybeUpdateStyle k style = 
   case k of 
        Character '|' -> Just <| toggleMapsto style
        Character '>' -> Just <| toggleHead style
        Character '(' -> Just <| toggleHook style
        Character '=' -> Just <| toggleDouble style
        Character '-' -> Just <| toggleDashed style
        Character 'b' -> Just <| {style | bend = style.bend + 0.1 |> norm0}
        Character 'B' -> Just <| {style | bend = style.bend - 0.1 |> norm0}
        Character 'A' -> Just <| toggleLabelAlignement style
        Character ']' -> if style.labelPosition + epsilon >= maxLabelPosition then Nothing else
               Just {style | labelPosition = style.labelPosition + 0.1 |> min maxLabelPosition}
        Character '[' -> 
               if style.labelPosition <= minLabelPosition + epsilon then Nothing else
               Just {style | labelPosition = style.labelPosition - 0.1 |> max minLabelPosition}
        _ -> Nothing

keyMaybeUpdateColor : Key -> Style -> Maybe Style
keyMaybeUpdateColor k style =
   case k of 
      Character c ->
         -- let _ = Debug.log "char" c in 
         Color.fromChar c
         |> Maybe.andThen 
            (\ color -> if color == style.color then Nothing else 
                        Just { style | color = color})
      _ -> Nothing

--keyUpdateStyle : Key -> Style -> Style
--keyUpdateStyle k style = keyMaybeUpdateStyle k style |> Maybe.withDefault style


quiverStyle : ArrowStyle -> List (String, JEncode.Value)
quiverStyle st =
   let { tail, head, kind, dashed } = st in
   let makeIf b x = if b then [x] else [] in
   let double = isDouble st in
   let headStyle = case head of 
          DefaultHead -> []       
          TwoHeads -> [("head", [("name", "epi")])]
          NoHead -> [("head", [("name", "none")])]
   in
   let tailStyle = case tail of 
          DefaultTail -> []
          Mapsto -> [("tail", [("name", "maps to")])]
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
   ++ (makeIf (st.labelPosition /= 0.5) ("label_position", JEncode.int <| floor (st.labelPosition * 100)))

-- from Quiver
{-type LabelAlignment =
    Centre
  | Over
  | Left 
  | Right
  -}

headTikzStyle : HeadStyle -> String
headTikzStyle hd =
    case hd of
            DefaultHead -> "->, "
            TwoHeads -> "onto, "
            NoHead -> "-,"

-- following the tikzcd-editor
dashedStr : String
dashedStr = "7, 3"

tikzStyle : ArrowStyle -> String
tikzStyle stl =
    "fore, " ++
    Color.toString stl.color ++ "," ++
      (case (stl.head, stl.kind) of
            (NoHead, DoubleArrow) -> "identity"
            (hd, DoubleArrow) -> (headTikzStyle hd) ++ "cell=0.2, "
            (hd, NormalArrow) -> (headTikzStyle hd)
            (hd, NoneArrow) -> "draw=none, "
       )
    ++ (if stl.dashed then "dashed, " else "")
    ++ (if stl.bend /= 0 then
           "curve={ratio=" ++ String.fromFloat stl.bend ++ "}, "
        else "")
    ++  (case stl.tail of
         DefaultTail -> ""
         Mapsto -> "mapsto,"
         Hook -> "into, "
         HookAlt -> "linto, ")

        
-- the original svg code comes from from https://github.com/yishn/tikzcd-editor/tree/master/img/arrow
-- the corresponding elm code was generated by github copilot
makeHeadShape : Style -> Svg a
makeHeadShape style =
   if style.kind == NoneArrow then Svg.g [] [] else
   --   let (xh, yh) = (x - imgHeight / 2, y - imgHeight / 2) in
   --   let f = String.fromFloat in
   let double = isDouble style in
     case (double, style.head) of
         (False, DefaultHead) -> 
         {-
         The elm version of the following svg:
          <g fill="none" stroke="#000" stroke-width=".498" stroke-miterlimit="10">
    <path d="M2.043.253c.473 1.794 1.528 2.64 2.59 2.99-1.062.348-2.117 1.194-2.59 2.988" stroke-linecap="round" stroke-linejoin="round" transform="translate(-1.8 0)"/>
    <path d="M0 3.243H2"/>
  </g>
         -}
           Svg.g [ Svg.fill "none", Svg.stroke "#000", Svg.strokeWidth ".498", Svg.strokeMiterlimit "10" ]
            [ Svg.path [ Svg.d "M2.043.253c.473 1.794 1.528 2.64 2.59 2.99-1.062.348-2.117 1.194-2.59 2.988", Svg.strokeLinecap "round", Svg.strokeLinejoin "round", Svg.transform "translate(-1.8 0)" ] []
            , Svg.path [ Svg.d "M0 3.243H2" ] []
            ]

         (True, DefaultHead) ->
         {-
         <g fill="none" stroke="#000" stroke-width=".498" stroke-miterlimit="10">
    <path d="M2.043.253c.473 1.794 1.528 2.64 2.59 2.99-1.062.348-2.117 1.194-2.59 2.988" stroke-linecap="round" stroke-linejoin="round"/>
    <path d="M0 3.243H2.441" transform="translate(0 -1.25)" />
    <path d="M0 3.243H2.441" transform="translate(0 1.25)" />
  </g>
         -}
             Svg.g [ Svg.fill "none", Svg.stroke "#000", Svg.strokeWidth ".498", Svg.strokeMiterlimit "10" ]
               [ Svg.path [ Svg.d "M2.043.253c.473 1.794 1.528 2.64 2.59 2.99-1.062.348-2.117 1.194-2.59 2.988", Svg.strokeLinecap "round", Svg.strokeLinejoin "round" ] []
               , Svg.path [ Svg.d "M0 3.243H2.441", Svg.transform "translate(0 -1.25)" ] []
               , Svg.path [ Svg.d "M0 3.243H2.441", Svg.transform "translate(0 1.25)" ] []
               ]
         (False, TwoHeads) ->
         {-
           <g fill="none" stroke="#000" stroke-width=".498" stroke-miterlimit="10" stroke-linecap="round" stroke-linejoin="round">
    <path d="M.25.252c.473 1.794 1.528 2.64 2.59 2.99C1.778 3.59.723 4.436.25 6.23"/>
    <path d="M2.043.252c.473 1.794 1.528 2.64 2.59 2.99-1.062.348-2.117 1.194-2.59 2.988"/>
    <path d="M0 3.243H4.882"/>
  </g>
         -}
             Svg.g [ Svg.fill "none", Svg.stroke "#000", Svg.strokeWidth ".498", Svg.strokeMiterlimit "10", Svg.strokeLinecap "round", Svg.strokeLinejoin "round" ]
               [ Svg.path [ Svg.d "M.25.252c.473 1.794 1.528 2.64 2.59 2.99C1.778 3.59.723 4.436.25 6.23" ] []
               , Svg.path [ Svg.d "M2.043.252c.473 1.794 1.528 2.64 2.59 2.99-1.062.348-2.117 1.194-2.59 2.988" ] []
               , Svg.path [ Svg.d "M0 3.243H4.882" ] []
               ]
         (True, TwoHeads) ->
         {-
           <g fill="none" stroke="#000" stroke-width=".498" stroke-miterlimit="10">
    <g stroke-linecap="round" stroke-linejoin="round">
      <path d="M.25.252c.473 1.794 1.528 2.64 2.59 2.99C1.778 3.59.723 4.436.25 6.23"/>
      <path d="M2.043.252c.473 1.794 1.528 2.64 2.59 2.99-1.062.348-2.117 1.194-2.59 2.988"/>
    </g>
    <path d="M0 3.243H1" transform="translate(0 1.25)" />
    <path d="M0 3.243H1" transform="translate(0 -1.25)" />
  </g>
         -}
               Svg.g [ Svg.fill "none", Svg.stroke "#000", Svg.strokeWidth ".498", Svg.strokeMiterlimit "10" ]
                  [ Svg.g [ Svg.strokeLinecap "round", Svg.strokeLinejoin "round" ]
                     [ Svg.path [ Svg.d "M.25.252c.473 1.794 1.528 2.64 2.59 2.99C1.778 3.59.723 4.436.25 6.23" ] []
                     , Svg.path [ Svg.d "M2.043.252c.473 1.794 1.528 2.64 2.59 2.99-1.062.348-2.117 1.194-2.59 2.988" ] []
                     ]
                  , Svg.path [ Svg.d "M0 3.243H1", Svg.transform "translate(0 1.25)" ] []
                  , Svg.path [ Svg.d "M0 3.243H1", Svg.transform "translate(0 -1.25)" ] []
                  ]
              
         _ -> Svg.g [] []

         
         


makeTailShape : Style -> Svg a
makeTailShape style =
     if style.kind == NoneArrow then Svg.g [] [] else
     let double = isDouble style in
   --   let (xh, yh) = (x - imgHeight / 2, y - imgHeight / 2) in
   --   let f = String.fromFloat in
     case (double, style.tail) of
         (False, Hook) -> 
{-
  <g fill="none" stroke="#000" stroke-width=".498" stroke-miterlimit="10">
    <path d="M2.335 3.243h.753"/>
    <path d="M2.335.803C1.48.803.79 1.348.79 2.023c0 .674.69 1.22 1.544 1.22" stroke-linecap="round"/>
  </g>
-}
             Svg.g [ Svg.fill "none", Svg.stroke "#000", Svg.strokeWidth ".498", Svg.strokeMiterlimit "10" ]
               [ Svg.path [ Svg.d "M2.335 3.243h.753" ] []
               , Svg.path [ Svg.d "M2.335.803C1.48.803.79 1.348.79 2.023c0 .674.69 1.22 1.544 1.22", Svg.strokeLinecap "round" ] []
               ]
         (True, Hook) ->
   {-
    <g fill="none" stroke="#000" stroke-width=".498" stroke-miterlimit="10">
    <g transform="translate(0 -1.25)">
      <path d="M2.335 3.243h.753"/>
      <path d="M2.335.803C1.48.803.79 1.348.79 2.023c0 .674.69 1.22 1.544 1.22" stroke-linecap="round"/>
    </g>
    <g transform="translate(0 1.25)">
      <path d="M2.335 3.243h.753"/>
      <path d="M2.335.803C1.48.803.79 1.348.79 2.023c0 .674.69 1.22 1.544 1.22" stroke-linecap="round"/>
    </g>
  </g>
   -}
               Svg.g [ Svg.fill "none", Svg.stroke "#000", Svg.strokeWidth ".498", Svg.strokeMiterlimit "10" ]
                  [ Svg.g [ Svg.transform "translate(0 -1.25)" ]
                     [ Svg.path [ Svg.d "M2.335 3.243h.753" ] []
                     , Svg.path [ Svg.d "M2.335.803C1.48.803.79 1.348.79 2.023c0 .674.69 1.22 1.544 1.22", Svg.strokeLinecap "round" ] []
                     ]
                  , Svg.g [ Svg.transform "translate(0 1.25)" ]
                     [ Svg.path [ Svg.d "M2.335 3.243h.753" ] []
                     , Svg.path [ Svg.d "M2.335.803C1.48.803.79 1.348.79 2.023c0 .674.69 1.22 1.544 1.22", Svg.strokeLinecap "round" ] []
                     ]
                  ]
         (False, HookAlt) ->
         {-
          <g fill="none" stroke="#000" stroke-width=".498" stroke-miterlimit="10" transform="translate(0 6.483) scale(1 -1)">
    <path d="M2.335 3.243h.753"/>
    <path d="M2.335.803C1.48.803.79 1.348.79 2.023c0 .674.69 1.22 1.544 1.22" stroke-linecap="round"/>
  </g>
         -}
               Svg.g [ Svg.fill "none", Svg.stroke "#000", Svg.strokeWidth ".498", Svg.strokeMiterlimit "10", Svg.transform "translate(0 6.483) scale(1 -1)" ]
                  [ Svg.path [ Svg.d "M2.335 3.243h.753" ] []
                  , Svg.path [ Svg.d "M2.335.803C1.48.803.79 1.348.79 2.023c0 .674.69 1.22 1.544 1.22", Svg.strokeLinecap "round" ] []
                  ]
         (True, HookAlt) ->
         {-
           <g fill="none" stroke="#000" stroke-width=".498" stroke-miterlimit="10" transform="translate(0 6.483) scale(1 -1)">
    <g transform="translate(0 -1.25)">
      <path d="M2.335 3.243h.753"/>
      <path d="M2.335.803C1.48.803.79 1.348.79 2.023c0 .674.69 1.22 1.544 1.22" stroke-linecap="round"/>
    </g>
    <g transform="translate(0 1.25)">
      <path d="M2.335 3.243h.753"/>
      <path d="M2.335.803C1.48.803.79 1.348.79 2.023c0 .674.69 1.22 1.544 1.22" stroke-linecap="round"/>
    </g>
  </g>
         -}
               Svg.g [ Svg.fill "none", Svg.stroke "#000", Svg.strokeWidth ".498", Svg.strokeMiterlimit "10", Svg.transform "translate(0 6.483) scale(1 -1)" ]
                  [ Svg.g [ Svg.transform "translate(0 -1.25)" ]
                     [ Svg.path [ Svg.d "M2.335 3.243h.753" ] []
                     , Svg.path [ Svg.d "M2.335.803C1.48.803.79 1.348.79 2.023c0 .674.69 1.22 1.544 1.22", Svg.strokeLinecap "round" ] []
                     ]
                  , Svg.g [ Svg.transform "translate(0 1.25)" ]
                     [ Svg.path [ Svg.d "M2.335 3.243h.753" ] []
                     , Svg.path [ Svg.d "M2.335.803C1.48.803.79 1.348.79 2.023c0 .674.69 1.22 1.544 1.22", Svg.strokeLinecap "round" ] []
                     ]
                  ]
         (False, Mapsto) ->
         {-
          <g fill="none" stroke="#000" stroke-width=".498" stroke-miterlimit="10">
    <path d="M1.71 3.243h1.38"/>
    <path d="M1.544 5.283V1.2" stroke-linecap="round"/>
  </g>
         -}
               Svg.g [ Svg.fill "none", Svg.stroke "#000", Svg.strokeWidth ".498", Svg.strokeMiterlimit "10" ]
                  [ Svg.path [ Svg.d "M1.71 3.243h1.38" ] []
                  , Svg.path [ Svg.d "M1.544 5.283V1.2", Svg.strokeLinecap "round" ] []
                  ]
         (True, Mapsto) ->
         {-
           <g fill="none" stroke="#000" stroke-width=".498" stroke-miterlimit="10">
    <path d="M1.71 3.243h1.38" transform="translate(0 -1.25)" />
    <path d="M1.71 3.243h1.38" transform="translate(0 1.25)" />
    <path d="M1.544 5.783 V 0.7" stroke-linecap="round"/>
  </g>
         -}
               Svg.g [ Svg.fill "none", Svg.stroke "#000", Svg.strokeWidth ".498", Svg.strokeMiterlimit "10" ]
                  [ Svg.path [ Svg.d "M1.71 3.243h1.38", Svg.transform "translate(0 -1.25)" ] []
                  , Svg.path [ Svg.d "M1.71 3.243h1.38", Svg.transform "translate(0 1.25)" ] []
                  , Svg.path [ Svg.d "M1.544 5.783 V 0.7", Svg.strokeLinecap "round" ] []
                  ]
         (_, _) -> Svg.g [] []