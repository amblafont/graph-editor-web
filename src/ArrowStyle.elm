module ArrowStyle exposing (ArrowStyle, empty, {- keyUpdateStyle, -} quiverStyle,
   tikzStyle,
   isDouble, doubleSize, updateEdgeColor, EdgePart(..),
   controlChars, MarkerStyle, isMarker,
   kindCodec, tailCodec, headCodec, alignmentCodec, markerCodec,
   toggleDashed, dashedStr, -- PosLabel(..),
   -- quiver
    keyMaybeUpdateStyle, shadow,
    increaseBend, decreaseBend,
    keyMaybeUpdateColor, isPartColorable,
    -- keyMaybeUpdateHeadColor, keyMaybeUpdateTailColor,
    makeHeadShape, 
    makeTailShape, getStyle, isNone, simpleLineStyle
    , invert)

import HtmlDefs exposing (Key(..))


import Drawing.Color as Color exposing (Color)
import Geometry.Epsilon exposing (norm0, epsilon)
import Geometry exposing (LabelAlignment(..))
import Json.Encode as JEncode
import List.Extra as List
import ListExtraExtra exposing (nextInList)
import String.Svg as Svg exposing (Svg)
import Codec exposing (Codec)

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
                     color : Color,
                     headColor : Color,
                     tailColor : Color,
                     marker : MarkerStyle,
                     wavy : Bool
                    } 

simpleLineStyle : Float -> Style
simpleLineStyle bend = { tail = DefaultTail, head = NoHead, kind = NormalArrow, dashed = False,
          bend = bend, labelAlignment = Left, marker = noMarker,
          labelPosition = 0.5, color = Color.black,
          headColor = Color.black, tailColor = Color.black, wavy = False }
type alias ArrowStyle = Style
type ArrowKind = NormalArrow | NoneArrow | DoubleArrow

kindCodec : Codec ArrowKind String
kindCodec = 
  let split none double normal v =
        case v of 
            NoneArrow -> none
            DoubleArrow -> double
            NormalArrow -> normal
  in
  Codec.customEnum split
  |> Codec.variant0 "none" NoneArrow
  |> Codec.variant0 "double" DoubleArrow
  |> Codec.variant0 "normal" NormalArrow
  |> Codec.buildVariant


tailCodec : Codec TailStyle String
tailCodec = 
  let split hook hookalt mapsto default v =
          case v of 
            Hook -> hook 
            HookAlt -> hookalt 
            Mapsto -> mapsto
            DefaultTail -> default 
  in
  Codec.customEnum split
  |> Codec.variant0 "hook" Hook
  |> Codec.variant0 "hookalt" HookAlt
  |> Codec.variant0 "mapsto" Mapsto
  |> Codec.variant0 "none" DefaultTail
  |> Codec.buildVariant


headCodec : Codec HeadStyle String
headCodec =
   let split twoheads none default v =
          case v of 
            TwoHeads -> twoheads
            NoHead -> none
            DefaultHead -> default 
   in
   Codec.customEnum split 
   |> Codec.variant0 "twoheads" TwoHeads
   |> Codec.variant0 "none" NoHead
   |> Codec.variant0 "default" DefaultHead
   |> Codec.buildVariant

alignmentCodec : Codec LabelAlignment String
alignmentCodec = 
   let split centre over left right v =
          case v of 
            Centre -> centre
            Over -> over
            Left -> left
            Right -> right

   in
   Codec.customEnum split
   |> Codec.variant0 "centre" Centre
   |> Codec.variant0 "over" Over
   |> Codec.variant0 "left" Left
   |> Codec.variant0 "right" Right
   |> Codec.buildVariant

markerCodec : Codec MarkerStyle String
markerCodec = Codec.identity
  --  let split bullet bar nomarker v =
  --         case v of 
  --           BulletMarker -> bullet
  --           BarMarker -> bar
  --           NoMarker -> nomarker 
  --  in
  --  Codec.customEnum split 
  --  |> Codec.variant0 "\\bullet" BulletMarker
  --  |> Codec.variant0 "|" BarMarker
  --  |> Codec.variant0 "" NoMarker
  --  |> Codec.buildVariant
 
empty : Style
empty = { tail = DefaultTail, head = DefaultHead, dashed = False,
          bend = 0, labelAlignment = Left,
          labelPosition = 0.5, color = Color.black, kind = NormalArrow,
          marker = noMarker,
          headColor = Color.black, tailColor = Color.black ,
          wavy = False }
isDouble : Style -> Basics.Bool
isDouble { kind } = kind == DoubleArrow
  
isNone : Style -> Bool
isNone { kind } = kind == NoneArrow

getStyle : { a | style : Style, isAdjunction : Bool} -> Style
getStyle {style, isAdjunction} =
   if isAdjunction then
      { style | head = NoHead, tail = DefaultTail, kind = NoneArrow, labelAlignment = Over }
   else style

type alias MarkerStyle = String 
-- NoMarker | BulletMarker | BarMarker

isMarker : MarkerStyle -> Bool
isMarker marker = marker /= "" -- NoMarker

noMarker : MarkerStyle 
noMarker = ""


type HeadStyle = DefaultHead | TwoHeads | NoHead
type TailStyle = DefaultTail | Hook | HookAlt | Mapsto

-- toggleMarker : Style -> Style
-- toggleMarker s =  { s | marker = nextInList [NoMarker, BulletMarker, BarMarker] s.marker }



toggleWavy : Style -> Style
toggleWavy s = { s | wavy = not s.wavy }


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
controlChars = "|>(=-~bBA]["
maxLabelPosition = 0.9
minLabelPosition = 0.1

increaseBend : Float -> Float
increaseBend b = b + 0.1

decreaseBend : Float -> Float
decreaseBend b = b - 0.1

-- doesn't update the color
keyMaybeUpdateStyle : Key -> Style -> Maybe Style
keyMaybeUpdateStyle k style = 
   case k of 
        Character '|' -> Just <| toggleMapsto style
        Character '>' -> Just <| toggleHead style
        Character '(' -> Just <| toggleHook style
        Character '=' -> Just <| toggleDouble style
        Character '-' -> Just <| toggleDashed style
        Character '~' -> Just <| toggleWavy style
        -- Character '.' -> Just <| toggleMarker style
        Character 'b' -> Just <| {style | bend = decreaseBend style.bend |> norm0}
        Character 'B' -> Just <| {style | bend = increaseBend style.bend |> norm0}
        Character 'A' -> Just <| toggleLabelAlignement style
        Character ']' -> if style.labelPosition + epsilon >= maxLabelPosition then Nothing else
               Just {style | labelPosition = style.labelPosition + 0.1 |> min maxLabelPosition}
        Character '[' -> 
               if style.labelPosition <= minLabelPosition + epsilon then Nothing else
               Just {style | labelPosition = style.labelPosition - 0.1 |> max minLabelPosition}
        _ -> Nothing

keyToNewColor : Color -> Key -> Maybe Color
keyToNewColor oldColor k =
   case k of 
      Character c ->
         -- let _ = Debug.log "char" c in 
         Color.fromChar c
         |> Maybe.andThen 
            (\ color -> if color == oldColor then Nothing else Just color)
                        -- Just (upd color))
      _ -> Nothing

keyMaybeUpdateColor : Key -> EdgePart -> Style -> Maybe Style
keyMaybeUpdateColor k p s = 
  keyToNewColor (getEdgeColor p s) k 
  |> Maybe.map (\ c -> updateEdgeColor p c s)


isPartColorable : EdgePart -> Style -> Bool
isPartColorable part s = 
  case part of
    HeadPart -> s.head /= NoHead
    TailPart -> s.tail /= DefaultTail
    MainEdgePart -> True


   

--keyUpdateStyle : Key -> Style -> Style
--keyUpdateStyle k style = keyMaybeUpdateStyle k style |> Maybe.withDefault style

type EdgePart =
      MainEdgePart
    | HeadPart
    | TailPart

getEdgeColor : EdgePart -> Style -> Color
getEdgeColor part s = 
  case part of
    HeadPart -> s.headColor
    TailPart -> s.tailColor
    MainEdgePart -> s.color

updateEdgeColor : EdgePart -> Color -> Style -> Style
updateEdgeColor part c s = 
  case part of    
    HeadPart -> { s | headColor = c }
    TailPart -> { s | tailColor = c }
    MainEdgePart ->
        { s | color = c,
          headColor = if s.headColor == s.color then c else s.headColor,
          tailColor = if s.tailColor == s.color then c else s.tailColor }
          

shadow : ArrowStyle -> ArrowStyle
shadow st = { st | color = Color.white, dashed = False, head = NoHead, tail = DefaultTail }

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

invert : ArrowStyle -> ArrowStyle
invert st = { st | labelAlignment = case st.labelAlignment of
                                Left -> Right
                                Right -> Left
                                _ -> st.labelAlignment
                  , bend = 0 - st.bend
                  , labelPosition = 1 - st.labelPosition
             }

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
    Color.toString stl.color ++ "," ++
      (case (stl.head, stl.kind) of
            (NoHead, DoubleArrow) -> "identity,"
            (hd, DoubleArrow) -> (headTikzStyle hd) ++ "cell=0.05, "
            (hd, NormalArrow) -> (headTikzStyle hd)
            (hd, NoneArrow) -> "draw=none, "
       )
    ++ (if stl.dashed then "dashed, " else "")
    ++ (if stl.bend /= 0 then
           "curve={ratio=" ++ String.fromFloat stl.bend ++ "}, "
        else "")
    ++ (if stl.wavy then "decorate,decoration={zigzag, pre length=3px, post length=3px,amplitude=0.05cm, segment length=0.15cm}, " 
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
   let strokeAttr = Svg.strokeFromColor style.headColor in
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
           Svg.g [ Svg.fill "none", strokeAttr, Svg.strokeWidth ".498", Svg.strokeMiterlimit "10" ]
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
             Svg.g [ Svg.fill "none", strokeAttr, Svg.strokeWidth ".498", Svg.strokeMiterlimit "10" ]
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
             Svg.g [ Svg.fill "none", strokeAttr, Svg.strokeWidth ".498", Svg.strokeMiterlimit "10", Svg.strokeLinecap "round", Svg.strokeLinejoin "round" ]
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
               Svg.g [ Svg.fill "none", strokeAttr, Svg.strokeWidth ".498", Svg.strokeMiterlimit "10" ]
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
     let strokeAttr = Svg.strokeFromColor style.tailColor in
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
             Svg.g [ Svg.fill "none", strokeAttr, Svg.strokeWidth ".498", Svg.strokeMiterlimit "10" ]
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
               Svg.g [ Svg.fill "none", strokeAttr, Svg.strokeWidth ".498", Svg.strokeMiterlimit "10" ]
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
               Svg.g [ Svg.fill "none", strokeAttr, Svg.strokeWidth ".498", Svg.strokeMiterlimit "10", Svg.transform "translate(0 6.483) scale(1 -1)" ]
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
               Svg.g [ Svg.fill "none", strokeAttr, Svg.strokeWidth ".498", Svg.strokeMiterlimit "10", Svg.transform "translate(0 6.483) scale(1 -1)" ]
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
               Svg.g [ Svg.fill "none", strokeAttr, Svg.strokeWidth ".498", Svg.strokeMiterlimit "10" ]
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
               Svg.g [ Svg.fill "none", strokeAttr, Svg.strokeWidth ".498", Svg.strokeMiterlimit "10" ]
                  [ Svg.path [ Svg.d "M1.71 3.243h1.38", Svg.transform "translate(0 -1.25)" ] []
                  , Svg.path [ Svg.d "M1.71 3.243h1.38", Svg.transform "translate(0 1.25)" ] []
                  , Svg.path [ Svg.d "M1.544 5.783 V 0.7", Svg.strokeLinecap "round" ] []
                  ]
         (_, _) -> Svg.g [] []