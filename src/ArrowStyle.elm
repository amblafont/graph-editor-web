module ArrowStyle exposing
    ( ArrowStyle
    ,  -- PosLabel(..),
       -- quiver
       LabelAlignment(..)

    , alignmentFromString
    , alignmentToString
    , controlChars
    , dashedStr
    , doubleSize
    , empty
    , headFromString
    , headToString
    , isDouble
    , keyMaybeUpdateStyle
    , makeHeadTailImgs
    , {- keyUpdateStyle, -} quiverStyle
    , tailFromString
    , tailToString
    , tikzStyle
    , toggleDashed
    )

import Geometry.Epsilon exposing (norm0)
import Geometry.Point as Point exposing (Point)
import Geometry.QuadraticBezier exposing (QuadraticBezier)
import HtmlDefs exposing (Key(..))
import Json.Encode as JEncode
import List.Extra as List
import ListExtraExtra exposing (nextInList)
import Svg exposing (Svg)
import Svg.Attributes as Svg


imgDir : String
imgDir =
    "img/arrow/"



-- following the tikzcd-editor


dashedStr : String
dashedStr =
    "7, 3"



-- from GridCellArrow in tikz-cd editor


imgWidth : Float
imgWidth =
    9.764


imgHeight : Float
imgHeight =
    13


doubleSize =
    2.5



{- }
   type PosLabel =
        DefaultPosLabel
      | LeftPosLabel
      | RightPosLabel
-}


type alias Style =
    { tail : TailStyle
    , head : HeadStyle
    , double : Bool
    , dashed : Bool
    , bend : Float
    , labelAlignment : LabelAlignment
    , -- betweeon 0 and 1, 0.5 by default
      labelPosition : Float
    }


type alias ArrowStyle =
    Style


tailToString : TailStyle -> String
tailToString tail =
    case tail of
        DefaultTail ->
            "none"

        Hook ->
            "hook"

        HookAlt ->
            "hookalt"


tailFromString : String -> TailStyle
tailFromString tail =
    case tail of
        "hook" ->
            Hook

        "hookalt" ->
            HookAlt

        _ ->
            DefaultTail


headToString : HeadStyle -> String
headToString head =
    case head of
        DefaultHead ->
            "default"

        TwoHeads ->
            "twoheads"

        NoHead ->
            "none"


headFromString : String -> HeadStyle
headFromString head =
    case head of
        "twoheads" ->
            TwoHeads

        "none" ->
            NoHead

        _ ->
            DefaultHead


alignmentToString : LabelAlignment -> String
alignmentToString tail =
    case tail of
        Centre ->
            "centre"

        Over ->
            "over"

        Left ->
            "left"

        Right ->
            "right"


alignmentFromString : String -> LabelAlignment
alignmentFromString tail =
    case tail of
        "centre" ->
            Centre

        "right" ->
            Right

        "over" ->
            Over

        _ ->
            Left


empty : Style
empty =
    { tail = DefaultTail
    , head = DefaultHead
    , double = False
    , dashed = False
    , bend = 0
    , labelAlignment = Left
    , labelPosition = 0.5
    }


isDouble : Style -> Basics.Bool
isDouble { double } =
    double


type HeadStyle
    = DefaultHead
    | TwoHeads
    | NoHead


type TailStyle
    = DefaultTail
    | Hook
    | HookAlt


toggleHead : Style -> Style
toggleHead s =
    { s | head = nextInList [ DefaultHead, NoHead, TwoHeads ] s.head }


toggleHook : Style -> Style
toggleHook s =
    { s | tail = nextInList [ DefaultTail, Hook, HookAlt ] s.tail }


toggleLabelAlignement : Style -> Style
toggleLabelAlignement s =
    { s
        | labelAlignment =
            nextInList [ Left, Right ]
                -- , Centre, Over]
                -- the other ones do not seem to work properly
                s.labelAlignment
    }


toggleDouble : Style -> Style
toggleDouble s =
    { s | double = not s.double }


toggleDashed : Style -> Style
toggleDashed s =
    { s | dashed = not s.dashed }


prefixDouble : Style -> String
prefixDouble { double } =
    if double then
        "double-"

    else
        ""


headFileName : Style -> String
headFileName s =
    prefixDouble s
        ++ headToString s.head
        ++ ".svg"


tailFileName : Style -> String
tailFileName s =
    prefixDouble s
        ++ tailToString s.tail
        ++ ".svg"


svgRotate : Point -> Float -> Svg.Attribute a
svgRotate ( x2, y2 ) angle =
    Svg.transform <|
        " rotate("
            ++ String.fromFloat angle
            ++ " "
            ++ String.fromFloat x2
            ++ " "
            ++ String.fromFloat y2
            ++ ")"


makeImg : Point -> Float -> String -> Svg a
makeImg ( x, y ) angle file =
    let
        ( xh, yh ) =
            ( x - imgHeight / 2, y - imgHeight / 2 )
    in
    let
        f =
            String.fromFloat
    in
    Svg.image
        [ Svg.xlinkHref <| imgDir ++ file
        , Svg.x <| f xh
        , Svg.y <| f yh
        , Svg.width <| f imgWidth
        , Svg.height <| f imgHeight
        , svgRotate ( x, y ) angle
        ]
        []


makeHeadTailImgs : QuadraticBezier -> Style -> List (Svg a)
makeHeadTailImgs { from, to, controlPoint } style =
    let
        angle delta =
            Point.pointToAngle delta * 180 / pi
    in
    -- let mkImg = makeImg from to angle in
    [ makeImg to (angle <| Point.subtract to controlPoint) <|
        headFileName style
    , makeImg from (angle <| Point.subtract controlPoint from) <|
        tailFileName style
    ]


keyMaybeUpdateStyle : Key -> Style -> Maybe Style
keyMaybeUpdateStyle k style =
    case k of
        Character '>' ->
            Just <| toggleHead style

        Character '(' ->
            Just <| toggleHook style

        Character '=' ->
            Just <| toggleDouble style

        Character '-' ->
            Just <| toggleDashed style

        Character 'b' ->
            Just <| { style | bend = style.bend + 0.1 |> norm0 }

        Character 'B' ->
            Just <| { style | bend = style.bend - 0.1 |> norm0 }

        Character 'A' ->
            Just <| toggleLabelAlignement style

        Character ']' ->
            Just <| { style | labelPosition = style.labelPosition + 0.1 |> min 0.9 }

        Character '[' ->
            Just <| { style | labelPosition = style.labelPosition - 0.1 |> max 0.1 }

        _ ->
            Nothing



--keyUpdateStyle : Key -> Style -> Style
--keyUpdateStyle k style = keyMaybeUpdateStyle k style |> Maybe.withDefault style
-- chars used to control in keyUpdateStyle


controlChars =
    ">(=-bBA]["


quiverStyle : ArrowStyle -> List ( String, JEncode.Value )
quiverStyle st =
    let
        { tail, head, double, dashed } =
            st
    in
    let
        makeIf b x =
            if b then
                [ x ]

            else
                []
    in
    let
        headStyle =
            case head of
                DefaultHead ->
                    []

                TwoHeads ->
                    [ ( "head", [ ( "name", "epi" ) ] ) ]

                NoHead ->
                    [ ( "head", [ ( "name", "none" ) ] ) ]
    in
    let
        tailStyle =
            case tail of
                DefaultTail ->
                    []

                Hook ->
                    [ ( "tail", [ ( "name", "hook" ), ( "side", "top" ) ] ) ]

                HookAlt ->
                    [ ( "tail", [ ( "name", "hook" ), ( "side", "bottom" ) ] ) ]
    in
    let
        style =
            List.map (\( x, y ) -> ( x, JEncode.object <| List.map (\( s, l ) -> ( s, JEncode.string l )) y )) <|
                headStyle
                    ++ tailStyle
                    ++ makeIf dashed ( "body", [ ( "name", "dashed" ) ] )
    in
    makeIf double ( "level", JEncode.int 2 )
        ++ [ ( "style", JEncode.object style ) ]
        ++ makeIf (st.bend /= 0) ( "curve", JEncode.int <| floor (st.bend * 10) )
        ++ makeIf (st.labelPosition /= 0.5) ( "label_position", JEncode.int <| floor (st.labelPosition * 100) )



-- from Quiver


type LabelAlignment
    = Centre
    | Over
    | Left
    | Right


tikzStyle : ArrowStyle -> String
tikzStyle stl =
    (case stl.tail of
         DefaultTail -> ""
         Hook -> "into, "
         HookAlt -> "linto, ")
    ++ (if stl.double then "cell=0, " else "")
    ++ (case stl.head of
            DefaultHead -> "->, "
            TwoHeads -> "onto, "
            NoHead -> "-,"
       )
    ++ (if stl.dashed then "dashed, " else "")
    ++ (let bnd = stl.bend * 180 / pi in
        if stl.bend /= 0 then
            "bend right={" ++ String.fromFloat bnd ++ "}, "
        else "")
