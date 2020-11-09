module Drawing exposing (Drawing, arrowDrawing, DrawingDims, 
  fromString, circle, base, html, group,
  Attribute, on, onClick, onMouseEnter, onMouseLeave, color,
  svg
  )

import Collage
import Collage.Layout
import Collage.Text
import Collage.Events
import Collage.Render
import Color
import Point exposing (Point)
import Html
import Json.Decode as D


svg : List (Html.Attribute a) -> Drawing a -> Html.Html a
svg l d =
  d |> drawingToCollage |> Collage.Render.svgExplicit l


type Attribute msg =
    On String (D.Decoder msg)
    | Color Color.Color

attrColor : Attribute msg -> Maybe Color.Color
attrColor a = case a of
   Color c -> Just c
   _ -> Nothing

attrsColor : List (Attribute msg) -> Maybe Color.Color
attrsColor l =  List.filterMap attrColor l
    |> List.head 

applyEvent : Attribute msg -> Collage.Collage msg -> Collage.Collage msg
applyEvent a =
   case a of
      On s d -> Collage.Events.on s d
      _ -> identity

attrFillStyle : List (Attribute msg) -> Collage.FillStyle
attrFillStyle l =
    attrsColor l
    |> Maybe.map Collage.uniform
    |> Maybe.withDefault Collage.transparent

      
applyColorText : Attribute msg -> Collage.Text.Text -> Collage.Text.Text
applyColorText a =
   case a of
      Color c -> Collage.Text.color c
      _ -> identity
      

on : String -> D.Decoder msg -> Attribute msg
on = On

simpleOn : String -> msg -> Attribute msg
simpleOn s m = on s (D.succeed m)

onClick : msg -> Attribute msg
onClick = simpleOn "click" 

onMouseEnter : msg -> Attribute msg
onMouseEnter = simpleOn "mouseenter" 

onMouseLeave : msg -> Attribute msg
onMouseLeave = simpleOn "mouseleave" 

-- onCreate : msg -> Attribute msg
-- onCreate = simpleOn "create" 

-- onRemove : msg -> Attribute msg
-- onRemove = simpleOn "remove" 



color : Color.Color -> Attribute msg
color = Color

type Drawing a
    = Drawing (Collage.Collage a)


drawingToCollage : Drawing a -> Collage.Collage a
drawingToCollage d = case d of 
    Drawing c -> c


type alias DrawingDims msg =
    { drawing : Drawing msg
    , dims : Maybe Point
    }




-- specific to Collage


arrow : List (Attribute a) -> Point -> Point -> Drawing a
arrow attrs from to =
    let
        c = attrsColor attrs |> Maybe.withDefault Color.black
        delta = Point.subtract to from

        -- pos = to
        offset = 15

        offsetP = Point.normalise offset delta

        pos = Point.subtract to offsetP

        fromOffset = Point.add from offsetP
    in
    Collage.group
        [ Collage.triangle 10
            |> Collage.filled (Collage.uniform c)
            |> Collage.rotate (Point.pointToAngle <| Point.flip <| Point.orthogonal delta)
            |> Collage.shift pos
        , Collage.segment fromOffset pos
            |> Collage.traced (Collage.solid Collage.thin (Collage.uniform c))
        ]
        |> fold applyEvent attrs
        |> Drawing


-- arrow between two collages
-- with their dimensions, if they are available
arrowDrawing : List (Attribute a) ->
               DrawingDims a ->
               DrawingDims a ->
               Drawing a
arrowDrawing attrs from to =
    let pfrom = Collage.Layout.base (drawingToCollage from.drawing)
        pto = Collage.Layout.base (drawingToCollage to.drawing)
    in
    let (end, start) = case (Point.raytraceRect pfrom pto (drawingToRect to),
                             Point.raytraceRect pto pfrom (drawingToRect from)) of
            (Just e, Just s) -> (e, s)
            _ -> (pto, pfrom)
    in
        arrow attrs start end





drawingToRect : DrawingDims a -> (Point, Point)
drawingToRect dr =
    let c = drawingToCollage dr.drawing in
    case dr.dims of
        Just dims ->
            let pc = Collage.Layout.base c
                d = Point.resize 0.5 dims
            in
                (Point.subtract pc d, Point.add pc d)
        Nothing ->
            (Collage.Layout.bottomLeft c,
                 Collage.Layout.topRight c)



fold : (a -> b -> b) -> List a -> b -> b 
fold f l a = List.foldl f a l



fromString : List (Attribute msg) -> Point -> String-> Drawing msg
fromString attrs pos str = 
   
  let text = Collage.Text.fromString str in
  text |> fold applyColorText attrs
       |> Collage.rendered
       |> Collage.shift pos
       |> fold applyEvent attrs
       |> Drawing

circle : List (Attribute msg) ->  Point -> Float -> Drawing msg
circle attrs pos n = Collage.circle n 
    |> Collage.filled (attrFillStyle attrs)
    |> fold applyEvent attrs
    |> Collage.shift pos |> Drawing

base : Drawing a -> Point
base d = d |> drawingToCollage |> Collage.Layout.base

html : Point -> Point -> Html.Html a -> Drawing a
html pos dim h = Collage.html dim h |> Collage.shift pos |> Drawing

group : List (Drawing a) -> Drawing a
group l =
  Collage.group (List.map drawingToCollage l) |> Drawing