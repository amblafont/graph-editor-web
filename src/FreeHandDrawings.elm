module FreeHandDrawings exposing (codec, drawingToJS, drawingFromJS, Drawing, DrawingId, DrawingJS, Drawings, DrawingsJS,
  getDrawings, get, add, remove, empty, emptyDrawingJS, draw)

import Geometry.Point as Point exposing (Point)
import IntDict exposing (IntDict)
import IntDictExtra
import Codec
import Html
import Drawing.Color as Color
import Drawing exposing (Drawing)

import Base64

type alias Drawing = List Point
type alias DrawingId = Int

type alias DrawingJS = 
 { start : Point,
 -- Base64 encoded stream of offsets (x,y)
   offsets : String 
 }


type alias Drawings =  
 { nextFreehandId : DrawingId, 
   freehandDrawings : IntDict Drawing }


type alias DrawingsJS = List DrawingJS

emptyDrawingJS : DrawingJS
emptyDrawingJS = { start = (0,0), offsets = "" }

getDrawings : Drawings -> IntDict Drawing
getDrawings {freehandDrawings} = freehandDrawings

empty : Drawings
empty = 
  { nextFreehandId = 0, freehandDrawings = IntDict.empty }

add : Drawings -> Drawing -> Drawings
add fhd points =
  { freehandDrawings = IntDict.insert fhd.nextFreehandId points fhd.freehandDrawings, 
     nextFreehandId = 1 + fhd.nextFreehandId }

remove : Drawings -> DrawingId -> Drawings
remove fhd id =
  { fhd | freehandDrawings = IntDict.remove id fhd.freehandDrawings }

get : Drawings -> DrawingId -> Maybe Drawing
get fhd id =
  IntDict.get id fhd.freehandDrawings

debug_chars : String -> List Char -> List Char
debug_chars s chars = 
--    let _ = Debug.log s (List.map Char.toCode chars) in
      chars



drawingToJS : Drawing -> DrawingJS
drawingToJS allPoints = 
--   case Debug.log "toJS" allPoints of
  case allPoints of
            [] -> { start = (0,0), offsets = ""}
            start :: points ->
               let chars = pointsToOffsets points 
                     { current = start, points = []} 
               in
               { start = start, offsets = 
                Maybe.withDefault "" 
                -- <| Debug.log "base64 toJS"
                <| Base64.fromString 
                <| String.fromList 
                -- <| debug_chars "chars toJS"
                <| List.map (Codec.decoder Codec.charInt)
                -- <| Debug.log "offsets toJS"
                <| chars
                }

drawingFromJS : DrawingJS -> Drawing
drawingFromJS drawing = 
    let chars =  (String.toList <| Maybe.withDefault "" <| Base64.toString 
            --  <| Debug.log "base64 fromJS" 
             drawing.offsets)
    in
    let _ = debug_chars "chars fromJS" chars in
          offsetsToPoint 
          (List.map (Codec.encoder Codec.charInt) chars)
          [drawing.start]
        --   |> Debug.log "fromJS"

-- terminal recursive
pointsToOffsets : List Point -> { current : Point, points : List Int} -> List Int
pointsToOffsets points acc = 
      case points of
        [] -> List.reverse acc.points
        p :: q -> 
            let (offx, offy) = Point.subtract p acc.current in
            let (offxn, offyn) = (truncate offx, truncate offy) in
            let offset = (toFloat offxn, toFloat offyn) in 
            pointsToOffsets q { acc | current = Point.add acc.current offset 
                , points = offxn :: offyn :: acc.points}
       

offsetsToPoint : List Int -> List Point -> List Point
offsetsToPoint chars points = 
  case (chars, points) of 
     (y :: x :: tail, current :: _)  -> 
        let offset = (toFloat <| x, toFloat <| y) in
        let p = Point.add current offset in
        offsetsToPoint tail <| p :: points
     (_, _)-> List.reverse points


-- codecDrawing : Codec.Codec Drawing DrawingJS
-- codecDrawing =
--    Codec.build       
--       drawingToJS
--       drawingFromJS

codec : Codec.Codec Drawings DrawingsJS
codec =
   Codec.build 
      (\ {freehandDrawings} -> 
         IntDict.values freehandDrawings
         |> List.map drawingToJS
      )
      (\ listDrawings -> 
      { freehandDrawings = 
         listDrawings |>
         List.map drawingFromJS
         |> IntDictExtra.fromBareList ,
        nextFreehandId = List.length listDrawings }
      )

draw : (DrawingId -> List (Html.Attribute a) ) -> Drawings -> Drawing.Drawing a
draw attrs drawings = 
    List.map (\(id,points) -> Drawing.singlePolyLine { color = Color.black
                                 , points = points } 
                                 (attrs id)   )
                (IntDict.toList <| getDrawings drawings)
        |> Drawing.group