module Format.Flags exposing (..)

import Drawing.Color exposing (Color)
import ArrowStyle exposing (TailStyle(..), HeadStyle(..), ArrowKind(..))
import Geometry exposing (LabelAlignment(..))
import Polygraph as Graph

type alias PullshoutOffsets = { offset1 : Float, offset2 : Float }

type EdgeFlag = 
      Dashed
    | Wavy
    | Kind ArrowKind
    | HeadStyle HeadStyle
    | TailStyle TailStyle
    | Alignment LabelAlignment
    | Pullshout PullshoutOffsets
    | Adjunction
    | Unrecognized
    | Color Color
    | TailColor Color
    | HeadColor Color
    | LabelColor Color
    | Marker String
    | Bend Float
    | Position Float
    | ShiftSource Float
    | ShiftTarget Float
    | LoopRadius Float
    | LoopAngle Float
    | ShortenHead Float
    | ShortenTail Float
    | Dependency


type NodeFlag = 
      CoqValidated
    | Text
    | Dependencies DepsArg
    | NodeColor Color
    | UnrecognizedNodeFlag

type alias DepsArg = List (String, Graph.Id)