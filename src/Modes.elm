module Modes exposing (..)

import ArrowStyle exposing (ArrowStyle, EdgePart)
import Geometry.Point exposing (Point)
import Polygraph as Graph exposing (EdgeId, NodeId)
import InputPosition exposing (InputPosition)
import GraphDefs exposing (NodeLabel, EdgeLabel)
import Format.GraphInfo as GraphInfo exposing (TabId)
import Msg exposing (ModifId, MoveMode, ModeReturn(..))
import Drawing.Color as Color



type Mode
    = DefaultMode
    | NewArrow NewArrowState
    | NewLine NewLineState
    | Move MoveState
    | RenameMode RenameState
    | DebugMode
    | SquareMode SquareState
    | RectSelect SelectState
    | SplitArrow SplitArrowState
    {-
    We should enlarge vertices below right the origin
    and vertices on the right and bottom that are connected to these
    -}
    | EnlargeMode EnlargeState
    | CutHead CutHeadState
    | ResizeMode ResizeState
    | PullshoutMode PullshoutState
    | CustomizeMode CustomizeModeState
    | BendMode BendState
    | MakeSaveMode
    | LatexPreamble String
type alias BendState =
  { edge : Graph.Edge EdgeLabel
  , componentState : BendComponentState
  }

-- only the bend component is going to change
type alias BendComponentState = 
    { captureState : CaptureState, origBend : Float }
type alias CaptureState = {direction : Point, value : Float, 
     bounds : Maybe (Float, Float)}

toString : Mode -> String
toString m = case m of
  DefaultMode -> "Default"
  NewArrow _ -> "New arrow"
  NewLine _ -> "New line"
  Move _ -> "Move"
  RenameMode _ -> "Rename"
  DebugMode -> "Debug"
  LatexPreamble _ -> "Latex preamble"
  SquareMode _ -> "Square"
  RectSelect _ -> "Rect select"
  SplitArrow _ -> "Split arrow"
  EnlargeMode _ -> "Enlarge"
  CutHead _ -> "Cut head"
  ResizeMode _ -> "Resize"
  PullshoutMode _ -> "Pullshout"
  CustomizeMode _ -> "Color"
  BendMode _ -> "Bend"
  MakeSaveMode -> "MakeSave"

type alias CutHeadState = { edge: Graph.Edge EdgeLabel
    , head : Bool -- the head or the tail?
    , duplicate : Bool -- duplicate the arrow? 
    -- , merge : Bool
    }

isResizeMode : Mode -> Bool
isResizeMode m = case m of 
   ResizeMode _ -> True
   _ -> False

type alias SelectState =
 { orig : Point, 
   hold : Bool}

type alias ResizeState = 
   { sizeGrid : Int,
     onlyGrid : Bool
   }
type alias RenameState = 
   { next:List { id : Graph.Id, tabId : TabId, label : String},
     idModif : ModifId,
     -- the input text is selected when first editing
     -- but on rerendering the input, ti should not select again
     alreadySelected : Bool }
        {- { next : List { id : Graph.Id, label : Maybe String, tabId : GraphInfo.TabId } 
        , idModif : ModifId
        } -}


type alias MoveState = 
   {   pos : InputPosition
      -- should we save at the end
     , idModif : ModifId
     , mode : MoveMode
     , direction : MoveDirection
    --  , merge : Bool
  }
type MoveDirection =
  Free | Vertical | Horizontal

    

type alias EnlargeState = 
   { orig : Point, -- mouse original point at the beginning of the move mode
     -- onlySubdiag : Bool,
     pos : InputPosition
   , direction : MoveDirection
   }

type alias SplitArrowState =
    { chosenEdge : EdgeId
    , source : Graph.Id
    , target : Graph.Id
    , pos : InputPosition
    , label : GraphDefs.GenericEdge GraphDefs.NormalEdgeLabel -- original edge label
    , labelOnSource : Bool -- shall we transfer the existing label from the source
    , guessPos : Bool -- do we try guessing the position of the new node
    }


type alias PullshoutState =
    { chosenEdge : EdgeId
    -- , source : Graph.Id
    -- , target : Graph.Id
    , color : Color.Color
    , kind : PullshoutKind
    , currentDest : EdgeId
    , possibilities : List EdgeId
    , offset1 : Float
    , offset2 : Float
    }

type PullshoutKind = Pullback | Pushout

type ArrowStateKind =
    CreateArrow Graph.Id
  | CreateCylinder
  | CreateCone


type alias CustomizeModeState = 
   { edges: (List (Graph.Edge EdgeLabel)),
      mode : CustomizeMode
      -- the style of the singleton edge being selected
      -- we can change its bend/shift
      -- style : Maybe ArrowStyle
    }

type alias ShiftComponentState = CaptureState
    -- { captureState : CaptureState, origShift : Float }
type alias CustomizeModeShiftState =
    { isSource : Bool
    , componentState : ShiftComponentState
    -- , edge : Graph.Edge EdgeLabel
    }
type CustomizeMode =
    CustomizeModePart EdgePart
                       -- true if source, false if target
  | CustomizeModeShift CustomizeModeShiftState
  

type alias NewLineState = {
    initialPos : Point,
    bend : Float
    }

type NewArrowMode = 
    NewArrowPart EdgePart
  | NewArrowBend BendComponentState
  | NewArrowShift Bool ShiftComponentState

type alias NewArrowState =
    { chosen : Graph.Graph NodeLabel EdgeLabel,
      kind : ArrowStateKind, 
      mode : NewArrowMode,
      style : ArrowStyle,
      pos : InputPosition, inverted : Bool,
      isAdjunction : Bool,
      merge : Bool
       }


type alias SquareState =
    { chosenNode : NodeId
    , chosenLabel : String
    , n1 : NodeId
    , n1Label : String
    , n1ToChosen : Bool
    , e1 : Graph.Edge (GraphDefs.GenericEdge GraphDefs.NormalEdgeLabel)
    , n2 : NodeId
    , n2Label : String
    , n2ToChosen : Bool
    , e2 : Graph.Edge (GraphDefs.GenericEdge GraphDefs.NormalEdgeLabel)

    -- next possibility of square to be tested (to determine the orientation)
    , configuration : Int
    -- next possibility of labels to be tested
    , labelConfiguration : Int

    -- should it guess the position of the new node
    , guessPos : Bool
    }
