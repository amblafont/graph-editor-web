module Modes exposing (..)

import ArrowStyle exposing (ArrowStyle)
import Geometry.Point exposing (Point)
import Polygraph as Graph exposing (EdgeId, NodeId)
import QuickInput
import InputPosition exposing (InputPosition)
import GraphDefs


type Mode
    = DefaultMode
    | NewArrow NewArrowState
    | Move MoveState
      -- the list of ids to be edited, with associated default labels 
      -- (which may differ from the labels of the objects in the model)
    | RenameMode (List (Graph.Id, String))
    | DebugMode    
    | QuickInputMode (Maybe QuickInput.Equation)
    | SquareMode SquareState
    | RectSelect Point
    -- Bool -- keep previous selection?
      -- | SplitArrow EdgeId
    | SplitArrow SplitArrowState
    {-
    We should enlarge vertices below right the origin
    and vertices on the right and bottom that are connected to these
    -}
    | EnlargeMode EnlargeState
    | CutHead CutHeadState
    | CloneMode
    | ResizeMode ResizeState -- current sizegrid
    | PullbackMode PullbackState

type alias CutHeadState = { id: Graph.EdgeId
    , head : Bool -- the head or the tail?
    , duplicate : Bool -- duplicate the arrow? 
    }

isResizeMode : Mode -> Bool
isResizeMode m = case m of 
   ResizeMode _ -> True
   _ -> False

type alias ResizeState = 
   { sizeGrid : Int,
     onlyGrid : Bool
   }

type alias MoveState = 
   { orig : Point,  -- mouse original point at the beginning of the move mode
     -- this was used to compute the move relative to this point, but this
     -- is no longer used
      pos : InputPosition
      -- , merge : Bool 
      }

type alias EnlargeState = 
   { orig : Point, -- mouse original point at the beginning of the move mode
     -- onlySubdiag : Bool,
     pos : InputPosition
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


type alias PullbackState =
    { chosenEdge : EdgeId
    , source : Graph.Id
    , target : Graph.Id
    , kind : PullbackKind
    , currentDest : EdgeId
    , possibilities : List EdgeId
    }

type PullbackKind = Pullback | Pushout




type alias NewArrowState =
    { chosenNode : NodeId, style : ArrowStyle, pos : InputPosition, inverted : Bool }


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
