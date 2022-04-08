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
    | EnlargeMode Point
    | CutHead CutHeadState
    | CloneMode
    | ResizeMode ResizeState -- current sizegrid

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

type alias SplitArrowState =
    { chosenEdge : EdgeId
    , source : Graph.Id
    , target : Graph.Id
    , pos : InputPosition
    , label : GraphDefs.EdgeLabel -- original edge label
    , labelOnSource : Bool -- shall we transfer the existing label from the source
    }





type alias NewArrowState =
    { chosenNode : NodeId, style : ArrowStyle, pos : InputPosition, inverted : Bool }


type alias SquareState =
    { chosenNode : NodeId
    , chosenLabel : String
    , n1 : NodeId
    , n1Label : String
    , n1ToChosen : Bool
    , e1 : Graph.Edge GraphDefs.EdgeLabel
    , n2 : NodeId
    , n2Label : String
    , n2ToChosen : Bool
    , e2 : Graph.Edge GraphDefs.EdgeLabel

    -- next possibility of square to be tested (to determine the orientation)
    , configuration : Int
    -- next possibility of labels to be tested
    , labelConfiguration : Int
    }
