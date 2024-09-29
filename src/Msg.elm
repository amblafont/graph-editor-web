module Msg exposing (Msg(..), noOp, focusId, unfocusId,
  onTabPreventDefault, Scenario(..), LoadGraphInfo, mapLoadGraphInfo,
  isSimpleScenario, loadGraphInfoToMsg, Command(..), ModifId, defaultModifId,
  trueModifId, modifIdsEq, idModifCodec
  , ProtocolMsg(..), ProtocolModif,  MoveMode(..)
  -- , RenameCommand, CreatePointCommand
  )

import Geometry.Point exposing (Point)
-- import Graph exposing (Graph, NodeId)
-- import GraphExtra exposing (EdgeId)
import Polygraph as Graph exposing (EdgeId, NodeId, Graph)
import Format.GraphInfo as GraphInfo exposing (GraphInfo)

import HtmlDefs exposing (Key)
import Task
import Codec exposing (Codec)

import Browser.Dom as Dom
import ArrowStyle exposing (ArrowStyle)
import GraphDefs exposing (NodeLabel, EdgeLabel)
import Html.Events.Extra.Mouse as MouseEvents
import Html
import Format.GraphInfoCodec
import Format.GraphInfo exposing (Modif, TabId)
import Json.Encode as JE
import IntDict exposing (IntDict)

type MoveMode = 
    -- the move stops when we release the key
      PressMove
    -- the move stops when we click
    | FreeMove
    -- we don't know yet
    | UndefinedMove



-- type alias CreatePointCommand =  { isMath : Bool, tabId : TabId, pos: Point}

{-

modifs is the list of modifs done by the sender, but we stack them
because we want to undo them all at once.

When receiving a RenameCommand, the first thing to do is to apply the head of modifs.

Pb of this approach: if sending the command fails (e.g., the network 
is interrupted for some time), the modifs will never be incorporated in
the undo list
    -}
    -- mais non ca marche pas! what if le point n'est pas encore cree?
    {-
type alias RenameCommand = { 
              --  modifs : List GraphInfo.Modif
               -- even if we cancel a rename, the initail modifs will remain
            --  , initialModifs : List GraphInfo.Modif
            -- if the label is Nothing, then it is the one saved in the 
            -- graph
              next : List { id : Graph.Id, label : Maybe String, tabId : GraphInfo.TabId } }
              -}
-- type alias SquareCommand = {}

type ModifId =
    TrueModifId Int
  | DefaultModifId
defaultModifId : ModifId
defaultModifId = DefaultModifId

trueModifId : Int -> ModifId
trueModifId = TrueModifId


modifIdsEq : ModifId -> ModifId -> Bool
modifIdsEq x y =
   case (x,y) of 
     (TrueModifId a,TrueModifId b) -> a == b
     _ -> False

idModifCodec : Codec ModifId Int
idModifCodec =
   Codec.build (
    \ id -> case id of
               DefaultModifId -> -1
               TrueModifId n ->  n
   )
   (\ n -> if n < 0 then DefaultModifId else TrueModifId n)

type alias ProtocolModif = {id : ModifId, modif : Modif, 
          selIds : IntDict (List Graph.Id),
          command : Command  }

type ProtocolMsg = 
    ModifProtocol ProtocolModif
  | Undo (List Modif)
  | LoadProtocol { graph : GraphInfo, scenario : Scenario}
  | ClearProtocol {scenario : Scenario, preamble : String}
  | Snapshot GraphInfo
  -- to notify the position of the mouse
  | FocusPosition {tabId : TabId, pos : Point, selIds : List Graph.Id}

type Command = 
     RenameCommand (List {id : Graph.Id, label : Maybe String, tabId : TabId})
   | MoveCommand MoveMode -- (List Graph.Id)
  --  | LoadCommand { graph : GraphInfo, scenario : Scenario}
   | Noop


-- SimpleScenario: just display the model status message
type Scenario = Standard | Exercise1 | SimpleScenario | Watch | CoqLsp

isSimpleScenario : Scenario -> Bool
isSimpleScenario s = s == SimpleScenario



type alias LoadGraphInfo a = 
   { graph : a, -- fileName : String, 
   -- TODO: remove this
     scenario : String,
     clipboard : Bool, -- is it a paste event?
     setFirstTab : Bool -- set the active tab on the first tab
     -- (if true, the clipboard flag is ignored)
   }

mapLoadGraphInfo : (a -> b) -> LoadGraphInfo a -> LoadGraphInfo b 
mapLoadGraphInfo f { graph, scenario, clipboard, setFirstTab } =
   { graph = f graph, scenario = scenario,
     clipboard = clipboard, setFirstTab = setFirstTab }

-- the model automatically updates its record of HtmlDefs.Keys (shift,alt,ctrl status) in any case
-- when the message gives it, so there is a kind of redundancy on this matter
type Msg
  = -- call some js function
    Do (Cmd Msg)
  | Save
  | MakeSave
  | ExportQuiver
    -- on reception of this message, the js function onMouseMove is called
    -- which sends back a MouseMove message with the relative position to 
    -- the canvas
  | MouseMoveRaw JE.Value HtmlDefs.Keys
  | MouseLeaveCanvas
  | KeyChanged Bool HtmlDefs.Keys Key
  | MouseMove Point
  | MouseClick 
  | MouseDown MouseEvents.Event -- is Shift selected?
  | MouseUp
  -- | NodeEnter NodeId
  -- | NodeLeave NodeId
  | NodeClick NodeId MouseEvents.Event
  | EltDoubleClick Graph.Id MouseEvents.Event
  | EdgeClick EdgeId MouseEvents.Event
 -- | EltHover Graph.Id 
  | EdgeLabelEdit EdgeId String
  | NodeLabelEdit NodeId String
  | CopyGraph
  | CutGraph
  | Loaded { graph : GraphInfo, scenario: String }
  | SetFirstTab GraphInfo  
  -- a graph is pasted
  | PasteGraph GraphInfo
  | QuickInput String
  | SetFirstTabEquation String
  | NodeRendered NodeId Point
  | EdgeRendered EdgeId Point
  | MouseOn Graph.Id
  | Clear { scenario : Scenario, preamble : String}
  | SizeGrid Int
  | RulerMargin Int
  | ToggleHideGrid
  | ToggleHideRuler
  | ToggleAlternativeLatex
  | ToggleAutosave
  | SaveRulerGridSize
  | OptimalGridSize
  | SwitchTab Int
  | NewTab
  | DuplicateTab
  | RemoveTab
  | RenameTab String
  | TabMoveRight
  | TabMoveLeft
  | FindReplace { search: String, replace:String}
  | MinuteTick
  -- means that some key has been pressed
  -- for a long time
  | PressTimeout
  | LatexPreambleSwitch
  | LatexPreambleEdit String
  | SimpleMsg String
  | AppliedProof { statement : String, script : String}
  | ProtocolReceive (List {isSender : Bool, msg : ProtocolMsg})
  | ProtocolRequestSnapshot
  | RenderedTextInput
  -- | ProtocolReceiveSnapshot GraphInfo
  -- | ComputeLayout
  -- | FindInitial
  -- | EditBottomText String
  -- pressing tab when editing the input text
  -- | TabInput

loadGraphInfoToMsg : LoadGraphInfo GraphInfo -> Msg
loadGraphInfoToMsg g =
   if g.setFirstTab then
      SetFirstTab g.graph
   else if g.clipboard then
  --  Debug.log "coucou" <|
      PasteGraph g.graph
   else
      Loaded { graph = g.graph, scenario = g.scenario}

noOp : Msg
noOp = Do Cmd.none


-- Focus on the input

focusId : String -> Cmd Msg
focusId s = Task.attempt (\_ -> noOp) (Dom.focus s)

unfocusId : String -> Cmd Msg
unfocusId s = Task.attempt (\_ -> noOp) (Dom.blur s)


onTabPreventDefault : Html.Attribute Msg
onTabPreventDefault = HtmlDefs.preventsDefaultOnKeyDown
   noOp (\ _ k -> k == HtmlDefs.Control "Tab")
