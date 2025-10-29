module Modes.Capture exposing (..)
-- this is not a mode per se, but a component to be used in other modes
import Modes exposing (CaptureState)
import Geometry.Point exposing (Point)
import Msg exposing (Msg(..))
import Model exposing (Model, noCmd, setMode)
import Geometry.Point as Point
import HtmlDefs exposing (Key(..))

help : String
help = "move the mouse. [Click] to confirm, [ESC] to cancel."




type UpdateResult = 
      NewState
    | Finalise
    | Cancel
    | NoChange
    -- | ToggleHelp


initialise : Point -> Point -> Float -> 
   CaptureState
initialise from to ini =
    ( { value = ini
    --   , origMouse = ini.origMouse
      , direction = Point.normalise 1 <| Point.subtract to from
      }
    -- , if isFix then Cmd.none else pointerLock ()
    )

-- newBendComponent : CaptureState -> Point -> Float
-- newBendComponent state mousePos =
--             let delta = Point.scalarProduct state.direction mousePos in
--             let newBend = state.origBend + delta / 100 in
--             let finalBend = toFloat (round (newBend * 10)) / 10 in
--             finalBend

update : CaptureState -> Msg -> (UpdateResult, CaptureState)
update state msg =
    let sameState v = (v , state) in
    -- let final = (Finalise, HtmlDefs.pointerUnlock ()) in
    case msg of
        MouseLockedDelta pos -> (NewState, 
            { state | value = Point.scalarProduct state.direction pos })
        KeyChanged False _ (Control "Escape") ->  sameState Cancel
        MouseUnlock -> sameState Cancel
        -- to be fixed but, currently can't capture any key when in pointer lock
        -- KeyChanged False _ (Character ' ') -> Finalise
        -- KeyChanged False _ (Control "Enter") -> Finalise
        MouseClick -> sameState Finalise
        _ -> sameState NoChange