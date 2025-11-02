module Modes.Capture exposing (..)
-- this is not a mode per se, but a component to be used in other modes
import Modes exposing (CaptureState)
import Geometry.Point exposing (Point)
import Msg exposing (Msg(..))
import Model exposing (Model, noCmd, setMode)
import Geometry.Point as Point
import HtmlDefs exposing (Key(..))

-- help : String
-- help = "move the mouse. [?] toggle help overlay, [Click] or [SPC] to confirm, [ESC] to cancel."




type UpdateResult = 
      NewState CaptureState
    -- | Finalise
    | Cancel
    | NoChange




update : CaptureState -> Msg -> UpdateResult
update state msg =
    -- let sameState v = (v , state) in
    let sameState v = v in
    -- let final = (Finalise, HtmlDefs.pointerUnlock ()) in
    case msg of
        MouseLockedDelta pos ->
            let delta = Point.scalarProduct state.direction pos in
            let newRawValue = state.value + delta in
            let newValue = case state.bounds of
                    Nothing -> newRawValue
                    Just (minV, maxV) -> 
                        max minV (min maxV newRawValue)
            in (NewState { state | value = newValue })
        KeyChanged False _ (Control "Escape") -> sameState Cancel
        MouseUnlock -> sameState Cancel
        _ -> sameState NoChange