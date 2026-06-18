module Format.Version19 exposing (Graph, Nodeo, Tabo, Edgeo, fromJSGraph, version
  , Grapho
  ,   fromJSGraphFlags)
{- 
Changes from Version 18:
- instead of flags being a list of strings, a field 'options' of type JSon.value
-}
import Polygraph as Graph exposing (Graph)
import Geometry.Point exposing (Point)
import ArrowStyle exposing (tailCodec, headCodec, alignmentCodec, kindCodec)
import ArrowStyle exposing (TailStyle(..), HeadStyle(..), ArrowKind(..))
import Geometry exposing (LabelAlignment(..))
import GraphDefs exposing (EdgeLabel, NodeLabel)
import Format.GraphInfo as GraphInfo exposing (GraphInfo)
import GraphDefs exposing (EdgeType(..))
import Drawing.Color as Color exposing (Color)
import Codec exposing (Codec)
import List.Extra
import FreeHandDrawings as FreeHand
import Json.Decode as JDecode
import Json.Encode as JEncode
import Dict exposing (Dict)
import Polygraph exposing (Edge)
import SpecialLabels exposing (SpecialLabel)
import Format.Version20 as NextVersion
import Format.Flags exposing (NodeFlag, EdgeFlag)
-- import Format.Version17Verbatim exposing (EdgeFlag(..))
-- import Format.Version17 exposing (textFlag, wavyFlag, coqValidatedFlag, dashedFlag, optionNames, bendFlag, positionFlag)
-- import Codec exposing (FinalCustomCodec)
-- import Format.Keys exposing (normalKey, pullshoutKey, adjunctionKey)

version = 19


type alias Edgeo e = NextVersion.Edgeo e
type alias Nodeo e = NextVersion.Nodeo e
type alias Tabo n e = NextVersion.Tabo n e

type alias Grapho n e = { 
      tabs : List (Tabo n e),
      latexPreamble : String,
      nextTabId : Int,
      activeTabId : Int}

type alias Graph = Grapho JDecode.Value JDecode.Value

-- auxiliary functions to define graphoMap


toNextGrapho : Grapho a b -> NextVersion.Grapho a b
toNextGrapho g =
  { tabs = g.tabs,
    latexPreamble = g.latexPreamble,
    latexBackgroundColor = "white",
    nextTabId = g.nextTabId,
    activeTabId = g.activeTabId
  }

fromJSGraphFlags : Grapho (List NodeFlag) (List EdgeFlag) -> GraphInfo
fromJSGraphFlags g = g |> toNextGrapho |> NextVersion.fromJSGraphFlags




fromJSGraph : Graph -> GraphInfo
fromJSGraph g =
    g |> toNextGrapho |> NextVersion.fromJSGraph