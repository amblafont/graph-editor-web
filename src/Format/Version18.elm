module Format.Version18 exposing (Graph, Node, Tab, ArrowStyle, Edge, fromJSGraph, version, 
  tailFlag, headFlag, addFlag
  , dashedFlag, wavyFlag,  textFlag, coqValidatedFlag, prefixes,
  bendFlag, positionFlag)
{- 
Changes from Version 17:
- free hand drawings
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
import FreeHandDrawings as FreeHand
import Format.Version19 as NextVersion exposing (EdgeFlag(..), NodeFlag(..))
-- import Format.Version17 exposing (textFlag, wavyFlag, coqValidatedFlag, dashedFlag, prefixes, bendFlag, positionFlag)
-- import Codec exposing (FinalCustomCodec)
-- import Format.Keys exposing (normalKey, pullshoutKey, adjunctionKey)

version = 18

type alias PullshoutOffsets = { offset1 : Float, offset2 : Float }

-- first argument is the max number of digits after the decimal point
stringFromFloat : Int -> Float -> String
stringFromFloat n f = 
  let s = String.fromFloat f in
  case String.split "." s of
    [s1 , s2] -> s1 ++ "." ++ String.left n s2
    _ -> s

maxDecimalDigits = 4

myStringFromFloat = stringFromFloat maxDecimalDigits
myStringToFloat = 
  let epsilon = 2 * 10^(1-maxDecimalDigits) in
  String.toFloat
  >> Maybe.map (\ d -> if epsilon < d && d < epsilon then 0 else d)

floatCodec : Codec Float String
floatCodec = 
  Codec.build 
    myStringFromFloat
    (myStringToFloat >> Maybe.withDefault 0)

-- intCodec : Codec Int String
-- intCodec = 
--   Codec.build 
--     String.fromInt
--     (String.toInt >> Maybe.withDefault 0)



dashedFlag : Codec Bool (List String)
dashedFlag = edgeMaybeFlagCodecFalse Dashed

textFlag : Codec Bool (List String)
textFlag = nodeMaybeFlagCodecFalse Text


coqValidatedFlag : Codec Bool (List String)
coqValidatedFlag = nodeMaybeFlagCodecFalse CoqValidated


wavyFlag : Codec Bool (List String)
wavyFlag = edgeMaybeFlagCodecFalse Wavy


bendFlag : Codec Float (List String)
bendFlag =
       edgeMaybeFlagCodec 0 Bend
    (\ flag -> case flag of 
        Bend a -> Just a
        _ -> Nothing
    )




positionFlag : Codec Float (List String)
positionFlag =
       edgeMaybeFlagCodec 0.5 Position
    (\ flag -> case flag of 
        Position a -> Just (a |> min 0.9 |> max 0.1)
        _ -> Nothing
    )

pullshoutOffsetCodec : Codec PullshoutOffsets String
pullshoutOffsetCodec = 
  Codec.build 
    (\{offset1, offset2} -> myStringFromFloat offset1 ++ " " ++ myStringFromFloat offset2)
    (\s -> 
          let default = {offset1 = 0, offset2 = 0} in
          case String.split " " s of
            [s1, s2] -> 
              case (myStringToFloat s1, myStringToFloat s2) of
                (Just f1, Just f2) ->  {offset1 = f1, offset2 = f2}
                _ -> default
            _ -> default
    )

prefixes = 
  {
    dashed = "dashed",
    unrecognized = "unrecognized",
    wavy = "wavy",
    adjunction = "adjunction",

    kind = "kind ",
    head = "head ",
    tail = "tail ",
    alignment = "alignment ",
    pullshout = "pullshout ",    
    color = "color ",
    tailColor = "tailColor ",
    headColor = "headColor ",
    marker = "marker ",
    bend = "bend ",
    position = "position ",
    shiftSource = "shiftSource ",
    shiftTarget = "shiftTarget ",
    loopRadius = "loopRadius ",
    loopAngle = "loopAngle "
  }

    

edgeFlagCodec : Codec EdgeFlag String
edgeFlagCodec =
    let split dashed marker pullshout bend position adjunction wavy kind headstyle tailstyle alignment color headcolor tailcolor shiftSource shiftTarget loopRadius loopAngle unrecognized v =
                    case v of
                        Dashed -> dashed
                        Marker s -> marker s
                        Pullshout r -> pullshout r
                        Bend f -> bend f
                        Position f -> position f
                        Adjunction -> adjunction
                        Wavy -> wavy
                        Kind k -> kind k
                        HeadStyle s -> headstyle s
                        TailStyle s -> tailstyle s
                        Alignment s -> alignment s
                        Color c -> color c
                        HeadColor c -> headcolor c
                        TailColor c -> tailcolor c
                        ShiftSource c -> shiftSource c
                        ShiftTarget c -> shiftTarget c
                        LoopRadius c -> loopRadius c
                        LoopAngle c -> loopAngle c
                        _ -> unrecognized
                        -- Unrecognized -> unrecognized
                        -- LabelColor _ -> unrecognized
                        -- Dependency -> unrecognized
   in
   Codec.customEnum split
   |> Codec.variant0 prefixes.dashed Dashed
   |> Codec.prefixVariant0 prefixes.marker Marker Codec.identity
   |> Codec.prefixVariant0 prefixes.pullshout Pullshout pullshoutOffsetCodec
   |> Codec.prefixVariant0 prefixes.bend Bend floatCodec
   |> Codec.prefixVariant0 prefixes.position Position floatCodec
  --  TODO: faire de variant0 un sous cas de subvariant0
   |> Codec.variant0 prefixes.adjunction Adjunction
    |> Codec.variant0 prefixes.wavy Wavy
    |> Codec.prefixVariant0 prefixes.kind Kind kindCodec    
    |> Codec.prefixVariant0 prefixes.head HeadStyle headCodec
    |> Codec.prefixVariant0 prefixes.tail TailStyle tailCodec
    |> Codec.prefixVariant0 prefixes.alignment Alignment alignmentCodec
    |> Codec.prefixVariant0 prefixes.color Color Color.codec
    |> Codec.prefixVariant0 prefixes.headColor HeadColor Color.codec
    |> Codec.prefixVariant0 prefixes.tailColor TailColor Color.codec
    |> Codec.prefixVariant0 prefixes.shiftSource ShiftSource floatCodec
    |> Codec.prefixVariant0 prefixes.shiftTarget ShiftTarget floatCodec
    |> Codec.prefixVariant0 prefixes.loopRadius LoopRadius floatCodec
    |> Codec.prefixVariant0 prefixes.loopAngle LoopAngle floatCodec
    |> Codec.variant0 prefixes.unrecognized Unrecognized
    |> Codec.buildVariant (always Unrecognized)



  
edgeFlagsCodec : Codec a (List EdgeFlag) -> Codec a (List String)
edgeFlagsCodec = Codec.compose (Codec.list edgeFlagCodec)

nodeFlagsCodec : Codec a (List NodeFlag) -> Codec a (List String)
nodeFlagsCodec = Codec.compose (Codec.list nodeFlagCodec)

edgeMaybeFlagCodec : a -> (a -> EdgeFlag) -> (EdgeFlag -> Maybe a) -> Codec a (List String)
edgeMaybeFlagCodec default constr destr = 
  Codec.maybeList default constr destr 
  |> edgeFlagsCodec

edgeMaybeFlagCodecFalse : EdgeFlag -> Codec Bool (List String)
edgeMaybeFlagCodecFalse flag = 
  Codec.boolList flag |> edgeFlagsCodec

nodeMaybeFlagCodecFalse : NodeFlag -> Codec Bool (List String)
nodeMaybeFlagCodecFalse flag = 
  Codec.boolList flag |> nodeFlagsCodec


  

tailFlag : Codec TailStyle (List String)
tailFlag =
       edgeMaybeFlagCodec DefaultTail TailStyle
    (\ flag -> case flag of 
        TailStyle a -> Just a
        _ -> Nothing
    )


  
headFlag : Codec HeadStyle (List String)
headFlag =
       edgeMaybeFlagCodec DefaultHead HeadStyle
    (\ flag -> case flag of 
        HeadStyle a -> Just a
        _ -> Nothing
    )




addFlag : EdgeFlag -> ArrowStyle -> ArrowStyle
addFlag flag l =
  Codec.encoder edgeFlagCodec flag :: l

type alias ArrowStyle = --{ -- bend : Float, -- alignment : String, 
   List String
  --  }

               --}

type alias Edgeo o = { label : String, style : List o,
       zindex : Int
      --  , selected : Bool 
       }
type alias Edge = Edgeo String



nodeFlagCodec : Codec NodeFlag String
nodeFlagCodec =
    Codec.customEnum (\ coq text unrecognized v ->
                    case v of
                        CoqValidated -> coq
                        Text -> text
                        _ -> unrecognized
   )
   |> Codec.variant0 "coqValidated" CoqValidated
   |> Codec.variant0 "text" Text
   |> Codec.variant0 "" UnrecognizedNodeFlag
   |> Codec.buildVariant (always UnrecognizedNodeFlag)

type alias Nodeo o = { pos : Point , label : String, zindex: Int,
   flags : List o
  -- , selected : Bool
  }
type alias Node = Nodeo String
type alias Tabo n e = { 
      title: String,
      sizeGrid : Int,
      id : Int,
      nodes: List (Graph.Node (Nodeo n)),
      edges: List (Graph.Edge (Edgeo e)),
      nextGraphId : Int,
      freehandDrawings : FreeHand.DrawingsJS
   }
type alias Tab = Tabo String String

type alias Grapho n e = { 
      tabs : List (Tabo n e),
      latexPreamble : String,
      nextTabId : Int,
      activeTabId : Int}

type alias Graph = Grapho String String

-- auxiliary functions to define graphoMap
nodeoMap : (n1 -> n2) -> (Nodeo n1 -> Nodeo n2)
nodeoMap fnNode {pos, label, zindex, flags} =
  { pos = pos, label = label, zindex = zindex, flags = List.map fnNode flags }

edgeoMap : (e1 -> e2) -> (Edgeo e1 -> Edgeo e2)
edgeoMap fnEdge {label, style, zindex} =
  { label = label, style = List.map fnEdge style, zindex = zindex }


taboMap : (n1 -> n2) -> (e1 -> e2) -> Tabo n1 e1 -> Tabo n2 e2
taboMap fnNode fnEdge {title, sizeGrid, id, nodes, edges, nextGraphId, freehandDrawings} =
  { title = title, sizeGrid = sizeGrid, id = id, nextGraphId = nextGraphId, freehandDrawings = freehandDrawings,
    nodes = List.map (Graph.nodeMap (nodeoMap fnNode)) nodes,
    edges = List.map (Graph.edgeMap (edgeoMap fnEdge)) edges
   }

graphoMap : (n1 -> n2) -> (e1 -> e2) -> Grapho n1 e1 -> Grapho n2 e2
graphoMap fnNode fnEdge {tabs, latexPreamble, nextTabId, activeTabId} =
  { tabs = List.map (taboMap fnNode fnEdge) tabs,
    latexPreamble = latexPreamble,
    nextTabId = nextTabId,
    activeTabId = activeTabId
  }
-----------
-- auxiliary function to define toNextGraphFlags

toNextNodeFlags : Nodeo NodeFlag -> NextVersion.Nodeo (List NodeFlag)
toNextNodeFlags {pos, label, zindex, flags} =
  { pos = pos, label = label, zindex = zindex, options = flags }

toNextEdgeFlags : Edgeo EdgeFlag -> NextVersion.Edgeo (List EdgeFlag)
toNextEdgeFlags {label, style, zindex} =
  { label = label, options = LabelColor Color.black :: style, zindex = zindex }

toNextTabFlags : Tabo NodeFlag EdgeFlag -> NextVersion.Tabo (List NodeFlag) (List EdgeFlag)
toNextTabFlags {id, title, sizeGrid, nodes, edges, nextGraphId, freehandDrawings} =
    { title = title, 
      id = id,
      sizeGrid = sizeGrid,
      nodes = List.map (Graph.nodeMap toNextNodeFlags) nodes,
      edges = List.map (Graph.edgeMap toNextEdgeFlags) edges,
      nextGraphId = nextGraphId,
      freehandDrawings = freehandDrawings
    }

toNextGraphFlags : Grapho NodeFlag EdgeFlag -> NextVersion.Grapho (List NodeFlag) (List EdgeFlag)
toNextGraphFlags g =
  { tabs = List.map toNextTabFlags g.tabs,
    latexPreamble = g.latexPreamble,
    nextTabId = g.nextTabId,
    activeTabId = g.activeTabId
  }




-- fromJSGraph : Graph -> GraphInfo
-- fromJSGraph = Codec.decoder graphInfoCodec

fromJSGraphFlags : Grapho NodeFlag EdgeFlag -> GraphInfo
fromJSGraphFlags g = 
     g |> toNextGraphFlags |> NextVersion.fromJSGraphFlags

stringToNodeFlag : String -> NodeFlag
stringToNodeFlag = Codec.decoder nodeFlagCodec

stringToEdgeFlag : String -> EdgeFlag
stringToEdgeFlag = Codec.decoder edgeFlagCodec

fromJSGraph : Graph -> GraphInfo
fromJSGraph g =
    g |> graphoMap stringToNodeFlag stringToEdgeFlag |> fromJSGraphFlags