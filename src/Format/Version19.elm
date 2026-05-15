module Format.Version19 exposing (Graph, Nodeo, Tabo, Tab, ArrowStyle, Edgeo, toJSGraph, fromJSGraph, version, tabCodec, graphInfoCodec, defaultGraph
  , Grapho, tailFlag, headFlag, Node, Edge, edgeCodec, nodeCodec
  , dashedFlag, wavyFlag,  textFlag, coqValidatedFlag, optionNames,
  bendFlag, positionFlag, NodeFlag(..), EdgeFlag(..), fromJSGraphFlags)
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
import Polygraph exposing (Edge)
-- import Format.Version17Verbatim exposing (EdgeFlag(..))
-- import Format.Version17 exposing (textFlag, wavyFlag, coqValidatedFlag, dashedFlag, optionNames, bendFlag, positionFlag)
-- import Codec exposing (FinalCustomCodec)
-- import Format.Keys exposing (normalKey, pullshoutKey, adjunctionKey)

version = 19


type NodeFlag = 
      CoqValidated
    | Text
    | UnrecognizedNodeFlag


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

type alias PullshoutOffsets = { offset1 : Float, offset2 : Float }
type alias DictValue = List (String, JDecode.Value)

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


-- intCodec : Codec Int String
-- intCodec = 
--   Codec.build 
--     String.fromInt
--     (String.toInt >> Maybe.withDefault 0)


dashedFlag : Codec Bool (List EdgeFlag)
dashedFlag = edgeMaybeFlagCodecFalse Dashed

textFlag : Codec Bool (List NodeFlag)
textFlag = nodeMaybeFlagCodecFalse Text



coqValidatedFlag : Codec Bool (List NodeFlag)
coqValidatedFlag = nodeMaybeFlagCodecFalse CoqValidated


wavyFlag : Codec Bool (List EdgeFlag)
wavyFlag = edgeMaybeFlagCodecFalse Wavy


bendFlag : Codec Float (List EdgeFlag)
bendFlag =
       edgeMaybeFlagCodec 0 Bend
    (\ flag -> case flag of 
        Bend a -> Just a
        _ -> Nothing
    )

loopRadiusFlag : Codec Float (List EdgeFlag)
loopRadiusFlag =
       edgeMaybeFlagCodec ArrowStyle.defaultLoopRadius LoopRadius
    (\ flag -> case flag of 
        LoopRadius a -> Just a
        _ -> Nothing
    )

loopAngleFlag : Codec Float (List EdgeFlag)
loopAngleFlag =
       edgeMaybeFlagCodec 0 LoopAngle
    (\ flag -> case flag of 
        LoopAngle a -> Just a
        _ -> Nothing
    )


positionFlag : Codec Float (List EdgeFlag)
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

optionNames = 
  {
    dashed = "dashed",
    unrecognized = "unrecognized",
    wavy = "wavy",
    adjunction = "adjunction",

    kind = "kind",
    head = "head",
    tail = "tail",
    alignment = "alignment",
    pullshout = "pullshout",    
    color = "color ",
    tailColor = "tailColor",
    headColor = "headColor",
    labelColor = "labelColor",
    marker = "marker",
    bend = "bend",
    position = "position",
    shiftSource = "shiftSource",
    shiftTarget = "shiftTarget",
    loopRadius = "loopRadius",
    loopAngle = "loopAngle"
  }

genericVariantTrue default tag v = Codec.variantTruePair tag v default Codec.trueJs

edgeFlagCodec : Codec EdgeFlag (String, JDecode.Value)
edgeFlagCodec =
    let variantTrue  = genericVariantTrue Unrecognized
            -- Codec.variantTruePair tag v Unrecognized Codec.trueJs
    in
    let variantString tag v c =
            Codec.variant1Pair tag v (Codec.compose Codec.stringJs c)
    in
    let split dashed marker pullshout bend position adjunction wavy kind headstyle tailstyle alignment color headcolor tailcolor labelcolor shiftSource shiftTarget loopRadius loopAngle unrecognized v =
                    case v of
                        Dashed -> dashed True
                        Marker s -> marker s
                        Pullshout r -> pullshout r
                        Bend f -> bend f
                        Position f -> position f
                        Adjunction -> adjunction True
                        Wavy -> wavy True
                        Kind k -> kind k
                        HeadStyle s -> headstyle s
                        TailStyle s -> tailstyle s
                        Alignment s -> alignment s
                        Unrecognized -> unrecognized
                        Color c -> color c
                        HeadColor c -> headcolor c
                        TailColor c -> tailcolor c
                        LabelColor c -> labelcolor c
                        ShiftSource c -> shiftSource c
                        ShiftTarget c -> shiftTarget c
                        LoopRadius c -> loopRadius c
                        LoopAngle c -> loopAngle c
   in
   Codec.customPair split ("", JEncode.null) 
   |> variantTrue optionNames.dashed Dashed
   |> Codec.variant1Pair optionNames.marker Marker Codec.stringJs
   |> variantString optionNames.pullshout Pullshout pullshoutOffsetCodec
   |> Codec.variant1Pair optionNames.bend Bend Codec.floatJs
   |> Codec.variant1Pair optionNames.position Position Codec.floatJs
  --  TODO: faire de variant0 un sous cas de subvariant0
   |> variantTrue optionNames.adjunction Adjunction 
    |> variantTrue optionNames.wavy Wavy
    |> variantString optionNames.kind Kind kindCodec    
    |> variantString optionNames.head HeadStyle headCodec
    |> variantString optionNames.tail TailStyle tailCodec
    |> variantString optionNames.alignment Alignment alignmentCodec
    |> variantString optionNames.color Color Color.codec
    |> variantString optionNames.headColor HeadColor Color.codec
    |> variantString optionNames.tailColor TailColor Color.codec
    |> variantString optionNames.labelColor LabelColor Color.codec
    |> Codec.variant1Pair optionNames.shiftSource ShiftSource Codec.floatJs
    |> Codec.variant1Pair optionNames.shiftTarget ShiftTarget Codec.floatJs
    |> Codec.variant1Pair optionNames.loopRadius LoopRadius Codec.floatJs
    |> Codec.variant1Pair optionNames.loopAngle LoopAngle Codec.floatJs
    |> Codec.variant0 optionNames.unrecognized Unrecognized
    |> Codec.buildVariant (always Unrecognized)



  
edgeFlagsCodec : Codec a (List EdgeFlag) -> Codec a DictValue
edgeFlagsCodec = Codec.compose (Codec.list edgeFlagCodec)

nodeFlagsCodec : Codec a (List NodeFlag) -> Codec a DictValue
nodeFlagsCodec = Codec.compose (Codec.list nodeFlagCodec)

edgeMaybeFlagCodec : a -> (a -> EdgeFlag) -> (EdgeFlag -> Maybe a) -> Codec a (List EdgeFlag)
edgeMaybeFlagCodec default constr destr = 
  Codec.maybeList default constr destr 
  -- |> edgeFlagsCodec
 
edgeMaybeFlagCodecFalse : EdgeFlag -> Codec Bool (List EdgeFlag)
edgeMaybeFlagCodecFalse flag = 
  Codec.boolList flag -- |> edgeFlagsCodec

 
nodeMaybeFlagCodecFalse : NodeFlag -> Codec Bool (List NodeFlag)
nodeMaybeFlagCodecFalse flag = 
  Codec.boolList flag  -- |> nodeFlagsCodec


alignmentFlag : Codec LabelAlignment (List EdgeFlag)
alignmentFlag =
    edgeMaybeFlagCodec Left Alignment
    (\ flag -> case flag of 
        Alignment a -> Just a
        _ -> Nothing
    )
  
 
tailFlag : Codec TailStyle (List EdgeFlag)
tailFlag =
       edgeMaybeFlagCodec DefaultTail TailStyle
    (\ flag -> case flag of 
        TailStyle a -> Just a
        _ -> Nothing
    )

markerFlag : Codec String (List EdgeFlag)
markerFlag =
       edgeMaybeFlagCodec "" Marker
    (\ flag -> case flag of 
        Marker a -> Just a
        _ -> Nothing
    )
  
headFlag : Codec HeadStyle (List EdgeFlag)
headFlag =
       edgeMaybeFlagCodec DefaultHead HeadStyle
    (\ flag -> case flag of 
        HeadStyle a -> Just a
        _ -> Nothing
    )


-- for backwards compatibility, we add conversion between 
-- the range [0,1] and [-5,5]
shiftSourceFlag : Codec Float (List EdgeFlag)
shiftSourceFlag =
       edgeMaybeFlagCodec 0.5 (\ x -> ShiftSource <| (x - 0.5) * 10)
    (\ flag -> case flag of 
        ShiftSource a -> Just <| (a + 5) / 10
        _ -> Nothing
    )

shiftTargetFlag : Codec Float (List EdgeFlag)
shiftTargetFlag =
       edgeMaybeFlagCodec 0.5 (\ x -> ShiftTarget <| (x - 0.5) * 10)
    (\ flag -> case flag of 
        ShiftTarget a -> Just <| (a + 5) / 10
        _ -> Nothing
    )

pullshoutFlag : Codec (Maybe PullshoutOffsets) (List EdgeFlag)
pullshoutFlag =
      --  edgeMaybeFlagCodec Nothing 
      Codec.build
       (Maybe.map (Pullshout >> List.singleton) >> Maybe.withDefault [])
    ( List.Extra.findMap 
           (\ flag -> case flag of 
                       Pullshout a -> Just a
                       _ -> Nothing
           ))
           

kindFlag : Codec ArrowKind (List EdgeFlag)
kindFlag =
  let dec = (\ flag -> case flag of 
        Kind a -> Just a
        _ -> Nothing
       )
  in
  -- edgeFlagsCodec <|
  Codec.build 
    (\ a -> if a == NormalArrow then [] else [ Kind a ])
    (\ bs ->  List.Extra.findMap dec bs |> Maybe.withDefault NormalArrow)
    --    Codec.buildBetween NormalArrow Kind
    -- (\ flag -> case flag of 
    --     Kind a -> Just a
    --     _ -> Nothing
    -- )



type alias ArrowStyle = --{ -- bend : Float, -- alignment : String, 
  JDecode.Value
  --  DictValue -- flags : List EdgeFlag
   
  --  }


pullshoutStyle : GraphDefs.PullshoutEdgeLabel -> List EdgeFlag
pullshoutStyle {color, offset1, offset2} =
  -- { 
  --   flags = 
    -- Codec.encoder edgeFlagCodec 
    (Pullshout {offset1 = offset1, offset2 = offset2})
        -- :: Codec.encoder flagCodec (Bend offset1)
        :: Codec.encoder colorsFlag
               { main = color, tail = color, head = color, label = color }
    -- |> JEncode.object
               --}


 

pullshoutEdge : Int -> GraphDefs.PullshoutEdgeLabel -> Edgeo (List EdgeFlag)
pullshoutEdge z label = 
    Edgeo "" (pullshoutStyle label) z -- False


nodeFlagCodec : Codec NodeFlag (String, JDecode.Value)
nodeFlagCodec =
    let variantTrue  = genericVariantTrue UnrecognizedNodeFlag
    in
    Codec.customPair (\ coq text unrecognized v ->
                    case v of
                        CoqValidated -> coq True
                        Text -> text True
                        UnrecognizedNodeFlag -> unrecognized
   ) ("", JEncode.null) 
   |> variantTrue "coqValidated" CoqValidated
   |> variantTrue "text" Text
   |> Codec.variant0 "" UnrecognizedNodeFlag
   |> Codec.buildVariant (always UnrecognizedNodeFlag)


type alias Edge = Edgeo JDecode.Value
type alias Node = Nodeo JDecode.Value


type alias Nodeo o = { pos : Point , label : String, zindex: Int,
   options : o
  -- , selected : Bool
  }

type alias Edgeo o = { label : String,
       options : o,
       zindex : Int
      --  , selected : Bool 
       }


nodeMap : (o1 -> o2) -> Nodeo o1 -> Nodeo o2
nodeMap f {pos, label, zindex, options} =
  { pos = pos, label = label, zindex = zindex, options = f options }

mapNodeCodec : Codec o1 o2 -> Codec (Nodeo o1) (Nodeo o2)
mapNodeCodec optionsCodec =
  Codec.build (nodeMap (Codec.encoder optionsCodec)) (nodeMap (Codec.decoder optionsCodec))

nodeCodecDict : Codec (Nodeo DictValue) (Nodeo JDecode.Value)
nodeCodecDict = mapNodeCodec Codec.objectJs

edgeMap : (o1 -> o2) -> Edgeo o1 -> Edgeo o2
edgeMap f {label, options, zindex} =
  { label = label, zindex = zindex, options = f options }

mapEdgeCodec : Codec o1 o2 -> Codec (Edgeo o1) (Edgeo o2)
mapEdgeCodec optionsCodec =
  Codec.build (edgeMap (Codec.encoder optionsCodec))
    (edgeMap (Codec.decoder optionsCodec))

edgeCodecDict : Codec (Edgeo DictValue) (Edgeo JDecode.Value)
edgeCodecDict = mapEdgeCodec (Codec.objectJs)

type alias Tabo n e = { 
      title: String,
      sizeGrid : Int,
      id : Int,
      nodes: List (Graph.Node (Nodeo n)),
      edges: List (Graph.Edge (Edgeo e)),
      nextGraphId : Int,
      freehandDrawings : FreeHand.DrawingsJS
   }

type alias Tab = Tabo JDecode.Value JDecode.Value
type alias Grapho n e = { 
      tabs : List (Tabo n e),
      latexPreamble : String,
      nextTabId : Int,
      activeTabId : Int}

type alias Graph = Grapho JDecode.Value JDecode.Value

-- auxiliary functions to define graphoMap

taboMap : (n1 -> n2) -> (e1 -> e2) -> Tabo n1 e1 -> Tabo n2 e2
taboMap fn fe {id, title, sizeGrid, nodes, edges, nextGraphId, freehandDrawings} =
    { title = title, 
      id = id,
      sizeGrid = sizeGrid,
      nodes = List.map (Graph.nodeMap (nodeMap fn)) nodes,
      edges = List.map (Graph.edgeMap (edgeMap fe)) edges,
      nextGraphId = nextGraphId,
      freehandDrawings = freehandDrawings
    }

graphoMap : (n1 -> n2) -> (e1 -> e2) -> Grapho n1 e1 -> Grapho n2 e2
graphoMap fn fe {tabs, latexPreamble, nextTabId, activeTabId} =
  { tabs = List.map (taboMap fn fe) tabs,
    latexPreamble = latexPreamble,
    nextTabId = nextTabId,
    activeTabId = activeTabId
  }

taboCodec : Codec n1 n2 -> Codec e1 e2 -> Codec (Tabo n1 e1) (Tabo n2 e2)
taboCodec c1 c2 =
  Codec.build 
    (taboMap (Codec.encoder c1) (Codec.encoder c2))
    (taboMap (Codec.decoder c1) (Codec.decoder c2))

graphoCodec : Codec n1 n2 -> Codec e1 e2 -> Codec (Grapho n1 e1) (Grapho n2 e2)
graphoCodec c1 c2 = 
  Codec.build 
    (graphoMap (Codec.encoder c1) (Codec.encoder c2))
    (graphoMap (Codec.decoder c1) (Codec.decoder c2))

defaultGraph : Graph
defaultGraph = { tabs = [], latexPreamble = "", nextTabId = 0, activeTabId = 0}


colorsFlag : Codec {main : Color, head : Color, tail : Color, label : Color} (List EdgeFlag)
colorsFlag =
    -- edgeFlagsCodec <|
     Codec.build 
    ( \ {main, head, tail, label} -> 
       (if main == Color.black then [] else [Color main])
       ++
       (if head == main then [] else [HeadColor head])
       ++
       (if tail == main then [] else [TailColor tail])
       ++
       (if label == main then [] else [LabelColor label])    
    )
    ( \ l -> 
       case 
        ((List.Extra.findMap (\ v -> case v of Color c -> Just c 
                                               _ -> Nothing) l,
         List.Extra.findMap (\ v -> case v of HeadColor c -> Just c
                                              _ -> Nothing) l),
         (List.Extra.findMap (\ v -> case v of TailColor c -> Just c
                                               _ -> Nothing) l,
         List.Extra.findMap (\ v -> case v of LabelColor c -> Just c
                                              _ -> Nothing) l)
                                              )
        of 
        ((mc, hc), (tc, lc)) ->
            let c = mc |> Maybe.withDefault Color.black in
            { main = c, head = hc |> Maybe.withDefault c, tail = tc |> Maybe.withDefault c,
            label = Maybe.withDefault c lc }
    )



arrowStyleCodec : Codec ArrowStyle.ArrowStyle (List EdgeFlag)
arrowStyleCodec =
  let flagField f1 codec = 
        Codec.fields f1 Basics.identity codec
  in
  Codec.object
  (\tail head kind dashed bend alignment position shiftSource shiftTarget colors marker wavy loopRadius loopAngle ->
      { tail = tail, head = head, kind = kind
   , dashed = dashed, bend = bend, labelAlignment = alignment, 
   shiftSource = shiftSource, shiftTarget = shiftTarget,
   labelPosition = position, color = colors.main, marker = marker,
   headColor = colors.head, tailColor = colors.tail, labelColor = colors.label,  wavy = wavy,
   loopRadius = loopRadius, loopAngle = loopAngle }
    
  )
  (\tail head kind dashed bend alignment position shiftSource shiftTarget colors marker wavy loopRadius loopAngle  ->
     shiftSource ++ shiftTarget ++ position ++ bend ++ marker ++ colors ++ dashed ++ alignment ++ tail ++ head ++ kind ++ wavy ++ loopRadius ++ loopAngle
    
  )
  |> flagField .tail tailFlag
  |> flagField .head headFlag
  |> flagField .kind kindFlag
  |> flagField .dashed dashedFlag
  |> flagField  .bend bendFlag
  |> flagField .labelAlignment alignmentFlag
  |> flagField .labelPosition positionFlag
  |> flagField .shiftSource shiftSourceFlag
  |> flagField .shiftTarget shiftTargetFlag
  -- |> Codec.fields .labelPosition (.position >> min 0.9 >> max 0.1) Codec.identity
  |> flagField (\{color, headColor, tailColor, labelColor} -> {main = color, head = headColor, tail = tailColor, label = labelColor})
                  colorsFlag
  |> flagField .marker markerFlag
  |> flagField .wavy wavyFlag
  |> flagField .loopRadius loopRadiusFlag
  |> flagField .loopAngle loopAngleFlag
  |> Codec.buildObject
  -- |> Codec.compose Codec.objectJs






nodeFCodec : Codec NodeLabel (Nodeo (List NodeFlag))
nodeFCodec =
   Codec.object
   (\ pos label isText zindex isCoqValidated ->
   { pos = pos, label = label
   , dims = Nothing,  weaklySelected = False, isMath = not isText,
     zindex = zindex, isCoqValidated = isCoqValidated , selected = False
     }
    )
    (\ pos label isText zindex isCoqValidated ->
    { pos = pos, label = label, options = (isText ++ isCoqValidated), zindex = zindex
      --, selected = selected
      })
    |> Codec.fields .pos .pos Codec.identity
    |> Codec.fields .label .label Codec.identity
    |> Codec.fields (.isMath >> not) .options textFlag
    |> Codec.fields .zindex .zindex Codec.identity
    |> Codec.fields .isCoqValidated .options coqValidatedFlag
    -- |> Codec.fields .selected .selected Codec.identity
    |> Codec.buildObject
    -- |> Codec.compose nodeCodecDict

-- nodeCodec : Codec NodeLabel (Nodeo JDecode.Value)
-- nodeCodec =
--    Codec.object
--    (\ pos label isText zindex isCoqValidated ->
--    { pos = pos, label = label
--    , dims = Nothing,  weaklySelected = False, isMath = not isText,
--      zindex = zindex, isCoqValidated = isCoqValidated , selected = False
--      }
--     )
--     (\ pos label isText zindex isCoqValidated ->
--     { pos = pos, label = label, options = (isText ++ isCoqValidated), zindex = zindex
--       --, selected = selected
--       })
--     |> Codec.fields .pos .pos Codec.identity
--     |> Codec.fields .label .label Codec.identity
--     |> Codec.fields (.isMath >> not) .options textFlag
--     |> Codec.fields .zindex .zindex Codec.identity
--     |> Codec.fields .isCoqValidated .options coqValidatedFlag
--     -- |> Codec.fields .selected .selected Codec.identity
--     |> Codec.buildObject
--     |> Codec.compose nodeCodecDict


fromEdgeLabel : EdgeLabel -> Edgeo (List EdgeFlag)
fromEdgeLabel e = 
   case e.details of
       PullshoutEdge l -> pullshoutEdge e.zindex l
       NormalEdge ({label, isAdjunction} as l)->
            let style = ArrowStyle.getStyle l in
            { label = label,
              -- kind = if isAdjunction then adjunctionKey else normalKey,       
              options = 
                let convertedStyle = Codec.encoder arrowStyleCodec style in 
                if isAdjunction then
                    Adjunction :: convertedStyle
                else
                    convertedStyle
              ,
              zindex = e.zindex 
              -- , selected = e.selected              
            } 

toEdgeLabel : Edgeo (List EdgeFlag) -> EdgeLabel
toEdgeLabel { label, options, zindex } = 
    let dec codec = Codec.decoder codec options in
   { selected = False -- selected
     , weaklySelected = False,
     zindex = zindex,
     details = 
       case dec pullshoutFlag of
          Just {offset1, offset2} ->
              PullshoutEdge {color = 
                          dec colorsFlag
                          |> .main,
                         offset1 = offset1, offset2 = offset2}
          Nothing ->
              NormalEdge { label = label
                , isAdjunction = List.member Adjunction options
                , style = Codec.decoder arrowStyleCodec options
                , dims = Nothing
              }
   }


edgeFCodec : Codec EdgeLabel (Edgeo (List EdgeFlag))
edgeFCodec = 
   Codec.build fromEdgeLabel toEdgeLabel
  --  |> Codec.compose edgeCodecDict

edgeCodec : Codec EdgeLabel Edge
edgeCodec = 
   Codec.compose 
   (Codec.build 
      (edgeMap (Codec.encoder codecEdgeFlagsJs))
      (edgeMap (Codec.decoder codecEdgeFlagsJs))
    )
   edgeFCodec

nodeCodec : Codec NodeLabel Node
nodeCodec = 
   Codec.compose 
   (Codec.build 
      (nodeMap (Codec.encoder codecNodeFlagsJs))
      (nodeMap (Codec.decoder codecNodeFlagsJs))
    )
   nodeFCodec

-- edgeCodec : Codec EdgeLabel (Edgeo JDecode.Value)
-- edgeCodec = 
--    Codec.build fromEdgeLabel toEdgeLabel
--    |> Codec.compose edgeCodecDict

tabFlagsCodec : Codec  GraphInfo.Tab (Tabo (List NodeFlag) (List EdgeFlag))
tabFlagsCodec =
  Codec.object
  (\ graph title sizeGrid tabId freehandDrawings ->
    { graph = graph,
      title = title, sizeGrid = sizeGrid,
      id = tabId,
      freehandDrawings = freehandDrawings
    }
  )
  (\ graph title sizeGrid tabId freehandDrawings ->
    { nodes = graph.nodes,
      edges = graph.edges,
      nextGraphId = graph.nextId,
      title = title, sizeGrid = sizeGrid,
      id = tabId,
      freehandDrawings = freehandDrawings
    }
  )
  |> Codec.fields .graph (\e -> { nextId = e.nextGraphId, nodes = e.nodes, edges = e.edges}) 
     ( -- Codec.compose (Graph.nextIdCodec)
       (Codec.compose Graph.codec (Graph.mapCodec nodeFCodec edgeFCodec))
        )
  |> Codec.fields .title .title Codec.identity
  -- |> Codec.fields .active .active Codec.identity
  |> Codec.fields .sizeGrid .sizeGrid Codec.identity
  |> Codec.fields .id .id Codec.identity
  |> Codec.fields .freehandDrawings .freehandDrawings FreeHand.codec
  |> Codec.buildObject

tabCodec : Codec  GraphInfo.Tab Tab
tabCodec = 
   Codec.compose 
   (taboCodec codecNodeFlagsJs codecEdgeFlagsJs)
   tabFlagsCodec
        

graphInfoCodecFlags : Codec GraphInfo (Grapho (List NodeFlag) (List EdgeFlag))
graphInfoCodecFlags = 
  Codec.object
  (\tabs nextTabId latexPreamble activeTabId ->
    { tabs = tabs, nextTabId = nextTabId, activeTabId = activeTabId, latexPreamble = latexPreamble }
  )
  (\tabs nextTabId latexPreamble activeTabId ->
    { tabs = tabs, nextTabId = nextTabId, activeTabId = activeTabId, latexPreamble = latexPreamble }
  )
  |> Codec.fields .tabs .tabs (Codec.list tabFlagsCodec)
  |> Codec.fields .nextTabId .nextTabId Codec.identity
  |> Codec.fields .latexPreamble .latexPreamble Codec.identity
  |> Codec.fields .activeTabId .activeTabId Codec.identity
  |> Codec.buildObject



fromJSGraphFlags : Grapho (List NodeFlag) (List EdgeFlag) -> GraphInfo
fromJSGraphFlags g = Codec.decoder graphInfoCodecFlags g

codecNodeFlagsJs : Codec (List NodeFlag) (JDecode.Value)
codecNodeFlagsJs = 
     Codec.compose Codec.objectJs 
     <| Codec.compose (Codec.list nodeFlagCodec) (Codec.filter ((/=) UnrecognizedNodeFlag))



codecEdgeFlagsJs : Codec (List EdgeFlag) (JDecode.Value)
codecEdgeFlagsJs = Codec.compose Codec.objectJs 
    <| Codec.compose (Codec.list edgeFlagCodec) (Codec.filter ((/=) Unrecognized))

codecGraphFlagsJs : Codec (Grapho (List NodeFlag) (List EdgeFlag)) (Grapho JDecode.Value JDecode.Value)
codecGraphFlagsJs = graphoCodec codecNodeFlagsJs codecEdgeFlagsJs

toJSGraph : GraphInfo -> Graph
toJSGraph  g = Codec.encoder graphInfoCodecFlags g |> Codec.encoder codecGraphFlagsJs


fromJSGraph : Graph -> GraphInfo
fromJSGraph = fromJSGraphFlags << Codec.decoder codecGraphFlagsJs

graphInfoCodec : Codec GraphInfo Graph
graphInfoCodec = Codec.build toJSGraph fromJSGraph