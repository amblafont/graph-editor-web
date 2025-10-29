module Format.Version17 exposing (Graph, Node,  nodeCodec, edgeCodec, Tab, ArrowStyle, Edge, toJSGraph, fromJSGraph, version, tabCodec, graphInfoCodec, defaultGraph
  , EdgeFlag(..), tailFlag, headFlag, addFlag
  , dashedFlag, wavyFlag,  textFlag, coqValidatedFlag, prefixes,
  bendFlag, positionFlag)
{- 
Changes from Version 16:
- more compact json format
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
-- import Codec exposing (FinalCustomCodec)
-- import Format.Keys exposing (normalKey, pullshoutKey, adjunctionKey)

version = 17

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
    | Marker String
    | Bend Float
    | Position Float
    | ShiftSource Float
    | ShiftTarget Float



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
    shiftTarget = "shiftTarget "
  }

    

edgeFlagCodec : Codec EdgeFlag String
edgeFlagCodec =
    let split dashed marker pullshout bend position adjunction wavy kind headstyle tailstyle alignment color headcolor tailcolor shiftSource shiftTarget unrecognized v =
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
                        Unrecognized -> unrecognized
                        Color c -> color c
                        HeadColor c -> headcolor c
                        TailColor c -> tailcolor c
                        ShiftSource c -> shiftSource c
                        ShiftTarget c -> shiftTarget c
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

alignmentFlag : Codec LabelAlignment (List String)
alignmentFlag =
    edgeMaybeFlagCodec Left Alignment
    (\ flag -> case flag of 
        Alignment a -> Just a
        _ -> Nothing
    )
  

tailFlag : Codec TailStyle (List String)
tailFlag =
       edgeMaybeFlagCodec DefaultTail TailStyle
    (\ flag -> case flag of 
        TailStyle a -> Just a
        _ -> Nothing
    )

markerFlag : Codec String (List String)
markerFlag =
       edgeMaybeFlagCodec "" Marker
    (\ flag -> case flag of 
        Marker a -> Just a
        _ -> Nothing
    )
  
headFlag : Codec HeadStyle (List String)
headFlag =
       edgeMaybeFlagCodec DefaultHead HeadStyle
    (\ flag -> case flag of 
        HeadStyle a -> Just a
        _ -> Nothing
    )

bendFlag : Codec Float (List String)
bendFlag =
       edgeMaybeFlagCodec 0.0 Bend
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

-- for backwards compatibility, we add conversion between 
-- the range [0,1] and [-5,5]
shiftSourceFlag : Codec Float (List String)
shiftSourceFlag =
       edgeMaybeFlagCodec 0.5 (\ x -> ShiftSource <| (x - 0.5) * 10)
    (\ flag -> case flag of 
        ShiftSource a -> Just <| (a + 5) / 10
        _ -> Nothing
    )

shiftTargetFlag : Codec Float (List String)
shiftTargetFlag =
       edgeMaybeFlagCodec 0.5 (\ x -> ShiftTarget <| (x - 0.5) * 10)
    (\ flag -> case flag of 
        ShiftTarget a -> Just <| (a + 5) / 10
        _ -> Nothing
    )

pullshoutFlag : Codec (Maybe PullshoutOffsets) (List String)
pullshoutFlag =
       edgeMaybeFlagCodec Nothing (Maybe.map Pullshout >> Maybe.withDefault Unrecognized)
    (\ flag -> case flag of 
        Pullshout a -> Just (Just a)
        _ -> Nothing
    )

kindFlag : Codec ArrowKind (List String)
kindFlag =
  let dec = (\ flag -> case flag of 
        Kind a -> Just a
        _ -> Nothing
       )
  in
  edgeFlagsCodec <|
  Codec.build 
    (\ a -> if a == NormalArrow then [] else [ Kind a ])
    (\ bs ->  List.Extra.findMap dec bs |> Maybe.withDefault NormalArrow)
    --    Codec.buildBetween NormalArrow Kind
    -- (\ flag -> case flag of 
    --     Kind a -> Just a
    --     _ -> Nothing
    -- )

getFlag : EdgeFlag -> ArrowStyle -> Bool
getFlag flag l =
  List.member (Codec.encoder edgeFlagCodec flag) l

addFlag : EdgeFlag -> ArrowStyle -> ArrowStyle
addFlag flag l =
  Codec.encoder edgeFlagCodec flag :: l

type alias ArrowStyle = --{ -- bend : Float, -- alignment : String, 
   List String
  --  }

pullshoutStyle : GraphDefs.PullshoutEdgeLabel -> ArrowStyle
pullshoutStyle {color, offset1, offset2} =
  -- { 
  --   flags = 
    Codec.encoder edgeFlagCodec (Pullshout {offset1 = offset1, offset2 = offset2})
        -- :: Codec.encoder flagCodec (Bend offset1)
        :: Codec.encoder colorsFlag
               { main = color, tail = color, head = color }
               --}

type alias Edge = { label : String, style : ArrowStyle,
       zindex : Int
      --  , selected : Bool 
       }

pullshoutEdge : Int -> GraphDefs.PullshoutEdgeLabel -> Edge
pullshoutEdge z label = 
    Edge "" (pullshoutStyle label) z -- False

type NodeFlag = 
      CoqValidated
    | Text
    | UnrecognizedNodeFlag

nodeFlagCodec : Codec NodeFlag String
nodeFlagCodec =
    Codec.customEnum (\ coq text unrecognized v ->
                    case v of
                        CoqValidated -> coq
                        Text -> text
                        UnrecognizedNodeFlag -> unrecognized
   )
   |> Codec.variant0 "coqValidated" CoqValidated
   |> Codec.variant0 "text" Text
   |> Codec.variant0 "" UnrecognizedNodeFlag
   |> Codec.buildVariant (always UnrecognizedNodeFlag)

type alias Node = { pos : Point , label : String, zindex: Int,
   flags : List String
  -- , selected : Bool
  }
type alias Tab = { 
      title: String,
      sizeGrid : Int,
      id : Int,
      nodes: List (Graph.Node Node),
      edges: List (Graph.Edge Edge),
      nextGraphId : Int
   }
type alias Graph = { 
      tabs : List Tab,
      latexPreamble : String,
      nextTabId : Int,
      activeTabId : Int}

defaultGraph : Graph
defaultGraph = { tabs = [], latexPreamble = "", nextTabId = 0, activeTabId = 0}


colorsFlag : Codec {main : Color, head : Color, tail : Color} (List String)
colorsFlag =
    edgeFlagsCodec <| Codec.build 
    ( \ {main, head, tail} -> 
       (if main == Color.black then [] else [Color main])
       ++
       (if head == main then [] else [HeadColor head])
       ++
       (if tail == main then [] else [TailColor tail])      
    )
    ( \ l -> 
       case 
        (List.Extra.findMap (\ v -> case v of Color c -> Just c 
                                              _ -> Nothing) l,
         List.Extra.findMap (\ v -> case v of HeadColor c -> Just c
                                              _ -> Nothing) l,
         List.Extra.findMap (\ v -> case v of TailColor c -> Just c
                                              _ -> Nothing) l)
        of 
        (mc, hc, tc) ->
            let c = mc |> Maybe.withDefault Color.black in
            { main = c, head = hc |> Maybe.withDefault c, tail = tc |> Maybe.withDefault c }
    )

dashedFlag : Codec Bool (List String)
dashedFlag = edgeMaybeFlagCodecFalse Dashed

textFlag : Codec Bool (List String)
textFlag = nodeMaybeFlagCodecFalse Text

coqValidatedFlag : Codec Bool (List String)
coqValidatedFlag = nodeMaybeFlagCodecFalse CoqValidated


wavyFlag : Codec Bool (List String)
wavyFlag = edgeMaybeFlagCodecFalse Wavy

arrowStyleCodec : Codec ArrowStyle.ArrowStyle ArrowStyle
arrowStyleCodec =
  let flagField f1 codec = 
        Codec.fields f1 Basics.identity codec
  in
  Codec.object
  (\tail head kind dashed bend alignment position shiftSource shiftTarget colors marker wavy ->
      { tail = tail, head = head, kind = kind
   , dashed = dashed, bend = bend, labelAlignment = alignment, 
   shiftSource = shiftSource, shiftTarget = shiftTarget,
   labelPosition = position, color = colors.main, marker = marker,
   headColor = colors.head, tailColor = colors.tail, wavy = wavy }
    
  )
  (\tail head kind dashed bend alignment position shiftSource shiftTarget colors marker wavy  ->
     shiftSource ++ shiftTarget ++ position ++ bend ++ marker ++ colors ++ dashed ++ alignment ++ tail ++ head ++ kind ++ wavy
    
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
  |> flagField (\{color, headColor, tailColor} -> {main = color, head = headColor, tail = tailColor})
                  colorsFlag
  |> flagField .marker markerFlag
  |> flagField .wavy wavyFlag
  |> Codec.buildObject


nodeCodec : Codec NodeLabel Node
nodeCodec =
   Codec.object
   (\ pos label isText zindex isCoqValidated ->
   { pos = pos, label = label
   , dims = Nothing,  weaklySelected = False, isMath = not isText,
     zindex = zindex, isCoqValidated = isCoqValidated , selected = False
     }
    )
    (\ pos label isText zindex isCoqValidated ->
    { pos = pos, label = label, flags = isText ++ isCoqValidated, zindex = zindex
      --, selected = selected
      })
    |> Codec.fields .pos .pos Codec.identity
    |> Codec.fields .label .label Codec.identity
    |> Codec.fields (.isMath >> not) .flags textFlag
    |> Codec.fields .zindex .zindex Codec.identity
    |> Codec.fields .isCoqValidated .flags coqValidatedFlag
    -- |> Codec.fields .selected .selected Codec.identity
    |> Codec.buildObject


fromEdgeLabel : EdgeLabel -> Edge
fromEdgeLabel e = 
   case e.details of
       PullshoutEdge l -> pullshoutEdge e.zindex l
       NormalEdge ({label, isAdjunction} as l)->
            let style = ArrowStyle.getStyle l in
            { label = label,
              -- kind = if isAdjunction then adjunctionKey else normalKey,       
              style = 
                let convertedStyle = Codec.encoder arrowStyleCodec style in 
                if isAdjunction then
                addFlag Adjunction convertedStyle
                else convertedStyle
                ,
              zindex = e.zindex 
              -- , selected = e.selected              
            }
     
toEdgeLabel : Edge -> EdgeLabel
toEdgeLabel { label, style, zindex } = 
    let dec codec = Codec.decoder codec style in
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
                , isAdjunction = getFlag Adjunction style
                , style = Codec.decoder arrowStyleCodec style
              , dims = Nothing
              }
   }

edgeCodec : Codec EdgeLabel Edge
edgeCodec = 
   Codec.build fromEdgeLabel toEdgeLabel



tabCodec : Codec  GraphInfo.Tab Tab 
tabCodec =
  Codec.object
  (\ graph title sizeGrid tabId ->
    { graph = graph,
      title = title, sizeGrid = sizeGrid,
      id = tabId
    }
  )
  (\ graph title sizeGrid tabId ->
    { nodes = graph.nodes,
      edges = graph.edges,
      nextGraphId = graph.nextId,
      title = title, sizeGrid = sizeGrid,
      id = tabId
    }
  )
  |> Codec.fields .graph (\e -> { nextId = e.nextGraphId, nodes = e.nodes, edges = e.edges}) 
     ( -- Codec.compose (Graph.nextIdCodec)
       (Codec.compose Graph.codec (Graph.mapCodec nodeCodec edgeCodec))
        )
  |> Codec.fields .title .title Codec.identity
  -- |> Codec.fields .active .active Codec.identity
  |> Codec.fields .sizeGrid .sizeGrid Codec.identity
  |> Codec.fields .id .id Codec.identity
  |> Codec.buildObject



graphInfoCodec : Codec GraphInfo Graph
graphInfoCodec = 
  Codec.object
  (\tabs nextTabId latexPreamble activeTabId ->
    { tabs = tabs, nextTabId = nextTabId, activeTabId = activeTabId, latexPreamble = latexPreamble }
  )
  (\tabs nextTabId latexPreamble activeTabId ->
    { tabs = tabs, nextTabId = nextTabId, activeTabId = activeTabId, latexPreamble = latexPreamble }
  )
  |> Codec.fields .tabs .tabs (Codec.list tabCodec)
  |> Codec.fields .nextTabId .nextTabId Codec.identity
  |> Codec.fields .latexPreamble .latexPreamble Codec.identity
  |> Codec.fields .activeTabId .activeTabId Codec.identity
  |> Codec.buildObject



toJSGraph : GraphInfo -> Graph
toJSGraph  g = Codec.encoder graphInfoCodec g

fromJSGraph : Graph -> GraphInfo
fromJSGraph = Codec.decoder graphInfoCodec
