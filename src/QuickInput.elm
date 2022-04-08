module QuickInput exposing (equalityParser, Equation, HandSide
   , splitWithChain, graphEquation,
   handsideStrings, edgesToHandside, edgeMapString,
   splitWithChainBetweenNodes)
-- This is not used anymore
import Parser exposing (Parser, variable, map, (|.), (|=), succeed, symbol, spaces, oneOf, end, lazy, andThen, backtrackable)
import ParserExtra as Parser
import Set
import Geometry.Point as Point exposing (Point)
import GraphDefs exposing (NodeLabel, EdgeLabel)
import Polygraph as Graph exposing (Graph, EdgeId, NodeId, Edge)
import List
import List.Extra as List
import ArrowStyle

endArrowChar = '⟩'
startSymbol = "--"
endSymbol = "->"


-- type alias EdgeTo a =  { a | edge : String, to : String } 
type alias Edge = {from : String, label : String, to : String } -- { from : String, to : String, edge : String }
type alias HandSide = List Edge -- { start : String, edges : List (EdgeTo a) }

{- consHandSide : String -> String -> HandSide -> HandSide
consHandSide source label h =
   { start = source, edges = {edge = label, to = h.start}}
 -}
emptyHandSide : HandSide
emptyHandSide = [] -- { start = "", edges = []}

type alias Equation = (HandSide, HandSide)


correctLabelChar : List Char -> Char -> Bool
correctLabelChar fb c = not <| List.member c <| fb -- ++ ['|', '-', '<', '>'] 

labelParser : List Char -> Parser String
labelParser fb = 
   succeed String.trim |=
            variable
              { start = correctLabelChar fb,
                inner = correctLabelChar fb,
                reserved = Set.empty}
        


vertexParser : Parser String
vertexParser = labelParser ['-', '='] -- specialChars


type alias EdgeTo = { label : String, to : String } 
makeHandSide : String -> List EdgeTo -> List Edge
makeHandSide from l =
   case l of
      [] -> []
      {to, label} :: q -> { from = from, to = to, label = label} ::
                           makeHandSide to q
       

edgeParser : Parser EdgeTo
edgeParser = 
             oneOf[
               succeed EdgeTo
           |. symbol startSymbol
           |. spaces
              -- some label?
           |= vertexParser -- labelParser [endArrowChar]
           |. spaces
           |. symbol endSymbol --(endArrowChar |> String.fromChar)
           |= vertexParser
         
                 ]

handSideParser : Parser HandSide
handSideParser =
    succeed makeHandSide
        |= vertexParser
        |= Parser.repeat edgeParser
            

equalityParser : Parser (HandSide, HandSide)
equalityParser =    
      succeed Tuple.pair
            |. spaces
            |= handSideParser 
            |. spaces
            |. symbol "="
            |. spaces 
            |= handSideParser
           


-- TODO
-- replaces the given edge with a composition of edges
splitWithChain : Graph NodeLabel EdgeLabel -> HandSide -> EdgeId -> Graph NodeLabel EdgeLabel
splitWithChain g ch id =
   -- to get the position
   -- let gd = GraphDrawing.toDrawingGraph g in
   Graph.getEdge id g |> Maybe.andThen (\ edge ->
     splitWithChainBetweenNodes g ch edge.from edge.to
     |> Maybe.map (Graph.removeEdge id)
    )
    |> Maybe.withDefault g
   -- let g2 = Graph.s

splitWithChainBetweenNodes : Graph NodeLabel EdgeLabel -> HandSide -> NodeId -> NodeId -> Maybe (Graph NodeLabel EdgeLabel)
splitWithChainBetweenNodes g ch id1 id2 = 
    let on1 = Graph.getNode id1 g
        on2 = Graph.getNode id2 g
    in
    case (on1, on2) of
        (Just n1, Just n2) ->
            Just <| buildGraphSegment 
            { from = n1.pos, to = n2.pos,
              fromId = id1, toId = id2,
              edges = ch,
              alignLeft = True           
            } g                 
        _ -> Nothing
    
    
    

buildGraphSegment : Segment 
     -> Graph NodeLabel EdgeLabel 
     -> Graph NodeLabel EdgeLabel 
buildGraphSegment s g =
    let offset = Point.subtract s.to s.from
             |> Point.resize (1 / (List.length s.edges |> toFloat))
    in
      buildGraphEdges g offset 
        (if s.alignLeft then ArrowStyle.Left else ArrowStyle.Right)
        s.from s.fromId s.toId s.edges


buildGraphEdges : Graph NodeLabel EdgeLabel -> Point -> ArrowStyle.LabelAlignment 
           -> Point -> NodeId -> NodeId -> List Edge -> Graph NodeLabel EdgeLabel
buildGraphEdges g offset alignment pos from to ch =
   let style =
          let st= ArrowStyle.empty in
           { st | labelAlignment = alignment } 
   in
   case ch of
       [] -> g
       [ e ] -> 
          Tuple.first <| Graph.newEdge g from to
                        <| GraphDefs.newEdgeLabel e.label style
       e :: tail -> 
          let posf = Point.add offset pos in          
          let (g2, idto, _ ) = GraphDefs.createNodeLabel g e.to posf in          
          let (g3, _) = Graph.newEdge g2 from idto
                        <| GraphDefs.newEdgeLabel e.label
                        <| style
          in
            buildGraphEdges g3 offset alignment posf idto to tail


type alias Segment = { edges : List Edge, from : Point, fromId : NodeId, to : Point, toId : NodeId,
                      alignLeft : Bool }

-- it implicitly assumes that source and targets are not empty
orientEquation : Point -> Equation -> Float -> Graph NodeLabel EdgeLabel ->
                   (Graph NodeLabel EdgeLabel, 
                   List Segment)
orientEquation iniP (source, but) offset g =
   
   let nsource = List.length source
       nbut    = List.length but
   in
   
   let ni1 = (nsource + 1) // 2
       nf1 = (nbut + 1) // 2
   in
   let (source1, source2) = List.splitAt ni1 source
       (but1, but2) = List.splitAt nf1 but
   in
   let ni2 = nsource - ni1
       nf2 = nbut - nf1
   in
   let maxud = max ni1 nf2 
       maxlr = max ni2 nf1
   in
   let (leftX, topY)  = iniP in
   let
       bottomY = topY + toFloat maxlr * offset 
       rightX = leftX + toFloat maxud * offset
   in
   let topLeftPos = (leftX, topY)
       bottomRightPos = (rightX, bottomY)
       topRightPos = (rightX, topY)
       bottomLeftPos = (leftX, bottomY)
   in
   let startLabel = source |> List.head |> Maybe.map .from |> Maybe.withDefault ""  in
   let lastLabel = List.last >> Maybe.map .to >> Maybe.withDefault "" in
   let endLabel = lastLabel source
       topRightLabel = lastLabel source1 
       bottomLeftLabel = lastLabel but1
   in
   
   let (g2, topLeftId, _)    = GraphDefs.createNodeLabel g  startLabel topLeftPos in
   let (g3, bottomRightId, _) = GraphDefs.createNodeLabel g2 endLabel   bottomRightPos   in
   let (g4, topRightId, _) = 
        if source2 == [] then
          (g3, bottomRightId, iniP)
        else
           GraphDefs.createNodeLabel g3 topRightLabel topRightPos   in
   let (g5, bottomLeftId, _) = 
        if but2 == [] then
          (g4, bottomRightId, iniP)
        else   
           GraphDefs.createNodeLabel g4 bottomLeftLabel bottomLeftPos in
   (g5, 
   [
      { edges = source1, from = topLeftPos, fromId = topLeftId, to = topRightPos, toId = topRightId, alignLeft = True }
    , { edges = but1, from = topLeftPos, fromId = topLeftId, to = bottomLeftPos, toId = bottomLeftId, alignLeft = False }
    , { edges = source2, from = topRightPos, fromId = topRightId, to = bottomRightPos, toId = bottomRightId, alignLeft = True }    
    , { edges = but2, from = bottomLeftPos, fromId = bottomLeftId, to = bottomRightPos, toId = bottomRightId, alignLeft = False }
   ])
   


graphEquation : Point -> Float -> Equation -> Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
graphEquation pos offset eq gi =
    let (gf, l) = orientEquation pos eq offset gi in
    List.foldl 
       buildGraphSegment
       gf l



edgesToHandside : Graph NodeLabel EdgeLabel -> List EdgeId -> HandSide
edgesToHandside g l =  
    let g2 = Graph.keepBelow l g in
    let g3 = Graph.mapRecAll identity .label 
         (always .label)
         (\ _ s t e -> {from = s, to = t, label = e.label} )
         g2
    in
    let edges = Graph.getEdges l g3 in
    if Graph.isPolyLine edges then
       List.map .label edges
    else
       []

handsideStrings : HandSide -> List String
handsideStrings l = 
  case l of 
    [] -> []
    [ t ] -> [t.from, t.label, t.to ]
    t :: q -> t.from :: t.label :: handsideStrings q

edgeMapString : (String -> String) -> Edge -> Edge
edgeMapString f { from, label, to } =
    { from = f from, label = f label, to = f to }


{- 
labelledEdgesToHandSide : List { source : String, but : String, label : String} 
   -> HandSide
labelledEdgesToHandSide l =
   let l2 = List.map (\ e -> {edge = e.label, to = e.but}) in
   case l of
       [] -> emptyHandSide
       t :: q -> { start = t.start, edges = l2}

edgesToHandside : List EdgeId -> Graph NodeLabel EdgeLabel -> HandSide
edgesToHandside l g =  
    let g2 = Graph.keepBelow l g in
    let g3 = Graph.mapRecAll identity .label 
         (always .label)
         (\ _ s t e -> {source = s, but = t, label = e.label} )
         g2
    in
    let edges = Graph.getEdges l g3 in
    labelledEdgesToHandSide edges

-- maybe not necessary?
diagramToHandside : Diagram -> Graph NodeLabel EdgeLabel -> (HandSide, HandSide)
diagramToHandside d g =
  let make_handSide l = edgeToHandside (List.map .id l) g in
   (make_handSide d.lhs, 
    make_handSide d.rhs) -}
   