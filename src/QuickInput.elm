module QuickInput exposing (equalityParser, Equation, HandSide
   , splitWithChain, graphEquation)
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
import Geometry
import GraphProof

endArrowChar = '⟩'
startSymbol = "--"
endSymbol = "->"



type alias Edge = { from : String, edge : String, to : String } 
type alias HandSide = List Edge
-- TODO: redefine HandSide as List { start:String, edge:Edge }



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


type alias ShortEdge = { edge : String, to : String } 
type alias ShortHandSide = { start : String, edges : List ShortEdge}

handSideFromShort : ShortHandSide -> HandSide
handSideFromShort {start, edges} =
   case edges of
     [] -> []
     t :: q -> { from = start, edge = t.edge , to = t.to} 
         :: handSideFromShort { start = t.to, edges = q}

edgeParser : Parser ShortEdge
edgeParser = 
             oneOf[
               succeed ShortEdge
           |. symbol startSymbol
           |. spaces
              -- some label?
           |= vertexParser -- labelParser [endArrowChar]
           |. spaces
           |. symbol endSymbol --(endArrowChar |> String.fromChar)
           |= vertexParser
         
                 ]

shortHandSideParser : Parser ShortHandSide
shortHandSideParser =
    succeed ShortHandSide
        |= vertexParser
        |= Parser.repeat edgeParser

handSideParser : Parser HandSide
handSideParser =
    succeed handSideFromShort |= shortHandSideParser
            

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
splitWithChain : Graph NodeLabel EdgeLabel -> Graph.ModifHelper NodeLabel EdgeLabel -> HandSide -> EdgeId -> Graph.ModifHelper NodeLabel EdgeLabel
splitWithChain g modifiedGraph ch id =
   -- to get the position
   -- let gd = GraphDrawing.toDrawingGraph g in
--    let modifiedGraph = Graph.newModif g in
   Graph.getEdge id g |> Maybe.map (\ edge ->
    let on1 = Graph.getNode edge.from g
        on2 = Graph.getNode edge.to g
    in
    
    case (on1, on2) of
        (Just n1, Just n2) ->
            buildGraphSegment 
            { from = n1.pos, to = n2.pos,
              fromId = edge.from, toId = edge.to,
              edges = ch,
              alignLeft = True           
            }
            <| (Graph.md_removeEdge edge.id modifiedGraph)            
        _ -> modifiedGraph
    )
    |> Maybe.withDefault modifiedGraph
   -- let g2 = Graph.s

buildGraphSegment : Segment 
     -> Graph.ModifHelper NodeLabel EdgeLabel 
     -> Graph.ModifHelper NodeLabel EdgeLabel 
buildGraphSegment s g =
    let offset = Point.subtract s.to s.from
             |> Point.resize (1 / (List.length s.edges |> toFloat))
    in
      buildGraphEdges g offset 
        (if s.alignLeft then Geometry.Left else Geometry.Right)
        s.from s.fromId s.toId s.edges


buildGraphEdges : Graph.ModifHelper NodeLabel EdgeLabel -> Point -> Geometry.LabelAlignment 
           -> Point -> NodeId -> NodeId -> List Edge -> Graph.ModifHelper NodeLabel EdgeLabel
buildGraphEdges g offset alignment pos from to ch =
   let style =
          let st= ArrowStyle.empty in
           { st | labelAlignment = alignment } 
   in
   case ch of
       [] -> g
       [ e ] -> 
          Tuple.first <| Graph.md_newEdge g from to
                        <| GraphDefs.newEdgeLabel e.edge style
       e :: tail -> 
          let posf = Point.add offset pos in          
          let (g2, idto, _ ) = GraphDefs.md_createNodeLabel g e.to posf in          
          let (g3, _) = Graph.md_newEdge g2 from idto
                        <| GraphDefs.newEdgeLabel e.edge
                        <| style
          in
            buildGraphEdges g3 offset alignment posf idto to tail


type alias Segment = { edges : List Edge, from : Point, fromId : NodeId, to : Point, toId : NodeId,
                      alignLeft : Bool }

-- it implicitly assumes that source and targets are not empty
orientEquation : Point -> Equation -> Float -> Graph NodeLabel EdgeLabel ->
                   (Graph.ModifHelper NodeLabel EdgeLabel, 
                   List Segment)
orientEquation iniP (source, but) offset origG =
   let g = Graph.newModif origG in
   
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
   let startLabel = List.head source |> Maybe.map .from |> Maybe.withDefault "" in
   let lastLabel = List.last >> Maybe.map .to >> Maybe.withDefault "" in
   let endLabel = lastLabel source
       topRightLabel = lastLabel source1 
       bottomLeftLabel = lastLabel but1
   in
   
   let (g2, topLeftId, _)    = GraphDefs.md_createNodeLabel g  startLabel topLeftPos in
   let (g3, bottomRightId, _) = GraphDefs.md_createNodeLabel g2 endLabel   bottomRightPos   in
   let (g4, topRightId, _) = 
        if source2 == [] then
          (g3, bottomRightId, iniP)
        else
           GraphDefs.md_createNodeLabel g3 topRightLabel topRightPos   in
   let (g5, bottomLeftId, _) = 
        if but2 == [] then
          (g4, bottomRightId, iniP)
        else   
           GraphDefs.md_createNodeLabel g4 bottomLeftLabel bottomLeftPos in
   (g5, 
   [
      { edges = source1, from = topLeftPos, fromId = topLeftId, to = topRightPos, toId = topRightId, alignLeft = True }
    , { edges = but1, from = topLeftPos, fromId = topLeftId, to = bottomLeftPos, toId = bottomLeftId, alignLeft = False }
    , { edges = source2, from = topRightPos, fromId = topRightId, to = bottomRightPos, toId = bottomRightId, alignLeft = True }    
    , { edges = but2, from = bottomLeftPos, fromId = bottomLeftId, to = bottomRightPos, toId = bottomRightId, alignLeft = False }
   ])
   


graphEquation : Point -> Float -> Equation -> Graph NodeLabel EdgeLabel -> Graph.ModifHelper NodeLabel EdgeLabel
graphEquation pos offset eq gi =
    let (gf, l) = orientEquation pos eq offset gi in
    List.foldl 
       buildGraphSegment
       gf l
    

   