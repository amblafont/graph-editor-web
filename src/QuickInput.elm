module QuickInput exposing (equalityParser, Orient,  
   orientToPoint, orientEquation, Equation, defOrient, NonEmptyChain(..), Edge(..)
   , splitWithChain, graphEquation)
-- This is not used anymore
import Parser exposing (Parser, variable, map, (|.), (|=), succeed, symbol, spaces, oneOf, end, lazy, andThen, backtrackable)
import ParserExtra as Parser
import Set
import Html exposing (source)
import Geometry.Point as Point exposing (Point)
import GraphDefs exposing (NodeLabel, EdgeLabel)
import Polygraph as Graph exposing (Graph, EdgeId, NodeId, Edge)
import List exposing (length)
import ArrowStyle
import Geometry exposing (PosDims)

endArrowChar = '⟩'
type alias Vertex = String
type OrientDir = Up | Down | Left | Right
type alias Orient = { dir : OrientDir, strength : Float }

defOrient : Orient
defOrient = { dir = Right, strength = 1 }

type Edge = Edge (Maybe String) (Maybe Orient)
-- TODO: changer ca en Vertex , List (Vertex Edge)
type NonEmptyChain = Singleton Vertex
           | Cons Vertex Edge NonEmptyChain 

{- reverseChainAcc : NonEmptyChain -> NonEmptyChain -> Edge -> NonEmptyChain
reverseChainAcc c acc edgeAcc = case c of
              Singleton v -> Cons v edgeAcc acc
              Cons v e tail -> reverseChainAcc tail (Cons v edgeAcc acc) e 


reverseChain : NonEmptyChain -> NonEmptyChain
reverseChain c = case c of
              Singleton _ -> c
              Cons v e tail -> reverseChainAcc tail (Singleton v) e
 -}
type alias Equation = (NonEmptyChain, NonEmptyChain)
lengthChain : NonEmptyChain -> Int
lengthChain c = case c of
   Singleton _ -> 0
   Cons _ _ tail -> 1 + lengthChain tail

orientChainSplitAt : Int -> Orient -> Orient -> NonEmptyChain -> NonEmptyChain 
orientChainSplitAt n firsts lasts c = case c of 
   Singleton s -> Singleton s
   Cons v (Edge s o) ch -> 
      let o2 = if n > 0 then firsts else lasts in
      Cons v (Edge s (Just (Maybe.withDefault o2 o)))
      <| orientChainSplitAt (n - 1) firsts lasts ch

{- orientChainSplit : Orient -> Orient -> NonEmptyChain -> NonEmptyChain 
orientChainSplit firsts lasts chain =
   let n = lengthChain chain in
   orientChainSplitAt (n // 2) firsts lasts chain
 -}
orientEquation : Equation -> Equation
orientEquation (source, but) =
   let nsource = lengthChain source 
       nbut    = lengthChain but
   in
   let ni1 = nsource // 2
       nf1 = nbut // 2
   in
   let ni2 = nsource - ni1
       nf2 = nbut - nf1
   in
   let maxud = max ni1 nf2 
       maxlr = max ni2 nf1
   in
   let mkOri d m n = { dir = d, strength = toFloat m / toFloat n } in
   
   (orientChainSplitAt ni1 (mkOri Right maxud ni1) (mkOri Down maxlr ni2) source,
    orientChainSplitAt nf1 (mkOri Down maxlr nf1)   (mkOri Right maxud nf2) but
    )


orientToPoint : Maybe Orient -> Point
orientToPoint o =
    let {strength, dir} = Maybe.withDefault defOrient o in
    Point.resize strength <| case dir of
        Up   -> (0, -1)
        Down -> (0, 1)
        Left -> (-1, 0)
        Right -> (1, 0)

-- specialChars = ['|', '-', '<', '>'] 
correctLabelChar : List Char -> Char -> Bool
correctLabelChar fb c = not <| List.member c <| fb -- ++ ['|', '-', '<', '>'] 

labelParser : List Char -> Parser String
labelParser fb = 
    oneOf [
     succeed identity |. symbol "\"" |= 
      variable { start = \ c -> c /= '\"',
                 inner = \ c -> c /= '\"',
                 reserved = Set.empty} |. symbol "\""
    , succeed String.trim |=
            variable
              { start = correctLabelChar fb,
                inner = correctLabelChar fb,
                reserved = Set.empty}
        ]


vertexParser : Parser Vertex
vertexParser = labelParser ['-', '='] -- specialChars

{- 
orientParser : Parser Orient
orientParser = oneOf
               [succeed Left  |. symbol "l", 
                succeed Right |. symbol "r", 
                succeed Up    |. symbol "u", 
                succeed Down  |. symbol "d" ] -}

edgeParser : Parser Edge
edgeParser = 
             oneOf[
               succeed Edge
           |. symbol "-"
           |. spaces
              -- some label?
           |= Parser.maybe (labelParser [endArrowChar, '-'])
           |. spaces
           |. symbol (endArrowChar |> String.fromChar)
           |= succeed Nothing
           -- |= Parser.maybe (succeed identity |. backtrackable (symbol "@") |= orientParser)
         {-   , succeed Edge
           |. symbol "-"
           |. spaces
              -- some label?
           |= Parser.maybe (labelParser specialChars)
           |. spaces
           |. symbol ">"
           |= Parser.maybe (succeed identity |. backtrackable (symbol "@") |= orientParser)

           ,  succeed Edge
           |. symbol "|"
           |. spaces
              -- some label?
           |= Parser.maybe (labelParser (['v', '^'] ++ specialChars)) 
           |. spaces
           |= oneOf [
                succeed (Just Down) |. symbol "v",
                succeed (Just Up) |. symbol "^"
               ]
           
 -}
                 ]

nonEmptyChainParser : Parser NonEmptyChain
nonEmptyChainParser =
        vertexParser |> andThen
            (\v ->
                 succeed identity |. spaces |=
                 oneOf [ 
                  succeed (Cons v) |= backtrackable edgeParser |. spaces
                      |= (oneOf [lazy (\_ -> nonEmptyChainParser), succeed <| Singleton "" ])
                  , succeed <| Singleton v ]
            )


chainParser : Parser (Maybe NonEmptyChain)
chainParser =
    succeed identity |. spaces |=
    oneOf [ succeed Nothing |. end,
            map Just nonEmptyChainParser 
          ]

equalityParser : Parser (Maybe (NonEmptyChain, NonEmptyChain))
equalityParser =
     succeed identity |. spaces |=
     oneOf [ succeed Nothing |. end,
      succeed (\a b -> Just (a, b))      
            |= nonEmptyChainParser 
            |. spaces
            |. symbol "="
            |. spaces 
            |= nonEmptyChainParser
           ]


-- TODO
-- replaces the given edge with a composition of edges
splitWithChain : Graph NodeLabel EdgeLabel -> NonEmptyChain -> EdgeId -> Graph NodeLabel EdgeLabel
splitWithChain g ch id =
   -- to get the position
   -- let gd = GraphDrawing.toDrawingGraph g in
   Graph.getEdge id g |> Maybe.map (\ edge ->
    let on1 = Graph.getNode edge.from g
        on2 = Graph.getNode edge.to g
    in
    case (on1, on2) of
        (Just n1, Just n2) ->
            let offset = Point.subtract n2.pos n1.pos
                       |> Point.resize (1 / (lengthChain ch |> toFloat))
            in
            let pos = n1.pos in -- Point.add n1.pos offset in
             buildChain (Graph.removeEdge id g) offset pos edge.from edge.to ch 
        _ -> g
    )
    |> Maybe.withDefault g
   -- let g2 = Graph.s

buildChain : Graph NodeLabel EdgeLabel -> Point -> Point -> NodeId -> NodeId -> NonEmptyChain -> Graph NodeLabel EdgeLabel
buildChain g offset pos from to ch = 
  case ch of
      Singleton _ -> g
      Cons _ (Edge olabel _) tail ->
          let label = Maybe.withDefault "" olabel in
          let (g3, idto) = buildChainRec g offset (Point.add pos offset) to tail in
             Tuple.first <|  Graph.newEdge g3 from idto
               <| GraphDefs.newEdgeLabel label ArrowStyle.empty            

buildChainRec : Graph NodeLabel EdgeLabel -> Point -> Point -> NodeId -> NonEmptyChain -> (Graph NodeLabel EdgeLabel, NodeId)
buildChainRec g offset pos to ch =
   case ch of
       Singleton _ -> (g, to)
       Cons v (Edge olabel _) tail -> 
          let label = Maybe.withDefault "" olabel in
          let (g2, id, _ ) = GraphDefs.createNodeLabel g v pos in
          let (g3, idto) = buildChainRec g2 offset (Point.add pos offset) to tail in
          let (g4, _) = Graph.newEdge g3 id idto
                        <| GraphDefs.newEdgeLabel label ArrowStyle.empty            
          in
            (g4, id)

finalNodeLabel : Point -> Float -> Equation -> NodeLabel
finalNodeLabel pos offset (eq1, eq2) =
   let longCh = if lengthChain eq1 > lengthChain eq2 then eq1 else eq2 in
   finalNodeLabelChain pos offset longCh

initialNodeLabel : Point -> Equation -> NodeLabel
initialNodeLabel pos (eq1, eq2) =
   case eq1 of
     Singleton v ->  GraphDefs.newNodeLabel pos v
     Cons v _ _ ->   GraphDefs.newNodeLabel pos v
  
nextPos : Point -> Float -> Maybe Orient -> Point
nextPos pos offset o =
   (Point.add pos (Point.resize offset 
            (orientToPoint o)))
finalNodeLabelChain : Point -> Float -> NonEmptyChain -> NodeLabel
finalNodeLabelChain pos offset ch =
   case ch of
       Singleton s -> GraphDefs.newNodeLabel pos s
       Cons _ (Edge _ orient) tail -> 
           finalNodeLabelChain 
           (nextPos pos offset orient)
            offset
            tail

buildGraphChain : Float -> NodeId -> NodeId -> Point -> NonEmptyChain -> 
   Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
buildGraphChain offset start final pos ch g =
   case ch of
       Singleton _ -> g
       Cons _ (Edge l _) (Singleton _) ->
           Tuple.first <| Graph.newEdge g start final
           <| GraphDefs.newEdgeLabel (Maybe.withDefault "" l)
              ArrowStyle.empty
       Cons _ (Edge l o) (Cons v e tail) ->
            let npos = nextPos pos offset o in
            let (g2, id, _ ) = GraphDefs.createNodeLabel g v npos in
            let (g3, _) = Graph.newEdge g2 start id 
                  <| GraphDefs.newEdgeLabel (Maybe.withDefault "" l)
                     ArrowStyle.empty
            in
            buildGraphChain offset id final npos (Cons v e tail)
            g3

graphEquation : Point -> Float -> Equation -> Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
graphEquation pos offset eq g =
    let labelf = finalNodeLabel pos offset eq in
    let labeli = initialNodeLabel pos eq in
    let (g2, finalId) = Graph.newNode g labelf in
    let (g3, startId) = Graph.newNode g2 labeli in
    let (eq1, eq2) = eq in
    g3 
    |>  buildGraphChain offset  startId finalId pos eq1
    |>  buildGraphChain offset  startId finalId pos eq2

   