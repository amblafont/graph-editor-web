module ParseLatex exposing (convertString, buildGraph)
import Parser exposing (Parser, variable, map, (|.), (|=), succeed, symbol, token, spaces, oneOf, end, lazy, andThen, backtrackable)
import Parser exposing (getChompedString)
import ParserExtra exposing (untilConsume, repeatSpaces, repeat)
import Maybe exposing (withDefault)
import Result
import GraphDefs exposing (NodeLabel, EdgeLabel)
import Polygraph as Graph
import ArrowStyle 


-- type alias Node = { x : Int, y : Int, label : String }

type NestedString = Cons String (List (NestedString, String) ) 



removeComments : Parser String
removeComments = succeed (String.join "") |= repeat ((Parser.chompUntilEndOr
 "%" |> getChompedString)
              |. Parser.chompUntilEndOr "\n")
nestedStringToString : NestedString -> String
nestedStringToString s =
   case s of
      Cons s1 s2 -> s1 
             ++ (List.map 
               (\ (sr, s3) -> "{" ++ nestedStringToString sr ++ "}" ++ s3) 
               s2
             |> String.join "")


suiteBracket : Parser (List (NestedString, String))
suiteBracket = 
   oneOf
   [
       succeed [] |. token "}"  ,
       succeed (\ a b tail -> (a, b) :: tail)       
       |= Parser.lazy (\ _ -> nestedBracket)
       |= (Parser.chompWhile (\c -> c /= '{' && c /= '}') |> getChompedString)
       |= lazy (\_ -> suiteBracket)
   ]

nestedBracket : Parser NestedString
nestedBracket = succeed identity |. Parser.token "{" 
    |= nestedBracketAlready

nestedBracketAlready : Parser NestedString
nestedBracketAlready =
  succeed Cons 
   |= (Parser.chompWhile (\c -> c /= '{' && c /= '}') 
   |> getChompedString)
   |=  suiteBracket

nestedBracketString : Parser String
nestedBracketString = Parser.succeed nestedStringToString |= nestedBracket

nestedBracketStringAlready : Parser String
nestedBracketStringAlready = Parser.succeed nestedStringToString |= nestedBracketAlready


nodeLabels : String -> List (List String)
nodeLabels s = String.split "\\\\" s |> List.map (String.split "\\&")
             |> List.map (List.map String.trim
             )



{- 
nodeLabelParser : Parser String
nodeLabelParser =
    oneOf [
    succeed String.trim |=
            variable
              { start = (\c -> c /= '& \\'),
                inner = (\c -> c /= '&'),
                reserved = Set.empty }
        ]

nodesLine : Parser (List String)
nodesLine =
Parser.sequence
    { start = ""
    , separator = "\&"
    , end = "\n"
    , spaces = succeed ()
    , item = statement
    , trailing = Optional
    }

nodesParser : Parser 
nodesParser = 
  Parser.sequence
    { start = "{"
    , separator = ";"
    , end = "}"
    , spaces = spaces
    , item = statement
    , trailing = Mandatory -- demand a trailing semi-colon
    }
 -}
parsePos : Parser (Int, Int)
parsePos = 
  succeed Tuple.pair |.
  token "(m-" |= Parser.int |. token "-" |= Parser.int |. token ")"

parseEdgeLabel : Parser String
parseEdgeLabel = 
    succeed identity
   |.
  ((token "edge[" |. spaces |. token "label" |. 
   Parser.chompUntil "=")
   |> getChompedString) 
   |. untilConsume "{"   
   |. spaces
   |. oneOf [ token "[" |. untilConsume "]" |. untilConsume "{",  succeed ()]
   |= nestedBracketStringAlready
   -- |. untilConsume "}"
   --|. Parser.map (Debug.log "offset avant") Parser.getOffset
   |. untilConsume "]"
   --|. Parser.map (Debug.log "offset apres") Parser.getOffset

type alias Edge = { from : (Int, Int), to : (Int, Int), label : String }

parseEdgeNext : Parser (List (String, (Int, Int)))
parseEdgeNext = 
   succeed identity |.
    spaces |=
      oneOf [
          succeed (\ label pos tail -> (label, pos) :: tail)
          |= parseEdgeLabel 
           |. spaces |= parsePos
           |= lazy (\_ -> parseEdgeNext),
          succeed []
      ]

parseEdges : Parser (List Edge)
parseEdges = 
     (succeed 
      (\ from s to l -> 
        { from = from, to = to, label = s} :: 
         List.map (\(label2, to2) -> { from = from, to = to2, label = label2})
           l

      ) |= parsePos |. spaces |= parseEdgeLabel |. spaces
       |= parsePos |= parseEdgeNext)
    |> repeatSpaces
    |> Parser.map List.concat

{- parseEdge : Parser Edge
parseEdge = succeed (\from label to -> {from = from, to = to, label = label})
    |= parsePos |. spaces |= (Parser.map (Debug.log "alors?") parseEdgeLabel) |. spaces
    |. Parser.map (Debug.log "offset") Parser.getOffset
     |= parsePos
 -}

{- parseEdges : Parser (List Edge)
parseEdges =  repeatSpaces parseEdge 
 -}{- Parser.sequence
    { start = ""
    , separator = ""
    , end = ""
    , spaces = spaces
    , item = parseEdge
    , trailing = Parser.Optional -- demand a trailing semi-colon
    } -}

parseBlocks : Parser (List String)
parseBlocks = repeatSpaces nestedBracketString

parseAll : Parser (List String)
parseAll = succeed identity |. spaces |. token "\\Diag" 
  |. spaces |. oneOf [ token "(" |. untilConsume ")", succeed ()] 
  |= parseBlocks

type alias Graph = (List (List String), List Edge)
convertString : String -> Maybe Graph
convertString s =
  let s2 = Parser.run removeComments s |> Result.withDefault ""
          |> String.replace "$" " "
   in
  case Parser.run parseAll s2 of
    Err e -> let _ = Debug.log "err" e in Nothing
    Ok [_ , nodes, edgesS ] -> 
       let _ = Debug.log "Edges" edgesS in
       case Parser.run parseEdges edgesS of
         Err e -> let _ = Debug.log "err2" e in Nothing
         Ok edges ->
            Just (nodeLabels nodes, edges)
    l -> let _ = Debug.log "err3" l in Nothing
  
buildGraph : Int -> Graph -> Graph.Graph NodeLabel EdgeLabel
buildGraph offset (nodes, edges) =
    let maxX = List.length nodes  + 1 in
    let mkPos x y = ((toFloat y + 0.5) * toFloat offset, 
                 (toFloat x + 0.5) * toFloat offset) in
    let mkId (x, y) = y * maxX + x in
    
    let nodesPos = List.indexedMap (\ x -> 
                  List.indexedMap (\ y s -> { id =  mkId (x + 1, y + 1)
                  , label = 
                      GraphDefs.newNodeLabel (mkPos x y) s True} ))
                  nodes |> List.concat
                  |> List.filter (\ { label } -> label.label /= "")  
                  |> Debug.log "nodes"
                  
    in
    let _ = Debug.log "ids" <| List.sort <| List.map .id nodesPos in
    
    List.foldl 
       (\ {from, to, label} g -> 
          Graph.newEdge g (from |> mkId |> Debug.log "idfrom") 
           (Debug.log "idto" <| 
               (to  |> mkId) )
            (GraphDefs.newEdgeLabel label ArrowStyle.empty)
            |> Tuple.first
       )
       (Graph.fromNodesAndEdges nodesPos [])
         edges
    
    
{- 

    SX \& Γ_{SSX} SX \& SSX \& \& SX \\
    \& Γ_{SX} SX \& Γ_{SX} SX \& \&  \\
    STX \& STΓ_{STX}X \& \& STSTX \\
     \& ST Γ_{SX}X \& \& STSX \& STX \\
    \& STX \\
    \& SX
  }{%
    (m-1-1) edge[labela={α^Γ_{SX,SSX}}] (m-1-2) %
    (m-1-5) edge[labela={η^S_{SX}}] (m-1-3) %
    (m-1-3) edge[labela={β^Γ_{SX,SSX}}] (m-1-2) %
    (m-1-2) edge[labelon={Γ_{μ^S_X}SX}] (m-2-2) %
    (m-1-1) edge[labelon={α^Γ_{SX,SX}}] (m-2-2) %
    (m-1-5) edge[labelon={β^Γ_{SX,SX}}] (m-2-3) %
    (m-2-3) edge[labelon={Γ_{η^S_{SX}}SX}] (m-1-2) %
    edge[identity] (m-2-2) %
    (m-2-2) edge[labelon={h_{X,X}}] (m-3-2) %
    (m-1-1) edge[labelon={Sηᵀ_X}] (m-3-1) %
    (m-3-1) edge[labela={ST α^Γ_{X,STX}}] (m-3-2) %
    (m-3-2) edge[labelon={STΓ_{S𝐚}X}] (m-4-2) %
    (m-3-1) edge[labelon={STα^Γ_{X,SX}}] (m-4-2) %
    (m-4-2) edge[labelon={ST𝐛}] (m-5-2) %
    (m-3-1) edge[identity] (m-5-2) %
    (m-5-2) edge[labelon={S𝐚}] (m-6-2) %
    (m-3-1) edge[labelon={S𝐚}] (m-6-2) %
    (m-1-1) edge[bend right=50,identity] (m-6-2) %
    (m-1-5) edge[labelon={S ηᵀ η^{ST} X}] (m-3-4) %
    (m-3-4) edge[labela={ST β^Γ_{X,STX}}] (m-3-2) %
    (m-3-4) edge[labelon={STS𝐚}] (m-4-4) %
    (m-4-4) edge[labela={ST β^Γ_{X,SX}}] (m-4-2) %
    (m-1-5) edge[labelon={Sηᵀ_X}] (m-4-5) %
    (m-4-5) edge[labela={STη^S X}] (m-4-4) %
    edge[identity] (m-5-2) %


 -}