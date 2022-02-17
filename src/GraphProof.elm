module GraphProof exposing (convertGraph, loopFrom, getAllDiagrams,
    isBorder, proofStatementToDebugString,
    proofStatementToString, loopToDiagram,
    fullProofs)

import GraphDefs exposing (NodeLabel, EdgeLabel)
import Polygraph as Graph exposing (Graph, NodeId, Edge, Node)
import Geometry.Point as Point exposing (Point)
import List.Extra as List
import Tuple exposing (first, second)
import Debug exposing (toString)
import IntDict exposing (IntDict)
import Collage.Layout exposing (debug)
import Set exposing (Set)

type alias LoopEdge = { pos : Point, angle : Float, label : String }
type alias LoopNode = { pos : Point }

convertGraph :  Graph NodeLabel EdgeLabel -> Graph LoopNode LoopEdge
convertGraph = 
    Graph.mapRecAll .pos 
             .pos
             (\ _ n -> { pos = n.pos })
             (\ _ fromP toP l -> 
                        { angle = Point.subtract toP fromP |> Point.pointToAngle ,
                          label = l.label,
                          pos = Point.middle fromP toP })

angleDir : Bool -> LoopEdge -> Float
angleDir dir edge = if dir then edge.angle else Point.flipAngle edge.angle

extremePath : Bool -> Graph.Id -> Graph LoopNode LoopEdge 
              -> Edge LoopEdge -> List (Edge LoopEdge, Bool)
extremePath direction target gi e =  
   let g = Graph.removeEdge e.id gi in
--    let _ = Debug.log "label" e in
--    let _ = Debug.log "target" target in
   let ret = (e, direction) in
   let end dir = if dir then .to else .from in   
   let nfId = end direction e in
   if nfId == target then [ ret ] else
      let angle = angleDir direction e.label in
      -- let angle = Point.subtract nfP niP |> Point.pointToAngle in
      let makeStuff dir l = l {- |> List.remove e -} |> List.map (\x -> (x, dir)) in
      let ins = Graph.incomings nfId g |> makeStuff False
          outs = Graph.outgoings nfId g |> makeStuff True
      in
      let finalAngle (edge, dir) =
              angleDir dir edge.label {- |> Point.subtract nfP 
                          |> Point.subtract (0,0)
                          |> Point.pointToAngle -} 
                          |> Point.distanceAngleSigned angle
      in
      let l = ins ++ outs in
    --   let _ = Debug.log "voisins" (List.map (\(ed, d) -> (finalAngle (ed, d), ed.label.label)) l) in
      let f = l |> List.maximumBy finalAngle
      in
              case f of 
                Nothing -> [ ret ]
                Just (eNext, dir) -> 
                  -- let _ = Debug.log "angle final" (finalAngle (eNext, dir)) in
                   ret :: extremePath dir target g eNext

loopFrom : Bool -> Graph LoopNode LoopEdge -> Edge LoopEdge -> List (Edge LoopEdge, Bool)
loopFrom direction g e = 
   extremePath direction (if direction then e.from else e.to) g e
   
type alias Diagram = { lhs : List (Edge LoopEdge), 
                       rhs : List (Edge LoopEdge)
                     }

invertDiagram : Diagram -> Diagram
invertDiagram { lhs, rhs } = { lhs = rhs, rhs = lhs }

-- is it the full diagram?
isBorder : Diagram -> Bool
isBorder {lhs, rhs} = 
   
   let makeAngles = List.map (.label >> .angle) in
   let anglesLhs = makeAngles lhs
       anglesRhs = makeAngles rhs |> List.map Point.flipAngle
   in
   
   let angles = (List.head anglesRhs |> Maybe.withDefault 0) :: anglesLhs
             ++ (anglesRhs |> List.reverse)
   in
   let _ = Debug.log "lhs" <| List.map (.label >> .label) lhs in
   let _ = Debug.log "lhs-angles" anglesLhs in
   
     
     Debug.log "rounds" (Point.countRounds angles) == -1

loopToDiagram : List (Edge LoopEdge, Bool) -> Diagram
loopToDiagram edges = 
    let findInitial l =
         case l of
           [] -> 0
           (_, False) :: (_, True) :: _ -> 1
           _ :: q -> findInitial q + 1
    in
    let (l1, l2) = List.splitAt (findInitial edges) edges in
    let lordered = l2 ++ l1 in
    { lhs = List.filter second lordered |> List.map first, 
      rhs = List.filterNot second lordered |> List.reverse |> List.map first }

-- edges: remaining edges to examine
getDiagrams : Graph LoopNode LoopEdge -> List (Edge LoopEdge, Bool) ->
           List Diagram
getDiagrams g edges =
   case edges of
      [] -> []
      (e, dir) :: tail -> 
         let loopEdges = loopFrom dir g e in
         let idLoopEdges = List.map (\(e2, dir2) -> (e2.id, dir2)) loopEdges in
         let tailBis = List.filterNot 
                        (\ (e2, dir2) -> List.member (e2.id, dir2) idLoopEdges)  
                        tail 
         in
          loopToDiagram loopEdges :: getDiagrams g tailBis         


getAllDiagrams : Graph LoopNode LoopEdge -> 
           List Diagram
getAllDiagrams g =
   let edges = Graph.edges g in
   let edgesR = List.map (\ e -> (e, True)) edges
       edgesL = List.map (\ e -> (e, False)) edges
   in
   let edgesFull = edgesR ++ edgesL in
     getDiagrams g edgesFull

type alias ProofStep = { diag : Diagram, startOffset : Int,
   backOffset : Int, endChain : List Graph.Id }

prefixProofStep id r = {r | startOffset = r.startOffset + 1
                         , endChain = id :: r.endChain}

applyDiag : Diagram -> List Graph.Id -> Maybe ProofStep
applyDiag d l =
   case List.stripPrefix (d.lhs |> List.map .id) l of
      Nothing -> 
         case l of
            [] -> Nothing
            t :: q -> applyDiag d q |> Maybe.map (prefixProofStep t)          
      Just tail ->
          Just ({diag = d
                , startOffset = 0
                , backOffset = List.length tail
                , endChain = List.map .id d.rhs ++ tail})

commuteProof : List Diagram -> List Graph.Id -> List ProofStep 
commuteProof diags l =
   case List.findMap (\ d -> applyDiag d l) diags of
      Nothing -> []
      Just step -> step :: commuteProof (List.remove step.diag diags) step.endChain 

type alias ProofStatement = { statement : Diagram, proof : List ProofStep }
finishedProof : ProofStatement -> Bool
finishedProof { statement, proof } = 
  List.reverse proof |> List.head |>
  Maybe.map (\ h -> h.endChain == List.map .id statement.rhs)
  |> Maybe.withDefault False

fullProofs : Graph LoopNode LoopEdge -> List ProofStatement
fullProofs g =
   let diags = getAllDiagrams g in
   let _ = List.map (Debug.log " " << statementToString) diags in
   let (bigDiags, smallDiags) = List.partition isBorder diags in
   let _ = Debug.log "nombre de gros diagrammes: " (List.length bigDiags) in
   let _ = Debug.log "nombre de petits diagrammes: " (List.length smallDiags) in
   List.filter finishedProof <|
   List.map (\d ->
       { statement = d, proof = d.lhs |> List.map .id |> commuteProof smallDiags
       }) <| List.map invertDiagram bigDiags 

statementToString : Diagram -> String
statementToString d = 
  let edgeToString = List.map (.label >> .label) >> String.join " · " in
  edgeToString d.lhs ++ " = " ++ edgeToString d.rhs

proofStepToString : ProofStep -> String
proofStepToString { startOffset, backOffset, diag} =
   let write0 n s = if n == 0 then [] else s in
   let do n s = case n of 
                0 -> []
                1 ->  ["  " ++ s]
                _ ->  ["  do " ++ String.fromInt n ++ " " ++ s] 
   in
   String.join "\n" <|
   [ "assert(eq : " ++ statementToString diag ++ ")."
   , "{"
   , "  admit."
   , "}"
   , "etrans."
   , "{" ]
   ++
   do backOffset "apply cancel_postcomposition."   
   ++
     write0 startOffset 
    (
    ([ "  repeat rewrite assoc'." ]
    ++
    do startOffset "apply cancel_precomposition."    
    ++ 
    [ "  repeat rewrite assoc." ]))
   ++
   [ "  apply eq."
   , "}" ]
   ++ (write0 startOffset ["repeat rewrite assoc."])
   ++ [
    "clear eq."
   ]



nodesOfDiag : Diagram -> Set Graph.NodeId
nodesOfDiag d =    
   List.map .from d.lhs ++ List.map .to d.rhs 
   |> Set.fromList
   

edgesOfDiag : Diagram -> IntDict ({from : Graph.Id, to : Graph.Id })
edgesOfDiag d = 
   let setOf e = (e.id, { from = e.from, to = e.to}) in
   List.map setOf d.lhs ++ List.map setOf d.rhs 
   |> IntDict.fromList
   

debugEdgeName id = "e" ++ String.fromInt id


renameDebugDiag : Diagram -> Diagram
renameDebugDiag diag = 
 let renameEdge e =
          let label = e.label in
          {e | label = { label | label = debugEdgeName e.id} }
  in
   {lhs = List.map renameEdge diag.lhs, 
    rhs = List.map renameEdge diag.rhs }

renameDebugProofStep : ProofStep -> ProofStep
renameDebugProofStep step =
   { step | diag = renameDebugDiag step.diag
     }



proofStatementToDebugString : ProofStatement -> String
proofStatementToDebugString st =
   let nodes = List.foldl Set.union Set.empty (List.map (.diag >> nodesOfDiag) st.proof) in
   let edges = List.foldl IntDict.union IntDict.empty (List.map (.diag >> edgesOfDiag) st.proof) in
   let nidS id = "o" ++ String.fromInt id in
   "Goal " 
   ++ "∏ (C : category)\n  "
   ++ (Set.toList nodes |> List.map (\ id -> "(" ++ nidS id ++ " : C)")
       |> String.join "")
   ++ "\n  "
   ++ (IntDict.toList edges |> 
      List.map (\(id, {from, to}) -> "(" ++ debugEdgeName id ++ " : " 
       ++ nidS from ++ " --> " ++ nidS to ++ ")") |> String.join "")
   ++ ",\n  "
   ++ statementToString (renameDebugDiag st.statement) ++ ".\n\nintros.\n"
   ++ (String.join "\n" <| List.map (renameDebugProofStep >> proofStepToString) st.proof)
   ++ "\n apply idpath."


proofStatementToString : ProofStatement -> String
proofStatementToString st =
   "Goal " ++ statementToString st.statement ++ ".\n\n"
   ++ (String.join "\n" <| List.map proofStepToString st.proof)
   ++ "\n apply idpath."
   ++ "\nQed."