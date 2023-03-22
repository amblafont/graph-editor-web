module GraphProof exposing (loopFrom, getAllValidDiagrams,
    isBorder, proofStatementToDebugString,
    proofStatementToString, proofStepToString, loopToDiagram,
    fullProofs, getIncompleteDiagram, isEmptyBranch, generateIncompleteProofStepFromDiagram, 
    LoopEdge, LoopNode, Diagram, incompleteProofStepToString,
    nodesOfDiag, edgesOfDiag)

import Polygraph as Graph exposing (Graph, NodeId, Edge, Node)
import Geometry.Point as Point exposing (Point)
import List.Extra as List
import ListExtraExtra as List
import Tuple exposing (first, second)
import IntDict exposing (IntDict)
import Collage.Layout exposing (debug)
import Set exposing (Set)
import Maybe.Extra as Maybe

type alias LoopEdge = { pos : Point, angle : Float, label : String, identity : Bool }
type alias LoopNode = { pos : Point, label : String }


nameIdentities : Graph LoopNode LoopEdge -> Graph LoopNode LoopEdge 
nameIdentities =
  Graph.mapRecAll (\n -> n.label)
             (\n -> n.label)
             (\ _ n -> n)
             (\ _ fromLabel _ l -> 
                        { l | label = 
                           if l.label == "" && l.identity then
                             "|" ++ fromLabel ++ "|" 
                           else l.label })

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

diagramFrom : Bool -> Graph LoopNode LoopEdge -> Edge LoopEdge -> Diagram
diagramFrom dir g e =
     let loopEdges = loopFrom dir g e in
          loopToDiagram loopEdges

   
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
   let _ = List.map (.label >> .label) lhs in
   let _ = anglesLhs in
   
     
     (Point.countRounds angles) == -1

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
    let (lhs0, rhs0) = List.partition second lordered in
    let lhs = List.map first lhs0
        rhs = List.reverse rhs0 |> List.map first
    in     
    { lhs = lhs,
      rhs = rhs }

getAllValidDiagrams : Graph LoopNode LoopEdge -> List Diagram
getAllValidDiagrams g =
   let inc = Graph.incidence g 
         |> IntDict.map 
          (\ _ i -> List.map (\e -> { edge = e, incoming = True } ) i.incomings
                 ++ List.map (\e -> { edge = e, incoming = False} ) i.outgoings
                 |> List.sortBy 
                     (\{edge, incoming} -> 
                         if incoming then edge.label.angle else 
                         Point.flipAngle edge.label.angle |> Point.normaliseAngle
                     )
                 |> List.succCyclePairs
          )
   in
   -- il faut deux next pour les deux sens
   let treatEdge (e1 ,e2) (start, next1, next2) =         
         case (e1.incoming, e2.incoming) of
           (True, True) -> (start, next1, next2)
           (False, False) -> 
              -- let _ = Debug.log "startEdge " (e1.edge.label.label, e2.edge.label.label) in
               ((e2.edge, e1.edge) :: start, next1, next2)
           (True, False) -> 
             -- let _ = Debug.log "succEdge " (e1.edge.label.label, e2.edge.label.label) in
              (start, IntDict.insert e1.edge.id e2.edge next1, next2)
           (False, True) -> 
              --let _ = Debug.log "AutresuccEdge'" (e1.edge.label.label, e2.edge.label.label) in
               (start, next1, IntDict.insert e2.edge.id e1.edge next2)
   in
   let buildNextStarts es (start, next1, next2) =
         List.foldl treatEdge (start, next1, next2) es
   in
   let (start, next1, next2) = IntDict.foldl (\ _ -> buildNextStarts) 
                     ([], IntDict.empty, IntDict.empty) inc 
   in
   let buildBranch next startEdge =
        case IntDict.get startEdge.id next of
          Nothing -> [ startEdge ]
          Just e -> startEdge :: buildBranch next e
   in
   let diags = List.map 
         (\ (rhs, lhs) -> 
           {rhs = buildBranch next1 rhs, 
            lhs = buildBranch next2 lhs})
         start
   in
   --let _ = List.map (statementToString >> Debug.log "a ") diags in
   let validDiag { lhs, rhs } =
        case (List.last lhs, List.last rhs) of
         (Just e1, Just e2) -> e1.to == e2.to
         _ -> False
   in
   let validDiags = List.filter validDiag diags in
   -- let _ = List.map (statementToString >> Debug.log "d ") validDiags in
    validDiags
     
                

{-
Returns the one diagram that the given input list of edges represents
(if the input list indeed represents a diagram)
-}
getIncompleteDiagram : Graph LoopNode LoopEdge -> List (Edge LoopEdge) ->
           Maybe Diagram
getIncompleteDiagram g l =
   case l of
       [ ] -> Nothing
       e :: _ ->
            let makeDiag dir = 
                  let d = diagramFrom dir g e in
                   
                  if List.sort (List.map .id l) == List.sort (List.map .id (d.lhs ++ d.rhs)) then
                     Just <| d
                  else
                     Nothing
                   
            in
            Maybe.or (makeDiag True) (makeDiag False)

{- generateIncompleteProofStep : Graph LoopNode LoopEdge ->  List (Edge LoopEdge)  -> Maybe ProofStep
generateIncompleteProofStep g l = 
  getIncompleteDiagram g l |> 
  Maybe.andThen (generateIncompleteProofStepFromDiagram g) -}

isEmptyBranch : List (Edge LoopEdge) -> Maybe Graph.EdgeId
isEmptyBranch l =
   case l of
       [ e ] -> if e.label.label == "" then Just e.id else Nothing
       _ -> Nothing

invertProofstepDiag : ProofStep -> ProofStep
invertProofstepDiag s = { s | diag = invertDiagram s.diag }

generateIncompleteProofStepFromDiagram : Graph LoopNode LoopEdge -> Diagram -> Maybe ProofStep
generateIncompleteProofStepFromDiagram g d =
   let flipIf b diag = if b then invertDiagram diag else diag in
   let surroundingDiag dir = 
        let d2 = flipIf dir d in        

        isEmptyBranch d2.rhs 
        |> Maybe.andThen (\id ->                                
             let g2 = Graph.removeEdge id g in
             d2.lhs |> List.head |>
             Maybe.andThen (diagramFrom (not dir) g2 >> flipIf dir >>
                           .lhs >> List.map .id
                               >> applyDiag d2
                               >> Maybe.map 
                               (if dir then invertProofstepDiag else identity)
                               ))
               
   in
   let step = Maybe.or 
             (surroundingDiag True)      
             (surroundingDiag False)            
   in
      step


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
fullProofs g0 =
   let g = nameIdentities g0 in
   let diags = getAllValidDiagrams g in
   let (bigDiags, smallDiags) = List.partition isBorder diags in
   -- let _ = List.map (Debug.log " stat:" << statementToString) diags in
   -- let _ = Debug.log "nombre de gros diagrammes: " (List.length bigDiags) in
   -- let _ = List.map (Debug.log " big:" << statementToString) bigDiags in
   -- let _ = Debug.log "nombre de petits diagrammes: " (List.length smallDiags) in
   -- let _ = List.map (Debug.log " small:" << statementToString) smallDiags in
   List.filter finishedProof <|
   List.map (\d ->
       { statement = d, proof = d.lhs |> List.map .id |> commuteProof smallDiags
       }) <| List.map invertDiagram bigDiags 

statementToString : Diagram -> String
statementToString d = 
  let edgeToString = List.map (.label >> .label) >> String.join " · " in
  "{ " ++ edgeToString d.lhs ++ " = " ++ edgeToString d.rhs ++ " }"


write0 n s = if n == 0 then [] else s 
repeat n s = case n of 
                0 -> []
                1 ->  ["  " ++ s]
                _ ->  ["  do " ++ String.fromInt n ++ " " ++ s] 
   
getToThePoint : Int -> Int -> String
getToThePoint startOffset backOffset =
   String.join "\n" <|
    repeat backOffset "apply cancel_postcomposition."   
   ++
     write0 startOffset 
    (
    ([ "  repeat rewrite assoc'." ]
    ++
    repeat startOffset "apply cancel_precomposition."    
    ++ 
    [ "  repeat rewrite assoc." ]))
proofStepToString : ProofStep -> String
proofStepToString { startOffset, backOffset, diag} =
 
   String.join "\n" <|
   [ "assert(eq : " ++ statementToString diag ++ ")."
   , "{"
   , "  admit."
   , "}"
   , "etrans."
   , "{" ,
     getToThePoint startOffset backOffset
   , "  apply eq."
   , "}" ]
   ++ (write0 startOffset ["repeat rewrite assoc."])
   ++ [
    "clear eq."
   ]

symmetryStr = "apply pathsinv0."

incompleteProofStepToString : ProofStep -> String
incompleteProofStepToString { startOffset, backOffset, diag} =
   let invert = isEmptyBranch diag.lhs |> Maybe.isJust in
   let symList =  (if invert then [ symmetryStr ] else [] ) in
    String.join "\n" <|
    symList ++
   [ -- "(* generated with YADE *)"
     "etrans."
   , "{" 
   , getToThePoint startOffset backOffset 
   , "  (* remove this after copying the goal *)"
   , "  duplicate_goal."
   -- , "  {"
   , "  admit."
   --, "  }" 
   , "  (* copy the result in the proof editor *)"
   , "  norm_graph."
   , "  admit."
   , "}"
   ]
   ++ (write0 startOffset ["repeat rewrite assoc."])
   ++ symList


nodesOfDiag : Diagram -> List Graph.NodeId
nodesOfDiag d =    
   List.map .from d.lhs ++ List.reverseMap .to d.rhs 
   
   

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
   let nodes = List.foldl Set.union Set.empty 
             (List.map (.diag >> nodesOfDiag >> Set.fromList) st.proof) 
   in
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
  -- ++ "(* generated with YADE *)"
   ++ (String.join "\n" <| List.map proofStepToString st.proof)
   ++ "\n apply idpath."
   ++ "\nQed."