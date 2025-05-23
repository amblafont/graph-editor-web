module GraphProof exposing (loopFrom, getAllValidDiagrams,
    proofStatementToDebugString, isInDiag, statementToString,
    proofStatementToString, proofStepToString, loopToDiagram,
    fullProofs, getIncompleteDiagram, isEmptyBranch, 
    LoopEdge, LoopNode, Diagram,
    nodesOfDiag, edgesOfDiag, findProofOfDiagram)

import Polygraph as Graph exposing (Graph, EdgeId, Edge, Node)
import Geometry.Point as Point exposing (Point)
import List.Extra as List
import ListExtraExtra as List
import Tuple exposing (first, second)
import IntDict exposing (IntDict)
import Set exposing (Set)
import Maybe.Extra as Maybe
import Verbatim
import Parser exposing (Step(..))

type alias LoopEdge = { pos : Point, from : Point, to : Point, angleIn : Float, angleOut : Float, label : String, identity : Bool }
type alias LoopNode = { pos : Point, label : String, proof : Maybe String }

positionsInDiagram : Diagram -> List Point
positionsInDiagram {lhs, rhs} =
  let getPositions {label} = [label.from, label.pos, label.to] in
  List.concatMap getPositions lhs ++ 
  (List.reverse <| List.concatMap getPositions rhs)

isInDiag : Graph LoopNode e -> Point -> Diagram -> Bool
isInDiag g pos d =
   -- Graph.getNodes (nodesOfDiag d)
   --             g |> List.map (.label >> .pos) 
                positionsInDiagram d |> Point.isInPoly pos

proofNodes : Graph LoopNode e -> List (Node LoopNode)
proofNodes g = 
   Graph.nodes g 
   |> List.filter (.label >> .proof >> Maybe.isJust) 

findProofOfDiagram : Graph LoopNode e -> List (Node LoopNode) -> Diagram -> Maybe String
findProofOfDiagram g l d =
   List.find (\ n -> isInDiag g n.label.pos d) l
   |> Maybe.andThen (.label >> .proof)

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
angleDir dir edge = if dir then edge.angleIn else edge.angleOut
-- Point.flipAngle edge.angle

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
                       rhs : List (Edge LoopEdge),
                       proof : Maybe String
                     }

invertDiagram : Diagram -> Diagram
invertDiagram { lhs, rhs, proof } = { lhs = rhs, rhs = lhs, proof = proof }

-- is it an outer diagram?
isOuterDiagram : Diagram -> Bool
isOuterDiagram {lhs, rhs} = 
   
   let makeAngles angleField = List.map (.label >> angleField) in
   let anglesLhs = makeAngles .angleIn lhs
       anglesRhs = makeAngles .angleOut rhs -- |> List.map Point.flipAngle
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
      rhs = rhs,
      proof = Nothing }

checkEndPoints : Diagram -> Bool
checkEndPoints { lhs, rhs } =
     case (List.last lhs, List.last rhs) of
         (Just e1, Just e2) -> 
           e1.to == e2.to
         _ -> False


adjacentEdges : Graph LoopNode LoopEdge -> 
               List ({ edge : Edge LoopEdge, incoming : Bool }, 
                     { edge : Edge LoopEdge, incoming : Bool })
adjacentEdges g = 
     let inc = (Graph.incidence g ) in
     let dumpEdge {label, angleIn, angleOut} =
              { label = label, angleIn = angleIn, angleOut = angleOut }
     in
     let dump (id, stuff) =
                     -- let stuff = ed.label in
                     let f = List.map (.label >> dumpEdge) stuff.incomings in
                     -- let _ = Debug.log "outgoings" id in
                     List.map (.label >> dumpEdge) stuff.outgoings
      in
   --   let _ = List.map (Debug.log "incidence") (IntDict.toList inc) in
   --   let _ = List.map dump (IntDict.toList inc) in
      inc
         |> IntDict.values
         |> List.concatMap
          (\ i -> -- let _ = Debug.log "incidence" "" in 
             List.map (\e -> { edge = e, incoming = True } ) i.incomings
                 ++ List.map (\e -> { edge = e, incoming = False} ) i.outgoings
                 |> List.sortBy 
                     (\{edge, incoming} -> 
                         if incoming then (edge.label.angleOut) else (edge.label.angleIn)
                         -- |> Point.normaliseAngle
                        --  Point.flipAngle edge.label.angle |> Point.normaliseAngle
                     )
                 |> List.map (\e -> -- let _ = Debug.log "" (dumpEdge e.edge.label, e.incoming) in 
                             e)
                 |> List.succCyclePairs
          )

adjacentListToDict : List ({ edge : Edge LoopEdge, incoming : Bool }, 
                           { edge : Edge LoopEdge, incoming : Bool })
                     -> IntDict (Edge LoopEdge)
adjacentListToDict l =
   IntDict.fromList <| List.map (\(e1, e2) -> (e1.edge.id, e2.edge)) l


-- computeNextLefts : List ({ edge : Edge LoopEdge, incoming : Bool }, 
--                          { edge : Edge LoopEdge, incoming : Bool })
--                -> IntDict Edge.Id
-- computeNextLefts l =
   
--    let next (e1, e2) =  if e1.incoming && not e2.incoming then Just (e1.edge.id e2.edge.id) else Nothing


getAllValidDiagrams : Graph LoopNode LoopEdge -> List Diagram
getAllValidDiagrams g =
   let inc = adjacentEdges g in
   -- let inc = Debug.log "adjacences" <| adjacentEdges g in
   let nextLefts  = List.filter (\ (e1, e2) -> e1.incoming && not e2.incoming) inc
       nextRights = List.filter (\ (e1, e2) -> not e1.incoming && e2.incoming) inc
                    |> List.map (\ (e1, e2) -> (e2, e1))
       starts =     List.filter (\ (e1, e2) -> not e1.incoming && not e2.incoming) inc
                    |> List.map (\ (e1, e2) -> (e2, e1))
   in
   -- let _ = Debug.log "nextLefts" (List.map (\(e1, e2) -> (e1.edge.label.label, e2.edge.label.label)) nextLefts) in
   -- let _ = Debug.log "nextRights" (List.map (\(e1, e2) -> (e1.edge.label.label, e2.edge.label.label)) nextRights) in
   -- let _ = Debug.log "starts" (List.map (\(e1, e2) -> (e1.edge.label.label, e2.edge.label.label)) starts) in
   let buildBranch next startEdge =
        case IntDict.get startEdge.id next of
          Nothing -> [ startEdge ]
          Just e -> startEdge :: buildBranch next e
   in
   let diags = List.map 
         (\ (rhs, lhs) -> 
           {rhs = buildBranch (adjacentListToDict nextLefts) rhs.edge, 
            lhs = buildBranch (adjacentListToDict nextRights) lhs.edge,
            proof = Nothing})
         starts
   in
 

   -- peut etre message d'erreur a la place?
   -- let _ = List.map (statementToString >> Debug.log "d ") diags in
   let validDiags = List.filter checkEndPoints diags in
   -- let _ = List.map (statementToString >> Debug.log "valid ") validDiags in
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

-- TODO: check if it is still used, remove if not.
isEmptyBranch : List (Edge LoopEdge) -> Maybe Graph.EdgeId
isEmptyBranch l =
   case l of
       [ e ] -> if e.label.label == "" then Just e.id else Nothing
       _ -> Nothing


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

commuteProof : List Diagram -> List Graph.EdgeId -> List ProofStep 
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
   let (bigDiags, smallDiags_without_proofs) = List.partition isOuterDiagram diags in
   let smallProofs = proofNodes g in
   let updateDiag d = { d | proof = findProofOfDiagram g smallProofs d } in
   let smallDiags = List.map updateDiag smallDiags_without_proofs in
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
  let expand s =  if s == "" then "{_}" else s in
  let edgeToString = List.map (.label >> .label >> expand >>
                               Verbatim.removeVerbatim) >> String.join " · " in
   "<YADE> " ++ 
  edgeToString d.lhs ++ " = " ++ edgeToString d.rhs 
   ++ " </YADE>"



   
getToThePoint : Int -> Int -> String
getToThePoint startOffset backOffset =
   "yade_strip " ++ String.fromInt startOffset ++ " " ++ String.fromInt backOffset ++ "."

proofStepToString : ProofStep -> String
proofStepToString { startOffset, backOffset, diag} =
   "eapply yade.transitivity.\n" ++
   getToThePoint startOffset backOffset ++
   "\n" ++ 
   "refine (_ :> " ++ statementToString diag ++ ").\n" ++
    "{\n"
   ++ "  " ++ (diag.proof |> Maybe.withDefault "admit.")
   ++ "\n}\nrepeat rewrite -> yade.assoc''.\n"




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
    rhs = List.map renameEdge diag.rhs,
    proof = Nothing }

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
   ++ "\n reflexivity."


proofStatementToString : ProofStatement -> String
proofStatementToString st =
   -- "(* Goal " ++ statementToString st.statement ++ ". *)\n\n"
  -- ++ "(* generated with YADE *)"
   -- ++ "make_infix.\n"
    "change (" ++ statementToString st.statement ++ ").\n\n"
   ++ (String.join "\n" <| List.map proofStepToString st.proof)
   ++ "\n reflexivity."
   ++ "\nQed."