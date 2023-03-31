module GraphDefs exposing (EdgeLabel, NodeLabel,
   NormalEdgeLabel, EdgeType(..), GenericEdge,
   filterLabelNormal, filterEdgeNormal, isNormalId, isPullshout,
   filterNormalEdges,
   newNodeLabel, newEdgeLabel, newPullshout, emptyEdge,
   selectedEdges, mapNormalEdge,  mapDetails, 
   createNodeLabel,
   getNodeLabelOrCreate, getNodeDims, getNodePos, getEdgeDims,
   addNodesSelection, selectAll, clearSelection, 
   clearWeakSelection,
   selectedGraph,
   fieldSelect,
   selectedNodes,
   isEmptySelection,
   selectedEdgeId, selectedNode, selectedId,
   removeSelected, getLabelLabel,
   getNodesAt, snapToGrid, snapNodeToGrid, exportQuiver,
   addOrSetSel, toProofGraph, selectedIncompleteDiagram,
   selectSurroundingDiagram, cloneSelected,
   centerOfNodes, mergeWithSameLoc,
   findReplaceInSelected, {- closestUnnamed, -} unselect, closest,
   makeSelection, addWeaklySelected, weaklySelect,
   getSurroundingDiagrams, updateNormalEdge,
   rectEnveloppe
   )

import IntDict
import Geometry.Point as Point exposing (Point)
import Geometry exposing (LabelAlignment(..))
import ArrowStyle exposing (ArrowStyle)
import Polygraph as Graph exposing (Graph, NodeId, EdgeId, Node, Edge)
import GraphProof exposing (LoopNode, LoopEdge, Diagram)

import Json.Encode as JEncode
import List.Extra as List
import Maybe.Extra as Maybe

type alias NodeLabel = { pos : Point , label : String, dims : Maybe Point, selected : Bool, weaklySelected : Bool,
                         isMath : Bool}

type alias EdgeLabel = GenericEdge EdgeType
type alias GenericEdge a = { details : a, selected : Bool,
                   weaklySelected : Bool,
                   zindex : Int}


type EdgeType = 
     PullshoutEdge
   | NormalEdge NormalEdgeLabel

type alias NormalEdgeLabel = { label : String, style : ArrowStyle, dims : Maybe Point}

filterNormalEdges : EdgeType -> Maybe NormalEdgeLabel
filterNormalEdges d =  case d of
             PullshoutEdge -> Nothing
             NormalEdge l -> Just l

filterLabelNormal : EdgeLabel -> Maybe (GenericEdge NormalEdgeLabel)
filterLabelNormal =
    mapDetails filterNormalEdges
             >> flattenDetails

filterEdgeNormal : Edge EdgeLabel -> Maybe (Edge (GenericEdge NormalEdgeLabel))
filterEdgeNormal e =
      filterLabelNormal e.label |>
                    Maybe.map (
                        \ l -> 
                        Graph.edgeMap (always l) e
                    )
       

keepNormalEdges : Graph NodeLabel EdgeLabel -> Graph NodeLabel (GenericEdge NormalEdgeLabel)
keepNormalEdges = Graph.filterMap Just
   filterLabelNormal

mapEdgeType : (NormalEdgeLabel -> NormalEdgeLabel) -> EdgeType -> EdgeType
mapEdgeType f e = case e of
    PullshoutEdge -> PullshoutEdge
    NormalEdge l -> NormalEdge (f l)

mapDetails : (a -> b) -> GenericEdge a -> GenericEdge b
mapDetails f e = 
    { weaklySelected = e.weaklySelected
    , selected = e.selected
    , details = f e.details
    , zindex = e.zindex}

isNormal : EdgeLabel -> Bool
isNormal = not << isPullshout

isPullshout : EdgeLabel -> Bool
isPullshout e = e.details == PullshoutEdge

isNormalId : Graph NodeLabel EdgeLabel -> Graph.Id -> Bool
isNormalId g id = Graph.get id (always True)
    isNormal g |> Maybe.withDefault False

mapNormalEdge : (NormalEdgeLabel -> NormalEdgeLabel) -> EdgeLabel -> EdgeLabel
mapNormalEdge = mapEdgeType >> mapDetails

flattenDetails : GenericEdge (Maybe a) -> Maybe (GenericEdge a)
flattenDetails e = 
   e.details |> Maybe.map
   (\d -> mapDetails (always d) e)

{-   
computeEdgePos : Point -> Point -> EdgeLabel -> Point
computeEdgePos from to e = e.style.bend
    let q = Geometry.segmentRectBent from to e.style.bend in
    if Bez.isLine q then
              Point.diamondPx q.from q.to offset
              
            else 
              let m = Bez.middle q in
              Point.add m <|
              Point.normalise offset <|        
               Point.subtract q.controlPoint <| m -}

toProofGraph :  Graph NodeLabel EdgeLabel -> Graph LoopNode LoopEdge
toProofGraph = 
    keepNormalEdges >>
   
    Graph.mapRecAll (\n -> n.pos)
             (\n -> n.pos)
             (\ _ n -> { pos = n.pos, label = n.label })
             (\ _ fromP toP {details}  -> 
                        { angle = Point.subtract toP fromP |> Point.pointToAngle ,
                          label = details.label, -- (if l.label == "" && l.style.double then fromLabel else l.label),
                          pos = Point.middle fromP toP,
                          identity = details.style.double })

selectedIncompleteDiagram : Graph NodeLabel EdgeLabel -> Maybe Diagram
selectedIncompleteDiagram g = 
   let gc = (toProofGraph g) in
    GraphProof.getIncompleteDiagram gc
     <| Graph.getEdges (selectedEdges g |> List.map .id) gc

updateNormalEdge : EdgeId -> (NormalEdgeLabel -> NormalEdgeLabel) -> Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
updateNormalEdge id f =
    Graph.updateEdge id 
     (mapNormalEdge f)


exportQuiver : Int -> Graph NodeLabel EdgeLabel -> JEncode.Value
exportQuiver sizeGrid g =
  let gnorm = g |> keepNormalEdges |> Graph.normalise in
  let nodes = Graph.nodes gnorm
      edges = Graph.edges gnorm
  in
  let coordInt x = floor (x / toFloat sizeGrid) |> JEncode.int in
  let encodePos (x, y) = [coordInt x, coordInt y] in
  let encodeNode n = JEncode.list identity <| encodePos n.label.pos ++ 
            [ JEncode.string (if n.label.label == "" then "\\bullet" else n.label.label)] in
  let encodeEdge e = JEncode.list identity <| 
               [JEncode.int e.from
               , JEncode.int e.to
               , JEncode.string e.label.details.label
               , JEncode.int (if e.label.details.style.labelAlignment == Right then 2 else 0) -- alignment
               , JEncode.object <| ArrowStyle.quiverStyle e.label.details.style
                  -- [("level", if e.label.style.double then JEncode.int 2 else JEncode.int 1)] --options
                ] in
  let jnodes = nodes |> List.map encodeNode
      jedges = edges |> List.map encodeEdge
  in
  JEncode.list identity <|
  [JEncode.int 0, JEncode.int <| List.length nodes] ++ jnodes ++ jedges

newNodeLabel : Point -> String -> Bool -> NodeLabel
newNodeLabel p s isMath = 
    { pos = p , label = s, dims = Nothing, selected = False, weaklySelected = False,
                         isMath = isMath}

newGenericLabel : a -> GenericEdge a
newGenericLabel d = { details = d,
                      selected = False,
                      weaklySelected = False,
                      zindex = 0}

newEdgeLabel : String -> ArrowStyle -> EdgeLabel
newEdgeLabel s style = newGenericLabel <| NormalEdge { label = s, style = style, dims = Nothing }

newPullshout : EdgeLabel
newPullshout = newGenericLabel PullshoutEdge


emptyEdge : EdgeLabel
emptyEdge = newEdgeLabel "" ArrowStyle.empty


createNodeLabel : Graph NodeLabel EdgeLabel -> String -> Point -> (Graph NodeLabel EdgeLabel,
                                                                       NodeId, Point)
createNodeLabel g s p =
    let label = newNodeLabel p s True in
    let (g2, id) = Graph.newNode g label in
     (g2, id, p)

getNodeLabelOrCreate : Graph NodeLabel EdgeLabel -> String -> Point -> (Graph NodeLabel EdgeLabel,
                                                                       NodeId, Point)
getNodeLabelOrCreate g s p =
    if s == "" then
       createNodeLabel g s p
    else
        case Graph.filterNodes g (\ l -> l.label == s) of
            [] -> createNodeLabel g s p
            t :: _ -> (g , t.id, t.label.pos)


defaultDims : String -> Point
defaultDims s = 
  let height = 16 in
  let size = 1 in --max 1 (String.length s) in
   -- copied from source code of Collage
   (height / 2 * toFloat size, height)

getNodeDims : NodeLabel -> Point
getNodeDims n =
    case  n.dims of
        Nothing -> defaultDims n.label
        Just p -> p

getNodePos : NodeLabel -> Point
getNodePos n =
   if n.isMath then n.pos else
   Point.add n.pos (Point.resize 0.5 (getNodeDims n))
    
getEdgeDims : NormalEdgeLabel -> Point
getEdgeDims n =
    case  n.dims of
        Nothing -> defaultDims n.label
        Just p -> p

-- select nodes and everything between them
addNodesSelection : Graph NodeLabel EdgeLabel -> (NodeLabel -> Bool) -> Graph NodeLabel EdgeLabel
addNodesSelection g f =
      Graph.mapRecAll .selected
        .selected
       (\_ n -> { n | selected = f n || n.selected})
       (\_ s1 s2 e -> {e | selected = (s1 && s2) || e.selected })
       g
    -- Graph.map 
    --    (\_ n -> { n | selected = f n })(\_ -> identity) g

selectAll : Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
selectAll g = addNodesSelection g (always True)

fieldSelect : Graph NodeLabel EdgeLabel -> ({ a | selected : Bool, weaklySelected : Bool} -> Bool)
fieldSelect g = if Graph.any .selected .selected g then .selected else .weaklySelected

selectedGraph : Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
selectedGraph g = 
   let f = fieldSelect g in
   Graph.keepBelow f f g

selectedNodes : Graph NodeLabel EdgeLabel -> List (Node NodeLabel)
selectedNodes g = Graph.nodes g |> List.filter (.label >> fieldSelect g)

selectedEdges : Graph NodeLabel EdgeLabel -> List (Edge EdgeLabel)
selectedEdges g = Graph.edges g |> List.filter (.label >> fieldSelect g)

isEmptySelection : Graph NodeLabel EdgeLabel -> Bool
isEmptySelection go =
   not (Graph.any .selected .selected go) && 
   not (Graph.any .weaklySelected .weaklySelected go)

selectedNode : Graph NodeLabel EdgeLabel -> Maybe (Node NodeLabel)
selectedNode g = 
    case selectedNodes g of
       [ x ] -> Just x
       _ -> Nothing

selectedEdge : Graph NodeLabel EdgeLabel -> Maybe (Edge EdgeLabel)
selectedEdge g = 
    case selectedEdges g of
       [ x ] -> Just x
       _ -> Nothing

selectedEdgeId : Graph NodeLabel EdgeLabel -> Maybe EdgeId
selectedEdgeId = selectedEdge >> Maybe.map .id
    
selectedId : Graph NodeLabel EdgeLabel -> Maybe Graph.Id
selectedId g = 
   case (List.map .id <| selectedNodes g)
          ++ (List.map .id <| selectedEdges g) of
      [ x ] -> Just x
      _ -> Nothing


removeSelected : Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
removeSelected g = 
   let f = fieldSelect g in
   Graph.drop f f g

clearSelection : Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
clearSelection g =
  Graph.map (\_ n -> {n | selected = False})
            (\_ e -> {e | selected = False}) g

clearWeakSelection : Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
clearWeakSelection g =
  Graph.map (\_ n -> {n | weaklySelected = False})
            (\_ e -> {e | weaklySelected = False}) g


getNodesAt : Graph NodeLabel e -> Point -> List NodeId
getNodesAt g p =
  Graph.filterNodes g
    (\n -> Geometry.isInPosDims { pos = getNodePos n, 
                                  dims = getNodeDims n} p)
  |> List.map .id


snapNodeToGrid : Int -> NodeLabel -> NodeLabel
snapNodeToGrid sizeGrid n =  { n | pos = Point.snapToGrid (toFloat sizeGrid) n.pos }

snapToGrid : Int -> Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
snapToGrid sizeGrid g =
   Graph.map (\_ -> snapNodeToGrid sizeGrid) (\_ -> identity ) g

getLabelLabel : Graph.Id -> Graph NodeLabel EdgeLabel -> Maybe String
getLabelLabel id g = g |> Graph.get id (Just << .label) 
          (.details >> filterNormalEdges >> Maybe.map .label)
          |> Maybe.join

addOrSetSel : Bool -> Graph.Id -> Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
addOrSetSel keep o gi =

    let g = if keep then gi else clearSelection gi in
    let g2 = Graph.update o (\n -> {n | selected = True}) 
                  (\n -> {n | selected = True}) g
    in
       {-   = case o of
          ONothing -> g
          ONode id -> Graph.updateNode id (\n -> {n | selected = True}) g
          OEdge id -> Graph.updateEdge id (\n -> {n | selected = True}) g
    in -}
   g2

unselect : Graph.Id -> Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
unselect id = Graph.update id 
               (\ n -> { n | selected = False})
               (\ e -> { e | selected = False})

weaklySelect : Graph.Id -> Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
weaklySelect id = 
      Graph.map 
         (\_ n -> { n | weaklySelected = False})
         (\_ e -> { e | weaklySelected = False})
         >>
      Graph.update id 
               (\ n -> { n | weaklySelected = True})
               (\ e -> { e | weaklySelected = True}) 


selectEdges : Graph NodeLabel EdgeLabel -> List EdgeId -> Graph NodeLabel EdgeLabel
selectEdges = List.foldl (\ e -> Graph.updateEdge e (\n -> {n | selected = True}))

getSurroundingDiagrams : Point -> Graph NodeLabel EdgeLabel -> List Diagram
getSurroundingDiagrams pos gi =   
   let gp = toProofGraph gi in
   let isInDiag d =
           Graph.getNodes (GraphProof.nodesOfDiag d)
               gi |> List.map (.label >> .pos) |> Point.isInPoly pos
   in     
   GraphProof.getAllValidDiagrams gp 
            |> List.filter isInDiag
   

selectSurroundingDiagram : Point -> Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
selectSurroundingDiagram pos gi =   
   case getSurroundingDiagrams pos gi of
       [] -> gi
       d :: _ ->
          let edges = GraphProof.edgesOfDiag d |> IntDict.keys in
          selectEdges (clearSelection gi) edges


cloneSelected : Graph NodeLabel EdgeLabel -> Point -> 
                Graph NodeLabel EdgeLabel
cloneSelected g offset =
  let g2 = selectedGraph g |> 
       Graph.map (\_ n -> {n | pos = Point.add n.pos offset, selected = True })
         (\_ e -> {e | selected = True } )
  in
  let gclearSel = clearSelection g in
  Graph.union gclearSel g2

centerOfNodes : List (Node NodeLabel) -> Point
centerOfNodes nodes = ((Geometry.rectEnveloppe <| List.map (.pos << .label) nodes) |> Geometry.centerRect)

mergeWithSameLoc : Node NodeLabel -> Graph NodeLabel EdgeLabel -> (Graph NodeLabel EdgeLabel, Bool)
mergeWithSameLoc n g =
    case getNodesAt g n.label.pos |> List.filterNot ((==) n.id) of
         [ i ] -> (Graph.removeLoops 
              <| Graph.merge i n.id g, True)
         _ -> (g, False)

findReplaceInSelected : Graph NodeLabel EdgeLabel -> {search : String, replace: String} ->  Graph NodeLabel EdgeLabel
findReplaceInSelected g r =
  let repl sel s = 
       if sel then
          String.replace r.search r.replace s
       else
          s
  in
  let f = fieldSelect g in
  Graph.map (\ _ n -> { n | label = repl (f n) n.label })
     (\_  e -> mapNormalEdge 
           (\ l -> { l |  label = repl (f e) l.label })
           e
           ) 
           
      g

distanceToNode : Point -> NodeLabel -> Float
distanceToNode p n = 
   let posDims = { pos = n.pos, dims = getNodeDims n } in
   let rect = Geometry.rectFromPosDims posDims in 
   Geometry.distanceToRect p rect

{- unnamedGraph : Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
unnamedGraph = 
   Graph.keepBelow (.label >> String.isEmpty)
     (.label >> String.isEmpty) -}

closest : Point -> Graph NodeLabel EdgeLabel -> Graph.Id
-- ordered by distance to Point
closest pos ug =
   
   case getNodesAt ug pos of
     t :: _ -> t
     _ -> 
   
   
         -- we need the pos
         let ug2 = Graph.mapRecAll .pos .pos 
               (\ _ n -> { distance = distanceToNode pos n, pos = n.pos})
               (\ _ p1 p2 e -> 
                  let epos = Point.middle p1 p2 in
                  { pos = epos,
                    distance = Point.distance pos <| Point.middle p1 p2
                  })
                  
               ug
         in
         let getEmptysDistance l = l
               -- |> List.filter (.label >> .empty)
               |> List.map (\ o -> 
                           {  id = o.id, 
                              distance = o.label.distance})
               
         in
         let unnamedEdges = Graph.edges ug2 |> getEmptysDistance in
         let unnamedNodes = Graph.nodes ug2 |> getEmptysDistance in
         let unnamedAll = unnamedEdges ++ unnamedNodes 
               |> List.minimumBy .distance 
               |> Maybe.map .id
               |> Maybe.withDefault 0
         in
         unnamedAll
{- 

closestUnnamed : Point -> Graph NodeLabel EdgeLabel -> List Graph.Id
-- ordered by distance to Point
closestUnnamed pos g = 
   let ug = unnamedGraph g in
   -- we need the pos
   let ug2 = Graph.mapRecAll .pos .pos 
         (\ _ n -> { empty = String.isEmpty n.label, pos = n.pos})
         (\ _ p1 p2 e -> { empty = String.isEmpty e.label, pos = Point.middle p1 p2})
         ug
   in
   let getEmptysDistance l = l
          |> List.filter (.label >> .empty)
          |> List.map (\ o -> 
                       {  id = o.id, 
                          distance = Point.distance o.label.pos pos})
          
   in
   let unnamedEdges = Graph.edges ug2 |> getEmptysDistance in
   let unnamedNodes = Graph.nodes ug2 |> getEmptysDistance in
   -- TODO: order them by distance to mousepos?
   let unnamedAll = unnamedEdges ++ unnamedNodes 
        |> List.sortBy .distance 
        |> List.map .id
   in
   unnamedAll -}

addWeaklySelected : Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
addWeaklySelected =
  Graph.map (\ _ n -> { n | selected = n.weaklySelected || n.selected})
     (\ _ n -> { n | selected = n.weaklySelected || n.selected})

makeSelection : Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
makeSelection g =
   if Graph.any .selected .selected g then
      g
   else
      addWeaklySelected g

rectEnveloppe : Graph NodeLabel EdgeLabel -> Geometry.Rect
rectEnveloppe g =
   let points = Graph.nodes g |> List.map (.label >> .pos) in
   Geometry.rectEnveloppe points