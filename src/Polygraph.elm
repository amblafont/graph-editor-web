module Polygraph exposing (Graph, Id, EdgeId, NodeId, empty, allIds, nodeIds,
     newNode, newEdge,
     update, updateNode, updateEdge, updateNodes, updateList,
     invertEdge, merge, recursiveMerge, makeCylinder, makeCone,
     getNode, getNodes, getEdge, getEdges, get, removeNode, removeEdge,
     map, mapRecAll, invalidEdges,
     nodes, edges, fromNodesAndEdges,
     filterNodes, keepBelow, filterMap,
     Node, Edge, nextId,
     incomings, outgoings, drop, 
     normalise, 
     disjointUnion, edgeMap, nodeMap, codec, mapCodec,
     {- findInitial, sourceNode, -} removeLoops,
     incidence, any, connectedClosure, minimal, maximal, complement, topmostObject)
import IntDict exposing (IntDict)
import IntDictExtra 
import Maybe.Extra as Maybe
import List.Extra
import Codec exposing (Codec)


type alias Id = Int
type alias EdgeId = Id
type alias NodeId = Id

type Object n e = NodeObj n | EdgeObj Id Id e
type alias Node n = 
  { id : NodeId,
    label : n }

type alias Edge e =
  { id : EdgeId,
    from : Id,
    to : Id,
    label : e
   }

edgeMap : (a -> b) -> Edge a -> Edge b
edgeMap f {id, from, to, label} = 
   { id = id, from = from, to = to, label = f label}

nodeMap : (a -> b) -> Node a -> Node b
nodeMap f {id, label} = { id = id, label = f label}


{- objUniv : (n -> a) -> (e -> a) -> Object n e -> a
objUniv fn fe o =
  case o of
     EdgeObj _ _ e -> fe e 
     NodeObj n -> fn n -}

objEdge : EdgeId -> Object n e -> Maybe (Edge e)
objEdge id o = case o of
  EdgeObj i1 i2 e -> Just { id = id, from = i1, to = i2, label = e }
  _ -> Nothing

objNode : Object n e -> Maybe n
objNode o = case o of
   NodeObj n -> Just n
   _ -> Nothing

type alias GraphRep n e = IntDict (Object n e)

type Graph n e =
   Graph (GraphRep n e)

graphRep : Graph n e -> GraphRep n e
graphRep (Graph g) = g


{- 
unGraph : Graph n e -> GraphRep n e
unGraph graph =
    case graph of
        Graph rep ->
            rep -}

mapRep : (GraphRep n e -> GraphRep n2 e2) -> (Graph n e -> Graph n2 e2)
mapRep f (Graph g) = Graph <| f g   
{-| An empty graph.

    size empty == 0

-}
empty : Graph n e
empty =
    Graph IntDict.empty

supId : GraphRep n e -> Id 
supId g =
    case IntDict.findMax g of
             Just (id, _) -> id + 1
             Nothing -> 0

nextId : Graph n e -> Id
nextId (Graph g) = supId g

newObject : Graph n e -> Object n e -> (Graph n e, Id)
newObject g o =
  let id = nextId g in
  (mapRep (IntDict.insert id o) g, id)


newNode : Graph n e -> n -> (Graph n e, NodeId)
newNode g n = newObject g <| NodeObj n

newEdge : Graph n e -> Id -> Id -> e -> (Graph n e, EdgeId)
newEdge g i1 i2 e = newObject g <| EdgeObj i1 i2 e


removeList : List Id -> Graph n e -> Graph n e
removeList l (Graph d) =
   IntDictExtra.removeList l d |> Graph |> sanitise
    

remove : Id -> Graph n e -> Graph n e
remove id = removeList [id]

-- TODO check that it is an edge
removeEdge :  EdgeId -> Graph n e ->Graph n e
removeEdge = remove

removeNode : NodeId -> Graph n e -> Graph n e
removeNode = remove
  

mapObj : (n -> n2) -> (e -> e2) -> Object n e -> Object n2 e2
mapObj fn fe o =
  case o of
     NodeObj n -> NodeObj (fn n)
     EdgeObj i1 i2 e -> EdgeObj i1 i2 (fe e)


update : Id -> (n -> n) -> (e -> e) -> Graph n e -> Graph n e
update i fn fe =
  mapRep <| IntDict.update i (Maybe.map (mapObj fn fe))

updateList : List Id -> (n -> n) -> (e -> e) -> Graph n e -> Graph n e
updateList l fn fe g =
    List.foldl (\ i -> update i fn fe)  g l
  -- mapRep <| IntDict.update i (Maybe.map (mapObj fn fe))


updateNode : NodeId -> (n -> n) -> Graph n e -> Graph n e
updateNode i fn g = update i fn identity g 

updateEdge : EdgeId -> (e -> e) -> Graph n e -> Graph n e
updateEdge i fe g = update i identity fe g

updateNodes : List (Node a) -> Graph a b -> Graph a b
updateNodes l g =  
  List.foldl (\ { id, label } g2 -> updateNode id (always label) g2) g l


map : (NodeId -> n1 -> n2) -> (EdgeId -> e1 -> e2) -> Graph n1 e1 -> Graph n2 e2
map fn fe = 
   mapRep <|
     IntDict.map (\ i -> mapObj (fn i) (fe i))

{- getFull : Id -> (n -> a) -> (Id -> Id -> e -> a) -> Graph n e -> Maybe a
getFull id fn fe (Graph g) =
   case IntDict.get id g of
      Just (NodeObj n) -> Just <| fn n
      Just (EdgeObj i1 i2 e) -> Just <| fe e i1 i2
      Nothing -> Nothing -}

get : Id -> (n -> a) -> (e -> a) -> Graph n e -> Maybe a
get id fn fe (Graph g) =
   case IntDict.get id g of
      Just (NodeObj n) -> Just <| fn n
      Just (EdgeObj i1 i2 e) -> Just <| fe e
      Nothing -> Nothing
   
getEdge : EdgeId -> Graph n e -> Maybe (Edge e)
getEdge id (Graph g) = IntDict.get id g |> Maybe.andThen (objEdge id)

getEdges : List EdgeId -> Graph n e -> List (Edge e)
getEdges l (Graph g) = 
      IntDictExtra.getList l g |> List.filterMap (\(id, e) -> objEdge id e)


getNode : NodeId -> Graph n e -> (Maybe n)
getNode id (Graph g) = IntDict.get id g |> Maybe.andThen objNode

getNodes : List NodeId -> Graph n e -> List (Node n)
getNodes l (Graph g) = 
      IntDictExtra.getList l g 
      |> List.filterMap (\(id, e) -> objNode e 
      |> Maybe.map (Node id))

nodes : Graph n e -> List (Node n)
nodes (Graph g) = 
   IntDict.toList g |> List.filterMap 
      (\(id, n) -> objNode n |> Maybe.map (Node id))

edges : Graph n e -> List (Edge e)
edges (Graph g) = 
   let mkEdge id (i1, i2, e) = Edge id i1 i2 e in
   IntDict.toList g |> List.filterMap 
      (\(id, e) -> objEdge id e)

fromNodesAndEdges : List (Node n) -> List (Edge e) -> Graph n e 
fromNodesAndEdges ln le =
  let dn = IntDict.fromList 
          <| List.map (\{ id, label} -> (id, NodeObj label)) ln
      de = IntDict.fromList 
          <| List.map (\{ id, from, to, label} -> 
          (id, EdgeObj from to label)) le
   in
     
  Graph <| IntDict.union dn de 

codec : Codec (Graph n e) { nodes : List (Node n), edges : List (Edge e) }
codec =
  Codec.build 
  (\g -> 
     { nodes = nodes g, edges = edges g }
  )
  (\ r -> fromNodesAndEdges r.nodes r.edges)

mapCodec : Codec n1 n2 -> Codec e1 e2 -> Codec (Graph n1 e1) (Graph n2 e2) 
mapCodec c1 c2 = 
     Codec.build
       (map (always (Codec.encoder c1)) (always (Codec.encoder c2)))
       (map (always (Codec.decoder c1)) (always (Codec.decoder c2)))

filterNodes : Graph n e -> (n -> Bool) -> List (Node n)
filterNodes g f = nodes g |> List.filter (f << .label)

{- updateNodes : List NodeId -> (a -> a) -> Graph a b -> Graph a b
updateNodes l f g =
  List.foldl (\ id g2 -> updateNode id f g2) g l
  -}



-- tail recursive function

type MapRecObj n1 e1 n2 e2 =
    Input (Object n1 e1)
  | Output (Object n2 e2)
  -- impossible to affect
--   | Missing
  -- we have asked for inspecting e1
  | Waiting Id Id e1



mapRecAux :      
         
     (n2 -> a)
   -> (e2 -> a)
   -> (NodeId -> n1 -> n2) 
      -- ah oui c'est pas bon...
      -> (EdgeId -> a -> a -> e1 -> e2)  
       -- input objects
       -> IntDict (MapRecObj n1 e1 n2 e2)
       -- to be treated objects
       -> List Id
       -- Treated objects       
       -> IntDict (MapRecObj n1 e1 n2 e2)
mapRecAux cn ce fn fe dict ids =
   let getA o = case o of
         NodeObj n -> cn n
         EdgeObj _ _ e -> ce e
   in
   let rec = mapRecAux cn ce fn fe in
   let ins id o = IntDict.insert id o dict in
    case ids of
       [] -> dict
       id :: tailIds ->
                   
         case IntDict.get id dict of           
            Just (Input (NodeObj n)) ->
                  rec 
                      (ins id (Output <| NodeObj <| fn id n)) 
                      tailIds                         
            Just (Input (EdgeObj i1 i2 e)) ->
                  rec (ins id <| Waiting i1 i2 e) (i1 :: i2 :: id :: tailIds)
            Just (Waiting i1 i2 e) ->
                  case (IntDict.get i1 dict, IntDict.get i2 dict) of                  
                     (Just (Output o1), Just (Output o2)) ->
                        let a1 = getA o1
                            a2 = getA o2
                        in
                        rec
                           (ins id 
                               (Output <| EdgeObj i1 i2 <| 
                                       fe id a1 a2 e) 
                                ) 
                               tailIds
                     _ ->  rec dict tailIds
                   
            _ -> rec dict tailIds


invalidEdges : Graph n e -> List (Edge e)
invalidEdges (Graph g) =
   let dict = mapRecAux (always ()) (always ()) 
        (always identity) (\_ _ _ -> identity)
        (IntDict.map (\_ -> Input) g)
        (IntDict.keys g)
   in
   let l = IntDict.toList dict in
   let missings = l
          |> List.filterMap 
           (\ (id, o) -> 
               case o of
                  Waiting i1 i2 e -> Just <| { id = id, from = i1, to = i2, label = e}
                  _ -> Nothing)
   in        
   missings
     
-- remove invalid edges
sanitise : Graph n e -> Graph n e
sanitise ((Graph d) as g) =
   let ids = invalidEdges g |> List.map .id in
    IntDictExtra.removeList ids d |> Graph


-- returns also the list of edges that could not be treated
mapRecAll : 
      (n2 -> a)
   -> (e2 -> a)
   ->  (NodeId -> n1 -> n2) 
   -> (EdgeId -> a -> a -> e1 -> e2) 
   -> Graph n1 e1 -> Graph n2 e2
mapRecAll cn ce fn fe (Graph g) = 
   mapRec cn ce fn fe (IntDict.keys g) (Graph g)

-- returns also the list of edges that could not be treated
mapRec : 
      (n2 -> a)
   -> (e2 -> a)
   ->  (NodeId -> n1 -> n2) 
    -> (EdgeId -> a -> a -> e1 -> e2)
    -> List Id
    -> Graph n1 e1 -> Graph n2 e2
mapRec cn ce fn fe ids (Graph g) = 
   let dict = mapRecAux cn ce fn fe
        (IntDict.map (\_ -> Input) g)
        ids 
   in
   let gf = IntDictExtra.filterMap
          (\ id o ->
             case o of
                  Output o2 -> Just o2
                  _ -> Nothing) dict       
   in
   Graph gf
   

rawFilterMapIds : (n -> Maybe n2) -> (Id -> Id -> e -> Maybe e2) -> GraphRep n e -> GraphRep n2 e2
rawFilterMapIds fn fe =
     IntDictExtra.filterMap
        (\_ o -> 
           case o of
               EdgeObj id1 id2 e -> fe id1 id2 e
                  |> Maybe.map (EdgeObj id1 id2)
               NodeObj n -> (fn n) |> Maybe.map NodeObj

         )

rawFilterIds : (n -> Bool) -> (Id -> Id -> e -> Bool) -> GraphRep n e -> GraphRep n e
rawFilterIds fn fe =
     rawFilterMapIds (Just >> Maybe.filter fn)
        (\id1 id2 -> (Just >> Maybe.filter (fe id1 id2)))


rawFilter : (n -> Bool) -> (e -> Bool) -> GraphRep n e -> GraphRep n e
rawFilter fn fe = rawFilterIds fn (\ _ _ -> fe)

rawFilterMap : (n -> Maybe n2) -> (e -> Maybe e2) -> GraphRep n e -> GraphRep n2 e2
rawFilterMap fn fe = rawFilterMapIds fn (\ _ _ -> fe)




-- if an element is dropped, all its ascendants will be dropped
-- as well
-- (dual of filter)

drop : (n -> Bool) -> (e -> Bool) -> Graph n e -> Graph n e
drop fn fe =
   filterMap (Just >> Maybe.filter (not << fn))
     (Just >> Maybe.filter (not << fe))

-- if an element is dropped, all its ascendants will be dropped
-- as well
-- (dual of filter)
filterMap : (n -> Maybe n2) -> (e -> Maybe e2) -> Graph n e -> Graph n2 e2
filterMap fn fe (Graph g) =
   let g2 = rawFilterMap fn fe g
   in
   Graph g2 |> sanitise

-- if an edge is kept, all its descendants will also be
-- whether or not they are explicitely kept
keepBelow : (n -> Bool) -> (e -> Bool) -> Graph n e -> Graph n e
keepBelow fn fe (Graph g) =
   let g2 = rawFilter fn fe g
   in
   let dict = mapRec (always ()) (always ()) (\_ -> identity) (\_ _ _ -> identity)
        
        (IntDict.keys g2) 
        (Graph g)
   in
   
   dict
   -- IntDict.intersect g dictIds |> Graph 

-- generates the full embedded subgraph containing
-- the given objects and everything between them
-- (the dual of filter)
{- keepUp : (n -> Bool) -> (e -> Bool) -> Graph n e -> Graph n e
keepUp fn fe (Graph g) =    
   let dict = mapRec fn fe (\_ -> fn) 
         (\_ b1 b2 e -> 
            -- on le garde si la source et le but sont deja dedans
            (b1 && b2) ||
             fe e)
         (IntDict.map (\_ -> Input) g)
         (IntDict.keys g) 
   in
   let dictIds = IntDict.filter 
          (\_ o -> case o of 
                     Output _ -> True
                     _ -> False )
         dict
   in
   IntDict.intersect g dictIds |> Graph -}

incomings : Id -> Graph n e -> List (Edge e)
incomings id g =
    edges g |> List.filter (\ { to } -> to == id)

outgoings : Id -> Graph n e -> List (Edge e)
outgoings id g =
    edges g |> List.filter  (\ { from } -> from == id)
   

invertEdge : EdgeId -> Graph n e -> Graph n e
invertEdge id (Graph g) =
  IntDict.update id 
     (\ e -> case e of
              Just (EdgeObj i1 i2 l) -> Just (EdgeObj i2 i1 l)
              _ -> e
      ) g |> Graph

-- merge two objects: the first one is kept, and all the
-- objects above the second one refers then to the first one.
-- Hence, the nature of the new object (edge/node) depends
-- only on the first object
merge : Id -> Id -> Graph n e -> Graph n e
merge i1 i2 g = 
  if i1 == i2 then g else 
  rawMerge i1 i2 g |> sanitise

rawMerge : Id -> Id -> Graph n e -> Graph n e
rawMerge i1 i2 (Graph g) =
   let repl k = if k == i2 then i1 else k in
   g |> IntDict.map (\_ o -> case o of
                   EdgeObj j1 j2 e ->                        
                        EdgeObj (repl j1) (repl j2) e
                   NodeObj _ -> o
      )
     |> IntDict.remove i2
   |> Graph
   

-- same as merge, but if i1 and i2 are edges, we first merge the sources and targets
-- (recursively)
-- if i1 is a vertex and i2 is an edge, we merge the source and the target of i2 with i1
recursiveMerge : Id -> Id -> Graph n e -> Graph n e
recursiveMerge i1 i2 g =
   if i1 == i2 then g else 
   recursiveMergeAux i1 i2 g |> sanitise |> removeLoops

recursiveMergeAux : Id -> Id -> Graph n e -> Graph n e
recursiveMergeAux i1 i2 (Graph g) =
   case (IntDict.get i1 g, IntDict.get i2 g) of
      (Just (EdgeObj a1 a2 _), Just (EdgeObj b1 b2 _)) ->
        Graph g |> recursiveMerge a1 b1 
        |> recursiveMerge a2 b2 
         |> rawMerge i1 i2
      (Just (NodeObj _), Just (EdgeObj a1 a2 _)) ->
         Graph g |> recursiveMerge i1 a1
                 |> recursiveMerge i1 a2
                 |> rawMerge i1 i2
      _ -> rawMerge i1 i2 (Graph g)

   

removeLoops : Graph n e -> Graph n e
removeLoops = 
       sanitise >> 
         (mapRep <| rawFilterIds (always True) (\ id1 id2 _ -> id1 /= id2))



addId : Int -> GraphRep n e -> GraphRep n e
addId n g =
   IntDict.toList g 
   |> List.map
     (\(id, o) -> (id + n, 
           case o of 
            NodeObj _ -> o
            EdgeObj i1 i2 e -> EdgeObj (i1 + n)(i2 + n) e
        )
     )
     |> IntDict.fromList

-- clash of indices?
union : Graph n e -> Graph n e -> Graph n e
union (Graph base) (Graph ext) = 
  IntDict.union base ext |> Graph

-- make the indices of the second graph disjoint from the first
makeDisjoint : Graph n e -> Graph n e -> Graph n e  
makeDisjoint (Graph base) (Graph ext) =
   let baseId = supId base in
   let extUp = addId baseId ext in  
   Graph extUp

-- indices in the base graphe are kept
disjointUnion : Graph n e -> Graph n e -> 
    { extendedGraph : Graph n e, subGraph : Graph n e }
disjointUnion base ext = 
   let subGraph = makeDisjoint base ext in
   { extendedGraph = union base subGraph, subGraph = subGraph }


computeDimensions : Graph n e -> Graph n (e, Int)
computeDimensions = 
  mapRecAll (always 0) Tuple.second (always identity)
     (\_ n1 n2 e -> (e, 1 + max n1 n2 ))


-- returns a graph where ids are ordered at least by the dimensions of the cells
-- (the first ids are vertices, then 1-cells, and so on)
normalise : Graph n e -> Graph n e
normalise g = 
   let getDim (_, o) =
         case o of
          NodeObj _ -> 0
          EdgeObj _ _ (_, dim) -> dim
   in
   let gWithDims = computeDimensions g |> graphRep |> IntDict.toList 
          |> List.sortBy getDim
   in
   let idDict = List.foldl 
        (\ (id, _) d -> IntDict.insert id (IntDict.size d) d)
        IntDict.empty gWithDims
   in
   let getId id = case IntDict.get id idDict of
             Nothing -> id
             Just i -> i
   in   
   let updateId o = case o of 
               NodeObj l -> NodeObj l
               EdgeObj i1 i2 (e, dim) -> EdgeObj (getId i1) (getId i2) e
   in
   gWithDims |>
   List.map (\(id, o) -> (getId id, updateId o))
   |> IntDict.fromList
   |> Graph

incidence : Graph n e -> IntDict { incomings : List (Edge e), outgoings : List (Edge e) }
incidence (Graph g) =
   let es = edges (Graph g) in
   let emptyInfo = { incomings = [], outgoings = []} in
   let insertIn e i = { i | incomings = e :: i.incomings} in
   let insertOut e i = { i | outgoings = e :: i.outgoings} in
   let aux l d =
         case l of 
           [] -> d
           e :: q ->
              aux q 
              <| IntDict.update e.from (Maybe.withDefault emptyInfo >> insertOut e >> Just)
              <| IntDict.update e.to (Maybe.withDefault emptyInfo >> insertIn e >> Just)
              <| d
   in
   let di = IntDict.map (\_ _ -> {incomings = [], outgoings = []}) g in
     aux es di

connectedClosure : (n -> Bool) -> (e -> Bool) -> Graph n e
                   -> Graph { n : n, isIn : Bool} { e : e, isIn : Bool}
connectedClosure fn fe (Graph g) =
   let li = rawFilter fn fe g |> IntDict.keys in
   let inc = incidence (Graph g) in   
   let aux d l =         
         case l of
          [] -> d
          t :: q ->            
            case IntDict.get t d of
              Nothing -> aux d q              
              Just i ->
                  let lsuite =
                         (case getEdge t (Graph g) of
                            Nothing -> []
                            Just { from, to } -> [ from, to ]
                           )
                       ++ List.map .id i.incomings
                       ++ List.map .id i.outgoings
                       ++ q
                   in
                   aux (IntDict.remove t d) lsuite
   in
   
   let ids = aux inc li |> IntDict.keys in
   
   map (\id n -> { n = n, isIn = True})
       (\id e -> { e = e, isIn = True})
   (Graph g)
   |> updateList ids 
   (\{n} -> { n = n, isIn = False})(\{e} -> { e = e, isIn = False})


minimal : Graph n e -> List NodeId
minimal g = 
  let gedges = edges g in
  nodes g |> List.map .id 
  |> List.filter (\ id -> List.all (\ e -> e.to /= id) gedges)

maximal : Graph n e -> List NodeId
maximal g = 
  let gedges = edges g in
  nodes g |> List.map .id 
  |> List.filter (\ id -> List.all (\ e -> e.from /= id) gedges)

topmostObjects : Graph n e -> List Id
topmostObjects g = 
  let gedges = edges g in
  allIds g |>
  List.filter (\ id -> List.all (\ e -> e.to /= id && e.from /= id) gedges)

topmostObject : Graph n e -> Maybe Id
topmostObject g =
   case topmostObjects g of
       [ id ] -> Just id
       _ -> Nothing

nodeIds : Graph n e -> List NodeId
nodeIds g = 
   nodes g |> List.map .id  

allIds : Graph n e -> List Id
allIds (Graph g) = IntDict.keys g

makeCylinder : Graph n e -> Graph n e -> e -> Bool -> 
   { extendedGraph : Graph n e, newSubGraph : Graph n e, edgeIds : List EdgeId}
makeCylinder g subGraph label inverted = 
   let extGraph = disjointUnion g subGraph in
   let idPairs = List.Extra.zip (nodeIds subGraph) (nodeIds extGraph.subGraph) 
           |> List.map (\ (id1, id2) -> if inverted then (id2, id1) else (id1, id2))
   in
   let (extendedGraph, idEdges) = newEdges extGraph.extendedGraph idPairs label in
   { extendedGraph = extendedGraph, newSubGraph = extGraph.subGraph, edgeIds = idEdges}

newEdges : Graph n e -> List (Id, Id) -> e -> (Graph n e, List EdgeId)
newEdges g idPairs labelEdge = 
    let (extendedGraph, idEdges) = 
               List.foldl (\(id1, id2) (graph, l) -> 
                              let (newGraph, idEdge) = (newEdge graph id1 id2 labelEdge) in
                              (newGraph, idEdge :: l)
                           ) 
                  (g, []) idPairs 
    in
      (extendedGraph, idEdges)

-- TODO: factor makeCone and makeCylinder
makeCone : Graph n e -> List Id -> n -> e -> Bool -> 
   { extendedGraph : Graph n e, newSubGraph : Graph n e, edgeIds : List EdgeId}
makeCone g ids labelNode labelEdge inverted = 
   let extGraph = newNode empty labelNode |> Tuple.first |> disjointUnion g in
   let newId = nodeIds extGraph.subGraph |> List.head |> Maybe.withDefault 0 in
   let idPairs = 
        List.map (\ id -> if inverted then (newId, id) else (id, newId))
        ids
   in
   let (extendedGraph2, idEdges) = newEdges extGraph.extendedGraph idPairs labelEdge in
   { extendedGraph = extendedGraph2, newSubGraph = extGraph.subGraph, edgeIds = idEdges}


complement : Graph n e -> Graph n e -> Graph n e
complement graph subGraph =
   let markedGraph = map (\ _ n -> (False, n)) (\ _ n -> (False, n)) graph in
   let ids = nodeIds subGraph in

   updateList ids (\ (_, n) -> (True, n)) (\ (_, n) -> (True, n)) markedGraph 
   |> drop (Tuple.first) (Tuple.first)
   |> map (always Tuple.second) (always Tuple.second)

any : (n -> Bool) -> (e -> Bool) -> Graph n e -> Bool
any fn fe (Graph g) =
       IntDictExtra.any 
       (\ o -> case o of 
                 NodeObj n -> fn n
                 EdgeObj _ _ e -> fe e) g
     

{- sourceNode : Graph n e -> Id -> NodeId
sourceNode (Graph g) id =
  case IntDict.get id g of
      Just (NodeObj n) -> id
      Just (EdgeObj i1 i2 e) -> sourceNode (Graph g) i1
      Nothing -> id

findInitial : Graph n e -> Id -> NodeId
findInitial g start =
  incomings start g 
  |> List.head
  |> Maybe.map .from
  |> Maybe.map (findInitial (remove start g))
  |> Maybe.withDefault start -}

