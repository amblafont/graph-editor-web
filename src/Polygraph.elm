module Polygraph exposing (Graph, Id, EdgeId, NodeId, empty, allIds, nodeIds,
     newNode, newEdge, isEmpty, exists, existsAll,
     update, updateNode, updateEdge, updateNodes, updateList,
     invertEdge, md_invertEdge, md_merge, md_recursiveMerge, makeCylinder, makeCone,
     getNode, getNodes, getEdge, getEdges, get, removeNode, removeEdge,
     md_removeEdge, emptyModifHelper,
     map, mapRecAll, invalidEdges,
     nodes, edges, fromNodesAndEdges,
     filterNodes, keepBelow, filterMap,
     Node, Edge, nextId, isEmptyModif,
     incomings, outgoings, drop, md_drop,
     normalise, 
     disjointUnion, md_disjointUnion, edgeMap, nodeMap, codec,  mapCodec,
     modifCodec, mapModifCodec,
     Modif, ModifJS, ModifHelper, finaliseModif, newModif, MergeFunctions
     , md_newNode, md_newEdge, md_update, md_map, 
     md_updateEdge, md_updateNodes, applyModifHelper,
     md_makeCylinder, md_makeCone, debugModifHelperGraph,
     translateId,TranslationId,applyModifTrans, defaultTranslation
     , {- findInitial, sourceNode, -} removeLoops,
     incidence, any, connectedClosure, minimal, maximal, complement, topmostObject)
import IntDict exposing (IntDict)
import IntDictExtra 
import Maybe.Extra as Maybe
import List.Extra
import Codec exposing (Codec)
import IntDict exposing (update)
import Modif


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
-- a partial graph may not be valid: the source/target of the edges
-- may not be included
type alias PartialGraphRep n e = GraphRep n e

type Graph n e = Graph {graph : GraphRep n e, nextId : Id}

isEmpty : Graph n e -> Bool
isEmpty g = graphRep g |> IntDict.isEmpty

graphRep : Graph n e -> GraphRep n e
graphRep (Graph {graph}) = graph

setGraphRep : Graph n e -> GraphRep n2 e2 -> Graph n2 e2
setGraphRep (Graph g0) g = Graph {nextId = g0.nextId, graph = g}

{- 
unGraph : Graph n e -> GraphRep n e
unGraph graph =
    case graph of
        Graph rep ->
            rep -}

mapRep : (GraphRep n e -> GraphRep n2 e2) -> (Graph n e -> Graph n2 e2)
mapRep f g = 
   setGraphRep g <| f <| graphRep g
{-| An empty graph.

    size empty == 0

-}
empty : Graph n e
empty =
    Graph {nextId = 0, graph = IntDict.empty}

{-
supId : GraphRep n e -> Id 
supId g =
    case IntDict.findMax g of
             Just (id, _) -> id + 1
             Nothing -> 0
-}

nextId : Graph n e -> Id
nextId (Graph g) = g.nextId -- supId g

newObjectAtId : Graph n e -> Object n e -> Id -> Graph n e
newObjectAtId (Graph g) o id =
  Graph { g | graph = IntDict.insert id o g.graph }

newObject : Graph n e -> Object n e -> (Graph n e, Id)
newObject (Graph g) o =
  let id = g.nextId in
  case newObjectAtId (Graph g) o id of
    Graph extendedGraph -> (Graph { extendedGraph | nextId = id + 1}, id)


newNode : Graph n e -> n -> (Graph n e, NodeId)
newNode g n = newObject g <| NodeObj n

newEdge : Graph n e -> Id -> Id -> e -> (Graph n e, EdgeId)
newEdge g i1 i2 e = newObject g <| EdgeObj i1 i2 e


rawRemoveList : List Id -> GraphRep n e -> GraphRep n e
rawRemoveList = IntDictExtra.removeList

removeList : List Id -> Graph n e -> Graph n e
removeList l g = mapRep (rawRemoveList l) g |> sanitise
    

remove : Id -> Graph n e -> Graph n e
remove id = removeList [id]

-- TODO check that it is an edge
removeEdge :  EdgeId -> Graph n e ->Graph n e
removeEdge = remove

md_removeEdge :  EdgeId -> ModifHelper n e -> ModifHelper n e
md_removeEdge id (ModifHelper m) =
    ModifHelper { m | 
        graph = removeEdge id m.graph, removeIds = id :: m.removeIds}

removeNode : NodeId -> Graph n e -> Graph n e
removeNode = remove
  

mapObj : (n -> n2) -> (e -> e2) -> Object n e -> Object n2 e2
mapObj fn fe o =
  case o of
     NodeObj n -> NodeObj (fn n)
     EdgeObj i1 i2 e -> EdgeObj i1 i2 (fe e)


update : Id -> (n -> n) -> (e -> e) -> Graph n e -> Graph n e
update i fn fe =
  mapRep <| updateRep i fn fe

updateList : List Id -> (n -> n) -> (e -> e) -> Graph n e -> Graph n e
updateList l fn fe g =
    List.foldl (\ i -> update i fn fe)  g l

updateRep : Id -> (n -> n) -> (e -> e) -> GraphRep n e -> GraphRep n e
updateRep i fn fe = IntDict.update i (Maybe.map (mapObj fn fe))

updateListRep : List Id -> (n -> n) -> (e -> e) -> GraphRep n e -> GraphRep n e
updateListRep l fn fe g =
    List.foldl (\ i -> updateRep i fn fe)  g l



updateNode : NodeId -> (n -> n) -> Graph n e -> Graph n e
updateNode i fn g = update i fn identity g 

updateEdge : EdgeId -> (e -> e) -> Graph n e -> Graph n e
updateEdge i fe g = update i identity fe g

updateNodes : List (Node a) -> Graph a b -> Graph a b
updateNodes l g =  
  List.foldl (\ { id, label } g2 -> updateNode id (always label) g2) g l


mapGraphRep : (NodeId -> n1 -> n2) -> (EdgeId -> e1 -> e2) -> GraphRep n1 e1 -> GraphRep n2 e2
mapGraphRep fn fe = IntDict.map (\ i -> mapObj (fn i) (fe i))

map : (NodeId -> n1 -> n2) -> (EdgeId -> e1 -> e2) -> Graph n1 e1 -> Graph n2 e2
map fn fe = 
   mapRep <| mapGraphRep fn fe

{- getFull : Id -> (n -> a) -> (Id -> Id -> e -> a) -> Graph n e -> Maybe a
getFull id fn fe (Graph g) =
   case IntDict.get id g of
      Just (NodeObj n) -> Just <| fn n
      Just (EdgeObj i1 i2 e) -> Just <| fe e i1 i2
      Nothing -> Nothing -}

get : Id -> (n -> a) -> (e -> a) -> Graph n e -> Maybe a
get id fn fe g =
   case IntDict.get id <| graphRep g of
      Just (NodeObj n) -> Just <| fn n
      Just (EdgeObj i1 i2 e) -> Just <| fe e
      Nothing -> Nothing

exists : Graph n e -> Id -> Bool
exists g id =
   case get id (always ()) (always ()) g of 
      Nothing -> False
      Just _ -> True

existsAll : Graph n e -> List Id -> Bool
existsAll g ids = List.all (exists g) ids

existsAny : Graph n e -> List Id -> Bool
existsAny g ids = List.any (exists g) ids
   
getEdge : EdgeId -> Graph n e -> Maybe (Edge e)
getEdge id g = graphRep g |> IntDict.get id |> Maybe.andThen (objEdge id)

getEdges : List EdgeId -> Graph n e -> List (Edge e)
getEdges l g = 
      graphRep g |> 
      IntDictExtra.getList l |> List.filterMap (\(id, e) -> objEdge id e)


getNode : NodeId -> Graph n e -> (Maybe n)
getNode id g = graphRep g |> IntDict.get id  |> Maybe.andThen objNode

getNodes : List NodeId -> Graph n e -> List (Node n)
getNodes l g = 
      graphRep g |> IntDictExtra.getList l
      |> List.filterMap (\(id, e) -> objNode e 
      |> Maybe.map (Node id))

nodesRep : GraphRep n e -> List (Node n)
nodesRep g =
   IntDict.toList g |> List.filterMap 
      (\(id, n) -> objNode n |> Maybe.map (Node id))

nodes : Graph n e -> List (Node n)
nodes g =
   graphRep g |> nodesRep 

edgesRep : GraphRep n e -> List (Edge e)
edgesRep g =
   IntDict.toList g |> List.filterMap 
      (\(id, e) -> objEdge id e)

edges : Graph n e -> List (Edge e)
edges g = 
   graphRep g |> edgesRep

repFromNodesAndEdges : List (Node n) -> List (Edge e) -> GraphRep n e
repFromNodesAndEdges ln le =
   let dn = IntDict.fromList 
          <| List.map (\{ id, label} -> (id, NodeObj label)) ln
       de = IntDict.fromList 
          <| List.map (\{ id, from, to, label} -> 
          (id, EdgeObj from to label)) le
   in
   IntDict.union dn de


fromNodesAndEdges : Int -> List (Node n) -> List (Edge e) -> Graph n e 
fromNodesAndEdges nId ln le =
   Graph { graph = repFromNodesAndEdges ln le, nextId = nId }  
     

codecRep : Codec (GraphRep n e) {nodes : List (Node n), edges : List (Edge e) }
codecRep =
  Codec.build 
  (\g -> 
     { nodes = nodesRep g, edges = edgesRep g }
  )
  (\ r -> repFromNodesAndEdges r.nodes r.edges)

codec : Codec (Graph n e) 
    { nextId : Id, nodes : List (Node n), edges : List (Edge e) }
codec =
  Codec.build 
  (\ g -> 
     { nodes = nodes g, edges = edges g, nextId = nextId g }
  )
  (\ r -> fromNodesAndEdges r.nextId r.nodes r.edges)
{-
nextIdCodec : Codec 
   { nextId : Int, nodes : List (Node n), edges : List (Edge e) }
   { nodes : List (Node n), edges : List (Edge e) }
nextIdCodec = 
   let maxList l =
        List.maximum (List.map .id l) |> Maybe.withDefault -1 
   in 
   Codec.build
   (\ c -> { nodes = c.nodes, edges = c.edges })
   (\ c -> { nextId = 
                        1 + max (maxList c.nodes)
                               (maxList c.edges)
                     , nodes = c.nodes
                     , edges = c.edges })
-}
mapCodec : Codec n1 n2 -> Codec e1 e2 -> Codec (Graph n1 e1) (Graph n2 e2) 
mapCodec c1 c2 = 
     Codec.build
       (map (always (Codec.encoder c1)) (always (Codec.encoder c2)))
       (map (always (Codec.decoder c1)) (always (Codec.decoder c2)))

mapRepCodec : Codec n1 n2 -> Codec e1 e2 -> Codec (GraphRep n1 e1) (GraphRep n2 e2) 
mapRepCodec c1 c2 = 
     Codec.build
       (mapGraphRep (always (Codec.encoder c1)) (always (Codec.encoder c2)))
       (mapGraphRep (always (Codec.decoder c1)) (always (Codec.decoder c2)))

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

-- edges whose source or target do not exist anymore.
invalidEdges : Graph n e -> List (Edge e)
invalidEdges fullGraph =
   let g = graphRep fullGraph in
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
sanitise g =
   let d = graphRep g in
   let ids = invalidEdges g |> List.map .id in
    IntDictExtra.removeList ids d |> setGraphRep g


-- returns also the list of edges that could not be treated
mapRecAll : 
      (n2 -> a)
   -> (e2 -> a)
   ->  (NodeId -> n1 -> n2) 
   -> (EdgeId -> a -> a -> e1 -> e2) 
   -> Graph n1 e1 -> Graph n2 e2
mapRecAll cn ce fn fe = 
   mapRep (mapRecRepAll cn ce fn fe)

mapRecRepAll : 
      (n2 -> a)
   -> (e2 -> a)
   ->  (NodeId -> n1 -> n2) 
   -> (EdgeId -> a -> a -> e1 -> e2) 
   -> GraphRep n1 e1 -> GraphRep n2 e2
mapRecRepAll cn ce fn fe g = 
   mapRecRep cn ce fn fe (IntDict.keys g) g


mapRec : 
      (n2 -> a)
   -> (e2 -> a)
   ->  (NodeId -> n1 -> n2) 
    -> (EdgeId -> a -> a -> e1 -> e2)
    -> List Id
    -> Graph n1 e1 -> Graph n2 e2
mapRec cn ce fn fe ids = 
   mapRep (mapRecRep cn ce fn fe ids)


-- returns also the list of edges that could not be treated
mapRecRep : 
      (n2 -> a)
   -> (e2 -> a)
   ->  (NodeId -> n1 -> n2) 
    -> (EdgeId -> a -> a -> e1 -> e2)
    -> List Id
    -> GraphRep n1 e1 -> GraphRep n2 e2
mapRecRep cn ce fn fe ids g = 
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
   gf

   

rawFilterMapIds : (Id -> n -> Maybe n2) -> (Id -> Id -> Id -> e -> Maybe e2) -> GraphRep n e -> GraphRep n2 e2
rawFilterMapIds fn fe =
     IntDictExtra.filterMap
        (\id o -> 
           case o of
               EdgeObj id1 id2 e -> fe id id1 id2 e
                  |> Maybe.map (EdgeObj id1 id2)
               NodeObj n -> (fn id n) |> Maybe.map NodeObj

         )

rawFilterIds : (n -> Bool) -> (Id -> Id -> e -> Bool) -> GraphRep n e -> GraphRep n e
rawFilterIds fn fe =
     rawFilterMapIds (\ _ -> Just >> Maybe.filter fn)
        (\_ id1 id2 -> (Just >> Maybe.filter (fe id1 id2)))

filterIds : (n -> Bool) -> (e -> Bool) -> GraphRep n e -> List Id
filterIds fn fe g = rawFilter fn fe g |> IntDict.keys

loopsIds : GraphRep n e -> List Id
loopsIds g = rawFilterIds (always False) (\ id1 id2 _ -> id1 == id2) g |> IntDict.keys

rawFilter : (n -> Bool) -> (e -> Bool) -> GraphRep n e -> GraphRep n e
rawFilter fn fe = rawFilterIds fn (\ _ _ -> fe)

rawFilterMap : (n -> Maybe n2) -> (e -> Maybe e2) -> GraphRep n e -> GraphRep n2 e2
rawFilterMap fn fe = rawFilterMapIds (always fn) (\ _ _ _ -> fe)




-- if an element is dropped, all its ascendants will be dropped
-- as well
-- (dual of filter)

drop : (n -> Bool) -> (e -> Bool) -> Graph n e -> Graph n e
drop fn fe =
   filterMap (Just >> Maybe.filter (not << fn))
     (Just >> Maybe.filter (not << fe))

md_drop : (n -> Bool) -> (e -> Bool) -> 
             ModifHelper n e -> ModifHelper n e
md_drop fn fe (ModifHelper m) =
   let gn = .label >> fn 
       ge = .label >> fe
   in
   let ids = filterIds gn ge <| graphRep m.graph in
   ModifHelper { m | graph = drop gn ge m.graph,
                  removeIds = ids ++ m.removeIds }
{-
-- doesn't drop the element, but all its dependencies
dropAbove : (n -> Bool) -> (e -> Bool) -> Graph n e -> Graph n e
dropAbove fn fe g =
   {- let removedGraph = drop fn fe g in
   let gd = graphRep g in
   let noneGraph = rawFilter fn fe gd in
   -- we add again the None stuff
   mapRep (IntDict.union noneGraph) removedGraph -}
   let dep = upwardDependencies fn fe g in
   let depG = rawFilter (not << fn) (not << fe) dep in
   IntDictExtra.removeList (IntDict.keys depG) dep 
   |> setGraphRep g
-}

-- the final graph may be invalid
upwardDependencies : (n -> Bool) -> (e -> Bool) -> GraphRep n e -> PartialGraphRep n e
upwardDependencies fn fe g =
    mapRecRepAll Tuple.first Tuple.first
      (\_ n -> (fn n, n)) 
      (\_ b1 b2 e -> (b1 || b2 || fe e, e))
      g
   |> rawFilterIds Tuple.first (\_ _ -> Tuple.first)
   |> mapGraphRep (always Tuple.second) (always Tuple.second)

upwardDependenciesIds : List Id -> GraphRep n e -> PartialGraphRep n e
upwardDependenciesIds ids g =
    let g2 = mapGraphRep (\ _ x -> (False, x) ) (\ _ x -> (False, x) ) g in
    let g3 = updateListRep ids (\ (_, n) -> (True, n)) (\ (_, e) -> (True, e)) g2 in
    upwardDependencies Tuple.first Tuple.first g3
    |> mapGraphRep (always Tuple.second) (always Tuple.second)
   
-- if an element is dropped, all its ascendants will be dropped
-- as well
-- (dual of filter)
filterMap : (n -> Maybe n2) -> (e -> Maybe e2) -> Graph n e -> Graph n2 e2
filterMap fn fe g =
   let g2 = rawFilterMap fn fe <| graphRep g
   in
   setGraphRep g g2 |> sanitise

-- if an edge is kept, all its descendants will also be
-- whether or not they are explicitely kept
keepBelow : (n -> Bool) -> (e -> Bool) -> Graph n e -> Graph n e
keepBelow fn fe g =
   let g2 = rawFilter fn fe (graphRep g)
   in
   let dict = mapRec (always ()) (always ()) (\_ -> identity) (\_ _ _ -> identity)
        
        (IntDict.keys g2) 
        g
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
invertEdge id  =
  mapRep
  (IntDict.update id 
     (\ e -> case e of
              Just (EdgeObj i1 i2 l) -> Just (EdgeObj i2 i1 l)
              _ -> e
      ))

md_invertEdge : EdgeId -> ModifHelper n e -> ModifHelper n e
md_invertEdge id (ModifHelper m) =
   ModifHelper {
      m | 
      graph = mapRep
  (IntDict.update id 
     (\ e -> case e of
              Just (EdgeObj i1 i2 l) -> Just (EdgeObj i2 i1 {l|edit = updateStatus l.edit})
              _ -> e
      )) m.graph
   }

updateStatus : EditStatus -> EditStatus
updateStatus s = case s of
   New -> New
   Edit -> Edit
   Keep -> Edit

-- merge two objects: the first one is kept, and all the
-- objects above the second one refers then to the first one.
-- Hence, the nature of the new object (edge/node) depends
-- only on the first object
md_merge : Id -> Id -> ModifHelper n e -> ModifHelper n e
md_merge i1 i2 g = 
  if i1 == i2 then g else 
  md_rawMerge i1 i2 g |> md_sanitise
{-
rawMerge : Id -> Id -> ModifHelper n e -> ModifHelper n e
rawMerge i1 i2 (ModifHelper m) =
   let repl k = if k == i2 then i1 else k in
   mapRep (IntDict.map (\_ o -> case o of
                   EdgeObj j1 j2 e ->                        
                        EdgeObj (repl j1) (repl j2) e
                   NodeObj _ -> o
      )
     >> IntDict.remove i2)
   -}


   

removeLoops : Graph n e -> Graph n e
removeLoops g = removeList (loopsIds <| graphRep g) g
       --sanitise << 
       --  (mapRep <| rawFilterIds (always True) (\ id1 id2 _ -> id1 /= id2))



addId : TranslationId  -> GraphRep n e -> GraphRep n e
addId tr g =
   let updateId i = translateId tr i
   in
   IntDict.toList g 
   |> List.map
     (\(id, o) -> (updateId id, 
           case o of 
            NodeObj _ -> o
            EdgeObj i1 i2 e -> EdgeObj (updateId i1)(updateId i2) e
        )
     )
     |> IntDict.fromList


-- make the indices of the second graph disjoint from the first
makeDisjoint : Graph n e -> GraphRep n e -> GraphRep n e  
makeDisjoint (Graph base) ext =
   -- let baseId = base.nextId in
   let extUp = addId (makeTranslationId 0 base.nextId) ext in  
   extUp
   -- Graph {graph = extUp, nextId = baseId + ext.nextId}

-- indices in the base graphe are kept
disjointUnion : Graph n e -> Graph n e -> 
    { extendedGraph : Graph n e, subGraph : Graph n e }
disjointUnion base (Graph ext) = 
   let subGraph = makeDisjoint base ext.graph in
   let newNextId = nextId base + ext.nextId in
   { extendedGraph = 
       Graph { graph = IntDict.union (graphRep base) subGraph
             , nextId = newNextId }
   , subGraph = Graph { graph = subGraph, nextId = newNextId }
   }

md_disjointUnion : ModifHelper n e -> Graph n e -> 
    { extendedGraph : ModifHelper n e, subGraph : Graph n e }
md_disjointUnion (ModifHelper m) ext = 
   let base = m.graph in
   let result = disjointUnion base
          <| map (\_ x -> {label = x,  edit = New}) (\_ x -> {label = x , edit = New}) ext
   in
   {extendedGraph = 
      ModifHelper { m | graph = result.extendedGraph},
      subGraph = map (always .label) (always .label) result.subGraph}
   


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
   let newGraph = 
         gWithDims |> 
         List.map (\(id, o) -> (getId id, updateId o))
         |> IntDict.fromList
   in
   Graph { graph = newGraph, nextId = IntDict.size idDict }


incidence : Graph n e -> IntDict { incomings : List (Edge e), outgoings : List (Edge e) }
incidence g =
   let gDict = graphRep g in
   let es = edges g in
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
   let di = IntDict.map (\_ _ -> {incomings = [], outgoings = []}) gDict in
   aux es di

connectedClosure : (n -> Bool) -> (e -> Bool) -> Graph n e
                   -> Graph { n : n, isIn : Bool} { e : e, isIn : Bool}
connectedClosure fn fe g =
   let li = rawFilter fn fe (graphRep g) |> IntDict.keys in
   let inc = incidence g in   
   let aux d l =         
         case l of
          [] -> d
          t :: q ->            
            case IntDict.get t d of
              Nothing -> aux d q              
              Just i ->
                  let lsuite =
                         (case getEdge t g of
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
   g
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
allIds = graphRep >> IntDict.keys

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

md_extGraph_converter : 
     ModifHelper n e 
     -> 
      {extendedGraph : GraphHelper n e, newSubGraph : GraphHelper n e, edgeIds : List EdgeId}
     ->
     { extendedGraph : ModifHelper n e, newSubGraph : Graph n e, edgeIds : List EdgeId}
md_extGraph_converter (ModifHelper m) extGraph =
      { extendedGraph = ModifHelper {m | graph = extGraph.extendedGraph}
   , newSubGraph = map (always .label) (always .label) extGraph.newSubGraph, 
     edgeIds = extGraph.edgeIds}

md_makeCone : ModifHelper n e -> List Id -> n -> e -> Bool ->
    { extendedGraph : ModifHelper n e, newSubGraph : Graph n e, edgeIds : List EdgeId}
md_makeCone (ModifHelper m) ids labelNode labelEdge inverted =
    makeCone m.graph ids {label = labelNode, edit = New}
                                       {label = labelEdge, edit = New}
                                       inverted
      |> md_extGraph_converter (ModifHelper m)

md_makeCylinder : ModifHelper n e -> Graph n e -> e -> Bool -> 
   { extendedGraph : ModifHelper n e, newSubGraph : Graph n e, edgeIds : List EdgeId}
md_makeCylinder (ModifHelper m) subGraph label inverted = 
     makeCylinder m.graph 
         (map (\ _ x -> {label = x, edit = New})(\ _ x -> {label = x, edit = New})subGraph)
         {label = label, edit = New} inverted
      |> md_extGraph_converter (ModifHelper m)
   

complement : Graph n e -> List Id -> Graph n e
complement graph ids =
   let markedGraph = map (\ _ n -> (False, n)) (\ _ n -> (False, n)) graph in
   -- let ids = nodeIds subGraph in

   updateList ids (\ (_, n) -> (True, n)) (\ (_, n) -> (True, n)) markedGraph 
   |> drop (Tuple.first) (Tuple.first)
   |> map (always Tuple.second) (always Tuple.second)

{-
upwardDependencies : Graph n e -> List Id -> Graph (Bool,n)  (Bool,e)
upwardDependencies graph ids =
   let  markedGraph = map (\ _ n -> (False, n)) (\ _ n -> (False, n)) graph in
   updateList ids (\ (_, n) -> (True, n)) (\ (_, n) -> (True, n)) markedGraph 
   |>
   mapRecAll Tuple.first Tuple.first
      (\_ (b, n) -> (b, n)) 
      (\_ b1 b2 (b3, e) -> (b1 || b2 || b3, e))
  -}    


any : (n -> Bool) -> (e -> Bool) -> Graph n e -> Bool
any fn fe =
   graphRep >>
   IntDictExtra.any 
   (\ o -> 
     case o of 
      NodeObj n -> fn n
      EdgeObj _ _ e -> fe e
   )
     

{-
type ModifNode n = Remove
         | Edit n
         | New n
   
type alias ModifEdge e = ModifNode e
-}
   
{-

The graph of a transaction may be invalid: the source of an edge may not exist
(because it is in the graph).
If a label is "none", then the object is removed.
Any id greater than the baseId is the id of a new object.
But an id smaller could also be the id of a new object (if it was previously existing)
The graph may contain objects that are smaller than the baseId: it either 
means that it is modifying an existing object, or that it is cancelling 
a previous "remove" transaction.

-}
type Modif n e = Modif
-- Edit or New 
           { editGraph : PartialGraphRep n e
           , newGraph : PartialGraphRep n e
           , nextId : Id
           , baseId : Id
           , removeIds : List Id
           }

type alias ModifJS n e = { baseId : Id, nextId : Id, 
    editNodes : List (Node n), 
    editEdges : List (Edge e),
    newNodes : List (Node n),
    newEdges : List (Edge e),
    removeIds : List Id }

modifCodec : Codec (Modif n e) (ModifJS n e)
modifCodec =
  Codec.compose
  (
  Codec.object 
  (\editGraph newGraph nId baseId removeIds -> 
     { editGraph = editGraph, newGraph = newGraph,
        nextId = nId,
        baseId = baseId, removeIds = removeIds })
  
  (\ editGraph newGraph nId baseId removeIds -> 
     { baseId = baseId, nextId = nId, removeIds = removeIds, 
       editNodes = editGraph.nodes,
       editEdges = editGraph.edges,
       newNodes = newGraph.nodes,
       newEdges = newGraph.edges
     })
   |>
   Codec.fields .editGraph 
     (\ r -> { nodes = r.editNodes, edges = r.editEdges })
     codecRep
   |> Codec.fields .newGraph
     (\ r -> { nodes = r.newNodes, edges = r.newEdges })
     codecRep
   |> Codec.fields .nextId .nextId Codec.identity
   |> Codec.fields .baseId .baseId Codec.identity
   |> Codec.fields .removeIds .removeIds Codec.identity
   |> Codec.buildObject)
   (Codec.build (\ (Modif m) -> m) Modif)
   

mapModifCodec : Codec n1 n2 -> Codec e1 e2 -> Codec (Modif n1 e1) (Modif n2 e2) 
mapModifCodec c1 c2 = 
     let cc = mapRepCodec c1 c2 in
     let getter f (Modif m) = f m in
     Codec.object 
       (\ editGraph newGraph  nId baseId removeIds -> 
          Modif { editGraph = editGraph, 
                  newGraph = newGraph,
                  nextId = nId, baseId = baseId, removeIds = removeIds })
      (\ editGraph newGraph  nId baseId removeIds -> 
          Modif { editGraph = editGraph, 
                  newGraph = newGraph,
                  nextId = nId, baseId = baseId, removeIds = removeIds })
     |>  Codec.fields (getter .editGraph) (getter .editGraph) cc
     |>  Codec.fields (getter .newGraph) (getter .newGraph) cc
     |>  Codec.fields (getter .nextId) (getter .nextId) Codec.identity
     |>  Codec.fields (getter .baseId) (getter .baseId) Codec.identity
     |>  Codec.fields (getter .removeIds) (getter .removeIds) Codec.identity
     |> Codec.buildObject
          

-- type ModifHelper n e = ModifHelper { modif : Modif n e, base : Graph n e}
type EditStatus = Edit | New | Keep
type alias GraphHelper n e = Graph { label : n, edit : EditStatus} { label : e, edit : EditStatus}
type ModifHelper n e = ModifHelper 
         { graph : GraphHelper n e -- {label : n, edit : Bool} {label : e, edit : Bool}
         , baseId : Id
         , removeIds : List Id
         -- This field is used when applying modifhlepr:
         -- it applies the modification to the base graphe
         -- of course there is a more optimal way
         -- but doing this way makes debugging easier
         -- , baseGraph : Graph n e
         }

debugModifHelperGraph : ModifHelper n e -> GraphHelper n e
debugModifHelperGraph (ModifHelper {graph}) = graph


emptyModifHelper : ModifHelper n e
emptyModifHelper = newModif empty
newModif : Graph n e -> ModifHelper n e
newModif g = ModifHelper { 
   graph = map (\ _ n -> { label = n, edit = Keep}) (\ _ e -> { label = e, edit = Keep}) g
   , baseId = nextId g
   , removeIds = []
   -- , baseGraph = g
  }

filterHelperByStatus : EditStatus -> GraphHelper n e -> PartialGraphRep n e
filterHelperByStatus status graph =
   rawFilterMap
   (\ { label, edit } -> if edit == status then Just label else Nothing)
   (\ { label, edit } -> if edit == status then Just label else Nothing) 
   <| graphRep graph

finaliseModif : ModifHelper n e -> Modif n e
finaliseModif (ModifHelper { graph, baseId, removeIds }) =
   let editGraph = filterHelperByStatus Edit graph in
   let newGraph = filterHelperByStatus New graph in
   -- let removeDict = IntDict.fromList <| List.map (\ id -> (id, Nothing)) removeIds in
   Modif { editGraph = editGraph, newGraph = newGraph,
           baseId = baseId, nextId = nextId graph, removeIds = removeIds }

-- TODO: version + efficace
applyModifHelper : ModifHelper n e ->  Graph n e
applyModifHelper (ModifHelper m) = 
  map (\ _ { label } -> label) (\ _ { label } -> label) m.graph
   -- doModif (finaliseModif m) baseGraph
   -- |> Tuple.first
{-applyModifHelper (ModifHelper m) = 
   map (\ _ { label } -> label) (\ _ { label } -> label) m.graph
   -- |> removeLoops |> sanitise
   -- -}

md_graphMap : (GraphHelper n e -> GraphHelper n e)
                -> ModifHelper n e -> ModifHelper n e
md_graphMap f (ModifHelper m) = 
   ModifHelper { m | graph = f m.graph }

md_graph : ModifHelper n e -> GraphHelper n e
md_graph (ModifHelper {graph}) = graph
   --  case modif of 
      --  Modif {graph} -> graph

-- md_base : ModifHelper n e -> Graph n e
-- md_base (ModifHelper {base}) = base

md_setGraph : ModifHelper n e -> GraphHelper n e -> ModifHelper n e
md_setGraph (ModifHelper t) g = 
     ModifHelper {t | graph = g}

md_newNode : ModifHelper n e -> n -> (ModifHelper n e, NodeId)
md_newNode m n =
     let (g, id) = newNode (md_graph m) { label = n, edit = New} in
       (md_setGraph m g, id)

md_newEdge : ModifHelper n e -> Id -> Id -> e -> (ModifHelper n e, NodeId)
md_newEdge m id1 id2 n =
     let (g, id) = newEdge (md_graph m) id1 id2 { label = n, edit = New} in
       (md_setGraph m g, id)

type TranslationId = Translation {baseId : Id, delta : Int}

defaultTranslation : TranslationId
defaultTranslation = Translation { baseId = 0, delta = 0 }

makeTranslationId : Id -> Int -> TranslationId
makeTranslationId baseId delta = Translation { baseId = baseId, delta = delta}

translateId : TranslationId -> Id -> Id
translateId (Translation {baseId, delta}) id =
    if id < baseId then id else id + delta

doModif : MergeFunctions n e -> Modif n e -> Graph n e -> (Graph n e, TranslationId)
doModif merge (Modif t) g =
   let baseId = t.baseId in
   let deltaId = nextId g - baseId in
   let trans = makeTranslationId baseId deltaId in

   let shiftedTransactionGraph = addId trans t.newGraph in
   let newGraph = 
               let mapObject _ o1 o2 =
                    case (o1, o2) of
                     (NodeObj n1, NodeObj n2) -> NodeObj <| merge.mergeNode n1 n2
                     (EdgeObj _ _ e1, EdgeObj id1 id2 e2) -> 
                                          EdgeObj id1 id2 <| merge.mergeEdge e1 e2
                     _ -> o2
               in
                  IntDict.uniteWith 
                  mapObject
                  (IntDict.union (graphRep g) shiftedTransactionGraph )
                  t.editGraph
               -- <| mapGraphRep (always Just) (always Just) 
               -- <| graphRep g
   in
   -- let loops = loopsIds newGraph in
   let noneIds = t.removeIds in -- filterIds Maybe.isNothing Maybe.isNothing newGraph in
   let depsIds = upwardDependenciesIds noneIds newGraph |> IntDict.keys in
   let prunedGraph = rawRemoveList depsIds newGraph in
   let newNextId = nextId g + t.nextId - baseId in
   let finalGraph =  Graph { graph = prunedGraph, nextId = newNextId } 
   in
   (finalGraph, trans) -- |> removeLoops |> sanitise
   -- removeList t.removeIds finalGraph

-- remove the objects that depends on a removed object


reverseModif : Graph n e -> Modif n e -> Modif n e
reverseModif g (Modif t) =
   -- let sanitisedGraph = dropAbove Maybe.isNothing Maybe.isNothing t.graph in
   -- let allNone = mapGraphRep (\_ _ -> Nothing) (\_ _ -> Nothing) 
                  --   (graphRep t.graph)
   -- in
   let noneIds = t.removeIds
    -- rawFilter Maybe.isNothing Maybe.isNothing (graphRep t.graph) 
       --       |> IntDict.keys
   in
   let dep = -- mapGraphRep (always Just) (always Just) 
               upwardDependenciesIds noneIds <| graphRep g
   in
   let modifiedGraph = IntDict.intersect (graphRep g) t.editGraph in
   let createdIds = IntDict.keys <| IntDict.diff t.newGraph (graphRep g) in
   -- what about sanitise removeLoops
   
   -- let graph = IntDict.union dep allNone in
   Modif {  editGraph = modifiedGraph , 
                  newGraph = dep
               --   |> sanitise |> removeLoops
                , nextId = t.nextId
                , baseId = t.nextId
                , removeIds = createdIds }
                 

-- isVacuousModif : Graph n e -> Modif n e -> Bool
-- isVacuousModif g (Modif m) = 
   
--    isEmpty m.graph &&
--    (if Debug.log "removeIds" m.removeIds == [] then True else 
--    let _ = Debug.log "graph" <| Codec.encoder codec g in
--     not <| Debug.log "exists any?" <| existsAny g m.removeIds)
   -- m.removeIds == [] &&  -- (graphRep m.graph |> IntDict.isEmpty)

isEmptyModif : Modif n e -> Bool
isEmptyModif (Modif m) =
   IntDict.isEmpty m.editGraph && IntDict.isEmpty m.newGraph && m.removeIds == []

normaliseModif : Graph n e -> Modif n e -> Modif n e
normaliseModif g (Modif m) =
   let grep = graphRep g in
   Modif {
      editGraph = IntDict.intersect m.editGraph grep ,
      newGraph = IntDict.diff m.newGraph grep,
      nextId = m.nextId,
      baseId = m.baseId,
      removeIds = List.filter (\ i -> IntDict.member i grep)
                m.removeIds
   }
   


applyModifTrans : MergeFunctions n e -> Graph n e -> Modif n e -> Modif.Result {translationId : TranslationId, graph : Graph n e} (Modif n e)
applyModifTrans merge g modif = 
   let t = normaliseModif g modif in
   if isEmptyModif t then Nothing else
   let (g2, trans) = doModif merge t g in
   Just { next = {graph = g2, translationId = trans }, undo = reverseModif g t }

type alias MergeFunctions n e = {
   mergeNode : n -> n -> n,
   mergeEdge : e -> e -> e
   }

-- applyModif : MergeFunctions n e -> Graph n e -> Modif n e -> Maybe {next : Graph n e, undo : Modif n e }
-- applyModif merge g t =
--    applyModifTrans g t |> 
--    Modif.map .graph identity -- Maybe.map (\ {next, undo} ->{next = next, undo = undo})



md_update : Id -> (n -> n) -> (e -> e) -> ModifHelper n e -> ModifHelper n e
md_update i fn fe =
  md_graphMap <| update i (\ { label, edit } -> { label = fn label, edit = updateStatus edit })
                          (\ { label, edit } -> { label = fe label, edit = updateStatus edit })
  -- md_updateNode i fn m |> md_updateEdge i fe

md_updateNode : NodeId -> (n -> n) -> ModifHelper n e -> ModifHelper n e
md_updateNode i fn =
  md_graphMap <| update i (\ { label, edit } -> { label = fn label, edit = updateStatus edit })
                          identity

md_updateEdge : NodeId -> (e -> e) -> ModifHelper n e -> ModifHelper n e
md_updateEdge i fe =
  md_graphMap <| update i identity (\ { label, edit } -> { label = fe label, edit = updateStatus edit })


   {-
   case getNode i (md_base m) of
     Just n -> md_setGraph m <| 
      newObjectAtId (md_graph m) (NodeObj <| Just <| fn n) i
     Nothing -> m

md_updateEdge : EdgeId -> (e -> e) -> ModifHelper n e -> ModifHelper n e
md_updateEdge i fn m =
   case getEdge i (md_base m) of
     Just e -> md_setGraph m <| 
      newObjectAtId (md_graph m) (EdgeObj e.from e.to <| Just <| fn e.label) i
     Nothing -> m

-}
md_map : (Id -> n -> Maybe n) -> (Id -> e -> Maybe e) -> ModifHelper n e -> ModifHelper n e
md_map fn fe (ModifHelper m) = 
   let makeFun f = \ i x -> Maybe.map (\ r -> { label = r, edit = updateStatus x.edit})
                  (f i x.label) |> Maybe.withDefault x
   in
   let mappedGraph = map (makeFun fn) (makeFun fe) m.graph in
   -- let noneIds = filterIds (Maybe.isNothing >> not) 
   --                         (Maybe.isNothing >> not) 
   --                         <| graphRep mappedGraph 
   -- in
   -- let newGraph = removeList noneIds <| graphRep mappedGraph in
   -- let finalGraph = filterMap identity identity mappedGraph
   -- in
   ModifHelper { m | graph = mappedGraph } -- , removeIds = noneIds ++ m.removeIds }
   {-
   let finalRep = IntDict.union g2 <| graphRep <| md_graph m in
   md_setGraph m <| setGraphRep (md_graph m) finalRep
   -}


md_updateNodes : List (Node a) -> ModifHelper a b -> ModifHelper a b
md_updateNodes l g =
  List.foldl (\ { id, label } g2 -> md_updateNode id (always label) g2) g l

md_removeLoops : ModifHelper a b -> ModifHelper a b
md_removeLoops (ModifHelper m) = 
   let loops = loopsIds <| graphRep m.graph in
   ModifHelper 
       { m | graph = removeList loops m.graph, removeIds = loops ++ m.removeIds }

-- no need to say that the invalid edges must be removed
md_sanitise : ModifHelper a b -> ModifHelper a b
md_sanitise (ModifHelper m) = ModifHelper { m | graph = sanitise m.graph }

md_rawMerge : Id -> Id -> ModifHelper n e -> ModifHelper n e
md_rawMerge i1 i2 (ModifHelper m) =
   
   let repl k = if k == i2 then i1 else k in
   let newGraph =
         mapRep (IntDict.map (\_ o -> case o of
                   EdgeObj j1 j2 e ->
                        if j1 == i2 || j2 == i2 then
                           EdgeObj (repl j1) (repl j2) {e | edit = updateStatus e.edit}
                        else o                            
                   NodeObj _ -> o
         )
          >> IntDict.remove i2) m.graph
   in 
   ModifHelper { m | graph = newGraph, removeIds = i2 :: m.removeIds}

-- same as merge, but if i1 and i2 are edges, we first merge the sources and targets
-- (recursively)
-- if i1 is a vertex and i2 is an edge, we merge the source and the target of i2 with i1
md_recursiveMerge : Id -> Id -> ModifHelper n e -> ModifHelper n e
md_recursiveMerge i1 i2 g =
   if i1 == i2 then g else 
   md_recursiveMergeAux i1 i2 g |> md_removeLoops |> md_sanitise

md_getObj : Id -> ModifHelper n e -> Maybe (Object n e)
md_getObj i m =
   Maybe.map (mapObj .label .label) <| IntDict.get i <| graphRep <| md_graph m
   {-
      Just o -> o
      Nothing -> IntDict.get i (graphRep <| md_base m)
      -}

md_recursiveMergeAux : Id -> Id -> ModifHelper n e -> ModifHelper n e
md_recursiveMergeAux i1 i2 g =
   case (md_getObj i1 g, md_getObj i2 g) of
      (Just (EdgeObj a1 a2 _), Just (EdgeObj b1 b2 _)) ->
         g |> md_recursiveMerge a1 b1 
        |> md_recursiveMerge a2 b2 
         |> md_rawMerge i1 i2
      (Just (NodeObj _), Just (EdgeObj a1 a2 _)) ->
         g |> md_recursiveMerge i1 a1
                 |> md_recursiveMerge i1 a2
                 |> md_rawMerge i1 i2
      _ -> md_rawMerge i1 i2 g
