module Polygraph exposing (Graph, Id, EdgeId, NodeId, empty,
     newNode, newEdge,
     update, updateNode, updateEdge, updateNodes, invertEdge,
     getNode, getEdge, get, removeNode, removeEdge,
     map, mapRec,
     nodes, edges, fromNodesAndEdges,
     filterNodes, filter,
     Node, Edge, nextId,
     incomings, outgoings, drop,
     union)
import IntDict exposing (IntDict)





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

{- objUniv : (n -> a) -> (e -> a) -> Object n e -> a
objUniv fn fe o =
  case o of
     EdgeObj _ _ e -> fe e 
     NodeObj n -> fn n -}

objEdge : Object n e -> Maybe (Id, Id, e)
objEdge o = case o of
  EdgeObj i1 i2 e -> Just (i1, i2, e)
  _ -> Nothing

objNode : Object n e -> Maybe n
objNode o = case o of
   NodeObj n -> Just n
   _ -> Nothing

type alias GraphRep n e = IntDict (Object n e)

type Graph n e =
   Graph (GraphRep n e)


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
removeList l (Graph g) =
   case l of 
     [] -> Graph g
     t :: q ->
         let gt = IntDict.remove t g in
         let newl =
              IntDict.filter (\ _ o -> case o of
                 EdgeObj i1 i2 _ -> i1 == t || i2 == t
                 _ -> False
              ) gt |> IntDict.keys
         in
           removeList (newl ++ q) (Graph gt)
           


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

updateNode : NodeId -> (n -> n) -> Graph n e -> Graph n e
updateNode i fn g = update i fn identity g 

updateEdge : EdgeId -> (e -> e) -> Graph n e -> Graph n e
updateEdge i fe g = update i identity fe g

map : (NodeId -> n1 -> n2) -> (EdgeId -> e1 -> e2) -> Graph n1 e1 -> Graph n2 e2
map fn fe = 
   mapRep <|
     IntDict.map (\ i -> mapObj (fn i) (fe i))


get : Id -> (n -> a) -> (e -> a) -> Graph n e -> Maybe a
get id fn fe (Graph g) =
   case IntDict.get id g of
      Just (NodeObj n) -> Just <| fn n
      Just (EdgeObj _ _ e) -> Just <| fe e
      Nothing -> Nothing

getEdge : EdgeId -> Graph n e -> Maybe (Id, Id, e)
getEdge id (Graph g) = IntDict.get id g |> Maybe.andThen objEdge

getNode : NodeId -> Graph n e -> Maybe n
getNode id (Graph g) = IntDict.get id g |> Maybe.andThen objNode

nodes : Graph n e -> List (Node n)
nodes (Graph g) = 
   IntDict.toList g |> List.filterMap 
      (\(id, n) -> objNode n |> Maybe.map (Node id))

edges : Graph n e -> List (Edge e)
edges (Graph g) = 
   let mkEdge id (i1, i2, e) = Edge id i1 i2 e in
   IntDict.toList g |> List.filterMap 
      (\(id, e) -> objEdge e |> Maybe.map (mkEdge id))

fromNodesAndEdges : List (Node n) -> List (Edge e) -> Graph n e 
fromNodesAndEdges ln le =
  let dn = IntDict.fromList 
          <| List.map (\{ id, label} -> (id, NodeObj label)) ln
      de = IntDict.fromList 
          <| List.map (\{ id, from, to, label} -> 
          (id, EdgeObj from to label)) le
   in
     
  Graph <| IntDict.union dn de 

filterNodes : Graph n e -> (n -> Bool) -> List (Node n)
filterNodes g f = nodes g |> List.filter (f << .label)

updateNodes : List NodeId -> (a -> a) -> Graph a b -> Graph a b
updateNodes l f g =
  List.foldl (\ id g2 -> updateNode id f g2) g l



-- tail recursive function

{- 

type MapRecObj a n1 e1 n2 e2 =
    Input (Object n1 e1)
  | Output a (Object n2 e2)
  -- impossible to affect
--   | Missing
  -- we have asked for inspecting e1
  | Waiting Id Id e1
mapRecAux : 
      (n1 -> a)
      -> (e1 -> a)
      -> (Id -> n1 -> n2) 
      -- ah oui c'est pas bon...
      -> (Id -> a -> a -> e1 -> e2)  
       -- input objects
       -> IntDict (MapRecObj a n1 e1 n2 e2)
       -- to be treated objects
       -> List Id
       -- Treated objects       
       -> IntDict (MapRecObj a n1 e1 n2 e2)
mapRecAux cn ce fn fe dict ids =
   let rec = mapRecAux cn ce fn fe in
   let ins id o = IntDict.insert id o dict in
    case ids of
       [] -> dict
       id :: tailIds ->
                   
         case IntDict.get id dict of           
            Just (Input (NodeObj n)) ->
                  rec 
                      (ins id (Output (cn n) <| NodeObj <| fn id n)) 
                      tailIds                         
            Just (Input (EdgeObj i1 i2 e)) ->
                  rec (ins id <| Waiting i1 i2 e) (i1 :: i2 :: id :: tailIds)
            Just (Waiting i1 i2 e) ->
                  case (IntDict.get i1 dict, IntDict.get i2 dict) of                  
                     (Just (Output a1 o1), Just (Output a2 o2)) ->
                        rec
                           (ins id 
                               (Output (ce e) <| EdgeObj i1 i2 <| 
                                       fe id a1 a2 e) 
                                ) 
                               tailIds
                     _ ->  rec dict tailIds
                   
            _ -> rec dict tailIds

mapRec : (n1 -> a)
      -> (e1 -> a)
    -> (Id -> n1 -> n2) 
    -> (Id -> a -> a -> e1 -> e2) 
    -> Graph n1 e1 -> (Graph n2 e2,  List (Edge e1))
mapRec cn ce fn fe (Graph g) = 
   let dict = mapRecAux cn ce fn fe
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
   --  Sadly, there is no IntDict.filterMap
   let gf = l
         |> List.filterMap (\(id, o) ->
             case o of
                  Output _ o2 -> Just (id, o2)
                  _ -> Nothing)
         |> IntDict.fromList  
        
   in
   (Graph gf, missings) -}

type MapRecObj a n1 e1 =
    Input (Object n1 e1)
  | Output (Object a a)
  -- impossible to affect
--   | Missing
  -- we have asked for inspecting e1
  | Waiting Id Id e1

mapRecAux :      
         (NodeId -> n1 -> a) 
      -- ah oui c'est pas bon...
      -> (EdgeId -> a -> a -> e1 -> a)  
       -- input objects
       -> IntDict (MapRecObj a n1 e1)
       -- to be treated objects
       -> List Id
       -- Treated objects       
       -> IntDict (MapRecObj a n1 e1)
mapRecAux fn fe dict ids =
   let getA o = case o of
         NodeObj a -> a
         EdgeObj _ _ a -> a
   in
   let rec = mapRecAux fn fe in
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
-- returns also the list of edges that could not be treated
mapRec : 
       (NodeId -> n1 -> a) 
    -> (EdgeId -> a -> a -> e1 -> a) 
    -> Graph n1 e1 -> (Graph a a,  List (Edge e1))
mapRec fn fe (Graph g) = 
   let dict = mapRecAux fn fe
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
   --  Sadly, there is no IntDict.filterMap
   let gf = l
         |> List.filterMap (\(id, o) ->
             case o of
                  Output o2 -> Just (id, o2)
                  _ -> Nothing)
         |> IntDict.fromList  
        
   in
   (Graph gf, missings)


rawFilter : (n -> Bool) -> (e -> Bool) -> GraphRep n e -> GraphRep n e
rawFilter fn fe =
  IntDict.filter (\_ o -> case o of
                             EdgeObj _ _ e -> fe e 
                             NodeObj n -> fn n)

-- if an element is dropped, all its ascendants will be dropped
-- as well
-- (dual of filter)
drop : (n -> Bool) -> (e -> Bool) -> Graph n e -> Graph n e
drop fn fe (Graph g) =
   let g2 = rawFilter fn fe g
   in
   removeList (IntDict.keys g2) (Graph g)

-- if an edge is kept, all its descendants will also be
-- whether or not they are explicitely kept
filter : (n -> Bool) -> (e -> Bool) -> Graph n e -> Graph n e
filter fn fe (Graph g) =
   let g2 = rawFilter fn fe g
   in
   let dict = mapRecAux (\_ _ -> ()) (\_ _ _ _ -> ())
        (IntDict.map (\_ -> Input) g)
        (IntDict.keys g2) 
   in
   let dictIds = IntDict.filter 
          (\_ o -> case o of 
                     Output _ -> True
                     _ -> False )
         dict
   in
   IntDict.intersect g dictIds |> Graph

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

-- indices in the base graphe are kept
union : Graph n e -> Graph n e -> Graph n e
union (Graph base) (Graph ext) = 
   let baseId = supId base in
   let extUp = addId baseId ext in   
     IntDict.union base extUp |> Graph