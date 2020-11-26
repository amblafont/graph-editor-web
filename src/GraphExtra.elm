-- complements to community-graph


module GraphExtra exposing (..)

import Graph exposing (..)
import IntDict



-- source and targets


type alias EdgeId =
    ( NodeId, NodeId )


removeEdge : EdgeId -> Graph a b -> Graph a b
removeEdge ( from, to ) g =
    Graph.update from
        (Maybe.map (\nc -> { nc | outgoing = IntDict.remove to nc.outgoing }))
        g


updateEdge : EdgeId -> (b -> b) -> Graph a b -> Graph a b
updateEdge ( from, to ) f g =
    Graph.update from
        (Maybe.map
            (\nc ->
                { nc
                    | outgoing =
                        IntDict.update to
                            (Maybe.map f)
                            nc.outgoing
                }
            )
        )
        g


setNodeLabel : Node a -> b -> Node b
setNodeLabel n l =
    { id = n.id, label = l }


mapNodesEdges : (Node n1 -> n2) -> (Edge e1 -> e2) -> Graph n1 e1 -> Graph n2 e2
mapNodesEdges nmap edgemap g =
    Graph.mapContexts
        (\nc ->
            { node = setNodeLabel nc.node (nmap nc.node)
            , outgoing =
                IntDict.map
                    (\to l ->
                        edgemap { from = nc.node.id, to = to, label = l }
                    )
                    nc.outgoing
            , incoming = IntDict.map (\from l -> edgemap { from = from, to = nc.node.id, label = l }) nc.incoming
            }
        )
        g


updateNode : NodeId -> (a -> a) -> Graph a b -> Graph a b
updateNode id f =
    Graph.update id
        (Maybe.map (\nc -> { nc | node = Node nc.node.id (f nc.node.label) }))

updateNodes : List NodeId -> (a -> a) -> Graph a b -> Graph a b
updateNodes l f g =
  List.foldl (\ id g2 -> updateNode id f g2) g l

getEdge : EdgeId -> Graph a b -> Maybe b
getEdge ( from, to ) g =
    Graph.get from g
        |> Maybe.andThen
            (\nc -> IntDict.get to nc.outgoing)


getNode : NodeId -> Graph a b -> Maybe a
getNode id g =
    Graph.get id g
        |> Maybe.map (.node >> .label)


addEdge : Graph a b -> EdgeId -> b -> Graph a b
addEdge g ( from, to ) label =
    Graph.update from
        (Maybe.map (\nc -> { nc | outgoing = IntDict.insert to label nc.outgoing }))
        g


addNode : Graph a b -> NodeId -> a -> Graph a b
addNode g id l =
    Graph.insert
        { node = { id = id, label = l }
        , outgoing = IntDict.empty
        , incoming = IntDict.empty
        }
        g


nextId : Graph n e -> NodeId
nextId g =
    case nodeIdRange g of
        Nothing ->
            1

        Just ( _, max ) ->
            max + 1


newNode : Graph n e -> n -> ( Graph n e, NodeId )
newNode g n =
    let
        id =
            nextId g
    in
    ( addNode g id n, id )


type alias EdgeNodes a b =
    { from : Node a
    , to : Node a
    , label : b
    }


edgeToEdgeWithNodes : Graph n e -> Edge e -> Maybe (EdgeNodes n e)
edgeToEdgeWithNodes g { from, to, label } =
    case ( getNode from g, getNode to g ) of
        ( Just fromN, Just toN ) ->
            Just { from = Node from fromN, to = Node to toN, label = label }

        _ ->
            Nothing

edgeWithNodesId : EdgeNodes n e -> EdgeId
edgeWithNodesId {from, to} = (from.id, to.id)

edgesWithNodes : Graph n e -> List (EdgeNodes n e)
edgesWithNodes g =
    Graph.edges g
        |> List.filterMap (edgeToEdgeWithNodes g)


filterNodes : Graph n e -> (n -> Bool) -> List (Node n)
filterNodes g f = Graph.nodes g |> List.filter (f << .label)


-- filterNodesId : Graph n e -> (Node n -> Bool) -> List (Node n)
-- filterNodesId g f =
--     Graph.nodes g |> List.filter f

--|> List.map .id


make_EdgeId : NodeId -> NodeId -> Bool -> EdgeId
make_EdgeId n1 n2 isTo =
    if isTo then
        ( n1, n2 )

    else
        ( n2, n1 )
