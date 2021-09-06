module Model exposing (..)


import Color exposing (..)
import GraphDrawing exposing (..)
import Polygraph as Graph exposing (EdgeId, NodeId, Graph, Node)
import Msg exposing (..)
import Geometry.Point as Point exposing (Point)
import InputPosition
import GraphDefs exposing (NodeLabel, EdgeLabel, newNodeLabel)
import HtmlDefs

import Modes exposing (Mode(..))

import GraphDefs




-- State -----------------------------------------------------------------------
-- core data that will be saved



type alias Model =
    { graph : Graph NodeLabel EdgeLabel
    -- , selectedObjs : List Obj
    , mousePos : Point
    , specialKeys : HtmlDefs.Keys
    -- , -- if the mouse is over some node or edge
    --  mousePointOver : Obj
    , statusMsg : String
    , -- unnamedFlag : Bool,
      quickInput : String
    , mode : Mode
    , hideGrid : Bool
    , sizeGrid : Int
    -- real dimensions of nodes
    -- , dimNodes : Dict NodeId Point
    -- quickInput : Maybe NonEmptyChain
    -- mouseOnCanvas : Bool
    -- blitzFlag : Bool
    }





-- inputPositionPoint : Point -> InputPosition -> Point
-- inputPositionPoint source pos =
--    case pos of
--       InputPosMouse p -> p
--       InputPosKeyboard p -> Point.add source <| deltaKeyboardPos p)


createModel : Int -> Graph NodeLabel EdgeLabel -> Model
createModel sizeGrid g =
    { graph = g
    , mode = DefaultMode
    , statusMsg = ""
    , -- Debug.toString ([pointToAngle (0,1), pointToAngle (0.001,1),
      --                    pointToAngle (1,0), pointToAngle(0,-1),
      --                        pointToAngle (-1, 0.01)]),
      quickInput = ""
    , mousePos = ( 0, 0 )
    , specialKeys = { ctrl = False, alt = False, shift = False }
    , hideGrid = True
    , sizeGrid = sizeGrid
   -- , mousePointOver = ONothing
  --  , selectedObjs = []
    -- , dimNodes = Dict.empty

    -- unnamedFlag = False
    -- mouseOnCanvas = False,
    -- quickInput = Nothing
    -- blitzFlag = False
    }

defaultGridSize = 200

iniModel : Model
iniModel = 
   let sizeGrid = defaultGridSize in
   createModel sizeGrid <| Graph.empty
      {- Tuple.first <|
        Graph.newNode Graph.empty
         { pos = (sizeGrid / 2, sizeGrid / 2), label = "", dims = Nothing,
           selected = True } -}

initialise_RenameModeWithDefault : List (Graph.Id, String) -> Model -> Model
initialise_RenameModeWithDefault l m =
  case l of
     [] -> { m | mode = DefaultMode }
     _ -> { m | mode = RenameMode l }



initialise_RenameMode : List Graph.Id -> Model -> Model
initialise_RenameMode l m =
    let ls =  List.map (\id -> (id, GraphDefs.getLabelLabel id m.graph)) l
    in
        initialise_RenameModeWithDefault ls m
        


addOrSetSel : Bool -> Obj -> Model -> Model
addOrSetSel keep o m =

    let g = if keep then m.graph else GraphDefs.clearSelection m.graph in
    let g2 
         = case o of
          ONothing -> g
          ONode id -> Graph.updateNode id (\n -> {n | selected = True}) g
          OEdge id -> Graph.updateEdge id (\n -> {n | selected = True}) g
    in
   {m | graph = g2 }
        
      

{- allSelectedNodesId : Model -> List NodeId
allSelectedNodesId m =
  List.concatMap 
    (\ o ->  case  o of
               ONothing -> []
               ONode id -> [ id ]
               OEdge (id1, id2) -> [ id1, id2 ]
    )
    m.selectedObjs -}

-- selectedNodes : Model -> List (Node NodeLabel)
-- selectedNodes m = Graph.nodes m.graph |> List.filter (.label >> .selected)

-- selectedEdges : Model -> List (EdgeNodes NodeLabel EdgeLabel)
-- selectedEdges m = Graph.edgesWithNodes m.graph |> List.filter (.label >> .selected)

allSelectedNodes : Model -> List (Node NodeLabel)
allSelectedNodes m = 
    m.graph |> GraphDefs.selectedGraph
    |> Graph.nodes
    
--     let (l1, l2) = selectedEdges m
--            |> List.map (\e -> (e.from, e.to)) |> List.unzip
--     in
--    selectedNodes m ++ l1 ++ l2 |> List.Extra.uniqueBy .id
     
--   |> List.filterMap (\ id -> Graph.get id m.graph |> Maybe.map .node)

-- captureKeyboard : State -> Bool
-- captureKeyboard m =
--     case m.mode of
--         QuickInputMode _ -> False
--         _ -> True


type Obj
    = ONothing
    | ONode NodeId
    | OEdge EdgeId


objToNode : Obj -> Maybe NodeId
objToNode o =
    case o of
        ONode n -> Just n
        _ -> Nothing

objId : Obj -> Maybe Graph.Id
objId o =
  case o of
     ONode n -> Just n
     OEdge e -> Just e
     ONothing -> Nothing

objToEdge : Obj -> Maybe EdgeId
objToEdge o =
    case o of
       OEdge n -> Just n
       _ -> Nothing

{- obj_NodeId : Obj -> NodeId
obj_NodeId x =
    case x of
        ONode id ->
            id

        _ ->
            0 -}


{- obj_EdgeId : Obj -> EdgeId
obj_EdgeId x =
    case x of
        OEdge id ->
            id

        _ ->
            0 -}

selectedObjs : Model -> List Obj
selectedObjs m =
    let edges = Graph.edges m.graph |> List.filter (.label >> .selected) |> List.map (.id >> OEdge)
        nodes = Graph.nodes m.graph |> List.filter (.label >> .selected) |> List.map (.id >> ONode)
        
    in
    edges ++ nodes 

activeObj : Model -> Obj
activeObj m =
    case selectedObjs m of
      [ o ] -> o
      _ -> ONothing
{- activeNode : Model -> NodeId
activeNode m =
    obj_NodeId <| activeObj m -}




graphRenameObj : Graph NodeLabel EdgeLabel -> Obj -> String -> Graph NodeLabel EdgeLabel
graphRenameObj g o s =
    case o of
        ONode id ->
            Graph.updateNode id (\nl -> { nl | label = s }) g

        OEdge id ->
            Graph.updateEdge id (\e -> { e | label = s}) g

        ONothing ->
            g


graphMakeEditable : Obj -> Graph NodeDrawingLabel EdgeDrawingLabel -> Graph NodeDrawingLabel EdgeDrawingLabel
graphMakeEditable o g =
    case o of
        ONode id ->
            Graph.updateNode id (\e -> { e | editable = True }) g

        OEdge id ->
            Graph.updateEdge id (\e -> { e | editable = True }) g

        ONothing ->
            g


graphMakeActive : Obj -> Graph NodeDrawingLabel EdgeDrawingLabel -> Graph NodeDrawingLabel EdgeDrawingLabel
graphMakeActive o g =
    case o of
        ONode id ->
            Graph.updateNode id (\e -> { e | isActive = True }) g

        OEdge id ->
            Graph.updateEdge id (\e -> { e | isActive = True }) g

        ONothing ->
            g








-- The graphs are updated according to the current mode
-- returns also the endpoint id of the arrow
-- graph_Square : SquareState -> State -> (Graph NodeLabel EdgeLabel, NodeId)
-- graph_Square state m =
--     let (graph, target) = mayCreateTargetNode m "" in
--          (Graph.addEdge graph (activeNode m, target) state.edgeLabel, target)
-- returns the target node of the new arrow, if it already exists.






{- getNodesInRect : Model -> Geometry.Rect -> List NodeId
getNodesInRect m r =
    Graph.filterNodes m.graph 
    (\n -> Geometry.isInRect r n.pos)
  |> List.map .id
 -}
{- getTargetNodes : Model -> List NodeId
getTargetNodes m = GraphDefs.getNodesAt m.graph m.mousePos
  -- List.head |> Maybe.map .id

-- not very reliable, let us use getNodeAt
getTargetNode : Bool -> Model -> Maybe Id
getTargetNode onlyNode m =
    -- if the mouse is over a node, that is the target node
    case getTargetNodes m of
        i :: _ ->
             if onlyNode then
               Graph.getNode i m.graph 
               |> Maybe.map (\_ -> i)             
             else 
               Just i

        _ ->
            Nothing -}






-- True if created


mayCreateTargetNodeAt : Model -> Point -> String -> ( ( Graph NodeLabel EdgeLabel, NodeId ), Bool )
mayCreateTargetNodeAt m pos s =
   case GraphDefs.getNodesAt m.graph pos of
      [ n ] -> ((m.graph, n), False)
      _ ->
            ( Graph.newNode m.graph 
              <| newNodeLabel pos s
            , True )

-- only Nodes ?
mayCreateTargetNode : Model -> String -> ( ( Graph NodeLabel EdgeLabel, NodeId ), Bool )
mayCreateTargetNode m s =
  mayCreateTargetNodeAt m m.mousePos s
    


-- switch between modes


noCmd : a -> ( a, Cmd Msg )
noCmd m =
    ( m, Cmd.none )


switch_Default : Model -> ( Model, Cmd Msg )
switch_Default m =
    noCmd { m | mode = DefaultMode }

isSelectedObj : Model -> Obj -> Bool
isSelectedObj m o = 
   case o of 
      ONothing -> False
      _ -> List.member o (selectedObjs m)

make_defaultNodeDrawingLabel : Model -> NodeLabel -> NodeDrawingLabel
make_defaultNodeDrawingLabel model n =
    make_nodeDrawingLabel
        { editable = False
        , isActive = n.selected
        -- isSelectedObj model (ONode n.id)
       -- , dims =  getNodeDims n.label  
        }
        n


collageGraphFromGraph : Model -> Graph NodeLabel EdgeLabel -> Graph NodeDrawingLabel EdgeDrawingLabel
collageGraphFromGraph model =
    Graph.map
        (\ _ -> make_defaultNodeDrawingLabel model)
        (\ _ e ->
            e
                |> make_edgeDrawingLabel
                    { editable = False, 
                      isActive = e.selected
                      -- isSelectedObj model <| OEdge ( e.from, e.to )  
                      }
        )


keyboardPosToPoint : Model -> NodeId -> (Int, Int) -> Point
keyboardPosToPoint m chosenNode p =
   case Graph.getNode chosenNode m.graph of
      Nothing -> m.mousePos
      Just { pos } -> 
         let delta = InputPosition.deltaKeyboardPos m.sizeGrid p in
         Point.add pos delta


