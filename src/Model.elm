module Model exposing (..)


import Color exposing (..)
import Graph exposing (..)
import GraphDrawing exposing (..)
import GraphExtra as Graph exposing (EdgeId)
import Msg exposing (..)
import QuickInput exposing (NonEmptyChain)

import Point exposing (Point)
import Dict exposing (Dict)



-- State -----------------------------------------------------------------------
-- core data that will be saved


type alias Model =
    { graph : Graph NodeLabel EdgeLabel
    , activeObj : Obj
    , mousePos : Point
    , -- if the mouse is over some node or edge
      mousePointOver : Obj
    , statusMsg : String
    , -- unnamedFlag : Bool,
      quickInput : String
    , mode : Mode
    -- real dimensions of nodes
    , dimNodes : Dict NodeId Point
    -- quickInput : Maybe NonEmptyChain
    -- mouseOnCanvas : Bool
    -- blitzFlag : Bool
    }


type Mode
    = DefaultMode
    | NewArrow NewArrowState
    | MoveNode
    | RenameMode String
    | DebugMode
    | NewNode
    | QuickInputMode (Maybe NonEmptyChain)
    | SquareMode SquareState


type alias NewArrowState =
    { step : NewArrowStep, chosenNode : NodeId }


type NewArrowStep
    = NewArrowMoveNode
      -- the moved node
    | NewArrowEditNode NodeId
    | NewArrowEditEdge NodeId


type alias SquareState =
    { data : SquareModeData, step : SquareStep }


type SquareStep
    = -- the argument is the next possibility of square to be tested
      SquareMoveNode Int
      -- the moved node
    | SquareEditNode NodeId
    | SquareEditEdge1 NodeId
    | SquareEditEdge2 NodeId


type alias SquareModeData =
    { chosenNode : NodeId
    , n1 : NodeId
    , n1ToChosen : Bool
    , n2 : NodeId
    , n2ToChosen : Bool
    }


createModel : Graph NodeLabel EdgeLabel -> Model
createModel g =
    { graph = g
    , mode = DefaultMode
    , statusMsg = ""
    , -- Debug.toString ([pointToAngle (0,1), pointToAngle (0.001,1),
      --                    pointToAngle (1,0), pointToAngle(0,-1),
      --                        pointToAngle (-1, 0.01)]),
      quickInput = ""
    , mousePos = ( 0, 0 )
    , mousePointOver = ONothing
    , activeObj = ONothing
    , dimNodes = Dict.empty

    -- unnamedFlag = False
    -- mouseOnCanvas = False,
    -- quickInput = Nothing
    -- blitzFlag = False
    }



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
        ONode n ->
            Just n

        _ ->
            Nothing


graphRemoveObj : Obj -> Graph a b -> Graph a b
graphRemoveObj o g =
    case o of
        ONothing ->
            g

        ONode id ->
            Graph.remove id g

        OEdge id ->
            Graph.removeEdge id g


graphRenameObj : Graph NodeLabel EdgeLabel -> Obj -> String -> Graph NodeLabel EdgeLabel
graphRenameObj g o s =
    case o of
        ONode id ->
            Graph.updateNode id (\nl -> { nl | label = s }) g

        OEdge id ->
            Graph.updateEdge id (\_ -> s) g

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


activeNode : Model -> NodeId
activeNode m =
    obj_NodeId m.activeObj


obj_NodeId : Obj -> NodeId
obj_NodeId x =
    case x of
        ONode id ->
            id

        _ ->
            0


obj_EdgeId : Obj -> EdgeId
obj_EdgeId x =
    case x of
        OEdge id ->
            id

        _ ->
            ( 0, 0 )



-- The graphs are updated according to the current mode
-- returns also the endpoint id of the arrow
-- graph_Square : SquareState -> State -> (Graph NodeLabel EdgeLabel, NodeId)
-- graph_Square state m =
--     let (graph, target) = mayCreateTargetNode m "" in
--          (Graph.addEdge graph (activeNode m, target) state.edgeLabel, target)
-- returns the target node of the new arrow, if it already exists.


getTargetNode : Model -> Maybe NodeId
getTargetNode m =
    -- if the mouse is over a node, that is the target node
    case m.mousePointOver of
        ONode i ->
            if Graph.member i m.graph then
                Just i

            else
                Nothing

        _ ->
            Nothing



-- True if created


mayCreateTargetNode : Model -> String -> ( ( Graph NodeLabel EdgeLabel, NodeId ), Bool )
mayCreateTargetNode m s =
    case getTargetNode m of
        Just n ->
            ( ( m.graph, n ), False )

        Nothing ->
            ( Graph.newNode m.graph { label = s, pos = m.mousePos }, True )



-- switch between modes


noCmd : a -> ( a, Cmd Msg )
noCmd m =
    ( m, Cmd.none )


switch_Default : Model -> ( Model, Cmd Msg )
switch_Default m =
    noCmd { m | mode = DefaultMode }


make_defaultNodeDrawingLabel : Model -> Node NodeLabel -> NodeDrawingLabel
make_defaultNodeDrawingLabel model n =
    make_nodeDrawingLabel
        { editable = False
        , isActive = n.id == activeNode model
        , dims = Dict.get n.id model.dimNodes
        }
        n.label


collageGraphFromGraph : Model -> Graph NodeLabel EdgeLabel -> Graph NodeDrawingLabel EdgeDrawingLabel
collageGraphFromGraph model =
    Graph.mapNodeEdges
        (make_defaultNodeDrawingLabel model)
        (\e ->
            e.label
                |> make_edgeDrawingLabel
                    { editable = False, isActive = ( e.from, e.to ) == (model.activeObj |> obj_EdgeId) }
        )



