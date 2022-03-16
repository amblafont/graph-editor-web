module Modes.Square exposing (help, graphDrawing, initialise, update)



-- import Graph exposing (..)

import Polygraph as Graph exposing (Graph, NodeId, EdgeId)
import Maybe exposing (withDefault)
import Msg exposing (Msg(..))
import HtmlDefs exposing (Key(..), computeLayout)
import GraphDefs exposing (NodeLabel, EdgeLabel)
import List.Extra exposing (uniquePairs, getAt)
import Modes exposing (SquareState, Mode(..))
import Model exposing (..)
import InputPosition exposing (InputPosition(..))
import ArrowStyle

import GraphDrawing exposing (NodeDrawingLabel, EdgeDrawingLabel)
import MyDiff
import Html exposing (q)



possibleSquareStates : Graph GraphDefs.NodeLabel GraphDefs.EdgeLabel -> Graph.NodeId {- NodeContext a b -} -> List SquareState
possibleSquareStates g id =
    let chosenLabel = Graph.get id .label .label g |> Maybe.withDefault "" in
    
    -- Boolean: is it going to the node?
    let
        ins = Graph.incomings id g
            -- IntDict.keys nc.incoming
             |> List.filterMap 
             (\x -> 
             Graph.get x.from .label .label g |>
             Maybe.map (\ labelNode -> 
             ( x, (labelNode, x.from), 
             True )))

        outs = Graph.outgoings id g
            -- IntDict.keys nc.outgoing 
            |> List.filterMap 
             (\x -> 
             Graph.get x.to .label .label g |>
             Maybe.map (\ labelNode -> 
             ( x, (labelNode, x.to), 
             False )))
    in
    ins
        ++ outs
        |> uniquePairs
        |> List.map
            (\( ( e1, (l1,n1), i1 ), ( e2, (l2,n2), i2 ) ) ->
                { chosenNode = id
                , chosenLabel = chosenLabel
                --   -- we don't care
                -- , movedNode = 0
                , n1 = n1
                , n2 = n2
                , e1 = e1
                , e2 = e2
                , n1ToChosen = i1
                , n2ToChosen = i2
                , configuration = 0
                , labelConfiguration = 0
                , n1Label = l1
                , n2Label = l2                
                }
            )







square_setPossibility : Int -> Graph GraphDefs.NodeLabel GraphDefs.EdgeLabel -> NodeId -> Maybe SquareState
square_setPossibility idx g chosenNode =
    -- Graph.get chosenNode g
    --     |> Maybe.map 
    let possibilities = possibleSquareStates g chosenNode in
    possibilities 
                    |> getAt idx
                    |> Maybe.map
                        (\s ->
                            { s | configuration =                             
                                    (modBy (List.length possibilities) (idx + 1))
                            }
                        )
            


square_updatePossibility : Model -> Int -> NodeId -> ( Model, Cmd Msg )
square_updatePossibility m idx node =
    square_setPossibility idx m.graph node
        |> Maybe.map (\state -> { m | mode = SquareMode state })
        |> Maybe.withDefault m
        |> noCmd



-- second argument: the n-th possibility


initialise : Model -> ( Model, Cmd Msg )
initialise m =
    GraphDefs.selectedNode m.graph
        |> Maybe.map (.id >> square_updatePossibility m 0)
        -- |> Maybe.map
        -- -- prevent bugs (if the mouse is thought
        -- -- to be kept on a point)
        --    (Tuple.mapFirst (\model -> { model | mousePointOver = ONothing}))
        |> Maybe.withDefault (noCmd m)






nextStep : Model -> Bool -> SquareState -> ( Model, Cmd Msg )
nextStep model finish state =
         
    let
        ( info, movedNode, created ) =
            moveNodeViewInfo model state
    in
    let m2 = addOrSetSel False movedNode { model | graph = info.graph } in
     if finish then ({ m2 | mode = DefaultMode }, computeLayout ()) else
        let ids = 
                         if created then [ movedNode , info.edges.ne1, info.edges.ne2 ]
                                    else [ info.edges.ne1, info.edges.ne2 ]
        in
        (initialise_RenameMode ids m2, computeLayout ())
                          



-- for view


type alias Edges =
    { e1 : EdgeId
    , e2 : EdgeId
    , ne1 : EdgeId
    , ne2 : EdgeId
    }


type alias ViewInfo =
    { edges : Edges
    , graph : Graph NodeLabel EdgeLabel
    }




-- movedNode


squareMode_activeObj : Edges -> List Graph.Id
squareMode_activeObj info =
    [ info.e1
    , info.e2
    , info.ne1
    , info.ne2
    ]

chooseAmong : List Int -> Int -> List Int
chooseAmong l n =
   case l of
      t :: q -> modBy t n :: (chooseAmong q (n // t))
      [] -> []



moveNodeViewInfo : Model -> SquareState -> ( ViewInfo, NodeId, Bool )
moveNodeViewInfo m data =
    
    {- let insert1 x l = case l of 
             [] -> [ x ]
             t :: q -> t :: x :: q
    in -}
    let atLeast1 l = if List.isEmpty l then [ "" ] else l in
    let commute str1 str2 =
      
           if str1 == "" || str2 == "" then
              [ ""]
           else
                   MyDiff.swapDiffStr (data.n1ToChosen == data.n2ToChosen) str1 
                       data.chosenLabel
                       str2 |> atLeast1                      
                       
    in
    let labelsNode = commute data.n1Label data.n2Label
        labelsEdge1 = commute data.n1Label data.e2.label.label 
        labelsEdge2 = commute data.e1.label.label data.n2Label                       
    in
      
           

    let possibleLabels = [labelsNode, labelsEdge1, labelsEdge2] in
    let lens = List.map List.length possibleLabels in
    let labels = 
         if modBy (List.product lens + 1) data.labelConfiguration == 1 then
           ["", "", ""]
         else 
          let lconf = if data.labelConfiguration == 0 then 0 else data.labelConfiguration - 1 in          
         
          let ids =  chooseAmong lens lconf in
            List.map2 getAt ids possibleLabels 
                       |> List.map (Maybe.withDefault "!!")  --should never happen
      
    in
    let (labelNode, labelEdge1, labelEdge2) =
          case labels of
            [a,b,c] -> (a,b,c)
            _ -> ("!","!","!") --this should never happen
    in
    let
        ( ( g, n ), created ) =
            mayCreateTargetNode m labelNode
    in
    {- let
        edges =
            makeEdges data n
    in -}
    let make_EdgeId n1 n2 isTo =
           if isTo then
               ( n1, n2 )
           else
               ( n2, n1 )
    in
    let (e1n1, e1n2) = make_EdgeId data.n1 n <| nToMoved data.n1ToChosen data.n2ToChosen in
    let (e2n1, e2n2) = make_EdgeId data.n2 n <| nToMoved data.n2ToChosen data.n1ToChosen in
    
    let (g1, ne1) = Graph.newEdge g e1n1 e1n2 <| GraphDefs.newEdgeLabel labelEdge1 ArrowStyle.empty in
    let (g2, ne2) = Graph.newEdge g1 e2n1 e2n2 <| GraphDefs.newEdgeLabel labelEdge2 ArrowStyle.empty in
    
        
            {- Graph.newEdge 
            (Graph.newEdge g edges.ne1 GraphDefs.emptyEdge)
            edges.ne2 
            GraphDefs.emptyEdge
             -}
    
    let edges = makeEdges data ne1 ne2 in
    ( { graph = g2, edges = edges }, n, created )


nToMoved : Bool -> Bool -> Bool
nToMoved nToChosen otherNToChosen =
    if nToChosen == otherNToChosen then
        not nToChosen

    else
        nToChosen


{- makeEdges : SquareModeData -> NodeId -> Edges
makeEdges data movedNode =
    { e1 = make_EdgeId data.n1 data.chosenNode data.n1ToChosen
    , e2 = make_EdgeId data.n2 data.chosenNode data.n2ToChosen
    , ne1 = make_EdgeId data.n1 movedNode <| nToMoved data.n1ToChosen data.n2ToChosen
    , ne2 = make_EdgeId data.n2 movedNode <| nToMoved data.n2ToChosen data.n1ToChosen --,

    -- movedNode = movedNode
    }
 -}
makeEdges : SquareState -> EdgeId -> EdgeId -> Edges
makeEdges data ne1 ne2 =
    { e1 = data.e1.id
    , e2 = data.e2.id
    , ne1 = ne1
    , ne2 = ne2    
    -- movedNode = movedNode
    }


stateInfo : Model -> SquareState -> ViewInfo
stateInfo m s =
            let
                ( info, _, _ ) =
                    moveNodeViewInfo m s
            in
            info

     


graphDrawingFromInfo :
    Edges ->
     Graph NodeDrawingLabel EdgeDrawingLabel
    -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawingFromInfo info =    
    GraphDrawing.makeActive (squareMode_activeObj info)


graphDrawing : Model -> SquareState -> Graph NodeDrawingLabel EdgeDrawingLabel
graphDrawing m state =
    let
        info =
            stateInfo m state
    in
    collageGraphFromGraph m info.graph
        |> graphDrawingFromInfo info.edges


update : SquareState -> Msg -> Model -> ( Model, Cmd Msg )
update state msg model =
    let next finish = nextStep model finish state in
    case msg of   

   
        KeyChanged False _ (Character 's') ->            
                    square_updatePossibility model state.configuration state.chosenNode
        KeyChanged False _ (Character 'a') ->
                    noCmd  { model | mode = SquareMode { state | labelConfiguration = state.labelConfiguration + 1}}                    

        KeyChanged False _ (Control "Escape") -> switch_Default model
        MouseClick -> next False          
        KeyChanged False _ (Control "Enter") -> next True
    --     TabInput -> Just <| ValidateNext
        KeyChanged False _ (Control "Tab") -> next False

      
        _ -> noCmd model

help : String
help =
            "[ESC] cancel, [click] name the point (if new), "
             ++ "[RET] terminate the square creation, "
             ++ " alternative possible [s]quares, "
             ++ " [a]lternative possible labels."
             
      