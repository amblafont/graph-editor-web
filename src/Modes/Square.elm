module Modes.Square exposing (help, graphDrawing, initialise, update, fixModel)



-- import Graph exposing (..)

import Polygraph as Graph exposing (Graph, NodeId, EdgeId)
import Maybe exposing (withDefault)
import Msg exposing (Msg(..), Command(..))
import HtmlDefs exposing (Key(..)) -- , computeLayout)
import GraphDefs exposing (NodeLabel, EdgeLabel)
import List.Extra exposing (uniquePairs, getAt)
import Modes exposing (SquareState, Mode(..))
import Model exposing (..)
import InputPosition exposing (InputPosition(..))
import ArrowStyle
import IntDict

import GraphDrawing exposing (NodeDrawingLabel, EdgeDrawingLabel)
import MyDiff
import Geometry.Point as Point exposing (Point)
import CommandCodec exposing (protocolSendGraphModif, protocolSendModif, protocolSend, updateModifHelperWithId)
import Modes.Rename
import Format.GraphInfo as GraphInfo



fixModel : Model -> SquareState -> Model
fixModel model state =
   if Graph.existsAll (getActiveGraph model)
       [state.n1, state.e1.id, state.n2, state.e2.id]
   then 
     model else {model | mode = DefaultMode}
 
possibleSquareStates : Graph GraphDefs.NodeLabel GraphDefs.EdgeLabel -> Graph.NodeId {- NodeContext a b -} -> List SquareState
possibleSquareStates g id =
 case GraphDefs.getLabelLabel id g of
  Nothing -> []
  Just chosenLabel ->
    -- Boolean: is it going to the node?
    let
        ins = Graph.incomings id g
             |> List.filterMap GraphDefs.filterEdgeNormal
            -- IntDict.keys nc.incoming
             |> List.filterMap 
             (\x -> 
             GraphDefs.getLabelLabel x.from g |>
             Maybe.map (\ labelNode -> 
             ( x, (labelNode, x.from), 
             True )))

        outs = Graph.outgoings id g
            |> List.filterMap GraphDefs.filterEdgeNormal
            -- IntDict.keys nc.outgoing 
            |> List.filterMap 
             (\x -> 
             GraphDefs.getLabelLabel x.to g |>
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
                , guessPos = True      
                })
            







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
    let modelGraph = getActiveGraph m in
    square_setPossibility idx modelGraph node
        |> Maybe.map (\state -> { m | mode = SquareMode state })
        |> Maybe.withDefault m
        |> noCmd



-- second argument: the n-th possibility


initialise : Model -> ( Model, Cmd Msg )
initialise m =
    let modelGraph = getActiveGraph m in
    GraphDefs.selectedNode modelGraph
        |> Maybe.map (.id >> square_updatePossibility m 0)
        -- |> Maybe.map
        -- -- prevent bugs (if the mouse is thought
        -- -- to be kept on a point)
        --    (Tuple.mapFirst (\model -> { model | mousePointOver = ONothing}))
        |> Maybe.withDefault (noCmd m)






nextStep : Model -> Bool -> SquareState -> ( Model, Cmd Msg )
nextStep model finish state =
    let tabId = model.graphInfo.activeTabId in
         
    let
        ( info, movedNode, created ) =
            moveNodeViewInfo finish model state
    in
    let modif = info.graph |> Graph.finaliseModif in
    let  selIds = IntDict.insert tabId [movedNode] IntDict.empty in
    -- let finalGraph = setSelModif movedNode info.graph in
     if finish then        
        ({ model | mode = DefaultMode }, -- computeLayout ()
          protocolSend
         {id =  Msg.defaultModifId,
          modif = GraphInfo.activeGraphModif model.graphInfo modif,
         selIds = selIds,
         command = Msg.Noop
         })
        
     else
        let ids = 
                (if created then [ movedNode , info.edges.ne1, info.edges.ne2 ]
                        else [ info.edges.ne1, info.edges.ne2 ])
                |>
                List.map (\id -> {id = id, label = Nothing, tabId = tabId})
        in
        let (nextModel, idModif) = popIdModif model in
        let finalModel = {nextModel | mode = DefaultMode } in
        (finalModel, 
            protocolSend -- finalModel idModif
          <| 
          
         {id = idModif,
         modif = GraphInfo.activeGraphModif model.graphInfo modif,
         selIds = selIds,
         command = RenameCommand ids
         })
        -- Modes.Rename.newState finalGraph ids model.graphInfo
        -- |> Msg.Rename
        -- |> protocolSend
        -- )
         -- computeLayout ())
                          



-- for view


type alias Edges =
    { e1 : EdgeId
    , e2 : EdgeId
    , ne1 : EdgeId
    , ne2 : EdgeId
    }


type alias ViewInfo =
    { edges : Edges
    , graph : Graph.ModifHelper NodeLabel EdgeLabel
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

guessPosition : Model -> SquareState -> Point
guessPosition m s = 
     let modelGraph = getActiveGraph m in
     case Graph.getNodes [s.n1, s.chosenNode, s.n2] modelGraph
                        |> List.map (.label >> .pos)  of
       [p1, p2, p3] -> Point.diamondPave p1 p2 p3
       _ -> m.mousePos

guessProofPosition : Model -> SquareState -> Point -> Point
guessProofPosition m s newPos  = 
     let modelGraph = getActiveGraph m in
     case Graph.getNode s.chosenNode modelGraph
                        |> Maybe.map .pos  of
       Just oldPos -> Point.middle oldPos newPos
       _ -> newPos
    
   -- case Graph.getNodes

moveNodeViewInfo : Bool -> Model -> SquareState -> ( ViewInfo, NodeId, Bool )
moveNodeViewInfo finish m data =
    
    {- let insert1 x l = case l of 
             [] -> [ x ]
             t :: q -> t :: x :: q
    in -}
    let atLeast1 l = if List.isEmpty l then [ "" ] else l in
    let commute str1 str2 =
      
           if str1 == "" || str2 == "" then
              [ ""]
           else
                   MyDiff.swapDiffStr (data.n1ToChosen == data.n2ToChosen) 
                       str1 
                       data.chosenLabel
                       str2 |> atLeast1                      
                       
    in
    -- basic idea:
    -- we want to close Gx <- Fx -> Fy by naturality
    -- the wanted final node label is Gy
    -- it amounts to permuting the two diff F y -> F x -> Gx
    -- (each arrow represents a diff) and applying the first diff
    -- to F y
    let labelsNode = commute data.n1Label data.n2Label
        labelsEdge1 = commute data.n1Label data.e2.label.details.label 
        labelsEdge2 = commute data.e1.label.details.label data.n2Label                       
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
    let newPos = if data.guessPos then guessPosition m data else m.mousePos in
    let
        ( ( g, n ), created ) =
            mayCreateTargetNodeAt m newPos labelNode finish
            -- mayCreateTargetNode m labelNode
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
    
    let (g1, ne1) = Graph.md_newEdge g e1n1 e1n2 <| GraphDefs.newEdgeLabel labelEdge1 ArrowStyle.empty in
    let (g2, ne2) = Graph.md_newEdge g1 e2n1 e2n2 <| GraphDefs.newEdgeLabel labelEdge2 ArrowStyle.empty in
    let g3 = if not m.squareModeProof then g2 else 
         let proofPos = guessProofPosition m data newPos in
         let (gg, _) = Graph.md_newNode g2 <| 
                    GraphDefs.createProofNodeLabel  "naturality." False proofPos
         in
           gg
    in
    
        
            {- Graph.newEdge 
            (Graph.newEdge g edges.ne1 GraphDefs.emptyEdge)
            edges.ne2 
            GraphDefs.emptyEdge
             -}
    
    let edges = makeEdges data ne1 ne2 in
    ( { graph = g3, edges = edges }, n, created )


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


stateInfo : Bool -> Model -> SquareState -> ViewInfo
stateInfo finish m s =
            let
                ( info, _, _ ) =
                    moveNodeViewInfo finish m s
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
            stateInfo False m state
    in
    collageGraphFromGraph m (Graph.applyModifHelper info.graph)
        |> graphDrawingFromInfo info.edges


update : SquareState -> Msg -> Model -> ( Model, Cmd Msg )
update state msg model =
    let next finish = nextStep model finish state in
    case msg of   
        KeyChanged False _ (Character '?') -> noCmd <| toggleHelpOverlay model   
        KeyChanged False _ (Character 'p') -> 
                    noCmd <| { model | squareModeProof = not model.squareModeProof }
        KeyChanged False _ (Character 's') ->            
                    square_updatePossibility model state.configuration state.chosenNode
        KeyChanged False _ (Character 'a') ->
                    noCmd  { model | mode = SquareMode { state | labelConfiguration = state.labelConfiguration + 1}}                    

        KeyChanged False _ (Control "Escape") -> switch_Default model
        MouseClick -> next False          
        MouseMove _ -> 
                 noCmd  { model | mode = SquareMode { state | guessPos = False}}                    
        KeyChanged False _ (Control "Enter") -> next True
    --     TabInput -> Just <| ValidateNext
        KeyChanged False _ (Control "Tab") -> next False

      
        _ -> noCmd model

help : String
help = HtmlDefs.overlayHelpMsg ++ 
            ", [ESC] cancel"
            ++ "[click] name the point (if new), "
             ++ "[RET] terminate the square creation, "
             ++ " alternative possible [s]quares, "
             ++ " [a]lternative possible labels, "
             ++ "toggle [p]roof node creation."
             
      