module GraphDefs exposing (defaultPullshoutShift, EdgeLabel, NodeLabel, allDimsReady,clearDims,
   NormalEdgeLabel, PullshoutEdgeLabel, EdgeType(..), GenericEdge, edgeToNodeLabel, getEdgeColor,
   newEdgeLabelAdj, selectIds, setColor, -- setColorEdgesId,
   filterLabelNormal, filterEdgeNormal, isNormalId, isNormal, isPullshout,
   filterNormalEdges, coqProofTexCommand,
   newNodeLabel, newEdgeLabel, newEdgeLabelVerbatimAdj, newPullshout, emptyEdge,
   selectedEdges, mapNormalEdge,  mapDetails, 
   createNodeLabel, md_createNodeLabel, md_createNodeLabelVerbatim, createProofNode, createProofNodeLabel,
   getNodeLabelOrCreate, getNodeDims, getNodePos, getEdgeDims,
   addNodesSelection, selectAll, clearSelection, 
   clearWeakSelection,
   selectedGraph, mergeFunctions,
   fieldSelect,
   selectedNodes,
   isEmptySelection,
   selectedEdge,
   selectedEdgeId, selectedNode, selectedId,selectedIds,
   removeSelected, getLabelLabel, getProofNodes,
   getNodesAt, snapToGrid, snapNodeToGrid,
   addOrSetSel, toProofGraph, selectedIncompleteDiagram,
   selectSurroundingDiagram,
   centerOfNodes, --mergeWithSameLoc,
   findReplaceInSelected, {- closestUnnamed, -} unselect, closest,
   makeSelection, addWeaklySelected, weaklySelect, weaklySelectMany,
   getSurroundingDiagrams, updateNormalEdge,
   rectEnveloppe, updateStyleEdges, updatePullshoutEdges,
   getSelectedProofDiagram, MaybeProofDiagram(..), selectedChain, MaybeChain(..),
   createValidProofAtBarycenter, isProofLabel, makeProofString, posGraph
   ,invertEdges
   , edgeScaleFactor
   , keyMaybeUpdatePullshout
   )

import IntDict
import Drawing.Color as Color
import Zindex exposing (defaultZ)
import Geometry.Point as Point exposing (Point)
import Geometry exposing (LabelAlignment(..))
import Geometry.QuadraticBezier as Bez
import EdgeShape exposing (EdgeShape(..), pullshoutHat)
import ArrowStyle exposing (ArrowStyle, EdgePart)
import Polygraph as Graph exposing (Graph, NodeId, EdgeId, Node, Edge)
import GraphProof exposing (LoopNode, LoopEdge, Diagram)
import Verbatim exposing (makeVerbatimLabel)
import Json.Encode as JEncode
import List.Extra as List
import Maybe.Extra as Maybe
import Geometry.Point
import Polygraph exposing (edges)
import HtmlDefs exposing (Key(..))

defaultPullshoutShift = 10
type alias NodeLabel = { pos : Point , label : String, dims : Maybe Point, 
                         selected : Bool, weaklySelected : Bool,
                         isMath : Bool, zindex: Int, isCoqValidated: Bool}

type alias EdgeLabel = GenericEdge EdgeType
type alias GenericEdge a = { details : a, selected : Bool,
                   weaklySelected : Bool,
                   zindex : Int}

type alias PullshoutEdgeLabel = { color : Color.Color, offset1 : Float, offset2 : Float}

type EdgeType = 
     PullshoutEdge PullshoutEdgeLabel
   | NormalEdge NormalEdgeLabel

type alias NormalEdgeLabel = 
  { label : String, style : ArrowStyle, dims : Maybe Point
  -- ArrowStyle.getStyle should be systematically applied to the style
  -- (TODO: remove this restriction)
  , isAdjunction : Bool}


edgeScaleFactor : Float
edgeScaleFactor = 0.7

allDimsReady : Graph.Graph NodeLabel EdgeLabel -> Bool
allDimsReady g = 
  let checkDim e = 
            if e.label == "" then True else 
            case e.dims of 
            Nothing -> False
            Just (x, _) -> x > 0
  in
  
  List.all (\n -> checkDim n.label) (Graph.nodes g)
  && 
   (Graph.edges g 
   |> List.filterMap (.label >> filterLabelNormal)   
   |> List.all (\e -> checkDim e.details)
   )

clearDims : Graph.Graph NodeLabel EdgeLabel -> Graph.Graph NodeLabel EdgeLabel
clearDims g = 
  Graph.map (\_ n -> { n | dims = Nothing}) 
    (\_ ->  mapNormalEdge (\ e -> { e | dims = Nothing})) g


coqProofTexCommand = "coqproof"

mergeFunctions : Graph.MergeFunctions NodeLabel EdgeLabel
mergeFunctions = 
  {  mergeNode = \ n1 {pos, label, dims, isMath, zindex, isCoqValidated} -> 
         {n1 | 
            pos = pos, label = label, dims = dims 
            , isMath = isMath, zindex = zindex
            , isCoqValidated = isCoqValidated
         }
    , mergeEdge = \ e1 {details, zindex} -> {e1 | details = details, zindex = zindex}
  }

edgeToNodeLabel : Point -> EdgeLabel -> NodeLabel
edgeToNodeLabel pos l = 
   let nodeLabel = { pos = pos, label = "", dims = Nothing,
                 selected = l.selected, weaklySelected = l.weaklySelected,
                 zindex = l.zindex, isMath = True, isCoqValidated = False}
   in
   case l.details of 
     PullshoutEdge _ -> nodeLabel
     NormalEdge {label, dims} -> {nodeLabel | label = label, dims = dims}

filterNormalEdges : EdgeType -> Maybe NormalEdgeLabel
filterNormalEdges d =  case d of
             PullshoutEdge _ -> Nothing
             NormalEdge l -> Just l

filterPullshoutEdges : EdgeType -> Maybe PullshoutEdgeLabel
filterPullshoutEdges d =  case d of
             PullshoutEdge l -> Just l
             NormalEdge _ -> Nothing

filterLabelNormal : EdgeLabel -> Maybe (GenericEdge NormalEdgeLabel)
filterLabelNormal =
    mapDetails filterNormalEdges
             >> flattenDetails

filterLabelPullshout : EdgeLabel -> Maybe (GenericEdge PullshoutEdgeLabel)
filterLabelPullshout =
    mapDetails filterPullshoutEdges
             >> flattenDetails


filterEdgeNormal : Edge EdgeLabel -> Maybe (Edge (GenericEdge NormalEdgeLabel))
filterEdgeNormal e =
      filterLabelNormal e.label |>
                    Maybe.map (
                        \ l -> 
                        Graph.edgeMap (always l) e
                    )

filterEdgePullshout : Edge EdgeLabel -> Maybe (Edge (GenericEdge PullshoutEdgeLabel))
filterEdgePullshout e =
      filterLabelPullshout e.label |>
                    Maybe.map (
                        \ l -> 
                        Graph.edgeMap (always l) e
                    )

keepNormalEdges : Graph NodeLabel EdgeLabel -> Graph NodeLabel (GenericEdge NormalEdgeLabel)
keepNormalEdges = Graph.filterMap Just
   filterLabelNormal

getEdgeColor : EdgeLabel -> Color.Color
getEdgeColor e = case e.details of
    PullshoutEdge {color} -> color
    NormalEdge l -> l.style.color

mapEdgeType : (NormalEdgeLabel -> NormalEdgeLabel) -> 
              (PullshoutEdgeLabel -> PullshoutEdgeLabel) ->
               EdgeType -> EdgeType
mapEdgeType f g e = case e of
    PullshoutEdge l -> PullshoutEdge (g l)
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
isPullshout e = case e.details of
    PullshoutEdge _ -> True
    NormalEdge _ -> False

isNormalId : Graph NodeLabel EdgeLabel -> Graph.Id -> Bool
isNormalId g id = Graph.get id (always True)
    isNormal g |> Maybe.withDefault False

mapNormalEdge : (NormalEdgeLabel -> NormalEdgeLabel) -> EdgeLabel -> EdgeLabel
mapNormalEdge f = mapEdgeType f identity |> mapDetails

mapPullshoutEdge : (PullshoutEdgeLabel -> PullshoutEdgeLabel) -> EdgeLabel -> EdgeLabel
mapPullshoutEdge f  = mapEdgeType identity f |> mapDetails


mapMaybeNormalEdge : (NormalEdgeLabel -> Maybe NormalEdgeLabel) -> EdgeLabel -> Maybe EdgeLabel
mapMaybeNormalEdge f e = 
  case e.details of
    PullshoutEdge _ -> Nothing
    NormalEdge l -> case (f l) of 
                      Nothing -> Nothing
                      Just x -> Just { e | details = NormalEdge x}

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

getProofFromLabel : String -> Maybe String
getProofFromLabel s =
   let s2 = String.trim s in
   let prefix = "\\" ++ coqProofTexCommand ++ "{" in
   if String.startsWith prefix s2 then
      Just (String.slice (String.length prefix) (-1) s2)
   else
      Nothing

isProofLabel : NodeLabel -> Bool
isProofLabel l = getProofFromLabel l.label /= Nothing

getProofNodes : Graph NodeLabel EdgeLabel -> List (Node NodeLabel)
getProofNodes g =
   Graph.nodes g |> List.filter (\n -> isProofLabel n.label)


selectedProofNode : Graph NodeLabel EdgeLabel -> Maybe (Node NodeLabel, String)
selectedProofNode g =
  case selectedNode g |> Maybe.map 
     (\n -> (n, getProofFromLabel n.label.label)) of
      Nothing -> Nothing
      Just (_, Nothing) -> Nothing
      Just (n, Just s) -> Just (n, s)

type MaybeProofDiagram =
     NoProofNode
   | NoDiagram
   | JustDiagram { proof : String, diagram : Diagram}

-- returns the diagram around the selected proof
getSelectedProofDiagram : Graph NodeLabel EdgeLabel -> MaybeProofDiagram
getSelectedProofDiagram g =
  case selectedProofNode g of
    Nothing -> NoProofNode
    Just (n, s) ->
       case getSurroundingDiagrams n.label.pos g of 
         [] -> NoDiagram
         d :: _ -> JustDiagram { proof = s, diagram = d }

toProofGraph :  Graph NodeLabel EdgeLabel -> Graph LoopNode LoopEdge
toProofGraph = 
    Graph.removeLoops >>
    posGraph >>
    Graph.filterMap Just (
      \e -> case (filterLabelNormal e.label, e.shape) of 
             (Just l, Bezier b) -> Just { details = l.details, bezier = b }
             (_ , _) -> Nothing
      )
    >>
    Graph.map 
             (\ _ n -> { pos = n.pos, label = n.label, proof = getProofFromLabel n.label })
             (\ _ {details, bezier}  -> 
                        { angleIn = Point.subtract bezier.controlPoint bezier.from |> Point.pointToAngle ,
                          angleOut= Point.subtract bezier.controlPoint bezier.to |> Point.pointToAngle,
                          label = details.label, -- (if l.label == "" && l.style.double then fromLabel else l.label),
                          pos = Bez.middle bezier,
                          from = bezier.from,
                          to = bezier.to,
                          identity = ArrowStyle.isDouble details.style })

selectedIncompleteDiagram : Graph NodeLabel EdgeLabel -> Maybe Diagram
selectedIncompleteDiagram g = 
   let gc = (toProofGraph g) in
    GraphProof.getIncompleteDiagram gc
     <| Graph.getEdges (selectedEdges g |> List.map .id) gc

type MaybeChain =
     JustChain (Graph.ModifHelper NodeLabel EdgeLabel, Diagram)
   | NoClearOrientation
   | NoChain
{-
Returns the graph with an edge added between the minimal and the maximal points, and the diagram
-}
selectedChain : Graph NodeLabel EdgeLabel -> MaybeChain
selectedChain g = 
   let gs = selectedGraph g in
   case (Graph.minimal gs, Graph.maximal gs) of
     ([ minId ] , [ maxId ]) ->
        if minId == maxId then NoChain else
        let (weakSel, trueSel) = if isTrueSelection gs then (False, True) else (True, False) in
        let label = { emptyEdge | weaklySelected = weakSel, selected = trueSel } in
        let (newGraph, _) = Graph.md_newEdge (Graph.newModif g) minId maxId label in
        case selectedIncompleteDiagram <| Graph.applyModifHelper newGraph of
          Nothing -> NoClearOrientation
          Just d -> JustChain (newGraph, d)
     (_, _) -> NoChain

updateNormalEdge : EdgeId -> (NormalEdgeLabel -> NormalEdgeLabel) -> Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
updateNormalEdge id f =
    Graph.updateEdge id 
     (mapNormalEdge f)

md_updateNormalEdge : EdgeId -> (NormalEdgeLabel -> NormalEdgeLabel) 
   -> Graph.ModifHelper NodeLabel EdgeLabel -> Graph.ModifHelper NodeLabel EdgeLabel
md_updateNormalEdge id f =
    Graph.md_updateEdge id 
     (mapNormalEdge f)

md_updatePullshoutEdge : EdgeId -> (PullshoutEdgeLabel -> PullshoutEdgeLabel) 
   -> Graph.ModifHelper NodeLabel EdgeLabel -> Graph.ModifHelper NodeLabel EdgeLabel
md_updatePullshoutEdge id f =
    Graph.md_updateEdge id 
     (mapPullshoutEdge f)


-- edgesToModif : List (Edge EdgeLabel) -> Graph NodeLabel EdgeLabel -> Graph.ModifHelper NodeLabel EdgeLabel
-- edgesToModif edges graph =
--    -- we compare
--    let compare edge = 
--          case Graph.getEdge edge.id graph of
--             Nothing -> Nothing
--             Just edgeOriginal -> 
--                if edge == edgeOriginal then Nothing else 
--                Just edge 
--    in
--    let modifEdges = List.filterMap edges in 



setColor : Color.Color -> EdgePart -> EdgeLabel -> EdgeLabel
setColor color part e = 
   case e.details of
   NormalEdge l -> 
      let oldStyle = l.style in
      let newStyle = ArrowStyle.updateEdgeColor part color oldStyle in
      { e | details = NormalEdge { l | style = newStyle }}
   PullshoutEdge x -> {e | details = PullshoutEdge { x | color = color}} 



-- setColorEdgesId : Color.Color -> EdgePart -> List EdgeId -> Graph NodeLabel EdgeLabel -> 
--            Graph.ModifHelper NodeLabel EdgeLabel
-- setColorEdgesId color part edges graph =
--      let updateColor e = 
--            case e.details of
--             NormalEdge l -> 
--                let oldStyle = l.style in
--                let newStyle = ArrowStyle.updateEdgeColor part color oldStyle in
--                { e | details = NormalEdge { l | style = newStyle }}
--             PullshoutEdge x -> {e | details = PullshoutEdge { x | color = color}} 
--      in
--      let modif = Graph.newModif graph in
--       Graph.md_updateEdgesId edges updateColor modif 
     

updatePullshoutEdges : (PullshoutEdgeLabel -> Maybe PullshoutEdgeLabel) 
       -> List (Edge EdgeLabel) -> Graph NodeLabel EdgeLabel -> 
           Graph.ModifHelper NodeLabel EdgeLabel 
updatePullshoutEdges update edges g =
     let updateStyle e = update e.label.details 
                  |> Maybe.map (\newStyle ->
                              { id = e.id , style = newStyle }
                           )   
     in
     let newEdges =   edges
                 |> List.filterMap filterEdgePullshout
                 |> List.filterMap updateStyle
     in
     let updateEdge edge graph =            
              md_updatePullshoutEdge edge.id 
              (\ e -> edge.style)
              graph
     in
     let modif = Graph.newModif g in
     if newEdges == [] then modif else
     let newGraph = List.foldl updateEdge modif newEdges in
   --   Just
      newGraph

updateStyleEdges : (ArrowStyle -> Maybe ArrowStyle) 
        -> List (Edge EdgeLabel) -> Graph NodeLabel EdgeLabel -> 
           Graph.ModifHelper NodeLabel EdgeLabel -- Maybe (Graph NodeLabel EdgeLabel)
updateStyleEdges update edges g =
     let updateStyle e = update e.label.details.style 
                           |> Maybe.map (\newStyle ->
                              { id = e.id , style = newStyle }
                           )                    
     in
     let newEdges =   edges
                 |> List.filterMap filterEdgeNormal
                 |> List.filterMap updateStyle
     in
     let updateEdge edge graph =            
              md_updateNormalEdge edge.id 
              (\ e -> { e | style = edge.style})
              graph
     in
     let modif = Graph.newModif g in
     if newEdges == [] then modif else
     let newGraph = List.foldl updateEdge modif newEdges in
   --   Just
      newGraph


newNodeLabel : Point -> String -> Bool -> Int -> NodeLabel
newNodeLabel p s isMath zindex = 
    { pos = p , label = s, dims = Nothing, selected = False, weaklySelected = False,
                         isMath = isMath, zindex = zindex, isCoqValidated = False}
                     

makeProofString : String -> String
makeProofString s = "\\" ++ coqProofTexCommand ++ "{" ++ s ++ "}"

newProofLabel : Point -> String -> NodeLabel
newProofLabel p s =
   newNodeLabel p (makeProofString s) True defaultZ

newGenericLabel : a -> GenericEdge a
newGenericLabel d = { details = d,
                      selected = False,
                      weaklySelected = False,
                      zindex = defaultZ}



newEdgeLabelVerbatimAdj : Bool -> Bool -> String -> ArrowStyle -> EdgeLabel
newEdgeLabelVerbatimAdj isVerbatim isAdjunction s style = 
   newGenericLabel 
    <| NormalEdge 
    { label = makeVerbatimLabel isVerbatim s, style = style, dims = Nothing, isAdjunction = isAdjunction}

newEdgeLabelAdj : String -> ArrowStyle -> Bool -> EdgeLabel
newEdgeLabelAdj s style isAdjunction = 
   newEdgeLabelVerbatimAdj False isAdjunction s style


newEdgeLabel : String -> ArrowStyle -> EdgeLabel
newEdgeLabel s style = newEdgeLabelAdj s style False

newPullshout : PullshoutEdgeLabel -> EdgeLabel
newPullshout l = newGenericLabel <| PullshoutEdge l


emptyEdge : EdgeLabel
emptyEdge = newEdgeLabel "" ArrowStyle.empty


createNodeLabel : Graph NodeLabel EdgeLabel -> String -> Point -> (Graph NodeLabel EdgeLabel,
                                                                       NodeId, Point)
createNodeLabel g s p =
    let label = newNodeLabel p s True defaultZ in
    let (g2, id) = Graph.newNode g label in
     (g2, id, p)

md_createNodeLabel : Graph.ModifHelper NodeLabel EdgeLabel -> String -> Point -> (Graph.ModifHelper NodeLabel EdgeLabel,
                                                                       NodeId, Point)
md_createNodeLabel =
   md_createNodeLabelVerbatim False

md_createNodeLabelVerbatim : Bool -> Graph.ModifHelper NodeLabel EdgeLabel -> 
            String -> Point -> (Graph.ModifHelper NodeLabel EdgeLabel,
                                                                       NodeId, Point)
md_createNodeLabelVerbatim isVerbatim g s p =
    let label = newNodeLabel p (makeVerbatimLabel isVerbatim s) True defaultZ in
    let (g2, id) = Graph.md_newNode g label in
     (g2, id, p)

createProofNodeLabel : String -> Bool -> Point -> NodeLabel
createProofNodeLabel s coqValidated p =
    let label = newProofLabel p s in
    { label | isCoqValidated = coqValidated } 

createProofNode : Graph NodeLabel EdgeLabel -> String -> Bool -> Point -> Graph NodeLabel EdgeLabel
createProofNode g s coqValidated p =
    let (g2, id) = Graph.newNode g <| createProofNodeLabel s coqValidated p in
     g2

createValidProofAtBarycenter : Graph.ModifHelper NodeLabel EdgeLabel -> List (Node NodeLabel) -> String -> Graph.ModifHelper NodeLabel EdgeLabel
createValidProofAtBarycenter g nodes proof =
   let nodePositions = List.map (.label >> .pos) <| nodes in
   let (g2, id) = Graph.md_newNode g <| createProofNodeLabel proof True
        <| Geometry.Point.barycenter nodePositions   
   in
     g2

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

isTrueSelection : Graph NodeLabel EdgeLabel -> Bool
isTrueSelection g = Graph.any .selected .selected g

fieldSelect : Graph NodeLabel EdgeLabel -> ({ a | selected : Bool, weaklySelected : Bool} -> Bool)
fieldSelect g = if isTrueSelection g then .selected else .weaklySelected

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
    
selectedIds : Graph NodeLabel EdgeLabel -> List Graph.Id 
selectedIds g = (List.map .id <| selectedNodes g)
          ++ (List.map .id <| selectedEdges g) 

selectedId : Graph NodeLabel EdgeLabel -> Maybe Graph.Id
selectedId g = 
   case selectedIds g of
      [ x ] -> Just x
      _ -> Nothing


removeSelected : Graph NodeLabel EdgeLabel -> Graph.ModifHelper NodeLabel EdgeLabel
removeSelected g = 
   let f = fieldSelect g in
   Graph.md_drop f f <| Graph.newModif g

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
          (.details >> filterNormalEdges >> 
            Maybe.andThen 
               (\ l -> if l.isAdjunction then Nothing else Just l.label))
          |> Maybe.join

selectIds : List Graph.Id -> Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
selectIds ids graph =
   clearSelection graph 
   |> Graph.updateList
   ids (\n -> {n | selected = True})  (\n -> {n | selected = True})

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
      weaklySelectMany [id]
weaklySelectMany : List Graph.Id -> Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
weaklySelectMany ids g =
   clearWeakSelection g 
   |> Graph.updateList ids  (\ n -> { n | weaklySelected = True})
               (\ e -> { e | weaklySelected = True}) 


selectEdges : Graph NodeLabel EdgeLabel -> List EdgeId -> Graph NodeLabel EdgeLabel
selectEdges = List.foldl (\ e -> Graph.updateEdge e (\n -> {n | selected = True}))

getSurroundingDiagrams : Point -> Graph NodeLabel EdgeLabel -> List Diagram
getSurroundingDiagrams pos gi =   
   let gp = toProofGraph gi in 
   GraphProof.getAllValidDiagrams gp 
            |> List.filter (GraphProof.isInDiag gp pos)
   

selectSurroundingDiagram : Point -> Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
selectSurroundingDiagram pos gi =   
   case getSurroundingDiagrams pos gi of
       [] -> gi
       d :: _ ->
          let edges = GraphProof.edgesOfDiag d |> IntDict.keys in
          selectEdges (clearSelection gi) edges

centerOfNodes : List (Node NodeLabel) -> Point
centerOfNodes nodes = ((Geometry.rectEnveloppe <| List.map (.pos << .label) nodes) |> Geometry.centerRect)
{-
mergeWithSameLoc : Node NodeLabel -> Graph NodeLabel EdgeLabel -> Maybe (Graph NodeLabel EdgeLabel)
mergeWithSameLoc n g =
    case getNodesAt g n.label.pos |> List.filterNot ((==) n.id) of
         [ i ] -> Just (Graph.removeLoops <| Graph.recursiveMerge i n.id g)
         _ -> Nothing
-}
findReplaceInSelected : Graph NodeLabel EdgeLabel -> {search : String, replace: String} ->  Graph.ModifHelper NodeLabel EdgeLabel
findReplaceInSelected g r =
  
  let sel = fieldSelect g in
  let repl b x = 
       if b && String.contains r.search x.label then
          Just { x | label = String.replace r.search r.replace x.label}
       else
          Nothing
  in
  Graph.md_map (\ _ n -> repl (sel n) n)
     (\_  e -> mapMaybeNormalEdge (repl (sel e)) e ) 
           <| Graph.newModif  g

distanceToNode : Point -> NodeLabel -> Float
distanceToNode p n = 
   let posDims = { pos = getNodePos n, dims = getNodeDims n } in
   let rect = Geometry.rectFromPosDims posDims in 
   Geometry.distanceToRect p rect

{- unnamedGraph : Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel
unnamedGraph = 
   Graph.keepBelow (.label >> String.isEmpty)
     (.label >> String.isEmpty) -}

posGraph : Graph NodeLabel EdgeLabel -> 
   Graph NodeLabel
         {label : EdgeLabel, shape : EdgeShape, pos : Point}
posGraph g = 
      let padding = 5 in
      let dummyBez =  {from = (0, 0), to = (2,2), controlPoint = (1,1)} in
      -- ca c'est utile pour calculer les coordonnes du symbole de pullback
      let dummyExtrem = { fromId = 0,
           fromPos = (0,0), toPos = (2,2),
           bez = dummyBez} in
      let dummyAcc id pos = {
                      id = id,
                      posDims = 
                         { pos = pos, dims = (0, 0)},
                      extrems = dummyExtrem,
                      isArrow = False
                      }
      in
      let computeEdge id n1 n2 e = 
            
             case e.details of
               PullshoutEdge {offset1, offset2} -> 
                 let h = pullshoutHat offset1 offset2 n1.extrems n2.extrems in
                 {label = e, 
                  acc = dummyAcc id h.summit,                     
                  shape = HatShape h }
               NormalEdge l ->
                  --  let dims = (padding, padding) |> Point.resize 4 in
                   let computePosDims isSource = 
                        let (n, part) = 
                                 if isSource then (n1, ArrowStyle.TailPart)
                                 else (n2, ArrowStyle.HeadPart)
                        in
                        if not n.isArrow then n.posDims else
                        let oldPosDims = n.posDims in
                        { oldPosDims | 
                          pos = Bez.point n.extrems.bez <| 
                           ArrowStyle.shiftRatioFromPart l.style part
                        }
                   in
                   let q = Geometry.segmentRectBent (computePosDims True) (computePosDims False) l.style.bend in
                   {label = e, 
                    shape = Bezier q,
                    acc = {
                     id = id,
                     isArrow = True,
                     posDims = 
                         {
                             pos = Bez.middle q,
                             dims = (padding, padding) |> Point.resize 4 
                         },
                     extrems = { fromId = n1.id, 
                                 bez = q,
                                 fromPos = n1.posDims.pos, 
                                 toPos = n2.posDims.pos
                                 -- controlPoint = q.controlPoint
                                 }
                   }
                     
                  }
            
      in
      Graph.mapRecAll     
              .acc .acc      
              (\id n -> { 
                      label = n,
                      acc = {
                        id = id,
                        isArrow = False,                        
                        extrems = dummyExtrem,
                        posDims = {
                           dims =                       
                        --  if n.editable then (0, 0) else
                        -- copied from source code of Collage                         
                                  getNodeDims n, 
                           pos = getNodePos n
                         } |> Geometry.pad padding
                        }
                       } )
                 computeEdge
              g
   |> Graph.map 
       (\_ {label} -> label) 
       (\_ {label, shape, acc } -> 
          {pos = acc.posDims.pos, 
           shape = shape, 
           label = label })

closest : Point -> Graph NodeLabel EdgeLabel -> Graph.Id
-- ordered by distance to Point
closest pos ug =
   
   case getNodesAt ug pos of
     t :: _ -> t
     _ -> 
        let edgeDistance e = e.pos |>            
                        Point.distance pos
                        -- |> Just
        in
        let ug2 = posGraph ug 
                 |> Graph.map 
                   (always <| distanceToNode pos)
                   (always edgeDistance)
        in
   
        let unnamedEdges = Graph.edges ug2 
                          |> List.map 
                             (\ {id, label} -> {id = id, label = label})
            unnamedNodes = Graph.nodes ug2 
        in
        let unnamedAll = unnamedEdges ++ unnamedNodes 
               |> List.minimumBy .label 
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

invertEdges : Graph NodeLabel EdgeLabel -> List EdgeId -> Graph.ModifHelper NodeLabel EdgeLabel
invertEdges g ids =
   List.foldl (Graph.md_invertEdge)
   (Graph.md_updateEdgesId ids
      (mapNormalEdge (\ l -> { l | style = ArrowStyle.invert l.style}))
      <| Graph.newModif g
   )
   ids
   
   


-- doesn't update the color
keyMaybeUpdatePullshout : Key -> { a | offset1 : Float, offset2 : Float} -> Maybe { a | offset1 : Float, offset2 : Float}
keyMaybeUpdatePullshout k style = 
   case k of         
        Character 'b' -> Just <| {style | offset1 = style.offset1 + 5 }
        Character 'B' -> Just <| {style | offset1 = max (style.offset1 - 5) 5}
        Character ']' -> Just <| {style | offset2 = style.offset2 + 5 }
        Character '[' -> Just <| {style | offset2 = max (style.offset2 - 5) 5}

        _ -> Nothing