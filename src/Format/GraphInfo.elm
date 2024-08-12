module Format.GraphInfo exposing (GraphInfo, Tab, TabId, emptyTab, isActiveTab, normalise, 
    getActiveTab, getActiveTabOption, getActiveSizeGrid, applyModif, applyModifSimple, Modif(..), ModifInfo
    , activeGraphModif, activeGraphModifHelper, getActiveGraph, getGraph, updateGraph
    , ModifResult, makeGraphChange, getTabById --, getActiveGraphOption
    -- getActiveGraphOption
    )
-- the data that we want to copy/save
import Polygraph as Graph exposing (Graph)
import GraphDefs exposing (EdgeLabel, NodeLabel)
import List.Extra as ListExtra
import ListExtraExtra as ListExtra
import Modif
import IntDict exposing (IntDict)

-- defaultGridSize : Int
-- defaultGridSize = 200

type alias Tab = 
  { graph : Graph.Graph NodeLabel EdgeLabel,
    title : String,
    -- active : Bool,
    id : TabId,
    sizeGrid : Int }

type alias TabId = Int

type alias GraphInfo = { tabs : List Tab,
                         nextTabId : TabId,
                         activeTabId : TabId,
                         latexPreamble : String}

emptyTab : Int -> Tab
emptyTab id = { id = id,
           title = "1", sizeGrid = 200, graph = Graph.empty}

isActiveTab : {a | activeTabId : TabId} -> Tab -> Bool
isActiveTab gi tab = tab.id == gi.activeTabId

getTabById : { a | tabs : List Tab } -> TabId -> Maybe Tab
getTabById {tabs} id = ListExtra.find (\tab -> tab.id == id) tabs

getGraph : GraphInfo -> TabId -> Maybe (Graph NodeLabel EdgeLabel)
getGraph gi id = 
  getTabById gi id |> Maybe.map .graph

updateTab : GraphInfo -> TabId -> (Tab -> Tab)
               -> GraphInfo
updateTab gi id f =
   { gi | tabs = List.map (\tab -> if tab.id == id then f tab else tab)
          gi.tabs }


updateGraph : GraphInfo -> TabId -> (Graph NodeLabel EdgeLabel -> Graph NodeLabel EdgeLabel)
               -> GraphInfo
updateGraph gi id f =
   updateTab gi id (\tab -> {tab | graph = f tab.graph})

getActiveTab : {a | tabs : List Tab, activeTabId : TabId} -> Tab
getActiveTab gi =
    getActiveTabOption gi
    |> Maybe.withDefault (emptyTab 0)

getActiveGraph : GraphInfo -> Graph.Graph NodeLabel EdgeLabel
getActiveGraph gi = getActiveTab gi |> .graph


getActiveTabOption : {a | tabs : List Tab, activeTabId : TabId} -> Maybe Tab
getActiveTabOption gi =
    ListExtra.find (isActiveTab gi) gi.tabs
{-
getActiveGraphOption : GraphInfo -> Maybe (Graph.Graph NodeLabel EdgeLabel)
getActiveGraphOption =
   getActiveTabOption >> Maybe.map .graph
-}
normalise : GraphInfo -> GraphInfo
normalise gi = 
   let idx = ListExtra.findIndex (isActiveTab gi) gi.tabs
            |> Maybe.withDefault 0
   in
  { gi | tabs = List.indexedMap 
                 (\id tab -> { tab | id = id, graph = Graph.normalise tab.graph }) 
                  gi.tabs 
      , activeTabId = idx

  }



type Modif =
    TabRename TabId String
  | TabSizeGrid TabId Int
  | TabMoveLeft TabId
  | TabMoveRight TabId
  | TabRemove TabId
  | TabUnremove Tab
  | TabDuplicate TabId
  | TabNew
  | GraphChange {tabId : TabId,
                 modif : Graph.Modif NodeLabel EdgeLabel }
  | LatexPreamble String
  | Noop


makeGraphChange : TabId ->
                 Graph.Modif NodeLabel EdgeLabel ->
                 Modif
makeGraphChange tabId modif =
    GraphChange { tabId = tabId, modif = modif}
    --  selIds = IntDict.empty, renIds = []}

type alias ModifInfo = Modif.Result GraphInfo Modif

activeGraphModif : GraphInfo -> Graph.Modif NodeLabel EdgeLabel -> Modif
activeGraphModif gi modif = 
   makeGraphChange gi.activeTabId modif
   
activeGraphModifHelper : GraphInfo -> Graph.ModifHelper NodeLabel EdgeLabel -> Modif
activeGraphModifHelper gi = Graph.finaliseModif >> activeGraphModif gi




applyTabMove : Bool -> TabId -> GraphInfo -> Maybe { next : GraphInfo, undo : Modif }
applyTabMove isRight id gi = 
   if List.length gi.tabs < 2 || getTabById gi id == Nothing then Nothing else
      Just { next = { gi | tabs = 
                         (if isRight then ListExtra.moveRightCycle
                          else ListExtra.moveLeftCycle
                         ) 
                         (.id >> (==) id) gi.tabs },
             undo = TabMoveRight id }


nextTabName : GraphInfo -> String
nextTabName m = 
   let n = Maybe.withDefault 0 <| List.maximum <| List.filterMap (.title >> String.toInt) m.tabs in
   String.fromInt (1 + n)



initialiseNewTab : GraphInfo ->
                     { a | graph : Graph.Graph NodeLabel EdgeLabel, 
                            sizeGrid : Int} -> GraphInfo
initialiseNewTab m tab =
  let title = nextTabName m in
  { m | tabs = m.tabs ++ 
    [ 
    { graph = tab.graph ,
        sizeGrid = tab.sizeGrid ,
        title = title ,
        id = m.nextTabId
      }
    ],
    nextTabId = m.nextTabId + 1
    , activeTabId = m.nextTabId
  }


getActiveSizeGrid : GraphInfo -> Int
getActiveSizeGrid m = getActiveTab m |> .sizeGrid

createNewTab : GraphInfo -> GraphInfo
createNewTab m =
  let sizeGrid = getActiveSizeGrid m in
  initialiseNewTab m { graph = Graph.empty, sizeGrid = sizeGrid, title = nextTabName m }
  

duplicateTab : GraphInfo -> Tab -> GraphInfo
duplicateTab m tab =
  initialiseNewTab m tab
  
applyModifSimple : GraphInfo -> Modif -> Modif.Result GraphInfo Modif
applyModifSimple gi m =
  applyModif gi m |> Modif.map .graphInfo identity



mapTabModifInfo : GraphInfo -> TabId -> 
                  (Tab -> Modif.Result (Tab, Graph.TranslationId) Modif)
                  -> Modif.Result                         
                           { graphInfo : GraphInfo
                           , idTranslator : Graph.TranslationId }
                        Modif
mapTabModifInfo gi id f  = 
  case ListExtra.findIndex (\tab -> tab.id == id) gi.tabs of
    Just idx -> 
      Maybe.andThen f (ListExtra.getAt idx gi.tabs)
      |> Modif.map 
          (\(tab, translator) -> 
             {idTranslator = translator,
              graphInfo = { gi | tabs = ListExtra.setAt idx tab gi.tabs }})
          identity
    Nothing -> Nothing

type alias ModifResult = Modif.Result {graphInfo : GraphInfo,
                                       idTranslator : Graph.TranslationId}
                                       Modif 

applyModif : GraphInfo -> Modif -> ModifResult
applyModif gi modif =
  let retModif = Modif.map (\ g -> {graphInfo = g, idTranslator = Graph.defaultTranslation}) 
              identity 
  in  
  let retTabModif = Modif.map (\ tab -> (tab, Graph.defaultTranslation)) identity in  
  case modif of
    Noop -> retModif Nothing
    LatexPreamble s -> 
      if s == gi.latexPreamble then Nothing else
      retModif <| Just { next = { gi | latexPreamble = s }, undo = LatexPreamble gi.latexPreamble }
    TabMoveLeft id ->
      retModif <| applyTabMove False id gi
    TabMoveRight id ->
      retModif <| applyTabMove True id gi
    TabSizeGrid id size ->
      mapTabModifInfo gi id <| 
          (\tab ->
               retTabModif <|
              if size == tab.sizeGrid then Nothing else
              Just { next
               = { tab | sizeGrid = size }, undo = TabSizeGrid id tab.sizeGrid }) 
    TabRename id s -> 
      mapTabModifInfo gi id <| retTabModif <<
          (\tab ->
              if s == tab.title then Nothing else
              Just { next
               = { tab | title = s }, undo = TabRename id tab.title }) 
    TabRemove id ->
       retModif <| case gi.tabs of 
         [] -> Nothing 
         [_] -> Nothing
         _ :: _ -> 
          getTabById gi id |> Maybe.map
            (\tab -> { next = { gi | 
                      tabs = List.filter (.id >> (/=) id) gi.tabs
                    -- , activeTabId = if gi.activeTabId == id then firstTab.id else gi.activeTabId
                    },
                undo = TabUnremove tab })
    TabUnremove tab ->
       retModif <| if getTabById gi tab.id /= Nothing then Nothing else 
       Just { next = { gi | tabs = gi.tabs ++ [tab] }, undo = TabRemove tab.id }
    TabNew -> 
       retModif <| Just { next = createNewTab gi, undo = TabRemove gi.nextTabId }
    TabDuplicate id ->
       retModif <| (getTabById gi id |> Maybe.map
            (\tab -> 
              { next = duplicateTab gi tab, undo = TabRemove gi.nextTabId }))
    GraphChange arg ->
       mapTabModifInfo gi arg.tabId
            (\tab -> Graph.applyModifTrans GraphDefs.mergeFunctions tab.graph arg.modif
            |> Modif.map 
                (\{translationId,graph} -> ({tab | graph = graph}, translationId)
                   
                 )
                (\m -> makeGraphChange arg.tabId m)
                )
